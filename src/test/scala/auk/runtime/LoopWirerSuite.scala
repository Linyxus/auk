package auk.runtime

import auk.TestFs
import auk.agent.LoopView
import auk.llm.tools.Json
import auk.loop.{Budgets, LoopEvent, LoopHistory, LoopStore, ParkReason}
import auk.platform.PathOps
import auk.runtime.LoopBridge.{Stage, Step}
import auk.workflow.{LoopStageWire, LoopWire}

/** What the browser is handed for one loop: the ledger's whole story merged with
  * the live phase and stage only the driver knows.
  *
  * The merge is the interesting part, so most of these check which of the two
  * sources won — the ledger for everything historical, the view for everything
  * happening now.
  */
class LoopWirerSuite extends munit.FunSuite:

  private val At = "2026-07-30T12:00:00Z"

  private def ledger(events: LoopEvent*): Vector[LoopEvent] =
    Vector(
      LoopEvent.LoopCreated("opt", "base-commit", "head", "s0", At),
      LoopEvent.DefAttached(2, "lib.loop.start:\n  check { c => c.pass() }", "cut p99 latency\nand keep it there",
        "faster is better", Budgets(9, 2, 3), Json.Null, At)
    ) ++ events

  /** A generation that was retried once, then accepted. */
  private val worked: Vector[LoopEvent] = ledger(
    LoopEvent.GenerationStarted(1, None, "sess-a", At),
    LoopEvent.AttemptSubmitted(1, 1, Json.Obj(List("p99Ms" -> Json.num(99.0))), "first try", None, Nil, At),
    LoopEvent.CheckCompleted(1, 1, false, List("too slow"), Map.empty, At),
    LoopEvent.AttemptSubmitted(1, 2, Json.Obj(List("p99Ms" -> Json.num(70.0))), "second try", None, List("cand-commit"), At),
    LoopEvent.CheckCompleted(1, 2, true, Nil, Map("p99Ms" -> 70.0, "allocMb" -> 12.0), At),
    LoopEvent.VerdictIssued(1, 2, true, "good enough", false, At),
    LoopEvent.GenerationAccepted(1, "snap-1", "commit-1", "made it faster", Map("p99Ms" -> 70.0, "allocMb" -> 12.0), At),
    LoopEvent.GenerationStarted(2, Some(1), "sess-b", At)
  )

  private def history(events: Vector[LoopEvent]): LoopHistory =
    LoopHistory.fold(events) match
      case Right(h)    => h
      case Left(error) => fail(s"the ledger this test wrote does not fold: $error")

  /** The view the bridge would compute for a loop it is driving. */
  private def view(events: Vector[LoopEvent], phase: String, stage: Option[Stage] = None, held: Boolean = true): LoopView =
    auk.loop.LoopState.fold(events) match
      case Right(state) => LoopBridge.loopView("opt", phase, stage, state, held = held)
      case Left(error)  => fail(s"the ledger this test wrote does not fold: $error")

  private def wired(events: Vector[LoopEvent], phase: String, stage: Option[Stage] = None, held: Boolean = true): LoopWire =
    LoopWirer.wire(history(events), view(events, phase, stage, held))

  /** A store rooted at a fresh project's `.auk`, holding `events` as loop `opt`. */
  private def storeWith(events: Vector[LoopEvent]): LoopStore =
    val store = LoopStore(PathOps.join(TestFs.tempDir("auk-wirer"), LoopStore.AukRelativePath))
    events.foreach(e => store.append("opt", e).getOrElse(fail("the fixture ledger could not be written")))
    store

  // -- which source wins ---------------------------------------------------------

  test("the goal is the ledger's whole text, not the one line the TUI's strip shows"):
    val w = wired(worked, LoopBridge.runningPhase(2))
    assertEquals(w.goal, "cut p99 latency\nand keep it there")
    assertEquals(view(worked, LoopBridge.runningPhase(2)).goal, "cut p99 latency")
    assertEquals(w.rubric, "faster is better")
    assertEquals(w.defSource, "lib.loop.start:\n  check { c => c.pass() }")
    assertEquals(w.defVersion, 2)
    assertEquals(w.budgets.maxGenerations, 9)
    assertEquals(w.budgets.patience, 2)
    assertEquals(w.budgets.maxAttemptsPerGeneration, 3)
    assertEquals(w.createdAt, At)
    assertEquals(w.id, "opt")

  test("what is happening right now comes from the view, which the ledger cannot know"):
    val w = wired(worked, LoopBridge.runningPhase(2), Some(Stage(2, 1, Step.Evaluating)))
    assertEquals(w.phase, "running (gen 2)")
    assertEquals(w.activity, Some("gen 2, attempt 1 — evaluating"))
    assertEquals(w.liveLabel, Some("gen-2-eval"))
    assertEquals(w.held, true)
    assertEquals(w.parked, None)
    assertEquals(w.orphaned, false)

  test("the stage crosses as parts, so the browser draws it rather than reading the sentence"):
    def stageOf(step: Step) = wired(worked, LoopBridge.runningPhase(2), Some(Stage(2, 1, step))).stage
    assertEquals(stageOf(Step.Working), Some(LoopStageWire(2, 1, "working")))
    assertEquals(stageOf(Step.Checking), Some(LoopStageWire(2, 1, "checking")))
    assertEquals(stageOf(Step.Evaluating), Some(LoopStageWire(2, 1, "evaluating")))
    // Between generations there is no stage, exactly as there is no activity line.
    assertEquals(wired(worked, LoopBridge.runningPhase(2)).stage, None)

  test("a loop being adopted reads as its phase says, not as its ledger still does"):
    // The resume is only written once the stored definition has re-checked.
    val parked = ledger(LoopEvent.Parked(ParkReason.UserRequested, At))
    assertEquals(wired(parked, LoopBridge.Adopting).phase, LoopBridge.Adopting)
    assertEquals(wired(parked, LoopBridge.Adopting).parked, None)
    val stopped = wired(parked, LoopBridge.phaseFor(ParkReason.UserRequested))
    assertEquals(stopped.parked, Some("user requested"))

  // -- the lineage ---------------------------------------------------------------

  test("each generation carries its number, parent, state and settle time"):
    val w = wired(worked, LoopBridge.runningPhase(2))
    assertEquals(w.generations.map(_.gen), List(1, 2))
    assertEquals(w.generations.map(_.state), List("accepted", "running"))
    assertEquals(w.generations.map(_.parent), List(None, Some(1)))
    assertEquals(w.generations.map(_.settledAt), List(Some(At), None))
    assertEquals(w.generations.map(_.startedAt), List(At, At))

  test("only an accepted generation carries a commit and what was measured, in key order"):
    val w = wired(worked, LoopBridge.runningPhase(2))
    val accepted = w.generations.head
    assertEquals(accepted.commit, Some("commit-1"))
    assertEquals(accepted.description, "made it faster")
    assertEquals(accepted.metrics, List("allocMb" -> 12.0, "p99Ms" -> 70.0))
    val running = w.generations(1)
    assertEquals(running.commit, None)
    assertEquals(running.metrics, Nil)

  test("an abandoned generation reads as abandoned and keeps its last attempt's account"):
    val events = ledger(
      LoopEvent.GenerationStarted(1, None, "sess-a", At),
      LoopEvent.AttemptSubmitted(1, 1, Json.Null, "a dead end", None, Nil, At),
      LoopEvent.CheckCompleted(1, 1, false, List("no"), Map.empty, At),
      LoopEvent.GenerationAbandoned(1, 1, Some("loop/opt/gen-1-abandoned"), At)
    )
    val g = wired(events, LoopBridge.runningPhase(2)).generations.head
    assertEquals(g.state, "abandoned")
    assertEquals(g.description, "a dead end")
    assertEquals(g.commit, None)

  // -- attempts ------------------------------------------------------------------

  test("every attempt is carried with the two gates' outcomes, the refused one having no verdict"):
    val attempts = wired(worked, LoopBridge.runningPhase(2)).generations.head.attempts
    assertEquals(attempts.map(_.attempt), List(1, 2))
    assertEquals(attempts.map(_.description), List("first try", "second try"))
    assertEquals(attempts.head.check.map(_.pass), Some(false))
    assertEquals(attempts.head.check.map(_.reasons), Some(List("too slow")))
    // The checker refused it, so the evaluator never saw it.
    assertEquals(attempts.head.verdict, None)
    assertEquals(attempts(1).check.map(_.metrics), Some(List("allocMb" -> 12.0, "p99Ms" -> 70.0)))
    assertEquals(attempts(1).verdict.map(_.accepted), Some(true))
    assertEquals(attempts(1).verdict.map(_.feedback), Some("good enough"))
    assertEquals(attempts(1).verdict.map(_.goalReached), Some(false))
    assertEquals(attempts(1).at, At)

  test("an attempt says whether a diff can be asked for at all"):
    val attempts = wired(worked, LoopBridge.runningPhase(2)).generations.head.attempts
    // The first attempt snapshotted nothing, so there is nothing to diff.
    assertEquals(attempts.map(_.hasSnapshot), List(false, true))

  test("the artifact travels as JSON text"):
    val attempts = wired(worked, LoopBridge.runningPhase(2)).generations.head.attempts
    assertEquals(attempts(1).artifact, """{"p99Ms":70}""")

  // -- reading a loop off the store ----------------------------------------------

  test("fromStore re-reads the ledger behind a view"):
    val store = storeWith(worked)
    val w = LoopWirer.fromStore(store, view(worked, LoopBridge.runningPhase(2)))
      .getOrElse(fail("a loop with a ledger should wire"))
    assertEquals(w.generations.map(_.gen), List(1, 2))
    assertEquals(w.goal, "cut p99 latency\nand keep it there")

  test("a loop with no ledger yet does not wire"):
    val store = storeWith(worked)
    val pending = LoopBridge.pendingView("brand-new", LoopBridge.Validating, "a goal")
    assertEquals(LoopWirer.fromStore(store, pending), None)

  test("a loop read off disk is unheld, and one recorded as running with nobody driving it is an orphan"):
    val store = storeWith(worked)
    val w = LoopWirer.fromDisk(store, "opt").getOrElse(fail("a loop with a ledger should wire"))
    assertEquals(w.held, false)
    assertEquals(w.phase, LoopBridge.Orphaned)
    assertEquals(w.orphaned, true)
    assertEquals(w.parked, None)
    assertEquals(w.activity, None)
    assertEquals(w.liveLabel, None)
    assertEquals(w.stage, None)
    // The lineage is the ledger's own, exactly as for a held loop.
    assertEquals(w.generations.map(_.gen), List(1, 2))

  test("a loop parked on disk reads with its reason rather than as an orphan"):
    val store = storeWith(worked :+ LoopEvent.Parked(ParkReason.GoalReached, At))
    val w = LoopWirer.fromDisk(store, "opt").getOrElse(fail("a parked loop should wire"))
    assertEquals(w.phase, "parked: goal reached")
    assertEquals(w.parked, Some("goal reached"))
    assertEquals(w.orphaned, false)
    assertEquals(w.held, false)

  test("a loop the store has never heard of does not wire"):
    assertEquals(LoopWirer.fromDisk(storeWith(worked), "nobody"), None)
