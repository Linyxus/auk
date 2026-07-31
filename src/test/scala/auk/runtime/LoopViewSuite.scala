package auk.runtime

import auk.agent.{LoopGenerationState, LoopStage, LoopView}
import auk.llm.tools.Json
import auk.loop.{Budgets, LoopEvent, LoopState, ParkReason}
import auk.runtime.LoopBridge.{Stage, Step}

/** What the loops window is handed, built from a ledger and nothing else.
  *
  * Pure all the way down: [[LoopBridge.loopView]] folds events the test wrote by hand
  * and pairs them with the phase and stage only a driver would know, so everything the
  * panel draws can be pinned without a bridge, a socket, a worker or a clock.
  */
class LoopViewSuite extends munit.FunSuite:

  private val At = "2026-07-30T12:00:00Z"

  /** A ledger under construction, in the order a driver would write it. */
  private class Ledger:
    private val events = Vector.newBuilder[LoopEvent]
    events += LoopEvent.LoopCreated("opt", "base", "head", "s0", At)
    events += LoopEvent.DefAttached(1, "source", "cut p99 latency", "a rubric", Budgets(), Json.Null, At)

    /** A generation that was checked, judged and taken into the lineage. */
    def accepted(gen: Int, p99: Double, description: String = "made it faster"): Ledger =
      started(gen)
      events += LoopEvent.AttemptSubmitted(gen, 1, artifact(p99), description, None, List("c"), At)
      events += LoopEvent.CheckCompleted(gen, 1, true, Nil, metrics(p99), At)
      events += LoopEvent.VerdictIssued(gen, 1, true, "good", false, At)
      events += LoopEvent.GenerationAccepted(gen, s"snap-$gen", s"commit-$gen", description, metrics(p99), At)
      lastAccepted = Some(gen)
      this

    /** A generation that spent its attempts and produced nothing worth keeping. */
    def abandoned(gen: Int): Ledger =
      started(gen)
      events += LoopEvent.GenerationAbandoned(gen, 2, None, At)
      this

    /** A generation left open — the shape a driver is in the middle of. */
    def inFlight(gen: Int, attempts: Int = 0): Ledger =
      started(gen)
      (1 to attempts).foreach: a =>
        events += LoopEvent.AttemptSubmitted(gen, a, artifact(90), "a try", None, List("c"), At)
        events += LoopEvent.CheckCompleted(gen, a, false, List("too slow"), Map.empty, At)
      this

    def parked(reason: ParkReason): Ledger =
      events += LoopEvent.Parked(reason, At)
      this

    // A generation branches from the last one ACCEPTED, which is not gen - 1 whenever
    // the generation before it was abandoned.
    private var lastAccepted: Option[Int] = None

    private def started(gen: Int): Unit =
      events += LoopEvent.GenerationStarted(gen, lastAccepted, "s0", At)

    private def artifact(p99: Double): Json = Json.Obj(List("p99Ms" -> Json.num(p99)))
    private def metrics(p99: Double): Map[String, Double] = Map("p99Ms" -> p99, "allocMb" -> 12.0)

    def state: LoopState = LoopState.fold(events.result()) match
      case Right(s)    => s
      case Left(error) => fail(s"the ledger this test wrote does not fold: $error")

  /** The fold as the session driving the loop sees it — which is every case here bar
    * the orphan, the one phase only a loop read off disk can be in. */
  private def view(ledger: Ledger, phase: String, stage: Option[Stage] = None, held: Boolean = true): LoopView =
    LoopBridge.loopView("opt", phase, stage, ledger.state, held = held)

  test("the lineage keeps every generation number, marking which were accepted and which were not"):
    val v = view(Ledger().accepted(1, 90).abandoned(2).accepted(3, 70), LoopBridge.runningPhase(4))
    assertEquals(v.generations.map(_.gen), Vector(1, 2, 3))
    assertEquals(
      v.generations.map(_.state),
      Vector(LoopGenerationState.Accepted, LoopGenerationState.Abandoned, LoopGenerationState.Accepted)
    )
    // Only an acceptance carries what was measured and what it claimed to do.
    assertEquals(v.generations(0).metrics, Vector("allocMb" -> 12.0, "p99Ms" -> 90.0))
    assertEquals(v.generations(1).metrics, Vector.empty)
    assertEquals(v.generations(0).description, "made it faster")
    assertEquals(v.generations(1).description, "")
    assertEquals(v.goal, "cut p99 latency")

  test("the generation in flight is marked as running, not as abandoned"):
    val v = view(Ledger().accepted(1, 90).inFlight(2), LoopBridge.runningPhase(2))
    assertEquals(v.generations.map(_.state), Vector(LoopGenerationState.Accepted, LoopGenerationState.Running))
    assertEquals(v.latestAccepted.map(_.gen), Some(1))

  test("the headline metric is the newest acceptance's first key, carrying the one before it"):
    val v = view(Ledger().accepted(1, 90).abandoned(2).accepted(3, 70), LoopBridge.runningPhase(4))
    val headline = v.headline.getOrElse(fail("a loop with two accepted generations has a headline"))
    // Key order, so the panel does not reshuffle between frames: allocMb sorts first.
    assertEquals(headline.key, "allocMb")
    assertEquals(headline.value, 12.0)
    // The comparison skips the generation that was abandoned — it measured nothing.
    assertEquals(headline.previous, Some(12.0))
    assertEquals(v.generations(2).metricsText, "allocMb 12 · p99Ms 70")

  test("a headline with nothing before it has no previous, and a loop with no lineage has no headline"):
    val first = view(Ledger().accepted(1, 90), LoopBridge.runningPhase(2))
    assertEquals(first.headline.flatMap(_.previous), None)
    val bare = view(Ledger().inFlight(1), LoopBridge.runningPhase(1))
    assertEquals(bare.headline, None)
    assertEquals(bare.latestAccepted, None)

  test("whole measurements read without a trailing zero, fractional ones as they are"):
    assertEquals(LoopView.number(42.0), "42")
    assertEquals(LoopView.number(-7.0), "-7")
    assertEquals(LoopView.number(1.5), "1.5")

  test("the stage says which agent is running, which attempt, and where its transcript is"):
    val ledger = Ledger().accepted(1, 90).inFlight(2, attempts = 1)
    val working = view(ledger, LoopBridge.runningPhase(2), Some(Stage(2, 2, Step.Working)))
    assertEquals(working.activity, Some("gen 2, attempt 2 — working"))
    assertEquals(working.liveLabel, Some("gen-2-worker"))
    val evaluating = view(ledger, LoopBridge.runningPhase(2), Some(Stage(2, 2, Step.Evaluating)))
    assertEquals(evaluating.activity, Some("gen 2, attempt 2 — evaluating"))
    assertEquals(evaluating.liveLabel, Some("gen-2-eval"))
    // A check runs as a closure in the gate worker and streams nothing, so it leaves
    // the worker's transcript up — which is what a reader wants while it is measured.
    val checking = view(ledger, LoopBridge.runningPhase(2), Some(Stage(2, 2, Step.Checking)))
    assertEquals(checking.activity, Some("gen 2, attempt 2 — checking"))
    assertEquals(checking.liveLabel, Some("gen-2-worker"))

  test("the same stage travels as parts, so a reader draws it rather than parsing it"):
    val ledger = Ledger().accepted(1, 90).inFlight(2, attempts = 1)
    def stageOf(step: Step): Option[LoopStage] =
      view(ledger, LoopBridge.runningPhase(2), Some(Stage(2, 2, step))).stage
    assertEquals(stageOf(Step.Working), Some(LoopStage(2, 2, "working")))
    assertEquals(stageOf(Step.Checking), Some(LoopStage(2, 2, "checking")))
    assertEquals(stageOf(Step.Evaluating), Some(LoopStage(2, 2, "evaluating")))
    // The one vocabulary, so the sentence and the structure cannot disagree.
    val v = view(ledger, LoopBridge.runningPhase(2), Some(Stage(2, 2, Step.Evaluating)))
    assertEquals(v.activity, v.stage.map(s => s"gen ${s.gen}, attempt ${s.attempt} — ${s.step}"))

  test("the transcript labels the panel reads are the ones the tee writes"):
    assertEquals(LoopBridge.workerTranscriptLabel(3), "gen-3-worker")
    assertEquals(LoopBridge.evalTranscriptLabel(3), "gen-3-eval")

  test("between generations nothing is claimed to be in flight"):
    val v = view(Ledger().accepted(1, 90), LoopBridge.runningPhase(1))
    assertEquals(v.activity, None)
    assertEquals(v.liveLabel, None)
    assertEquals(v.stage, None)
    assert(v.live, "a running loop between generations is still live")

  test("a parked loop carries its reason without the phase's prefix, and is not live"):
    val ledger = Ledger().accepted(1, 90).parked(ParkReason.BudgetExhausted)
    val v = view(ledger, LoopBridge.phaseFor(ParkReason.BudgetExhausted))
    assertEquals(v.parked, Some("budget exhausted"))
    assertEquals(v.orphaned, false)
    assert(!v.live, "a parked loop is not being driven")

  test("an orphaned loop is neither parked nor live, and says so on its own"):
    val v = view(Ledger().accepted(1, 90).inFlight(2), LoopBridge.Orphaned, held = false)
    assertEquals(v.parked, None)
    assertEquals(v.orphaned, true)
    assertEquals(v.held, false)
    assert(!v.live, "a loop nobody is driving is not live")

  test("a loop being adopted reads as live even though its ledger still says parked"):
    // The resume is only written once the stored definition has re-checked, so the
    // ledger lags the phase here — and the phase is what the panel must believe.
    val ledger = Ledger().accepted(1, 90).parked(ParkReason.UserRequested)
    val v = view(ledger, LoopBridge.Adopting)
    assertEquals(v.parked, None)
    assert(v.live, "a loop being picked up is being worked on")

  test("a loop still being validated is reported from the configuration it arrived with"):
    val v = LoopBridge.pendingView("opt", LoopBridge.Validating, "cut p99 latency\nand keep it there")
    assertEquals(v.phase, LoopBridge.Validating)
    assertEquals(v.goal, "cut p99 latency")
    assertEquals(v.generations, Vector.empty)
    assertEquals(v.stage, None, "a loop with no generation yet is at no stage of one")
    assert(v.live, "a loop on its way to running is live")
    assert(v.held, "the session validating a loop is the one holding it")

  test("a goal written as a paragraph is reduced to its first line"):
    val ledger = Ledger()
    val v = LoopBridge.loopView("opt", LoopBridge.Validating, None, ledger.state, held = true)
    assertEquals(v.goal, "cut p99 latency")
    assertEquals(v.generations, Vector.empty)
