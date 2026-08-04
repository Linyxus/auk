package auk.runtime

import auk.TestFs
import auk.llm.tools.Json
import auk.loop.{Budgets, LoopEvent, LoopStore, ParkReason}
import auk.platform.PathOps

/** What a session says about the loops it finds in `.auk/loops` when it opens.
  *
  * Pure: [[LoopStartup]] reads ledgers and renders text, and nothing here spawns a
  * worker, a bridge or a model. The ledgers are written by hand, which is also the
  * point — this is the code path that runs against a directory some OTHER session left
  * behind, so building the input by hand is closer to the truth than driving a loop
  * would be.
  */
class LoopStartupSuite extends munit.FunSuite:

  private def store(): LoopStore =
    LoopStore(PathOps.join(TestFs.tempDir("auk-loop-startup"), LoopStore.AukRelativePath))

  private val At = "2026-07-30T12:00:00Z"

  /** A ledger for a loop that accepted `accepted` generations and then stopped for
    * `parked` (or was left running by a session that ended, when that is `None`). */
  private def ledger(
      store: LoopStore,
      id: String,
      goal: String = "cut p99 latency",
      accepted: Int = 0,
      abandoned: Int = 0,
      parked: Option[ParkReason] = Some(ParkReason.UserRequested)
  ): Unit =
    store.append(id, LoopEvent.LoopCreated(id, "base", "head", "s0", At))
    store.append(id, LoopEvent.DefAttached(1, "source", goal, "a rubric", Budgets(), Json.Null, At))
    var gen = 0
    (1 to accepted).foreach: _ =>
      gen += 1
      val artifact = Json.Obj(List("p99Ms" -> Json.num(100 - gen)))
      store.append(id, LoopEvent.GenerationStarted(gen, Some(gen - 1).filter(_ > 0), "s0", At))
      store.append(id, LoopEvent.AttemptSubmitted(gen, 1, artifact, s"gen $gen", None, List("c"), 1000, 100, At))
      store.append(id, LoopEvent.CheckCompleted(gen, 1, true, Nil, Map("p99Ms" -> (100.0 - gen)), At))
      store.append(id, LoopEvent.VerdictIssued(gen, 1, true, "good", false, 300, 30, At))
      store.append(id, LoopEvent.GenerationAccepted(gen, s"snap-$gen", s"commit-$gen", s"gen $gen did it",
        Map("p99Ms" -> (100.0 - gen)), 0, 0, At))
    (1 to abandoned).foreach: _ =>
      gen += 1
      store.append(id, LoopEvent.GenerationStarted(gen, None, "s0", At))
      store.append(id, LoopEvent.GenerationAbandoned(gen, 2, None, 0, 0, At))
    parked.foreach(reason => store.append(id, LoopEvent.Parked(reason, At)))
    ()

  test("a project with no loops says nothing at all"):
    val s = store()
    assertEquals(LoopStartup.scan(s), Nil)
    assertEquals(LoopStartup.section(Nil), None)

  test("a parked loop is found with where it stopped and how far it got"):
    val s = store()
    ledger(s, "opt", accepted = 3, parked = Some(ParkReason.BudgetExhausted))
    val found = LoopStartup.scan(s)
    assertEquals(found.map(_.id), List("opt"))
    val opt = found.head
    assertEquals(opt.phase, "parked: budget exhausted")
    assertEquals(opt.latestGen, Some(3))
    assertEquals(opt.metrics, Map("p99Ms" -> 97.0))
    assertEquals(opt.started, 3)
    assertEquals(opt.goal, "cut p99 latency")
    assert(!opt.settled, "a loop that ran out of budget has not finished")

  test("a loop whose session ended while it ran reads as orphaned, not as parked"):
    val s = store()
    ledger(s, "opt", accepted = 1, parked = None)
    val opt = LoopStartup.scan(s).head
    assertEquals(opt.phase, LoopBridge.Orphaned)
    assert(!opt.settled)

  test("a loop that reached its goal is finished, and no session is greeted with it again"):
    val s = store()
    ledger(s, "done", accepted = 2, parked = Some(ParkReason.GoalReached))
    val found = LoopStartup.scan(s)
    assert(found.head.settled, "a loop parked on goal reached is history")
    assertEquals(LoopStartup.unfinished(found), Nil)
    assertEquals(LoopStartup.section(found), None)

  test("a ledger that cannot be folded is left out rather than reported"):
    val s = store()
    ledger(s, "fine", accepted = 1)
    // A ledger whose first event is not a creation cannot describe a loop. Startup is
    // not the place to complain about it: nobody has asked for this loop yet.
    s.append("broken", LoopEvent.Parked(ParkReason.UserRequested, At))
    assertEquals(LoopStartup.scan(s).map(_.id), List("fine"))

  test("the prompt section names each loop, where it stopped, what it measured and what it was for"):
    val s = store()
    ledger(s, "alpha", goal = "cut p99 latency by 30%", accepted = 2, parked = Some(ParkReason.PatienceExhausted))
    ledger(s, "beta", goal = "migrate the parser", accepted = 0, abandoned = 1, parked = None)
    ledger(s, "gamma", goal = "already done", accepted = 1, parked = Some(ParkReason.GoalReached))
    val section = LoopStartup.section(LoopStartup.scan(s)).getOrElse(fail("expected a section"))

    assert(section.contains("- `alpha` (parked: patience exhausted): accepted through generation 2 of 2 started, measured p99Ms=98 — goal: cut p99 latency by 30%"), section)
    assert(section.contains(s"- `beta` (${LoopBridge.Orphaned}): 1 generation(s) started, none accepted — goal: migrate the parser"), section)
    // The finished loop is not offered: it is a completed piece of work.
    assert(!section.contains("gamma"), section)

    // It says how to act on them, and that being listed is not a reason to.
    assert(section.contains("""lib.loop.get("<id>").resume()"""), section)
    assert(section.contains(".reconfigure(...)"), section)
    assert(section.contains(".amend(...)"), section)
    assert(section.contains("resume it when the user asks"), section)

  test("a loop with nothing accepted yet says so instead of naming a generation"):
    val s = store()
    ledger(s, "fresh", accepted = 0, parked = Some(ParkReason.UserRequested))
    val section = LoopStartup.section(LoopStartup.scan(s)).getOrElse(fail("expected a section"))
    assert(section.contains("nothing started yet"), section)

  // -- the orphan transcript note ------------------------------------------------------

  test("nothing was left running ⇒ no note, and a PARKED loop is not something left running"):
    val s = store()
    assertEquals(LoopStartup.orphanNote(Nil), None)
    ledger(s, "opt", accepted = 1, parked = Some(ParkReason.BudgetExhausted))
    ledger(s, "paused", accepted = 0, parked = Some(ParkReason.UserRequested))
    // Both were parked on purpose: the window lists them and the prompt section names
    // them, and saying so again at every session open is the noise this replaced.
    assertEquals(LoopStartup.orphanNote(LoopStartup.scan(s)), None)

  test("one loop left running by a dead session is named in a single sentence"):
    val s = store()
    ledger(s, "slowlang", accepted = 2, parked = None)
    ledger(s, "opt", accepted = 1, parked = Some(ParkReason.BudgetExhausted))
    assertEquals(
      LoopStartup.orphanNote(LoopStartup.scan(s)),
      Some("a loop ('slowlang') was left running by a session that ended — ctrl+c l to view")
    )

  test("several of them pluralise, and every one is named"):
    val s = store()
    ledger(s, "a", accepted = 1, parked = None)
    ledger(s, "b", accepted = 0, parked = None)
    assertEquals(
      LoopStartup.orphanNote(LoopStartup.scan(s)),
      Some("2 loops ('a', 'b') were left running by sessions that ended — ctrl+c l to view")
    )

  test("a loop that reached its goal is history and never reads as left running"):
    val s = store()
    ledger(s, "done", accepted = 2, parked = Some(ParkReason.GoalReached))
    assertEquals(LoopStartup.orphanNote(LoopStartup.scan(s)), None)
    // …nor does it mask a real one sitting beside it.
    ledger(s, "stray", accepted = 1, parked = None)
    assertEquals(
      LoopStartup.orphanNote(LoopStartup.scan(s)),
      Some("a loop ('stray') was left running by a session that ended — ctrl+c l to view")
    )
