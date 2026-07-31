package auk.loop

import auk.llm.tools.Json

/** The dashboard's fold, pinned against ledgers written by hand.
  *
  * Everything here is what [[LoopState]] is entitled to forget — the attempts that
  * lost, what each gate said about them, which session held a generation, which
  * commits it snapshotted — so the tests are mostly about what SURVIVES, and about
  * the fold staying readable when a ledger is stranger than the drive cycle allows.
  */
class LoopHistorySuite extends munit.FunSuite:

  private val At = "2026-07-30T12:00:00Z"

  /** A ledger under construction, in the order a driver would write it. */
  private class Ledger:
    private val events = Vector.newBuilder[LoopEvent]
    private var lastAccepted: Option[Int] = None
    events += LoopEvent.LoopCreated("opt", "base-commit", "head", "s0", At)
    events += LoopEvent.DefAttached(1, "def source", "cut p99 latency\nand keep it there", "faster is better", Budgets(), Json.Null, At)

    def raw(event: LoopEvent): Ledger =
      events += event
      this

    def started(gen: Int, session: String = "s-worker"): Ledger =
      events += LoopEvent.GenerationStarted(gen, lastAccepted, session, At)
      this

    /** An attempt the checker refused. */
    def rejected(gen: Int, attempt: Int, reason: String): Ledger =
      events += LoopEvent.AttemptSubmitted(gen, attempt, artifact(99), s"try $attempt", Some("learned a thing"), List(s"c-$gen-$attempt"), At)
      events += LoopEvent.CheckCompleted(gen, attempt, false, List(reason), Map.empty, At)
      this

    /** An attempt that passed both gates. */
    def passed(gen: Int, attempt: Int, p99: Double, description: String): Ledger =
      events += LoopEvent.AttemptSubmitted(gen, attempt, artifact(p99), description, None, List(s"c-$gen-$attempt"), At)
      events += LoopEvent.CheckCompleted(gen, attempt, true, Nil, metrics(p99), At)
      events += LoopEvent.VerdictIssued(gen, attempt, true, "good", false, At)
      this

    def accept(gen: Int, p99: Double, description: String): Ledger =
      events += LoopEvent.GenerationAccepted(gen, s"snap-$gen", s"commit-$gen", description, metrics(p99), At)
      lastAccepted = Some(gen)
      this

    def abandon(gen: Int, rescue: Option[String]): Ledger =
      events += LoopEvent.GenerationAbandoned(gen, 2, rescue, At)
      this

    def parked(reason: ParkReason): Ledger =
      events += LoopEvent.Parked(reason, At)
      this

    def resumed(session: String): Ledger =
      events += LoopEvent.Resumed(session, At)
      this

    private def artifact(p99: Double): Json = Json.Obj(List("p99Ms" -> Json.num(p99)))
    private def metrics(p99: Double): Map[String, Double] = Map("p99Ms" -> p99, "allocMb" -> 12.0)

    def result: Vector[LoopEvent] = events.result()

    def history: LoopHistory = LoopHistory.fold(result) match
      case Right(h)    => h
      case Left(error) => fail(s"the ledger this test wrote does not fold: $error")

  private def generation(h: LoopHistory, gen: Int): GenerationHistory =
    h.generation(gen).getOrElse(fail(s"expected generation $gen in ${h.generations.map(_.gen)}"))

  // -- what the loop is ----------------------------------------------------------

  test("the definition's fields are carried whole, goal included"):
    val h = Ledger().history
    assertEquals(h.loopId, "opt")
    assertEquals(h.baselineCommit, "base-commit")
    assertEquals(h.createdAt, At)
    assertEquals(h.defVersion, 1)
    assertEquals(h.defSource, "def source")
    // The whole goal, not the first line the TUI's strip shows.
    assertEquals(h.goal, "cut p99 latency\nand keep it there")
    assertEquals(h.rubric, "faster is better")
    assertEquals(h.budgets, Budgets())

  test("an amendment overlays the fields it names and a new definition resets the base"):
    val amended = Ledger()
      .raw(LoopEvent.ConfigAmended(Some("a new goal"), None, Some(Budgets(9, 1, 2)), At))
      .history
    assertEquals(amended.goal, "a new goal")
    assertEquals(amended.rubric, "faster is better")
    assertEquals(amended.budgets, Budgets(9, 1, 2))
    val reattached = Ledger()
      .raw(LoopEvent.ConfigAmended(Some("a new goal"), None, None, At))
      .raw(LoopEvent.DefAttached(2, "v2 source", "the version 2 goal", "a new rubric", Budgets(), Json.Null, At))
      .history
    assertEquals(reattached.defVersion, 2)
    assertEquals(reattached.defSource, "v2 source")
    assertEquals(reattached.goal, "the version 2 goal")

  test("a parked loop keeps its reason, and a resume clears it"):
    assertEquals(Ledger().parked(ParkReason.BudgetExhausted).history.parkedReason, Some(ParkReason.BudgetExhausted))
    assertEquals(Ledger().parked(ParkReason.GoalReached).resumed("s1").history.parkedReason, None)
    assertEquals(Ledger().history.status, LoopStatus.Running)

  // -- what a generation keeps ---------------------------------------------------

  test("every attempt survives, each carrying what the two gates said about it"):
    val h = Ledger()
      .started(1)
      .rejected(1, 1, "too slow")
      .rejected(1, 2, "still too slow")
      .passed(1, 3, 70, "made it faster")
      .accept(1, 70, "made it faster")
      .history
    val g = generation(h, 1)
    assertEquals(g.attempts.map(_.attempt), Vector(1, 2, 3))
    assertEquals(g.attempts.map(_.check.map(_.pass)), Vector(Some(false), Some(false), Some(true)))
    assertEquals(g.attempts(0).check.map(_.reasons), Some(List("too slow")))
    // The checker refused the first two, so the evaluator never saw them.
    assertEquals(g.attempts.map(_.verdict.isDefined), Vector(false, false, true))
    assertEquals(g.attempts(2).verdict.map(_.feedback), Some("good"))
    assertEquals(g.attempts(0).knowledge, Some("learned a thing"))
    assertEquals(g.attempts(2).artifact, Json.Obj(List("p99Ms" -> Json.num(70.0))))

  test("an attempt keeps the commits it snapshotted, which is what a diff is computed from"):
    val g = generation(Ledger().started(1).rejected(1, 1, "no").passed(1, 2, 70, "yes").history, 1)
    assertEquals(g.attempts.map(_.snapshotCommits), Vector(List("c-1-1"), List("c-1-2")))

  test("a generation keeps the session that worked it, which names its transcript files"):
    val h = Ledger().started(1, "sess-a").passed(1, 1, 70, "d").accept(1, 70, "d").started(2, "sess-b").history
    assertEquals(generation(h, 1).sessionId, "sess-a")
    assertEquals(generation(h, 2).sessionId, "sess-b")
    assertEquals(generation(h, 1).startedAt, At)

  test("an accepted generation carries where its work was stored and what was measured"):
    val g = generation(Ledger().started(1).passed(1, 1, 70, "made it faster").accept(1, 70, "made it faster").history, 1)
    assertEquals(g.accepted, true)
    assertEquals(g.commit, Some("commit-1"))
    assertEquals(g.settledAt, Some(At))
    assertEquals(g.description, "made it faster")
    // Key order, so a metric strip does not reshuffle between frames.
    assertEquals(g.metrics, Vector("allocMb" -> 12.0, "p99Ms" -> 70.0))
    g.outcome match
      case GenerationOutcome.Accepted(snapshotId, _, _, _, _) => assertEquals(snapshotId, "snap-1")
      case other                                              => fail(s"expected an acceptance, got $other")

  test("an abandoned generation keeps its rescue snapshot and falls back to its last attempt's account"):
    val g = generation(Ledger().started(1).rejected(1, 1, "no").rejected(1, 2, "still no").abandon(1, Some("loop/opt/gen-1-abandoned")).history, 1)
    assertEquals(g.accepted, false)
    assertEquals(g.rescueSnapshotId, Some("loop/opt/gen-1-abandoned"))
    assertEquals(g.commit, None)
    assertEquals(g.metrics, Vector.empty)
    // What it tried, which the acceptance would otherwise have said.
    assertEquals(g.description, "try 2")
    assertEquals(g.settledAt, Some(At))

  test("a generation that died before submitting anything says so with an empty description"):
    val g = generation(Ledger().started(1).abandon(1, None).history, 1)
    assertEquals(g.attempts, Vector.empty)
    assertEquals(g.description, "")
    assertEquals(g.rescueSnapshotId, None)

  test("the generation in flight has settled on nothing"):
    val g = generation(Ledger().started(1).rejected(1, 1, "no").history, 1)
    assertEquals(g.outcome, GenerationOutcome.Running)
    assertEquals(g.settledAt, None)
    assertEquals(g.commit, None)

  test("generations come in the order they were started, abandoned numbers in their places"):
    val h = Ledger()
      .started(1).passed(1, 1, 90, "first").accept(1, 90, "first")
      .started(2).rejected(2, 1, "no").abandon(2, None)
      .started(3).passed(3, 1, 70, "third").accept(3, 70, "third")
      .started(4)
      .history
    assertEquals(h.generations.map(_.gen), Vector(1, 2, 3, 4))
    assertEquals(h.generations.map(_.accepted), Vector(true, false, true, false))

  // -- what a diff is read against -----------------------------------------------

  test("a generation is read against its parent's accepted commit, or the baseline"):
    val h = Ledger()
      .started(1).passed(1, 1, 90, "first").accept(1, 90, "first")
      .started(2).rejected(2, 1, "no").abandon(2, None)
      .started(3)
      .history
    // Generation 1 branched from nothing, so the loop's own baseline.
    assertEquals(h.baseFor(1), "base-commit")
    // 2 and 3 both branch from the accepted 1 — an abandonment does not move the base.
    assertEquals(generation(h, 2).parent, Some(1))
    assertEquals(generation(h, 3).parent, Some(1))
    assertEquals(h.baseFor(2), "commit-1")
    assertEquals(h.baseFor(3), "commit-1")

  test("a generation nobody started, or one whose parent was never accepted, reads against the baseline"):
    assertEquals(Ledger().history.baseFor(7), "base-commit")
    // No correct driver writes this; a diff against the baseline still beats none.
    val h = Ledger().started(1).raw(LoopEvent.GenerationStarted(2, Some(1), "s", At)).history
    assertEquals(h.baseFor(2), "base-commit")

  // -- ledgers that are not ledgers ----------------------------------------------

  test("a ledger must open with exactly one loop_created event"):
    assert(LoopHistory.fold(Vector.empty).isLeft, "an empty ledger describes no loop")
    assert(LoopHistory.fold(Vector(LoopEvent.Parked(ParkReason.UserRequested, At))).isLeft)
    val twice = Ledger().raw(LoopEvent.LoopCreated("other", "b", "h", "s", At)).result
    LoopHistory.fold(twice) match
      case Left(msg) => assert(msg.contains("loop_created"), s"the message should name the offending event: $msg")
      case Right(_)  => fail("two loop_created events describe two loops")

  test("an event about a generation nobody started names its position and its event"):
    val orphan = Ledger().raw(LoopEvent.CheckCompleted(4, 1, true, Nil, Map.empty, At)).result
    LoopHistory.fold(orphan) match
      case Left(msg) =>
        assert(msg.startsWith("event 3 (check_completed):"), s"unexpected message: $msg")
        assert(msg.contains("generation 4"), s"the message should name the generation: $msg")
      case Right(_) => fail("a judgement about a generation that never ran has nowhere to go")

  // -- being more forgiving than the driver --------------------------------------

  test("a ledger the engine's fold accepts is always one this fold accepts"):
    val rich = Ledger()
      .started(1).rejected(1, 1, "no").passed(1, 2, 90, "first").accept(1, 90, "first")
      .started(2).rejected(2, 1, "no").abandon(2, Some("rescue"))
      .started(3).passed(3, 1, 70, "third").accept(3, 70, "third")
      .parked(ParkReason.GoalReached)
      .result
    assert(LoopState.fold(rich).isRight, "the fixture should be a ledger the engine accepts")
    assert(LoopHistory.fold(rich).isRight, "the reader must accept everything the driver does")

  test("orderings the drive cycle forbids are absorbed rather than refused"):
    // Every one of these is a Left from LoopState.fold; a reader shows what it can.
    val backwards = Ledger().raw(LoopEvent.DefAttached(1, "again", "same goal", "same rubric", Budgets(), Json.Null, At)).result
    assertEquals(LoopHistory.fold(backwards).map(_.defVersion), Right(1))
    val outOfOrder = Ledger().started(5).passed(5, 1, 70, "d").accept(5, 70, "d").result
    assertEquals(LoopHistory.fold(outOfOrder).map(_.generations.map(_.gen)), Right(Vector(5)))
    val afterPark = Ledger().parked(ParkReason.UserRequested).started(1).result
    assertEquals(LoopHistory.fold(afterPark).map(_.generations.map(_.gen)), Right(Vector(1)))
    assert(List(backwards, outOfOrder, afterPark).forall(LoopState.fold(_).isLeft), "the fixtures should all be ledgers the engine refuses")

  test("a judgement of an attempt that was never submitted is dropped, not invented"):
    val h = Ledger().started(1).raw(LoopEvent.CheckCompleted(1, 1, true, Nil, Map.empty, At)).history
    assertEquals(generation(h, 1).attempts, Vector.empty)

  test("a generation started twice keeps the first start's session"):
    val h = Ledger().started(1, "sess-a").raw(LoopEvent.GenerationStarted(1, None, "sess-b", At)).history
    assertEquals(h.generations.map(_.gen), Vector(1))
    assertEquals(generation(h, 1).sessionId, "sess-a")

  test("the fold is deterministic: the same ledger always gives the same history"):
    val events = Ledger().started(1).rejected(1, 1, "no").passed(1, 2, 70, "d").accept(1, 70, "d").result
    assertEquals(LoopHistory.fold(events), LoopHistory.fold(events))
