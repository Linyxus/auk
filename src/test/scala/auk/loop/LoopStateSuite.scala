package auk.loop

import auk.llm.tools.Json

/** The fold is what every other part of the loop feature reads the world through,
  * so these cover both halves of its contract: the shapes a real loop takes — retries,
  * abandonment, amendments, parking — and the shapes it refuses to read at all.
  */
class LoopStateSuite extends munit.FunSuite:

  // --- ledger vocabulary, so the tests read as sequences rather than constructors ---

  private val created = LoopEvent.LoopCreated("loop-1", "base-commit", "head-commit", "session-0", "t0")

  private def attach(version: Int = 1, goal: String = "make it fast", rubric: String = "be strict", budgets: Budgets = Budgets()) =
    LoopEvent.DefAttached(version, s"source v$version", goal, rubric, budgets, Json.Obj(List("v" -> Json.num(version))), "t")

  private def amend(goal: Option[String] = None, rubric: Option[String] = None, budgets: Option[Budgets] = None) =
    LoopEvent.ConfigAmended(goal, rubric, budgets, "t")

  private def start(gen: Int, parent: Option[Int] = None) =
    LoopEvent.GenerationStarted(gen, parent, s"worker-$gen", s"t-start-$gen")

  private def submit(gen: Int, attempt: Int) =
    LoopEvent.AttemptSubmitted(gen, attempt, Json.Str(s"artifact-$gen.$attempt"), s"work $gen.$attempt", None, List(s"c$gen.$attempt"), "t")

  private def check(gen: Int, attempt: Int, pass: Boolean, reasons: List[String] = Nil) =
    LoopEvent.CheckCompleted(gen, attempt, pass, reasons, Map("ms" -> attempt.toDouble), "t")

  private def judge(gen: Int, attempt: Int, accepted: Boolean, feedback: String = "", goalReached: Boolean = false) =
    LoopEvent.VerdictIssued(gen, attempt, accepted, feedback, goalReached, "t")

  private def accept(gen: Int) =
    LoopEvent.GenerationAccepted(gen, s"snap-$gen", s"commit-$gen", s"generation $gen", Map("score" -> gen.toDouble), "t")

  private def abandon(gen: Int, attempts: Int = 3) =
    LoopEvent.GenerationAbandoned(gen, attempts, Some(s"rescue-$gen"), "t")

  /** A whole generation that is submitted once, passes both gates and is accepted. */
  private def cleanGeneration(gen: Int, parent: Option[Int]): List[LoopEvent] =
    List(start(gen, parent), submit(gen, 1), check(gen, 1, true), judge(gen, 1, true), accept(gen))

  private def fold(events: LoopEvent*): Either[String, LoopState] = LoopState.fold(events.toVector)

  private def folded(events: LoopEvent*)(using munit.Location): LoopState =
    LoopState.fold(events.toVector) match
      case Right(state)  => state
      case Left(message) => fail(s"expected a valid ledger but the fold refused it: $message")

  private def refusal[A](result: Either[String, A])(using munit.Location): String = result match
    case Left(message) => message
    case Right(value)  => fail(s"expected the fold to refuse this ledger but it gave: $value")

  // --- the shapes a loop takes -------------------------------------------------

  test("a fresh loop knows where it came from and nothing else"):
    val state = folded(created)
    assertEquals(state.loopId, "loop-1")
    assertEquals(state.baselineCommit, "base-commit")
    assertEquals(state.headAtCreation, "head-commit")
    assertEquals(state.sessionId, "session-0")
    assertEquals(state.createdAt, "t0")
    assertEquals(state.defVersion, 0)
    assertEquals(state.defAttached, false)
    assertEquals(state.goal, "")
    assertEquals(state.budgets, Budgets())
    assertEquals(state.artifactSchema, Json.Null)
    assertEquals(state.nextGen, 1)
    assertEquals(state.generations, Vector.empty)
    assertEquals(state.abandoned, Vector.empty)
    assertEquals(state.inFlight, None)
    assertEquals(state.status, LoopStatus.Running)

  test("attaching a definition fills in the whole configuration"):
    val state = folded(created, attach(budgets = Budgets(7, 1, 2)))
    assertEquals(state.defVersion, 1)
    assertEquals(state.defSource, "source v1")
    assertEquals(state.goal, "make it fast")
    assertEquals(state.rubric, "be strict")
    assertEquals(state.budgets, Budgets(7, 1, 2))
    assertEquals(state.artifactSchema, Json.Obj(List("v" -> Json.num(1))))
    assertEquals(state.defAttached, true)

  test("three accepted generations build a lineage"):
    val state = folded(
      (List(created, attach()) ++ cleanGeneration(1, None) ++ cleanGeneration(2, Some(1)) ++ cleanGeneration(3, Some(2)))*
    )
    assertEquals(state.generations.map(_.gen), Vector(1, 2, 3))
    assertEquals(state.generations.map(_.parent), Vector(None, Some(1), Some(2)))
    assertEquals(state.generations.map(_.commit), Vector("commit-1", "commit-2", "commit-3"))
    assertEquals(state.latestAccepted.map(_.gen), Some(3))
    assertEquals(state.nextGen, 4)
    assertEquals(state.generationsStarted, 3)
    assertEquals(state.consecutiveAbandoned, 0)
    assertEquals(state.inFlight, None)
    assertEquals(state.status, LoopStatus.Running)

  test("an accepted generation records the artifact of the attempt that was accepted"):
    val state = folded(created, attach(), start(1), submit(1, 1), check(1, 1, false, List("no")), submit(1, 2), check(1, 2, true), accept(1))
    assertEquals(state.generations.head.artifact, Json.Str("artifact-1.2"))
    assertEquals(state.generations.head.description, "generation 1")
    assertEquals(state.generations.head.metrics, Map("score" -> 1.0))
    assertEquals(state.generations.head.snapshotId, "snap-1")

  test("retries stay inside one generation and do not consume a generation number"):
    val state = folded(
      created, attach(), start(1),
      submit(1, 1), check(1, 1, false, List("too slow", "leaks")),
      submit(1, 2), check(1, 2, true), judge(1, 2, false, "not what I asked for"),
      submit(1, 3)
    )
    val flight = state.inFlight.getOrElse(fail("expected a generation in flight"))
    assertEquals(flight.gen, 1)
    assertEquals(flight.attempts, 3)
    assertEquals(flight.sessionId, "worker-1")
    assertEquals(flight.startedAt, "t-start-1")
    assertEquals(flight.lastSubmission.map(_.attempt), Some(3))
    assertEquals(state.nextGen, 2)
    assertEquals(state.generations, Vector.empty)

  test("the newest rejection is the one a retry prompt should quote"):
    // The verdict came after the check on the same attempt, so it is the later word.
    val afterVerdict = folded(created, attach(), start(1), submit(1, 1), check(1, 1, true), judge(1, 1, false, "shallow"))
    assertEquals(afterVerdict.inFlight.flatMap(_.latestRejection), Some("shallow"))

    // A newer attempt failed its check, which now outranks the older verdict.
    val afterCheck = folded(created, attach(), start(1), submit(1, 1), judge(1, 1, false, "shallow"), submit(1, 2), check(1, 2, false, List("too slow", "leaks")))
    assertEquals(afterCheck.inFlight.flatMap(_.latestRejection), Some("too slow; leaks"))

    // Nothing has been rejected, so there is nothing to quote.
    val noRejection = folded(created, attach(), start(1), submit(1, 1), check(1, 1, true))
    assertEquals(noRejection.inFlight.flatMap(_.latestRejection), None)

  test("an abandoned generation is followed by a fresh one from the same parent"):
    val state = folded(
      (List(created, attach()) ++ cleanGeneration(1, None)
        ++ List(start(2, Some(1)), submit(2, 1), check(2, 1, false, List("no")), abandon(2))
        ++ cleanGeneration(3, Some(1)))*
    )
    assertEquals(state.generations.map(_.gen), Vector(1, 3))
    assertEquals(state.generations.map(_.parent), Vector(None, Some(1)))
    assertEquals(state.nextGen, 4)
    assertEquals(state.generationsStarted, 3)
    assertEquals(state.consecutiveAbandoned, 0)
    assertEquals(state.inFlight, None)

  test("the abandoned streak grows until an acceptance resets it"):
    val budgets = Budgets(maxGenerations = 50, patience = 2, maxAttemptsPerGeneration = 3)
    val afterOne = folded(created, attach(budgets = budgets), start(1), abandon(1, 0))
    assertEquals(afterOne.consecutiveAbandoned, 1)
    assertEquals(afterOne.patienceExhausted, false)

    val afterTwo = folded(created, attach(budgets = budgets), start(1), abandon(1, 0), start(2), abandon(2, 0))
    assertEquals(afterTwo.consecutiveAbandoned, 2)
    assertEquals(afterTwo.patienceExhausted, true)

    val afterAcceptance = folded(
      (List(created, attach(budgets = budgets), start(1), abandon(1, 0), start(2), abandon(2, 0)) ++ cleanGeneration(3, None))*
    )
    assertEquals(afterAcceptance.consecutiveAbandoned, 0)
    assertEquals(afterAcceptance.patienceExhausted, false)

  // --- what an abandoned generation leaves behind ------------------------------

  test("an abandoned generation is digested into what it tried and what refused it"):
    val state = folded(
      created, attach(), start(1),
      submit(1, 1), check(1, 1, false, List("too slow", "leaks")),
      abandon(1, 1)
    )
    assertEquals(
      state.abandoned,
      Vector(AbandonedDigest(1, 1, Some("work 1.1"), Some("too slow; leaks"), "t"))
    )

  test("the digest quotes the evaluator's prose over the checker's reasons"):
    // Both gates rejected the final attempt; the verdict came second and says more.
    val state = folded(
      created, attach(), start(1),
      submit(1, 1), check(1, 1, false, List("too slow")),
      submit(1, 2), check(1, 2, true), judge(1, 2, false, "this is a rewrite, not a refinement"),
      abandon(1, 2)
    )
    assertEquals(state.abandoned.map(_.rejection), Vector(Some("this is a rewrite, not a refinement")))
    assertEquals(state.abandoned.map(_.description), Vector(Some("work 1.2")))
    assertEquals(state.abandoned.map(_.attempts), Vector(2))

    // A newer attempt that failed its CHECK outranks the older verdict, so the reasons
    // are what the digest keeps.
    val newerCheck = folded(
      created, attach(), start(1),
      submit(1, 1), check(1, 1, true), judge(1, 1, false, "shallow"),
      submit(1, 2), check(1, 2, false, List("p99 regressed")),
      abandon(1, 2)
    )
    assertEquals(newerCheck.abandoned.map(_.rejection), Vector(Some("p99 regressed")))

  test("a generation that died before submitting is digested honestly, not blankly"):
    // The rescue case: a driver's session ended mid-generation, so nothing was ever
    // offered and there is nothing to say about it beyond that.
    val state = folded(created, attach(), start(1), abandon(1, 0))
    assertEquals(state.abandoned, Vector(AbandonedDigest(1, 0, None, None, "t")))

  test("abandonments accumulate in the order they happened"):
    val state = folded(
      created, attach(),
      start(1), submit(1, 1), check(1, 1, false, List("no")), abandon(1, 1),
      start(2), submit(2, 1), judge(2, 1, false, "nowhere near"), abandon(2, 1),
      start(3), abandon(3, 0)
    )
    assertEquals(state.abandoned.map(_.gen), Vector(1, 2, 3))
    assertEquals(state.abandoned.map(_.rejection), Vector(Some("no"), Some("nowhere near"), None))
    assertEquals(state.consecutiveAbandoned, 3)

  test("an accepted generation is in the lineage and never in the digest"):
    val state = folded(
      (List(created, attach()) ++ cleanGeneration(1, None)
        ++ List(start(2, Some(1)), submit(2, 1), check(2, 1, false, List("no")), abandon(2, 1))
        ++ cleanGeneration(3, Some(1)))*
    )
    assertEquals(state.generations.map(_.gen), Vector(1, 3))
    assertEquals(state.abandoned.map(_.gen), Vector(2))
    // An acceptance resets the streak but never forgets the failure that came before it.
    assertEquals(state.consecutiveAbandoned, 0)

  test("a ledger with nothing abandoned folds exactly as it always did"):
    // The digest is derived, not recorded: no new event carries it, so a ledger written
    // before it existed reads the same and reports no failures.
    val state = folded(
      (List(created, attach()) ++ cleanGeneration(1, None) ++ cleanGeneration(2, Some(1))
        ++ List(LoopEvent.Parked(ParkReason.GoalReached, "t")))*
    )
    assertEquals(state.abandoned, Vector.empty)
    assertEquals(state.generations.map(_.gen), Vector(1, 2))

  test("budgets are reported, never enforced: the fold parks nothing on its own"):
    val budgets = Budgets(maxGenerations = 2, patience = 5, maxAttemptsPerGeneration = 2)
    val state = folded(
      (List(created, attach(budgets = budgets)) ++ cleanGeneration(1, None) ++ List(start(2, Some(1)), submit(2, 1), submit(2, 2)))*
    )
    assertEquals(state.generationsStarted, 2)
    assertEquals(state.budgetExhausted, true)
    assertEquals(state.attemptsExhausted, true)
    assertEquals(state.status, LoopStatus.Running)

    // Nothing is in flight, so no attempt budget can be spent.
    assertEquals(folded(created, attach(budgets = budgets)).attemptsExhausted, false)

  test("parking records why, and resuming hands the loop to a new session"):
    val parked = folded((List(created, attach()) ++ cleanGeneration(1, None) ++ List(LoopEvent.Parked(ParkReason.PatienceExhausted, "t")))*)
    assertEquals(parked.status, LoopStatus.Parked(ParkReason.PatienceExhausted))
    assertEquals(parked.parkedReason, Some(ParkReason.PatienceExhausted))
    assertEquals(parked.sessionId, "session-0")

    val resumed = folded(
      (List(created, attach()) ++ cleanGeneration(1, None)
        ++ List(LoopEvent.Parked(ParkReason.ApiFailure("429 forever"), "t"), LoopEvent.Resumed("session-9", "t"))
        ++ cleanGeneration(2, Some(1)))*
    )
    assertEquals(resumed.status, LoopStatus.Running)
    assertEquals(resumed.parkedReason, None)
    assertEquals(resumed.sessionId, "session-9")
    assertEquals(resumed.generations.map(_.gen), Vector(1, 2))

  test("a park can land mid-generation, and that generation may still finish"):
    val state = folded(
      created, attach(), start(1), submit(1, 1),
      LoopEvent.Parked(ParkReason.UserRequested, "t"),
      check(1, 1, true), judge(1, 1, true), accept(1)
    )
    assertEquals(state.generations.map(_.gen), Vector(1))
    assertEquals(state.status, LoopStatus.Parked(ParkReason.UserRequested))
    assertEquals(state.inFlight, None)

  test("an amendment overlays only the fields it names"):
    val state = folded(created, attach(goal = "make it fast", rubric = "be strict", budgets = Budgets(7, 1, 2)), amend(goal = Some("make it correct")))
    assertEquals(state.goal, "make it correct")
    assertEquals(state.rubric, "be strict")
    assertEquals(state.budgets, Budgets(7, 1, 2))
    assertEquals(state.defVersion, 1)

    val untouched = folded(created, attach(), amend())
    assertEquals(untouched.goal, "make it fast")
    assertEquals(untouched.rubric, "be strict")
    assertEquals(untouched.budgets, Budgets())

  test("a new definition version resets the overlay base, and later amendments build on it"):
    val state = folded(
      created,
      attach(version = 1, goal = "v1 goal", rubric = "v1 rubric", budgets = Budgets(7, 1, 2)),
      amend(goal = Some("amended goal"), budgets = Some(Budgets(99, 9, 9))),
      attach(version = 2, goal = "v2 goal", rubric = "v2 rubric", budgets = Budgets(4, 4, 4)),
      amend(rubric = Some("amended rubric"))
    )
    assertEquals(state.defVersion, 2)
    assertEquals(state.defSource, "source v2")
    assertEquals(state.goal, "v2 goal")
    assertEquals(state.rubric, "amended rubric")
    assertEquals(state.budgets, Budgets(4, 4, 4))
    assertEquals(state.artifactSchema, Json.Obj(List("v" -> Json.num(2))))

  test("an amendment before any definition overlays the defaults"):
    val state = folded(created, amend(goal = Some("early goal")))
    assertEquals(state.goal, "early goal")
    assertEquals(state.defVersion, 0)

  // --- the shapes it refuses ---------------------------------------------------

  test("an empty ledger is not a loop"):
    assertEquals(refusal(fold()), "empty ledger: a loop ledger must begin with a loop_created event")

  test("a ledger that begins with anything else is not a loop"):
    assertEquals(
      refusal(fold(attach(), created)),
      "event 1 (def_attached): a loop ledger must begin with a loop_created event"
    )

  test("a loop is created exactly once"):
    assertEquals(
      refusal(fold(created, created)),
      "event 2 (loop_created): a ledger holds exactly one loop_created event, at its head"
    )

  test("generation numbers must run in order"):
    assertEquals(
      refusal(fold(created, attach(), start(2))),
      "event 3 (generation_started): generations must run in order: expected generation 1 but saw 2"
    )

  test("a generation cannot start while another is in flight"):
    assertEquals(
      refusal(fold(created, attach(), start(1), start(2))),
      "event 4 (generation_started): generation 2 started while generation 1 is still in flight"
    )

  test("a generation cannot start while the loop is parked"):
    assertEquals(
      refusal(fold(created, attach(), LoopEvent.Parked(ParkReason.UserRequested, "t"), start(1))),
      "event 4 (generation_started): generation 1 started while the loop is parked (UserRequested); a resumed event must come first"
    )

  test("a generation cannot name a parent that was never accepted"):
    assertEquals(
      refusal(fold(created, attach(), start(1, Some(4)))),
      "event 3 (generation_started): generation 1 names parent 4, which is not an accepted generation"
    )

  test("an abandoned generation is not a parent"):
    assertEquals(
      refusal(fold(created, attach(), start(1), abandon(1), start(2, Some(1)))),
      "event 5 (generation_started): generation 2 names parent 1, which is not an accepted generation"
    )

  test("attempt numbers must run in order"):
    assertEquals(
      refusal(fold(created, attach(), start(1), submit(1, 1), submit(1, 3))),
      "event 5 (attempt_submitted): generation 1: attempts must run in order: expected attempt 2 but saw 3"
    )

  test("an event about a generation that is not in flight is refused"):
    assertEquals(
      refusal(fold(created, attach(), submit(1, 1))),
      "event 3 (attempt_submitted): attempt 1 names generation 1, which is not in flight"
    )
    assertEquals(
      refusal(fold(created, attach(), start(1), submit(2, 1))),
      "event 4 (attempt_submitted): attempt 1 names generation 2 but generation 1 is in flight"
    )

  test("an attempt cannot be checked or judged before it is submitted"):
    assertEquals(
      refusal(fold(created, attach(), start(1), check(1, 1, true))),
      "event 4 (check_completed): generation 1: attempt 1 was checked before it was submitted"
    )
    assertEquals(
      refusal(fold(created, attach(), start(1), submit(1, 1), judge(1, 2, true))),
      "event 5 (verdict_issued): generation 1: attempt 2 was judged but attempt 1 is the latest submitted"
    )

  test("a generation cannot be accepted without a submitted attempt"):
    assertEquals(
      refusal(fold(created, attach(), start(1), accept(1))),
      "event 4 (generation_accepted): generation 1 was accepted without a submitted attempt"
    )

  test("a generation cannot be abandoned or accepted twice"):
    assertEquals(
      refusal(fold(created, attach(), start(1), abandon(1), abandon(1))),
      "event 5 (generation_abandoned): abandonment names generation 1, which is not in flight"
    )

  test("definition versions must increase"):
    assertEquals(
      refusal(fold(created, attach(version = 2), attach(version = 2))),
      "event 3 (def_attached): definition versions must increase but version 2 follows version 2"
    )
    assertEquals(
      refusal(fold(created, attach(version = 2), attach(version = 1))),
      "event 3 (def_attached): definition versions must increase but version 1 follows version 2"
    )
