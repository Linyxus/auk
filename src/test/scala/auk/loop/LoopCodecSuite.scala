package auk.loop

import auk.llm.tools.Json

/** The ledger is the loop's only memory, so what matters here is that nothing is
  * quietly lost or quietly invented: every case survives a round trip byte-exactly,
  * and every malformed line is refused with a message that says where the problem is.
  */
class LoopCodecSuite extends munit.FunSuite:

  private val schema = Json.Obj(List(
    "kind"   -> Json.Str("object"),
    "fields" -> Json.Arr(List(Json.Str("score"), Json.Str("notes")))
  ))

  private val artifact = Json.Obj(List(
    "score" -> Json.num(0.5),
    "notes" -> Json.Arr(List(Json.Str("first"))),
    "flag"  -> Json.Bool(true),
    "gap"   -> Json.Null
  ))

  /** Awkward on purpose: non-ASCII, an emoji, a newline, a tab and a quote, all of
    * which have to survive a JSON string unscathed. */
  private val unicode = "目標: résumé ✅ \"quoted\"\nsecond line\ttabbed"

  private val samples: List[(String, LoopEvent)] = List(
    "loop_created" ->
      LoopEvent.LoopCreated("loop-1", "abc123", "def456", "session-1", "2026-07-30T12:00:00Z"),
    "loop_created with unicode ids" ->
      LoopEvent.LoopCreated("loop-1", unicode, "", "", "2026-07-30T12:00:00Z"),
    "def_attached" ->
      LoopEvent.DefAttached(1, "def check = ???", unicode, "be strict", Budgets(10, 3, 5), schema, "t1"),
    "def_attached with an empty schema and source" ->
      LoopEvent.DefAttached(2, "", "", "", Budgets(), Json.Obj(Nil), "t2"),
    "def_attached with a null schema" ->
      LoopEvent.DefAttached(3, "src", "g", "r", Budgets(), Json.Null, "t3"),
    "config_amended with nothing set" ->
      LoopEvent.ConfigAmended(None, None, None, "t4"),
    "config_amended with everything set" ->
      LoopEvent.ConfigAmended(Some(unicode), Some("tighter"), Some(Budgets(1, 1, 1)), "t5"),
    "config_amended with only budgets" ->
      LoopEvent.ConfigAmended(None, None, Some(Budgets(99, 0, 7)), "t6"),
    "generation_started without a parent" ->
      LoopEvent.GenerationStarted(1, None, "worker-1", "t7"),
    "generation_started with a parent" ->
      LoopEvent.GenerationStarted(4, Some(3), "worker-4", "t8"),
    "attempt_submitted" ->
      LoopEvent.AttemptSubmitted(1, 1, artifact, unicode, Some("learned a thing"), List("c1", "c2"), "t9"),
    "attempt_submitted with nothing optional" ->
      LoopEvent.AttemptSubmitted(1, 2, Json.Null, "", None, Nil, "t10"),
    "check_completed that passed" ->
      LoopEvent.CheckCompleted(1, 1, true, Nil, Map.empty, "t11"),
    "check_completed that failed" ->
      LoopEvent.CheckCompleted(1, 2, false, List("too slow", unicode), Map("ms" -> 12.5, "alloc" -> -3.0), "t12"),
    "verdict_issued accepting" ->
      LoopEvent.VerdictIssued(1, 2, true, unicode, true, "t13"),
    "verdict_issued rejecting" ->
      LoopEvent.VerdictIssued(1, 2, false, "", false, "t14"),
    "generation_accepted" ->
      LoopEvent.GenerationAccepted(1, "snap-1", "commit-1", unicode, Map("score" -> 0.75), "t15"),
    "generation_accepted with no metrics" ->
      LoopEvent.GenerationAccepted(2, "snap-2", "commit-2", "", Map.empty, "t16"),
    "generation_abandoned with a rescue snapshot" ->
      LoopEvent.GenerationAbandoned(3, 3, Some("rescue-3"), "t17"),
    "generation_abandoned without one" ->
      LoopEvent.GenerationAbandoned(4, 0, None, "t18"),
    "external_edits_adopted" ->
      LoopEvent.ExternalEditsAdopted("loop/opt/adopted-1", "adopted-commit", "session-3", "t18b"),
    "external_edits_adopted with a unicode snapshot id" ->
      LoopEvent.ExternalEditsAdopted(unicode, "", "", "t18c"),
    "parked: goal reached" -> LoopEvent.Parked(ParkReason.GoalReached, "t19"),
    "parked: budget exhausted" -> LoopEvent.Parked(ParkReason.BudgetExhausted, "t20"),
    "parked: patience exhausted" -> LoopEvent.Parked(ParkReason.PatienceExhausted, "t21"),
    "parked: user requested" -> LoopEvent.Parked(ParkReason.UserRequested, "t22"),
    "parked: api failure" -> LoopEvent.Parked(ParkReason.ApiFailure(unicode), "t23"),
    "parked: anomaly" -> LoopEvent.Parked(ParkReason.Anomaly(""), "t24"),
    "resumed" -> LoopEvent.Resumed("session-2", "t25")
  )

  for (name, event) <- samples do
    test(s"round-trip: $name"):
      assertEquals(LoopCodec.decode(LoopCodec.encode(event)), Right(event))

  test("every event case is covered by the round-trip samples"):
    assertEquals(samples.map(_._2.kind).toSet.size, 12)

  test("non-finite metrics survive, since a checker can produce them"):
    val event = LoopEvent.CheckCompleted(1, 1, false, Nil, Map("nan" -> Double.NaN, "inf" -> Double.PositiveInfinity, "ninf" -> Double.NegativeInfinity), "t")
    LoopCodec.decode(LoopCodec.encode(event)) match
      case Right(LoopEvent.CheckCompleted(_, _, _, _, metrics, _)) =>
        assert(metrics("nan").isNaN, "NaN should come back as NaN")
        assertEquals(metrics("inf"), Double.PositiveInfinity)
        assertEquals(metrics("ninf"), Double.NegativeInfinity)
      case other => fail(s"expected a check_completed but got $other")

  test("metric keys are written in sorted order, so a ledger line is stable"):
    val event = LoopEvent.CheckCompleted(1, 1, true, Nil, Map("zeta" -> 1.5, "alpha" -> 2.25, "mid" -> 3.75), "t")
    val line = LoopCodec.encode(event)
    assert(line.contains("""{"alpha":2.25,"mid":3.75,"zeta":1.5}"""), s"metrics were not sorted in: $line")

  test("a whole-valued metric is written without a trailing .0, and still reads back as a Double"):
    val event = LoopEvent.CheckCompleted(1, 1, true, Nil, Map("count" -> 2.0), "t")
    assert(LoopCodec.encode(event).contains("""{"count":2}"""), LoopCodec.encode(event))
    assertEquals(LoopCodec.decode(LoopCodec.encode(event)), Right(event))

  test("an optional field written as an explicit null reads as absent"):
    val line = """{"type":"generation_started","gen":1,"parent":null,"sessionId":"s","at":"t"}"""
    assertEquals(LoopCodec.decode(line), Right(LoopEvent.GenerationStarted(1, None, "s", "t")))

  // --- refusals ---------------------------------------------------------------

  private def failure[A](result: Either[String, A])(using munit.Location): String = result match
    case Left(message) => message
    case Right(value)  => fail(s"expected a failure but got: $value")

  test("a line that is not JSON at all is refused"):
    assert(failure(LoopCodec.decode("{not json")).nonEmpty)

  test("a line that is JSON but not an object is refused by kind"):
    assertEquals(failure(LoopCodec.decode("[1,2,3]")), "a loop event must be an object but was array")

  test("an unknown event type is named in the failure"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"generation_teleported","at":"t"}""")),
      "type: unknown loop event type 'generation_teleported'"
    )

  test("a line with no type at all is refused"):
    assertEquals(failure(LoopCodec.decode("""{"at":"t"}""")), "type: missing field")

  test("a missing field is named by its path"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"generation_accepted","gen":1,"snapshotId":"s","commit":"c","metrics":{},"at":"t"}""")),
      "generation_accepted.description: missing field"
    )

  test("a field of the wrong type is named by its path"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"resumed","sessionId":7,"at":"t"}""")),
      "resumed.sessionId: expected a string but found number"
    )

  test("a fractional value where a whole number belongs is refused"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"generation_started","gen":1.5,"sessionId":"s","at":"t"}""")),
      "generation_started.gen: expected a whole number but found 1.5"
    )

  test("a nested field is named by its full path"):
    assertEquals(
      failure(LoopCodec.decode(
        """{"type":"def_attached","version":1,"source":"","goal":"","rubric":"","budgets":{"maxGenerations":5,"patience":"lots","maxAttemptsPerGeneration":3},"artifactSchema":{},"at":"t"}"""
      )),
      "def_attached.budgets.patience: expected a number but found string"
    )

  test("a bad element of a string array is named by its index"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"check_completed","gen":1,"attempt":1,"pass":false,"reasons":["ok",3],"metrics":{},"at":"t"}""")),
      "check_completed.reasons[1]: expected a string but found number"
    )

  test("a metric that is not a number is named by its key"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"check_completed","gen":1,"attempt":1,"pass":true,"reasons":[],"metrics":{"ms":true},"at":"t"}""")),
      "check_completed.metrics.ms: expected a number but found boolean"
    )

  test("an unknown park reason is named by its path"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"parked","reason":{"kind":"bored"},"at":"t"}""")),
      "parked.reason.kind: unknown park reason 'bored'"
    )

  test("a park reason missing its detail is refused"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"parked","reason":{"kind":"anomaly"},"at":"t"}""")),
      "parked.reason.detail: missing field"
    )

  test("an optional field of the wrong type is refused rather than ignored"):
    assertEquals(
      failure(LoopCodec.decode("""{"type":"generation_abandoned","gen":1,"attempts":2,"rescueSnapshotId":[],"at":"t"}""")),
      "generation_abandoned.rescueSnapshotId: expected a string but found array"
    )
