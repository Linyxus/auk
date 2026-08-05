package auk.workflow

/** The one definition of how a running `eval_scala` row reads. Every surface
  * shares it, so these are the assertions that keep the TUI and the dashboard
  * from telling a reader two different things about one call. */
class EvalDisplaySuite extends munit.FunSuite:

  private def phase(name: String, ms: String = "0"): Map[String, String] =
    Map(EvalDisplay.MetaPhase -> name, EvalDisplay.MetaPhaseMs -> ms)

  // -- stage mapping -----------------------------------------------------------

  test("compiling reads as Compiling"):
    assertEquals(EvalDisplay.stageName(phase("compiling")), Some("Compiling"))

  test("running and rendering both read as Running — the difference is internal"):
    assertEquals(EvalDisplay.stageName(phase("running")), Some("Running"))
    assertEquals(EvalDisplay.stageName(phase("rendering")), Some("Running"))

  test("a stage token this build has no word for is shown as no stage at all"):
    for token <- List("linking", "Compiling", "COMPILING", "compile", "", "run") do
      assertEquals(EvalDisplay.stageName(phase(token)), None, s"token=$token")

  test("a report naming no stage has no stage"):
    assertEquals(EvalDisplay.stageName(Map.empty), None)
    assertEquals(EvalDisplay.stageName(Map(EvalDisplay.MetaPhaseMs -> "900")), None)

  // -- extrapolation -----------------------------------------------------------

  test("the stage clock counts the staleness of the report on top of it"):
    assertEquals(EvalDisplay.stageMs(phase("compiling", "900"), Some(1_000L), 1_400L), Some(1_300L))
    assertEquals(EvalDisplay.stage(phase("compiling", "900"), Some(1_000L), 1_400L), Some("Compiling (1.3s)"))

  test("with no anchor the reported figure is used as reported"):
    assertEquals(EvalDisplay.stageMs(phase("running", "900"), None, 9_999_999L), Some(900L))
    assertEquals(EvalDisplay.stage(phase("running", "900"), None, 9_999_999L), Some("Running (0.9s)"))

  test("a clock reading behind the report is clamped — a report only extrapolates forward"):
    assertEquals(EvalDisplay.stageMs(phase("compiling", "900"), Some(5_000L), 1_000L), Some(900L))

  test("a report arriving at this instant reads exactly as reported"):
    assertEquals(EvalDisplay.stageMs(phase("compiling", "900"), Some(1_000L), 1_000L), Some(900L))

  test("a stage clock that is not a number is no clock — the stage shows alone"):
    for bad <- List("soon", "9.5", "", "1e3") do
      assertEquals(EvalDisplay.stageMs(Map(EvalDisplay.MetaPhase -> "compiling", EvalDisplay.MetaPhaseMs -> bad),
        Some(1_000L), 1_400L), None, s"value=$bad")
    assertEquals(EvalDisplay.stage(Map(EvalDisplay.MetaPhase -> "compiling", EvalDisplay.MetaPhaseMs -> "soon"),
      Some(1_000L), 1_400L), Some("Compiling"))

  test("a report with a clock but no stage shows nothing — the number needs its noun"):
    assertEquals(EvalDisplay.stage(Map(EvalDisplay.MetaPhaseMs -> "900"), Some(1_000L), 1_400L), None)

  // -- the composed suffixes ---------------------------------------------------

  test("stageSuffix is the segment a renderer appends, separator included"):
    assertEquals(EvalDisplay.stageSuffix(phase("compiling", "3100"), Some(0L), 0L), Some(" · Compiling (3.1s)"))

  test("stageSuffix is None when there is no stage, so a caller never appends an empty segment"):
    assertEquals(EvalDisplay.stageSuffix(Map.empty, Some(0L), 0L), None)
    assertEquals(EvalDisplay.stageSuffix(phase("linking", "900"), Some(0L), 0L), None)

  test("liveSuffix carries both clocks"):
    assertEquals(EvalDisplay.liveSuffix(12_400L, phase("compiling", "900"), Some(1_000L), 1_400L),
      " (12.4s) · Compiling (1.3s)")

  test("liveSuffix is the total alone when the worker named no stage"):
    assertEquals(EvalDisplay.liveSuffix(12_400L, Map.empty, None, 0L), " (12.4s)")

  test("liveSuffix on a call that has just begun reads as zero, not as nothing"):
    assertEquals(EvalDisplay.liveSuffix(0L, Map.empty, None, 0L), " (0.0s)")

  // -- the total taken from the report, for a viewer that never saw the call start

  test("the call's clock counts the staleness of the report on top of it, like the stage's"):
    val p = Map(EvalDisplay.MetaElapsedMs -> "12400", EvalDisplay.MetaPhase -> "compiling", EvalDisplay.MetaPhaseMs -> "900")
    assertEquals(EvalDisplay.elapsedMs(p, Some(1_000L), 1_400L), Some(12_800L))
    // Both clocks advance by the same 400ms — they can never drift apart.
    assertEquals(EvalDisplay.stageMs(p, Some(1_000L), 1_400L), Some(1_300L))

  test("with no anchor the reported total is used as reported, and never runs backwards"):
    val p = Map(EvalDisplay.MetaElapsedMs -> "12400")
    assertEquals(EvalDisplay.elapsedMs(p, None, 9_999_999L), Some(12_400L))
    assertEquals(EvalDisplay.elapsedMs(p, Some(5_000L), 1_000L), Some(12_400L))

  test("a total that is not a number is no total"):
    for bad <- List("soon", "9.5", "", "1e3") do
      assertEquals(EvalDisplay.elapsedMs(Map(EvalDisplay.MetaElapsedMs -> bad), Some(1_000L), 1_400L), None, s"value=$bad")
    assertEquals(EvalDisplay.elapsedMs(Map.empty, Some(1_000L), 1_400L), None)

  test("reportedSuffix is both clocks, taken from the one report"):
    val p = Map(EvalDisplay.MetaElapsedMs -> "12400", EvalDisplay.MetaPhase -> "compiling", EvalDisplay.MetaPhaseMs -> "900")
    assertEquals(EvalDisplay.reportedSuffix(p, Some(1_000L), 1_400L), " (12.8s) · Compiling (1.3s)")

  test("reportedSuffix says nothing at all before the first report"):
    // The row must read exactly as it did without any of this until a report lands.
    assertEquals(EvalDisplay.reportedSuffix(Map.empty, None, 1_400L), "")
    assertEquals(EvalDisplay.reportedSuffix(Map.empty, Some(1_000L), 1_400L), "")

  test("reportedSuffix shows whichever half the report carried"):
    assertEquals(EvalDisplay.reportedSuffix(Map(EvalDisplay.MetaElapsedMs -> "12400"), None, 0L), " (12.4s)")
    assertEquals(EvalDisplay.reportedSuffix(phase("running", "800"), None, 0L), " · Running (0.8s)")

  test("reportedSuffix is liveSuffix once a total is in hand"):
    val p = Map(EvalDisplay.MetaElapsedMs -> "12400", EvalDisplay.MetaPhase -> "running", EvalDisplay.MetaPhaseMs -> "800")
    assertEquals(
      EvalDisplay.reportedSuffix(p, Some(1_000L), 1_400L),
      EvalDisplay.liveSuffix(12_800L, p, Some(1_000L), 1_400L)
    )

  // -- formatting --------------------------------------------------------------

  test("documented rounding boundaries"):
    assertEquals(EvalDisplay.duration(0L), "0.0s")
    assertEquals(EvalDisplay.duration(950L), "1.0s")
    assertEquals(EvalDisplay.duration(1050L), "1.1s")
    assertEquals(EvalDisplay.duration(49L), "0.0s")
    assertEquals(EvalDisplay.duration(50L), "0.1s")
    assertEquals(EvalDisplay.duration(3_000_000L), "3000.0s")

  /** The formula is a port of the TUI's, which is itself a port of `%.1f`; the
    * TUI's `FmtSuite` guards the full range. Re-checking it here is what makes
    * this module's copy safe to delegate to — if the two ever drift, this fails
    * beside the code that drifted rather than in a suite one project away. */
  test("duration formatting matches %.1f across 0..200,000 ms"):
    var ms = 0L
    while ms <= 200_000L do
      assertEquals(EvalDisplay.duration(ms), f"${ms / 1000.0}%.1fs", s"ms=$ms")
      ms += 1L

  test("oneDecimal is the shared round-half-up helper, in tenths"):
    assertEquals(EvalDisplay.oneDecimal(0L), "0.0")
    assertEquals(EvalDisplay.oneDecimal(15L), "1.5")
    assertEquals(EvalDisplay.oneDecimal(1500L), "150.0")

  // -- the metadata keys are the worker's, verbatim -----------------------------

  test("the metadata keys are exactly what the REPL worker publishes"):
    assertEquals(EvalDisplay.MetaState, "replState")
    assertEquals(EvalDisplay.MetaSilentMs, "replSilentMs")
    assertEquals(EvalDisplay.MetaElapsedMs, "replElapsedMs")
    assertEquals(EvalDisplay.MetaWorkerPid, "workerPid")
    assertEquals(EvalDisplay.MetaLiveness, "liveness")
    assertEquals(EvalDisplay.MetaPhase, "replPhase")
    assertEquals(EvalDisplay.MetaPhaseMs, "replPhaseMs")
    assertEquals(EvalDisplay.ToolName, "eval_scala")
