package auk.webui.dev

import auk.workflow.*
import TranscriptEvent.*

/** The `loops` scenario: three loops, one of them alive.
  *
  * Its own file because it is data rather than script — a loop reaches the browser
  * whole, so a scenario for one is a sequence of whole loops rather than the deltas
  * [[Scenarios.life]] builds. It carries every state the loop UI has to draw: a
  * parked loop and an orphaned one sitting in the snapshot, and a held loop whose
  * lineage grows while you watch — accepted generations on the line, a stack of
  * abandoned ones branching off, a checker rejection that costs an attempt, the
  * evaluator's turn, and an acceptance that starts the next generation.
  *
  * It also answers the two on-demand routes, with payloads built from the same ids:
  * a settled generation's transcript is the JSONL a tee would have written, and a
  * patch is a patch. Keeping them here rather than in [[Scenarios]] is what keeps
  * them consistent with the lineage they belong to.
  */
object LoopScenario:

  val LiveId = "tokenizer-p99"
  val ParkedId = "readme-polish"
  val OrphanId = "flaky-tests"
  val ReachedId = "docs-examples"

  // -- the definitions ---------------------------------------------------------

  private val liveGoal =
    """Cut tokenizer p99 latency by 30% without losing accuracy.
      |
      |The tokenizer sits on the hot path of every request, so its tail latency is the
      |floor under everything else. Keep the public API and the vocabulary file format
      |exactly as they are — downstream services read both.""".stripMargin

  private val liveRubric =
    "Accepted when `sbt tokenizer/test` is green, tokens_per_s does not regress, and p99_ms " +
      "improves on the previous accepted generation by at least 2%."

  private val liveSource =
    """case class Perf(p99Ms: Double, tokensPerSecond: Double) derives LibToolInput
      |
      |lib.loop.start[Perf](
      |  id = "tokenizer-p99",
      |  goal = "cut tokenizer p99 latency by 30% without losing accuracy",
      |  rubric = "accepted when `sbt tokenizer/test` is green and p99 improves",
      |  budgets = LoopBudgets(maxGenerations = 20, patience = 2, maxAttemptsPerGeneration = 3)
      |) { (prev, cand) =>
      |  val tests = lib.shell.run("sbt", "tokenizer/test")
      |  if !tests.ok then CheckResult.fail("the tests did not pass: " + tests.stderr.trim)
      |  else
      |    val bench = lib.shell.run("sbt", "tokenizerBench")
      |    if !bench.ok then CheckResult.fail("the benchmark did not run")
      |    else if prev.exists(_.artifact.tokensPerSecond > cand.artifact.tokensPerSecond * 1.01) then
      |      CheckResult.fail("throughput regressed")
      |    else if prev.exists(_.artifact.p99Ms * 0.98 <= cand.artifact.p99Ms) then
      |      CheckResult.fail(f"p99 did not improve enough (${cand.artifact.p99Ms}%.1f ms)")
      |    else
      |      CheckResult.pass.withMetrics(
      |        "p99_ms" -> cand.artifact.p99Ms,
      |        "tokens_per_s" -> cand.artifact.tokensPerSecond)
      |}""".stripMargin

  private val budgets = LoopBudgetsWire(maxGenerations = 20, patience = 2, maxAttemptsPerGeneration = 3)

  // -- small builders ----------------------------------------------------------

  private def artifact(p99: Double, tps: Int, note: String): String =
    s"""{
       |  "p99Ms": $p99,
       |  "tokensPerSecond": $tps,
       |  "notes": "$note"
       |}""".stripMargin

  private def pass(p99: Double, tps: Int): LoopCheckWire =
    LoopCheckWire(true, Nil, List("p99_ms" -> p99, "tokens_per_s" -> tps.toDouble))

  private def fail(reasons: String*): LoopCheckWire = LoopCheckWire(false, reasons.toList, Nil)

  /** A spend, as an `(input, output)` pair — the shape the wire carries, and the shape
    * that adds: a generation's figure is every agent run it has paid for, so the
    * lineage below is written as its runs and summed rather than quoted. */
  private type Spend = (Long, Long)

  private def sum(runs: Spend*): Spend = (runs.map(_._1).sum, runs.map(_._2).sum)

  private val nothing: Spend = (0L, 0L)

  private def attempt(
      n: Int,
      description: String,
      art: String,
      at: String,
      check: Option[LoopCheckWire] = None,
      verdict: Option[LoopVerdictWire] = None,
      snapshot: Boolean = true
  ): LoopAttemptWire = LoopAttemptWire(n, description, art, snapshot, check, verdict, at)

  private def generation(
      gen: Int,
      parent: Option[Int],
      state: String,
      description: String,
      attempts: List[LoopAttemptWire],
      metrics: List[(String, Double)] = Nil,
      commit: Option[String] = None,
      startedAt: String = "2026-07-30T09:00:00Z",
      settledAt: Option[String] = Some("2026-07-30T09:40:00Z"),
      spent: Spend = nothing
  ): LoopGenerationWire =
    LoopGenerationWire(gen, parent, state, description, metrics, commit, attempts, startedAt, settledAt,
      inputTokens = spent._1, outputTokens = spent._2)

  // -- the live loop's settled lineage -----------------------------------------

  private val gen1 = generation(
    gen = 1,
    parent = None,
    state = "accepted",
    description = "Replace the regex scanner with a hand-rolled DFA over the byte stream.",
    metrics = List("p99_ms" -> 41.8, "tokens_per_s" -> 268000.0),
    commit = Some("a1f39c2e7b4d5061"),
    startedAt = "2026-07-30T09:02:11Z",
    settledAt = Some("2026-07-30T09:31:44Z"),
    spent = (78_400, 5_900),
    attempts = List(
      attempt(1, "Hand-rolled DFA over bytes; the regex engine was 60% of the profile.",
        artifact(41.8, 268000, "byte DFA replaces the regex scanner"), "2026-07-30T09:28:02Z",
        check = Some(pass(41.8, 268000)),
        verdict = Some(LoopVerdictWire(true, "A clean win and a smaller surface than what it replaces. The DFA table is generated at build time, so nothing is paid at startup.", goalReached = false)))
    )
  )

  private val gen2 = generation(
    gen = 2,
    parent = Some(1),
    state = "abandoned",
    description = "Batch the vocabulary lookups behind a single hash probe.",
    startedAt = "2026-07-30T09:33:02Z",
    settledAt = Some("2026-07-30T10:14:50Z"),
    spent = (121_600, 9_050),
    attempts = List(
      attempt(1, "One probe per token span instead of one per byte.",
        artifact(43.1, 259000, "batched vocabulary probe"), "2026-07-30T09:52:20Z",
        check = Some(fail("p99 did not improve enough (43.1 ms)"))),
      attempt(2, "Same idea with the probe inlined and the span buffer reused.",
        artifact(40.9, 271000, "batched probe, reused span buffer"), "2026-07-30T10:11:07Z",
        check = Some(pass(40.9, 271000)),
        verdict = Some(LoopVerdictWire(false, "Under 2% for a change that makes the lookup path considerably harder to follow. Not worth carrying — try the index instead.", goalReached = false)))
    )
  )

  private val gen3 = generation(
    gen = 3,
    parent = Some(1),
    state = "accepted",
    description = "Intern the vocabulary and index it by byte trigram.",
    metrics = List("p99_ms" -> 33.4, "tokens_per_s" -> 331000.0),
    commit = Some("7d2be08194aa3c15"),
    startedAt = "2026-07-30T10:16:30Z",
    settledAt = Some("2026-07-30T10:58:12Z"),
    spent = (92_300, 6_800),
    attempts = List(
      attempt(1, "Trigram index over an interned vocabulary; the tail was all cache misses.",
        artifact(33.4, 331000, "trigram index over interned vocab"), "2026-07-30T10:54:41Z",
        check = Some(pass(33.4, 331000)),
        verdict = Some(LoopVerdictWire(true, "20% off the tail and the throughput went up with it. The index is rebuilt from the vocabulary file, so the format is untouched as the goal requires.", goalReached = false)))
    )
  )

  private val gen4 = generation(
    gen = 4,
    parent = Some(3),
    state = "abandoned",
    description = "Precompute the whole merge table at load time.",
    startedAt = "2026-07-30T11:00:02Z",
    settledAt = Some("2026-07-30T11:22:19Z"),
    spent = (104_900, 8_240),
    attempts = List(
      attempt(1, "Materialise every merge up front and look it up by pair id.",
        artifact(31.9, 338000, "precomputed merge table"), "2026-07-30T11:11:55Z",
        check = Some(fail("the tests did not pass: 3 failures in TokenizerRoundTripSuite"))),
      attempt(2, "Same table, built lazily per shard to keep memory down.",
        artifact(33.0, 329000, "sharded merge table"), "2026-07-30T11:20:40Z",
        check = Some(fail("p99 did not improve enough (33.0 ms)")))
    )
  )

  private val gen5 = generation(
    gen = 5,
    parent = Some(3),
    state = "abandoned",
    description = "Cache merges per document rather than globally.",
    startedAt = "2026-07-30T11:24:00Z",
    settledAt = Some("2026-07-30T11:47:33Z"),
    spent = (71_500, 5_120),
    attempts = List(
      attempt(1, "A per-document merge cache, cleared between documents.",
        artifact(32.8, 334000, "per-document merge cache"), "2026-07-30T11:44:12Z",
        check = Some(pass(32.8, 334000)),
        verdict = Some(LoopVerdictWire(false, "The cache only pays on documents long enough to repeat a merge, and the benchmark's are. On the traffic mix this is noise, and it adds a lifetime to reason about.", goalReached = false)))
    )
  )

  private val settled = List(gen1, gen2, gen3, gen4, gen5)

  // -- generation 6, as it unfolds ---------------------------------------------

  private val gen6Started = "2026-07-30T11:49:10Z"

  /** What generation 6 costs, run by run: two worker attempts and the evaluator's read
    * of the second, each caught once part-way through and once finished.
    *
    * A generation's figure is every run it has paid for INCLUDING the one in flight as
    * far as it has got — which is the whole reason the host sends it rather than leaving
    * the page to add a settled number to a live one. So each point of the timeline
    * passes the sum of the runs that have happened by then, and the stage beside it
    * carries the last term alone. The checker's steps carry nothing: that gate is the
    * loop's own Scala and asks no model anything.
    */
  private val g6Worker1Part: Spend = (3_200, 640)
  private val g6Worker1: Spend = (18_400, 2_150)
  private val g6Worker2Part: Spend = (2_100, 380)
  private val g6Worker2: Spend = (16_900, 1_980)
  private val g6EvalPart: Spend = (5_400, 610)
  private val g6Eval: Spend = (12_600, 1_420)

  /** Generation 7's worker, a few hundred tokens into its first attempt. */
  private val g7Worker1Part: Spend = (1_800, 260)

  private def gen6(attempts: List[LoopAttemptWire], spent: Spend): LoopGenerationWire =
    generation(6, Some(3), "running", "Fuse the merge loop into the trigram scan.", attempts,
      startedAt = gen6Started, settledAt = None, spent = spent)

  private val gen6Attempt1 = attempt(
    1,
    "Fuse the merge pass into the trigram scan so a token is walked once.",
    artifact(30.2, 349000, "fused merge into the trigram scan"),
    "2026-07-30T11:56:31Z"
  )

  private val gen6Attempt1Checked = gen6Attempt1.copy(
    check = Some(fail("the tests did not pass: TokenizerBoundarySuite — 2 failures on multi-byte boundaries")))

  private val gen6Attempt2 = attempt(
    2,
    "Fused scan again, this time carrying the partial code point across the buffer edge.",
    artifact(28.9, 356000, "fused scan with a carried boundary"),
    "2026-07-30T12:04:02Z"
  )

  private val gen6Attempt2Checked = gen6Attempt2.copy(check = Some(pass(28.9, 356000)))

  private val gen6Attempt2Accepted = gen6Attempt2Checked.copy(
    verdict = Some(LoopVerdictWire(true,
      "13% off the tail with the boundary bug fixed and a test that would have caught it. This is the shape the loop was looking for.",
      goalReached = false)))

  private val gen6Accepted = generation(
    gen = 6,
    parent = Some(3),
    state = "accepted",
    description = "Fuse the merge loop into the trigram scan.",
    metrics = List("p99_ms" -> 28.9, "tokens_per_s" -> 356000.0),
    commit = Some("c04a7f1329ed6b88"),
    startedAt = gen6Started,
    settledAt = Some("2026-07-30T12:09:55Z"),
    attempts = List(gen6Attempt1Checked, gen6Attempt2Accepted),
    spent = sum(g6Worker1, g6Worker2, g6Eval)
  )

  private val gen7 = generation(7, Some(6), "running", "", Nil,
    startedAt = "2026-07-30T12:10:20Z", settledAt = None, spent = g7Worker1Part)

  // -- the three loops ---------------------------------------------------------

  private def live(
      generations: List[LoopGenerationWire],
      phase: String,
      stage: Option[LoopStageWire],
      liveLabel: Option[String]
  ): LoopWire =
    priced(LoopWire(
      id = LiveId,
      phase = phase,
      goal = liveGoal,
      rubric = liveRubric,
      budgets = budgets,
      defSource = liveSource,
      defVersion = 2,
      held = true,
      parked = None,
      orphaned = false,
      activity = stage.map(activityLine),
      stage = stage,
      liveLabel = liveLabel,
      generations = generations,
      createdAt = "2026-07-30T09:01:40Z"
    ))

  /** A loop's own figure, which the host sends rather than leaving the page to add up:
    * the sum of its generations', the run in flight included, since a running
    * generation's own figure already carries it. Done here, where the host would do it,
    * so no frame can quote a total the lineage under it disagrees with. */
  private def priced(l: LoopWire): LoopWire =
    l.copy(inputTokens = l.generations.map(_.inputTokens).sum, outputTokens = l.generations.map(_.outputTokens).sum)

  private def stage(gen: Int, attempt: Int, step: String, spent: Spend = nothing): Option[LoopStageWire] =
    Some(LoopStageWire(gen, attempt, step, inputTokens = spent._1, outputTokens = spent._2))

  /** The sentence the host prints beside the stage, written here the way
    * `LoopBridge.activityLine` writes it — derived from the same stage rather than
    * typed out beside it, so the two halves of a frame cannot drift apart. */
  private def activityLine(s: LoopStageWire): String =
    s"gen ${s.gen}, attempt ${s.attempt} \u2014 ${s.step}"

  private val parkedLoop = priced(LoopWire(
    id = ParkedId,
    phase = "parked: patience exhausted",
    goal = "Rewrite README.md so a new contributor can run the test suite within five minutes of landing.",
    rubric = "Accepted when every command in the README runs verbatim on a clean checkout and the reading level stays under grade 11.",
    budgets = LoopBudgetsWire(maxGenerations = 8, patience = 2, maxAttemptsPerGeneration = 2),
    defSource =
      """lib.loop.start[Readme](
        |  id = "readme-polish",
        |  goal = "a new contributor can run the tests within five minutes",
        |  rubric = "every command runs verbatim on a clean checkout",
        |  budgets = LoopBudgets(maxGenerations = 8, patience = 2)
        |) { (prev, cand) =>
        |  val dry = lib.shell.run("bash", "scripts/readme-commands.sh")
        |  if !dry.ok then CheckResult.fail("a README command failed: " + dry.stderr.trim)
        |  else CheckResult.pass.withMetrics("grade_level" -> cand.artifact.gradeLevel)
        |}""".stripMargin,
    defVersion = 1,
    held = false,
    parked = Some("patience exhausted"),
    orphaned = false,
    activity = None,
    stage = None,
    liveLabel = None,
    generations = List(
      generation(1, None, "accepted", "Split the quickstart out of the overview and pin every command.",
        List(attempt(1, "Quickstart first, prose after.", artifact(0, 0, "quickstart split"), "2026-07-29T14:20:00Z",
          check = Some(LoopCheckWire(true, Nil, List("grade_level" -> 12.4))),
          verdict = Some(LoopVerdictWire(true, "Much better ordering. The grade level is still above the bar.", goalReached = false)))),
        metrics = List("grade_level" -> 12.4), commit = Some("11c9a2f0"),
        startedAt = "2026-07-29T14:02:00Z", settledAt = Some("2026-07-29T14:31:00Z"), spent = (41_200, 3_100)),
      generation(2, Some(1), "abandoned", "Cut the architecture section down to a paragraph.",
        List(
          attempt(1, "Architecture reduced to one paragraph and a link.", artifact(0, 0, "architecture cut"),
            "2026-07-29T14:50:00Z", check = Some(LoopCheckWire(false, List("a README command failed: sbt tokenizer/test — no such project"), Nil))),
          attempt(2, "Same cut, with the stale command removed.", artifact(0, 0, "architecture cut, command fixed"),
            "2026-07-29T15:05:00Z", check = Some(LoopCheckWire(true, Nil, List("grade_level" -> 12.1))),
            verdict = Some(LoopVerdictWire(false, "The paragraph lost the one thing a newcomer needs: where the entry point is.", goalReached = false)))
        ),
        startedAt = "2026-07-29T14:33:00Z", settledAt = Some("2026-07-29T15:12:00Z"), spent = (58_700, 4_260)),
      generation(3, Some(1), "abandoned", "Rewrite the opening in the second person.",
        List(attempt(1, "Second-person opening throughout.", artifact(0, 0, "second person"), "2026-07-29T15:30:00Z",
          check = Some(LoopCheckWire(true, Nil, List("grade_level" -> 11.8))),
          verdict = Some(LoopVerdictWire(false, "Reads well but the grade level barely moved, and two generations in a row have not moved it.", goalReached = false)))),
        startedAt = "2026-07-29T15:14:00Z", settledAt = Some("2026-07-29T15:36:00Z"), spent = (33_900, 2_480))
    ),
    createdAt = "2026-07-29T14:00:00Z"
  ))

  /** The one loop in the fixture that has spent nothing — a ledger written before any
    * of this was counted, which is what a project's older loops look like. Everything
    * priced drops its figure rather than printing a zero, and this is what shows that
    * on screen: no cell in the strip, no stat in a generation window, no fact in an
    * overview. */
  private val orphanLoop = priced(LoopWire(
    id = OrphanId,
    phase = "orphaned (dead session)",
    goal = "Make the integration suite deterministic — no retries, no sleeps, no ordering assumptions.",
    rubric = "Accepted when 50 consecutive runs of `sbt it/test` are green and the suite takes no longer than it does today.",
    budgets = LoopBudgetsWire(maxGenerations = 12, patience = 3, maxAttemptsPerGeneration = 3),
    defSource =
      """lib.loop.start[Flake](
        |  id = "flaky-tests",
        |  goal = "make the integration suite deterministic",
        |  rubric = "50 consecutive green runs, no slower than today",
        |  budgets = LoopBudgets(maxGenerations = 12, patience = 3)
        |) { (prev, cand) =>
        |  val runs = (1 to 50).map(_ => lib.shell.run("sbt", "it/test"))
        |  val failed = runs.count(!_.ok)
        |  if failed > 0 then CheckResult.fail(s"$failed of 50 runs failed")
        |  else CheckResult.pass.withMetrics("flake_rate" -> 0.0, "suite_s" -> cand.artifact.suiteSeconds)
        |}""".stripMargin,
    defVersion = 1,
    held = false,
    parked = None,
    orphaned = true,
    activity = None,
    stage = None,
    liveLabel = None,
    generations = List(
      generation(1, None, "accepted", "Replace the sleeps in the fixture with a readiness probe.",
        List(attempt(1, "Readiness probe instead of sleep(2000).", artifact(0, 0, "readiness probe"), "2026-07-28T18:20:00Z",
          check = Some(LoopCheckWire(true, Nil, List("flake_rate" -> 0.04, "suite_s" -> 212.0))),
          verdict = Some(LoopVerdictWire(true, "The obvious first cut, and it took the flake rate down by half.", goalReached = false)))),
        metrics = List("flake_rate" -> 0.04, "suite_s" -> 212.0), commit = Some("9e0c4471"),
        startedAt = "2026-07-28T18:02:00Z", settledAt = Some("2026-07-28T18:29:00Z")),
      generation(2, Some(1), "running", "Pin the fixture's port allocation.",
        List(attempt(1, "Allocate the port from the OS and thread it through.",
          artifact(0, 0, "ephemeral port allocation"), "2026-07-28T18:44:00Z")),
        startedAt = "2026-07-28T18:31:00Z", settledAt = None)
    ),
    createdAt = "2026-07-28T18:00:00Z"
  ))

  /** A loop that finished: parked because the evaluator said the goal was reached,
    * which is the one ending the lineage marks with a terminal tick. */
  private val reachedLoop = priced(LoopWire(
    id = ReachedId,
    phase = "parked: goal reached",
    goal = "Give every public method in `lib` a runnable example that the doc test suite executes.",
    rubric = "Accepted when `sbt docTest` is green and the share of public methods with an executed example rises.",
    budgets = LoopBudgetsWire(maxGenerations = 10, patience = 3, maxAttemptsPerGeneration = 2),
    defSource =
      """lib.loop.start[Docs](
        |  id = "docs-examples",
        |  goal = "every public method in `lib` has a runnable example",
        |  rubric = "`sbt docTest` is green and coverage rises",
        |  budgets = LoopBudgets(maxGenerations = 10, patience = 3)
        |) { (prev, cand) =>
        |  val doc = lib.shell.run("sbt", "docTest")
        |  if !doc.ok then CheckResult.fail("the doc tests did not pass")
        |  else if prev.exists(_.artifact.covered >= cand.artifact.covered) then
        |    CheckResult.fail("coverage did not rise")
        |  else CheckResult.pass.withMetrics("covered" -> cand.artifact.covered)
        |}""".stripMargin,
    defVersion = 1,
    held = false,
    parked = Some("goal reached"),
    orphaned = false,
    activity = None,
    stage = None,
    liveLabel = None,
    generations = List(
      generation(1, None, "accepted", "Examples for the eight `lib.fs` methods, executed by the doc suite.",
        List(attempt(1, "Eight `lib.fs` examples, all executed.", artifact(0, 0, "lib.fs examples"), "2026-07-27T10:40:00Z",
          check = Some(LoopCheckWire(true, Nil, List("covered" -> 0.31))),
          verdict = Some(LoopVerdictWire(true, "Real examples that run, not snippets that look like they would.", goalReached = false)))),
        metrics = List("covered" -> 0.31), commit = Some("4b7e0092"),
        startedAt = "2026-07-27T10:20:00Z", settledAt = Some("2026-07-27T10:52:00Z"), spent = (36_500, 2_900)),
      generation(2, Some(1), "abandoned", "Generate the remaining examples from the type signatures.",
        List(attempt(1, "Signature-derived examples for everything left.", artifact(0, 0, "generated examples"),
          "2026-07-27T11:14:00Z",
          check = Some(LoopCheckWire(true, Nil, List("covered" -> 0.88))),
          verdict = Some(LoopVerdictWire(false, "Coverage went up and the examples say nothing. An example that only restates the signature teaches a reader less than no example at all.", goalReached = false)))),
        startedAt = "2026-07-27T10:55:00Z", settledAt = Some("2026-07-27T11:20:00Z"), spent = (44_100, 3_640)),
      generation(3, Some(1), "accepted", "Hand-written examples for `lib.shell` and `lib.git`, each doing one real task.",
        List(attempt(1, "Twenty-one hand-written examples across shell and git.", artifact(0, 0, "shell + git examples"),
          "2026-07-27T12:02:00Z",
          check = Some(LoopCheckWire(true, Nil, List("covered" -> 0.74))),
          verdict = Some(LoopVerdictWire(true, "These read like something a person would want to copy.", goalReached = false)))),
        metrics = List("covered" -> 0.74), commit = Some("d61c33ae"),
        startedAt = "2026-07-27T11:22:00Z", settledAt = Some("2026-07-27T12:09:00Z"), spent = (52_800, 4_010)),
      generation(4, Some(3), "accepted", "The last nine methods, plus a check that keeps new ones covered.",
        List(attempt(1, "The remaining nine, and a doc-coverage assertion in the suite.",
          artifact(0, 0, "final nine + coverage gate"), "2026-07-27T12:48:00Z",
          check = Some(LoopCheckWire(true, Nil, List("covered" -> 1.0))),
          verdict = Some(LoopVerdictWire(true, "Every public method has an example that runs, and the suite will fail if a new one arrives without one. That is the goal.", goalReached = true)))),
        metrics = List("covered" -> 1.0), commit = Some("f9204ab7"),
        startedAt = "2026-07-27T12:11:00Z", settledAt = Some("2026-07-27T12:55:00Z"), spent = (39_700, 3_120))
    ),
    createdAt = "2026-07-27T10:18:00Z"
  ))

  // -- the timeline ------------------------------------------------------------

  private def loopMsg(l: LoopWire): WireMessage = WireMessage.Loop(l)
  private def act(e: TranscriptEvent): WireMessage = WireMessage.Activity(e)

  private def worker(gen: Int): String = s"gen-$gen-worker"
  private def evaluator(gen: Int): String = s"gen-$gen-eval"

  /** The whole scenario. The snapshot lands first (as the host's connect order has
    * it), then the live loop is upserted whole at each step of its drive cycle while
    * its worker and evaluator stream into their own transcripts. */
  def script: Scenarios.Script =
    val snapshot: Scenarios.Script = Vector(
      0 -> WireMessage.LoopSnapshot(List(
        live(settled :+ gen6(Nil, g6Worker1Part), "running (gen 6)",
          stage(6, 1, "working", g6Worker1Part), Some(worker(6))),
        parkedLoop,
        orphanLoop,
        reachedLoop
      ))
    )
    snapshot ++ gen6Working ++ gen6FirstCheck ++ gen6SecondAttempt ++ gen6Evaluating ++ gen6Acceptance

  /** Attempt 1: the worker reasons, reads, edits, benchmarks, submits. */
  private def gen6Working: Scenarios.Script =
    val id = LiveId
    val w = worker(6)
    Vector(
      300 -> act(Thought(id, w, "The trigram scan and the merge pass walk the same token twice. ")),
      520 -> act(Thought(id, w, "If the merge table lookup happens inside the scan the second walk disappears — the risk is the buffer boundary.")),
      900 -> act(Said(id, w, "Generation 5 showed the merge cache is the wrong lever. ")),
      1150 -> act(Said(id, w, "The two passes over each token are the real cost, so let me fuse them.")),
      1500 -> act(ToolCalled(id, w, "g6a1-c1", "eval_scala",
        raw"""{"code": "// where the scan hands off to the merge pass\nlib.grep(\"mergePass\\\\(\", path = \"tokenizer/src/main/scala\").take(6)"}""")),
      2000 -> act(ToolReturned(id, w, "g6a1-c1",
        "tokenizer/src/main/scala/Tokenizer.scala:88:    val merged = mergePass(spans)\ntokenizer/src/main/scala/MergeTable.scala:41:  def mergePass(spans: Array[Span]): Array[Token] =",
        isError = false)),
      2300 -> act(Said(id, w, "\n\nOne call site. I'll inline the merge into the scan loop and keep `mergePass` as a thin wrapper for the tests.")),
      2700 -> act(ToolCalled(id, w, "g6a1-c2", "eval_scala",
        raw"""{"code": "lib.shell.run(\"sbt\", \"tokenizerBench\").stdout.linesIterator.filter(_.contains(\"p99\")).mkString(\"\\n\")"}""")),
      3300 -> act(ToolReturned(id, w, "g6a1-c2", "p99  30.2 ms   (was 33.4 ms)\nthroughput  349k tok/s", isError = false)),
      3600 -> act(Said(id, w, "\n\n30.2 ms, down from 33.4. Submitting.")),
      3900 -> loopMsg(live(settled :+ gen6(List(gen6Attempt1), sum(g6Worker1)), "running (gen 6)",
        stage(6, 1, "checking"), Some(worker(6))))
    )

  /** The checker refuses attempt 1 — which is what the second attempt is for. */
  private def gen6FirstCheck: Scenarios.Script =
    Vector(
      5000 -> loopMsg(live(settled :+ gen6(List(gen6Attempt1Checked), sum(g6Worker1, g6Worker2Part)),
        "running (gen 6)", stage(6, 2, "working", g6Worker2Part), Some(worker(6))))
    )

  /** Attempt 2: the boundary bug, found and fixed. */
  private def gen6SecondAttempt: Scenarios.Script =
    val id = LiveId
    val w = worker(6)
    Vector(
      5200 -> act(Said(id, w, "\n\nThe checker is right — the fused loop drops a partial code point at the buffer edge. ")),
      5450 -> act(Said(id, w, "The old two-pass version re-scanned from the span start, which hid it.")),
      5800 -> act(Thought(id, w, "Carry the partial code point across the boundary in the scan state rather than re-reading it.")),
      6200 -> act(ToolCalled(id, w, "g6a2-c1", "eval_scala",
        raw"""{"code": "lib.shell.run(\"sbt\", \"tokenizer/testOnly *BoundarySuite\").stdout.takeRight(200)"}""")),
      6900 -> act(ToolReturned(id, w, "g6a2-c1", "TokenizerBoundarySuite:\n  + multi-byte at buffer edge  12ms\n  + surrogate pair split  9ms\nAll tests passed.", isError = false)),
      7200 -> act(Said(id, w, "\n\nGreen, and I added a case for the split surrogate pair. Submitting.")),
      7500 -> loopMsg(live(settled :+ gen6(List(gen6Attempt1Checked, gen6Attempt2), sum(g6Worker1, g6Worker2)),
        "running (gen 6)", stage(6, 2, "checking"), Some(worker(6))))
    )

  /** The checker passes it, and the evaluator reads the patch. */
  private def gen6Evaluating: Scenarios.Script =
    val id = LiveId
    val e = evaluator(6)
    val withCheck = settled :+ gen6(List(gen6Attempt1Checked, gen6Attempt2Checked),
      sum(g6Worker1, g6Worker2, g6EvalPart))
    Vector(
      8600 -> loopMsg(live(withCheck, "running (gen 6)",
        stage(6, 2, "evaluating", g6EvalPart), Some(evaluator(6)))),
      8900 -> act(Thought(id, e, "Two attempts, and the second only differs by the carried boundary state. Reading the patch.")),
      9300 -> act(Said(id, e, "The fused scan is a genuine structural win rather than a tuning constant: ")),
      9600 -> act(Said(id, e, "each token is now walked once instead of twice, which is why throughput moved with the tail.")),
      10000 -> act(ToolCalled(id, e, "g6-eval-c1", "eval_scala",
        raw"""{"code": "lib.shell.run(\"git\", \"diff\", \"--stat\", \"HEAD~1\").stdout"}""")),
      10500 -> act(ToolReturned(id, e, "g6-eval-c1",
        " tokenizer/src/main/scala/Tokenizer.scala  | 34 +++++++++++++-------\n tokenizer/src/main/scala/MergeTable.scala | 12 ++------\n tokenizer/src/test/scala/BoundarySuite.scala | 18 ++++++++++\n 3 files changed, 45 insertions(+), 19 deletions(-)",
        isError = false)),
      10900 -> act(Said(id, e, "\n\nThe boundary test the first attempt lacked is in the patch. Accepting."))
    )

  /** Accepted — the lineage grows a circle and the next generation starts. */
  private def gen6Acceptance: Scenarios.Script =
    val id = LiveId
    val w = worker(7)
    Vector(
      11600 -> loopMsg(live((settled :+ gen6Accepted) :+ gen7, "running (gen 7)",
        stage(7, 1, "working", g7Worker1Part), Some(worker(7)))),
      12000 -> act(Thought(id, w, "28.9 ms against a 41.8 ms baseline is 31% — the goal is within reach. What is left in the profile?")),
      12500 -> act(Said(id, w, "The scan is fused now, so the remaining tail is the vocabulary probe. ")),
      12900 -> act(Said(id, w, "Let me look at where the misses are."))
    )

  // -- the on-demand payloads --------------------------------------------------

  /** Answer one `/api/loop/...` path, or nothing — which the server turns into the
    * 404 the real host would send for a generation that never wrote a tee. */
  def api(segments: List[String]): Option[String] = segments match
    case "loop" :: LiveId :: "transcript" :: label :: Nil => transcripts.get(label).map(jsonl(LiveId, label, _))
    case "loop" :: LiveId :: "diff" :: gen :: attempt :: Nil => diffs.get((gen, attempt))
    case _ => None

  /** A settled generation's tee, as the file on disk would hold it: one encoded
    * `Activity` frame per line. */
  private def jsonl(loopId: String, label: String, events: Vector[(String, String) => TranscriptEvent]): String =
    events.map(f => WireCodec.encode(WireMessage.Activity(f(loopId, label)))).mkString("\n")

  private type Frame = (String, String) => TranscriptEvent

  private def said(text: String): Frame = (r, n) => Said(r, n, text)
  private def thought(text: String): Frame = (r, n) => Thought(r, n, text)
  private def called(id: String, tool: String, input: String): Frame = (r, n) => ToolCalled(r, n, id, tool, input)
  private def returned(id: String, out: String): Frame = (r, n) => ToolReturned(r, n, id, out, isError = false)

  private val transcripts: Map[String, Vector[Frame]] = Map(
    worker(1) -> Vector(
      thought("The profile says 60% of the time is in the regex engine. A hand-rolled DFA over bytes should take most of that."),
      said("Starting from the profile: the regex scanner dominates. "),
      said("I'll generate a DFA table at build time and walk bytes directly."),
      called("g1-c1", "eval_scala", raw"""{"code": "lib.shell.run(\"sbt\", \"tokenizerBench\").stdout.linesIterator.take(4).mkString(\"\\n\")"}"""),
      returned("g1-c1", "p99  41.8 ms   (was 63.5 ms)\nthroughput  268k tok/s"),
      said("\n\n41.8 ms from 63.5. The API is unchanged and the vocabulary file is untouched.")
    ),
    evaluator(1) -> Vector(
      thought("A large diff, but almost all of it is the generated table."),
      said("The hand-written part is small and the generated table is reproducible from the vocabulary file. "),
      said("A 34% cut on the tail with no format change. Accepting.")
    ),
    worker(3) -> Vector(
      thought("Generation 2 was refused for being complicated rather than wrong. An index is simpler and should pay more."),
      said("Interning the vocabulary lets me index by byte trigram, which turns the probe into one cache line. "),
      called("g3-c1", "eval_scala", raw"""{"code": "lib.shell.run(\"sbt\", \"tokenizerBench\").stdout.linesIterator.take(4).mkString(\"\\n\")"}"""),
      returned("g3-c1", "p99  33.4 ms   (was 41.8 ms)\nthroughput  331k tok/s"),
      said("\n\n33.4 ms, and throughput went up rather than down.")
    ),
    evaluator(3) -> Vector(
      said("20% off the tail and the throughput moved with it. The index is derived from the vocabulary file at load, so the format is untouched. Accepting.")
    ),
    worker(6) -> Vector(
      thought("The trigram scan and the merge pass walk the same token twice."),
      said("Fusing the two passes. The risk is the buffer boundary."),
      called("g6-c1", "eval_scala", raw"""{"code": "lib.shell.run(\"sbt\", \"tokenizerBench\").stdout"}"""),
      returned("g6-c1", "p99  28.9 ms   (was 33.4 ms)\nthroughput  356k tok/s"),
      said("\n\n28.9 ms with the boundary case fixed.")
    )
  )

  private val diffs: Map[(String, String), String] = Map(
    ("6", "2") ->
      """diff --git a/tokenizer/src/main/scala/Tokenizer.scala b/tokenizer/src/main/scala/Tokenizer.scala
        |index 8f1c2a9..c04a7f1 100644
        |--- a/tokenizer/src/main/scala/Tokenizer.scala
        |+++ b/tokenizer/src/main/scala/Tokenizer.scala
        |@@ -84,17 +84,26 @@ final class Tokenizer(vocab: Vocabulary):
        |   def encode(bytes: Array[Byte]): Array[Token] =
        |-    val spans = scanTrigrams(bytes)
        |-    val merged = mergePass(spans)
        |-    merged
        |+    // One walk: the merge table is consulted as each span is produced, so a
        |+    // token is never visited twice. `carry` holds a code point split across
        |+    // the buffer edge, which the two-pass version re-read instead.
        |+    var carry = Carry.empty
        |+    val out = Array.newBuilder[Token]
        |+    scanTrigrams(bytes, carry): span =>
        |+      out += vocab.merge(span, carry)
        |+    out.result()
        |
        |diff --git a/tokenizer/src/main/scala/MergeTable.scala b/tokenizer/src/main/scala/MergeTable.scala
        |index 41ab003..7bd9910 100644
        |--- a/tokenizer/src/main/scala/MergeTable.scala
        |+++ b/tokenizer/src/main/scala/MergeTable.scala
        |@@ -38,12 +38,6 @@ object MergeTable:
        |-  def mergePass(spans: Array[Span]): Array[Token] =
        |-    val out = Array.newBuilder[Token]
        |-    var i = 0
        |-    while i < spans.length do
        |-      out += merge(spans(i))
        |-      i += 1
        |-    out.result()
        |+  // kept as a thin wrapper: the round-trip suite calls it directly
        |+  def mergePass(spans: Array[Span]): Array[Token] = spans.map(merge)
        |
        |diff --git a/tokenizer/src/test/scala/BoundarySuite.scala b/tokenizer/src/test/scala/BoundarySuite.scala
        |index e2f0011..1a93b7c 100644
        |--- a/tokenizer/src/test/scala/BoundarySuite.scala
        |+++ b/tokenizer/src/test/scala/BoundarySuite.scala
        |@@ -12,6 +12,24 @@ class TokenizerBoundarySuite extends munit.FunSuite:
        |+  test("a multi-byte code point split across the buffer edge round-trips"):
        |+    val text = "ünicode" * 700
        |+    assertEquals(tok.decode(tok.encode(text.getBytes("UTF-8"))), text)
        |+
        |+  test("a surrogate pair split across the buffer edge round-trips"):
        |+    val text = "\uD83D\uDC0D" * 900
        |+    assertEquals(tok.decode(tok.encode(text.getBytes("UTF-8"))), text)
        |""".stripMargin,
    ("6", "1") ->
      """diff --git a/tokenizer/src/main/scala/Tokenizer.scala b/tokenizer/src/main/scala/Tokenizer.scala
        |index 8f1c2a9..b7712de 100644
        |--- a/tokenizer/src/main/scala/Tokenizer.scala
        |+++ b/tokenizer/src/main/scala/Tokenizer.scala
        |@@ -84,10 +84,14 @@ final class Tokenizer(vocab: Vocabulary):
        |   def encode(bytes: Array[Byte]): Array[Token] =
        |-    val spans = scanTrigrams(bytes)
        |-    val merged = mergePass(spans)
        |-    merged
        |+    val out = Array.newBuilder[Token]
        |+    scanTrigrams(bytes): span =>
        |+      out += vocab.merge(span)
        |+    out.result()
        |""".stripMargin,
    ("1", "1") ->
      """diff --git a/tokenizer/src/main/scala/Scanner.scala b/tokenizer/src/main/scala/Scanner.scala
        |index 3a01f77..a1f39c2 100644
        |--- a/tokenizer/src/main/scala/Scanner.scala
        |+++ b/tokenizer/src/main/scala/Scanner.scala
        |@@ -5,12 +5,9 @@ object Scanner:
        |-  private val Token = raw"[\p{L}\p{N}]+|[^\s\p{L}\p{N}]".r
        |-  def scan(s: String): Iterator[String] = Token.findAllIn(s)
        |+  // Generated at build time from the vocabulary; see `dfaTable.scala`.
        |+  def scan(bytes: Array[Byte]): SpanIterator = new DfaSpanIterator(bytes, DfaTable.states)
        |""".stripMargin,
    ("3", "1") ->
      """diff --git a/tokenizer/src/main/scala/Vocabulary.scala b/tokenizer/src/main/scala/Vocabulary.scala
        |index a1f39c2..7d2be08 100644
        |--- a/tokenizer/src/main/scala/Vocabulary.scala
        |+++ b/tokenizer/src/main/scala/Vocabulary.scala
        |@@ -18,8 +18,15 @@ final class Vocabulary(entries: Array[String]):
        |-  private val byString = entries.zipWithIndex.toMap
        |-  def lookup(s: String): Int = byString.getOrElse(s, Unknown)
        |+  // Interned once at load, then indexed by the first three bytes so a probe
        |+  // touches one cache line instead of walking a hash chain.
        |+  private val interned: Array[String] = entries.map(_.intern())
        |+  private val byTrigram: Array[Array[Int]] = buildTrigramIndex(interned)
        |+  def lookup(span: Span): Int =
        |+    val bucket = byTrigram(span.trigram)
        |+    var i = 0
        |+    while i < bucket.length do
        |+      if span.matches(interned(bucket(i))) then return bucket(i)
        |+      i += 1
        |+    Unknown
        |""".stripMargin
  )
