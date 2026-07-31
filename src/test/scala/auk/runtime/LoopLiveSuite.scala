package auk.runtime

import scala.concurrent.duration.Duration
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given

import auk.TestFs
import auk.agent.{LoopView, SystemPrompt}
import auk.llm.endpoint.LLMConfig
import auk.llm.provider.{ModelSelection, ModelSession}
import auk.llm.tools.{ApprovalPolicy, RuntimeContext}
import auk.loop.{LoopEvent, LoopState, LoopStore}
import auk.platform.{PathOps, Platform}
import auk.platform.js.{Interop, ReplArtifacts}
import auk.runtime.repl.{ReplPreamble, ScalaRepl}
import auk.snapshot.Snapshot

/** Opt-in live check: real z.ai glm-5.2 drives a real refinement loop, end to end.
  *
  * A throwaway git repository holds a Node program with an honest optimisation
  * opportunity — an O(n²) dedupe, a benchmark that prints a throughput metric, and a
  * test that says whether the function still works. The loop's definition is written
  * in a REAL lead REPL and travels the production path (`lib.loop.start` → marker →
  * [[EvalScala]] → [[LoopBridge.announceDef]] → gate validation), and from then on the
  * host drives generations: a live worker agent edits the tree, the definition's own
  * Scala checker runs the tests and the benchmark, and a live evaluator agent judges
  * the result against the rubric.
  *
  * Everything the loop touches is inside the temp repository: its agents' REPLs are
  * spawned with the tree as their working directory (the [[chdirPreamble]]), which is
  * what production gives them, and the checker addresses the tree by absolute path.
  *
  * Runs only when `AUK_LIVE_TESTS=1` and `ZAI_API_KEY` are set:
  *   bash -c 'source .envrc && AUK_LIVE_TESTS=1 sbt "testOnly auk.runtime.LoopLiveSuite"'
  */
class LoopLiveSuite extends munit.FunSuite:

  // Three generations of a real model editing, benchmarking and judging.
  override val munitTimeout = Duration(1800, "s")

  private def enabled: Boolean =
    Platform.env.get("AUK_LIVE_TESTS").contains("1") &&
      Platform.env.get("ZAI_API_KEY").isDefined &&
      ReplArtifacts.resolve().isRight

  // -- the program under optimisation ------------------------------------------------

  /** The slow implementation the loop is pointed at: correct, exported, and quadratic. */
  private val LibJs =
    """'use strict';
      |
      |// Remove duplicate items, preserving the order they were first seen in.
      |function dedupe(items) {
      |  const out = [];
      |  for (let i = 0; i < items.length; i++) {
      |    let seen = false;
      |    for (let j = 0; j < out.length; j++) {
      |      if (out[j] === items[i]) { seen = true; break; }
      |    }
      |    if (!seen) out.push(items[i]);
      |  }
      |  return out;
      |}
      |
      |module.exports = { dedupe };
      |""".stripMargin

  /** The measurement the loop is gated on. Deterministic input (a fixed LCG), one
    * untimed warm-up pass, then a fixed amount of work whose rate is reported. */
  private val BenchJs =
    """'use strict';
      |const { dedupe } = require('./lib.js');
      |
      |// A Lehmer generator, whose products stay inside the exact integer range of a
      |// double — so the input is the same varied 984-of-1000 distinct items on every
      |// run, and the quadratic scan really is quadratic.
      |function makeItems(n) {
      |  let seed = 1;
      |  const items = [];
      |  for (let i = 0; i < n; i++) {
      |    seed = (seed * 48271) % 2147483647;
      |    items.push('item-' + (seed % Math.floor(n / 4)));
      |  }
      |  return items;
      |}
      |
      |const N = 4000;
      |const REPS = 150;
      |const items = makeItems(N);
      |
      |dedupe(items);
      |
      |const started = process.hrtime.bigint();
      |let sink = 0;
      |for (let r = 0; r < REPS; r++) sink += dedupe(items).length;
      |const elapsedSec = Number(process.hrtime.bigint() - started) / 1e9;
      |
      |if (sink <= 0) throw new Error('the benchmark did no work');
      |console.log('METRIC opsPerSec ' + Math.round((N * REPS) / elapsedSec));
      |""".stripMargin

  /** What "still works" means. A Set-based rewrite passes all of it; returning the
    * input unchanged, or reordering, does not. */
  private val TestJs =
    """'use strict';
      |const assert = require('node:assert');
      |const { dedupe } = require('./lib.js');
      |
      |function check(name, fn) {
      |  try {
      |    fn();
      |  } catch (e) {
      |    console.error('FAIL ' + name + ': ' + e.message);
      |    process.exitCode = 1;
      |  }
      |}
      |
      |check('removes duplicates and keeps first-seen order', function () {
      |  assert.deepStrictEqual(dedupe(['b', 'a', 'b', 'c', 'a']), ['b', 'a', 'c']);
      |});
      |check('an empty array stays empty', function () {
      |  assert.deepStrictEqual(dedupe([]), []);
      |});
      |check('all-identical collapses to one', function () {
      |  assert.deepStrictEqual(dedupe(['x', 'x', 'x']), ['x']);
      |});
      |check('an array with nothing to remove is unchanged', function () {
      |  assert.deepStrictEqual(dedupe(['1', '2', '3']), ['1', '2', '3']);
      |});
      |check('the input array is not mutated', function () {
      |  const input = ['a', 'b', 'a'];
      |  dedupe(input);
      |  assert.deepStrictEqual(input, ['a', 'b', 'a']);
      |});
      |check('a large array dedupes correctly', function () {
      |  const items = [];
      |  for (let i = 0; i < 5000; i++) items.push('k-' + (i % 700));
      |  const out = dedupe(items);
      |  assert.strictEqual(out.length, 700);
      |  assert.strictEqual(out[0], 'k-0');
      |  assert.strictEqual(out[699], 'k-699');
      |});
      |
      |if (process.exitCode) console.error('tests failed');
      |else console.log('all tests passed');
      |""".stripMargin

  // -- fixtures ------------------------------------------------------------------

  private def git(dir: String, args: String*): String =
    val cp = js.Dynamic.global.require("node:child_process")
    val opts = js.Dynamic.literal(cwd = dir, encoding = "utf8")
    cp.execFileSync("git", js.Array(args.map(a => a: js.Any)*), opts).asInstanceOf[String].trim

  private def node(dir: String, script: String): String =
    val cp = js.Dynamic.global.require("node:child_process")
    val opts = js.Dynamic.literal(cwd = dir, encoding = "utf8")
    cp.execFileSync("node", js.Array(script: js.Any), opts).asInstanceOf[String]

  /** The repository the loop works in: the three files, committed. */
  private def tempRepo(): String =
    val dir = TestFs.tempDir("auk-loop-live")
    git(dir, "init", "-b", "main")
    TestFs.write(PathOps.join(dir, "lib.js"), LibJs)
    TestFs.write(PathOps.join(dir, "bench.js"), BenchJs)
    TestFs.write(PathOps.join(dir, "test.js"), TestJs)
    git(dir, "add", "-A")
    git(dir, "-c", "user.name=Test", "-c", "user.email=t@example.com", "commit", "-m", "the slow dedupe")
    dir

  /** Copies of the two files the loop may not edit, kept OUTSIDE the working tree so a
    * generation cannot reach them and the checker can compare against them. */
  private def pristineDir(): String =
    val dir = TestFs.tempDir("auk-loop-live-pristine")
    TestFs.write(PathOps.join(dir, "bench.js"), BenchJs)
    TestFs.write(PathOps.join(dir, "test.js"), TestJs)
    dir

  private def metricOf(output: String): Option[Double] =
    output.linesIterator
      .find(_.startsWith("METRIC opsPerSec "))
      .map(_.substring("METRIC opsPerSec ".length).trim)
      .flatMap(_.toDoubleOption)

  private def tmpSock(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-looplive-$name-${js.Dynamic.global.process.pid}.sock").asInstanceOf[String]

  /** Every REPL this loop spawns — worker, gate and evaluator — starts in the loop's
    * working tree, which is what production gives them (there the tree IS the project
    * the agent process runs in). Without it the agents would read and edit auk's own
    * checkout, since a worker is told to work in "the working tree" and reaches it
    * through `lib.fs.cwd`. */
  private def chdirPreamble(repo: String): String =
    ReplPreamble.Source + "\n" + s"""scala.scalajs.js.Dynamic.global.process.chdir("$repo")"""

  /** The loop's whole definition, as the lead would write it in one eval: the artifact
    * type, the helpers the checker needs, and the checker itself.
    *
    * The checker is the honest part of this fixture — it re-runs the tests, refuses a
    * candidate that edited the benchmark or the tests, measures the throughput itself
    * rather than trusting the worker's claim, and demands a real improvement over what
    * the last accepted generation measured. */
  private def definition(loopId: String, repo: String, pristine: String, baseline: Double): String =
    s"""case class Perf(opsPerSec: Double, notes: String) derives LibToolInput
       |val repo = "$repo"
       |val pristine = "$pristine"
       |// Ascribed: a whole Double renders without its `.0` under Scala.js, and an Int
       |// here would make the comparison below a union type that does not arithmetic.
       |val baseline: Double = $baseline
       |val sh = lib.shell.at(lib.path(repo)).withTimeout(180000)
       |def content(p: String): String =
       |  try lib.path(p).openAsFile.rawContent
       |  catch
       |    case _: Throwable => ""
       |def edited(name: String): Boolean =
       |  content(repo + "/" + name) != content(pristine + "/" + name)
       |def metric(out: String): Option[Double] =
       |  out.linesIterator
       |    .find(_.startsWith("METRIC opsPerSec "))
       |    .map(_.substring("METRIC opsPerSec ".length).trim)
       |    .flatMap(_.toDoubleOption)
       |lib.loop.start[Perf](
       |  id = "$loopId",
       |  goal =
       |    "Make dedupe() in lib.js faster. It is the only file you may change: bench.js and " +
       |      "test.js are fixed and a candidate that edits either one is rejected. The function " +
       |      "must keep its exact behaviour — same name, same export, first-seen order preserved, " +
       |      "input never mutated — and `node test.js` must exit 0. Report the throughput " +
       |      "`node bench.js` prints as opsPerSec.",
       |  rubric =
       |    "Accepted when node test.js passes, bench.js and test.js are untouched, and the " +
       |      "measured opsPerSec is meaningfully higher than the previous generation's. Reject " +
       |      "anything that weakens the tests, special-cases the benchmark's input, or caches " +
       |      "results across calls instead of making the function itself faster.",
       |  budgets = LoopBudgets(maxGenerations = 3, patience = 1, maxAttemptsPerGeneration = 2)
       |) { (prev, cand) =>
       |  if edited("bench.js") then CheckResult.fail("bench.js was modified; the benchmark is fixed")
       |  else if edited("test.js") then CheckResult.fail("test.js was modified; the tests are fixed")
       |  else
       |    val tested = sh.run("node", "test.js")
       |    if !tested.ok then
       |      CheckResult.fail("node test.js failed (exit " + tested.exitCode + "): " + tested.output.trim.takeRight(800))
       |    else
       |      val benched = sh.run("node", "bench.js")
       |      metric(benched.stdout) match
       |        case None =>
       |          CheckResult.fail("node bench.js printed no METRIC line: " + benched.output.trim.takeRight(400))
       |        case Some(ops) =>
       |          // A quarter faster, which run-to-run noise (a few percent on this bench)
       |          // cannot manufacture and an actual algorithmic fix clears many times over.
       |          val floor = prev.flatMap(_.metrics.get("opsPerSec")).getOrElse(baseline) * 1.25
       |          if ops <= floor then
       |            CheckResult
       |              .fail("throughput did not improve enough: " + ops.round + " ops/sec, needs more than " + floor.round)
       |              .withMetrics("opsPerSec" -> ops)
       |          else CheckResult.pass.withMetrics("opsPerSec" -> ops)
       |}""".stripMargin

  private def awaitPhase(bridge: LoopBridge, loopId: String, pred: String => Boolean, timeoutMs: Int)(using
      Async
  ): String =
    var waited = 0
    def phase = bridge.statusOf(loopId).getOrElse("")
    while !pred(phase) && waited < timeoutMs do
      Interop.sleep(200.0)
      waited += 200
    phase

  // -- the dogfood ---------------------------------------------------------------

  test("glm-5.2 drives a refinement loop: a real generation is measured, judged and accepted"):
    assume(enabled, "set AUK_LIVE_TESTS=1 and ZAI_API_KEY to run this live test")
    Async.fromSync:
      // CLAUDE.md pins the test model, so this suite names it rather than resolving
      // whatever `.auk/config` happens to hold.
      val resolved = ModelSelection.byRef("ZAI", "glm-5.2") match
        case Right(r)  => r
        case Left(err) => fail(s"model resolve failed: $err")
      val models = ModelSession.of(
        resolved.endpoint,
        LLMConfig(model = resolved.model.id, thinking = Some(resolved.model.thinking))
      )

      val repo = tempRepo()
      val pristine = pristineDir()
      // The bar generation 1 has to clear, measured on the tree as committed.
      val baseline = metricOf(node(repo, "bench.js")).getOrElse(fail("the baseline bench printed no metric"))
      println(s"[LIVE] repo=$repo baseline=${baseline.round} ops/sec")

      val notices = UnboundedChannel[String]()
      val chatter = UnboundedChannel[String]()
      val views = scala.collection.mutable.ListBuffer.empty[Vector[LoopView]]
      val bridge = LoopBridge(
        socketPath = tmpSock("dogfood"),
        models = models,
        makeRepl = env =>
          ScalaRepl(
            () => ReplArtifacts.resolve().map(s => s.copy(env = s.env ++ env)),
            preamble = chdirPreamble(repo)
          )
        ,
        // The production tool set for a loop agent, minus the orchestration sockets:
        // this loop's agents edit a tree, they do not delegate.
        baseTools = repl => List(EvalScala(repl)),
        workerSystemPrompt = SystemPrompt.workflowAgent(),
        context = RuntimeContext(repo, ApprovalPolicy.AllowAll),
        notifyLead = msg => { notices.sendImmediately(msg); println(s"[LIVE lead] $msg") },
        onNotice = msg => { chatter.sendImmediately(msg); println(s"[LIVE user] $msg") },
        onLoop = snapshot => { views += snapshot; () }
      )
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await

      val lead = ScalaRepl(() =>
        ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_LOOP_SOCK" -> bridge.socketPath))))
      val started = js.Date.now()
      try
        given RuntimeContext = RuntimeContext(repo, ApprovalPolicy.AllowAll).withCallId("live-loop")
        val source = definition("dedupe", repo, pristine, baseline)
        // The production path: the eval prints the marker, the tool hands the source to
        // the bridge, and the bridge validates it in a gate worker before returning.
        val created = EvalScala(lead, loopBridge = Some(bridge)).execute(EvalScalaParams(source, Some(300_000)))
        assert(!created.isError, created.output)
        assert(created.output.contains("Loop(dedupe"), created.output)

        // From here the host drives on its own fiber. Three generations of a live model
        // is the budget; whatever it does with them, the loop must end parked.
        val phase = awaitPhase(bridge, "dedupe", _.startsWith(LoopBridge.ParkedPrefix), 1_500_000)
        val elapsed = (js.Date.now() - started) / 1000.0
        val store = LoopStore(PathOps.join(repo, LoopStore.AukRelativePath))
        val ledger = store.readAll("dedupe").getOrElse(Vector.empty[LoopEvent])
        println(f"[LIVE] parked as '$phase' after $elapsed%.0fs; ledger: ${ledger.map(_.kind).mkString(", ")}")

        assert(phase.startsWith(LoopBridge.ParkedPrefix), s"the loop should have parked; it is '$phase'")

        // The ledger folds — the record of what happened is well-formed.
        val state: LoopState = store.state("dedupe") match
          case Right(s)    => s
          case Left(error) => fail(s"the ledger did not fold: $error")
        assertEquals(state.loopId, "dedupe")
        assert(state.defAttached, "the definition should be attached")
        assert(state.inFlight.isEmpty, s"nothing should be in flight in a parked loop: ${state.inFlight}")

        // At least one generation was accepted, with a measurement, and its snapshot is
        // still resolvable.
        assert(state.generations.nonEmpty, s"no generation was accepted; ledger: ${ledger.map(_.kind).mkString(", ")}")
        val accepted = state.generations.last
        val measured = accepted.metrics.getOrElse(
          "opsPerSec",
          fail(s"generation ${accepted.gen} recorded no opsPerSec metric: ${accepted.metrics}")
        )
        assert(
          measured > baseline * 1.25,
          s"the accepted generation should beat the baseline: $measured vs $baseline"
        )
        assertEquals(Snapshot.resolve(repo, accepted.snapshotId), Right(accepted.commit))

        // The tree really holds working, faster code: the tests pass on what is on disk
        // now, and the fixed files came through untouched.
        assertEquals(TestFs.read(PathOps.join(repo, "bench.js")), BenchJs)
        assertEquals(TestFs.read(PathOps.join(repo, "test.js")), TestJs)
        val verified = metricOf(node(repo, "bench.js")).getOrElse(fail("the final tree's bench printed no metric"))
        println(
          f"[LIVE] baseline ${baseline.round} -> accepted ${measured.round} -> re-measured ${verified.round} ops/sec " +
            f"(${verified / baseline}%.1fx), ${state.generations.size} accepted of ${state.generationsStarted} started")
        println("[LIVE] final lib.js:\n" + TestFs.read(PathOps.join(repo, "lib.js")))

        // The lead was told the loop's milestones, and the panel was pushed snapshots.
        val leadHeard = List.unfold(())(_ => notices.readSource.poll().flatMap(_.toOption).map(m => (m, ())))
        assert(leadHeard.exists(_.contains("validated and is now running")), leadHeard.toString)
        assert(leadHeard.exists(_.contains("generation")), leadHeard.toString)
        assert(views.nonEmpty, "the loops window should have been pushed at least one snapshot")
      finally
        Async.fromSync(lead.close())
        Async.fromSync(bridge.close())
