package auk.runtime

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js

import gears.async.{Async, Future}
import gears.async.default.given

import auk.llm.tools.{ApprovalPolicy, ProgressSink, RuntimeContext, ToolResult}
import auk.platform.Platform
import auk.platform.js.{Interop, ReplArtifacts}
import auk.runtime.repl.ScalaRepl

/** What [[EvalScala]] says about an eval *while it runs*, and what it says about
  * one that died — against fake workers rather than the real REPL, so a wedge
  * costs a second rather than a compile.
  *
  * The workers are the `node -e` scripts of `ScalaReplSuite`: small programs
  * that speak the JSONL protocol and misbehave deliberately. The subject here is
  * one layer up — the progress traffic the tool emits and the text it renders —
  * so the scripts are only as detailed as that needs.
  *
  * Timing is designed around one number: [[EvalScala.LivenessPollMs]]. A worker
  * that must be watched more than once holds its answer for a few multiples of
  * it; a worker that must still look healthy at its death declares a heartbeat
  * far slower than the one it actually keeps, so no scheduling hiccup on the
  * machine running this can turn "working" into "blocked".
  */
class EvalScalaLivenessSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 120.seconds

  /** Long enough for at least two polls, short enough that no test waits on it
    * for more than a few seconds. */
  private val TwoPolls = EvalScala.LivenessPollMs * 2 + 600

  // -- fake workers --------------------------------------------------------------

  /** The node running this suite also runs the fakes; `node` need not be on PATH. */
  private def nodeExec: String = js.Dynamic.global.process.execPath.asInstanceOf[String]

  private def spawnOf(script: String): ReplArtifacts.Spawn =
    ReplArtifacts.Spawn(List(nodeExec, "-e", script), Map.empty)

  /** A fake worker. `onRequest` is the body of the request handler, with `n` as
    * the 1-based request number and `reply()` as a successful eval response.
    * Every fake answers `shutdown` at once, so `close()` never waits out its
    * grace period; `hello` and the tick/ack chatter are the scenario's choice.
    *
    * `tickMs` is the worker's own claim about its heartbeat, and so sets the
    * silence [[ScalaRepl]] will call blocked. */
  private def workerScript(hello: Boolean, onRequest: String, tickMs: Int = 2_000): String =
    val greeting =
      if hello then s"say({ op: 'hello', protocol: 2, tickMs: $tickMs, pid: process.pid });" else ""
    s"""const rl = require('readline').createInterface({ input: process.stdin });
       |const say = o => console.log(JSON.stringify(o));
       |const reply = () => say({ op: 'eval', ok: true, output: 'val x: Int = 2', stdout: '', stderr: '', stateVersion: 1 });
       |$greeting
       |let n = 0;
       |rl.on('line', line => {
       |  if (JSON.parse(line).op === 'shutdown') { say({ op: 'shutdown', ok: true }); return; }
       |  n += 1;
       |$onRequest
       |});
       |""".stripMargin

  /** Acks, heartbeats briskly, and answers after `replyAfterMs`. */
  private def ticking(replyAfterMs: Int): String =
    workerScript(
      hello = true,
      s"""  say({ op: 'received' });
         |  let seq = 0;
         |  setInterval(() => say({ op: 'tick', seq: ++seq }), 50);
         |  setTimeout(reply, $replyAfterMs);""".stripMargin
    )

  /** Acks, says what stage it is at, heartbeats briskly, and answers after
    * `replyAfterMs`. */
  private def phased(replyAfterMs: Int): String =
    workerScript(
      hello = true,
      s"""  say({ op: 'received' });
         |  say({ op: 'progress', phase: 'compiling' });
         |  let seq = 0;
         |  setInterval(() => say({ op: 'tick', seq: ++seq }), 50);
         |  setTimeout(reply, $replyAfterMs);""".stripMargin
    )

  /** Answers the first request after `replyAfterMs` and then heartbeats forever
    * without ever answering again — a second eval that stays readable for as
    * long as a test needs to watch for traffic that should have stopped. */
  private def ticksThenWedges(replyAfterMs: Int): String =
    workerScript(
      hello = true,
      s"""  say({ op: 'received' });
         |  let seq = 0;
         |  setInterval(() => say({ op: 'tick', seq: ++seq }), 50);
         |  if (n === 1) setTimeout(reply, $replyAfterMs);""".stripMargin
    )

  /** Acks and heartbeats forever without answering: an eval awaiting something
    * that never completes. */
  private val neverAnswers: String =
    workerScript(
      hello = true,
      """  say({ op: 'received' });
        |  let seq = 0;
        |  setInterval(() => say({ op: 'tick', seq: ++seq }), 50);""".stripMargin
    )

  /** Acks and then goes completely silent. Its declared heartbeat is tiny, so
    * the silence is called blocked within a tenth of a second — long before the
    * first poll looks. */
  private val goesSilent: String =
    workerScript(hello = true, "  say({ op: 'received' });", tickMs = 25)

  /** A pre-liveness worker: no hello, no ack, no ticks — it just answers, slowly
    * enough that a watcher would have had plenty of chances to say something. */
  private def legacySlow(replyAfterMs: Int): String =
    workerScript(hello = false, s"  setTimeout(reply, $replyAfterMs);")

  /** A pre-liveness worker that never answers at all. */
  private val legacyMute: String = workerScript(hello = false, "  ;")

  // -- harness -------------------------------------------------------------------

  private def replOf(script: String): ScalaRepl =
    ScalaRepl(spawnSpec = () => Right(spawnOf(script)), preamble = "")

  /** Every update the tool reported, in order. */
  private final class RecordingSink extends ProgressSink:
    private val buf = ListBuffer.empty[Map[String, String]]

    def report(update: Map[String, String])(using Async): Unit = buf += update

    def updates: List[Map[String, String]] = buf.toList
    def states: List[String] = updates.flatMap(_.get(ScalaRepl.MetaState))
    def elapsed: List[Long] = updates.flatMap(_.get(ScalaRepl.MetaElapsedMs)).map(_.toLong)
    def phases: List[String] = updates.flatMap(_.get(ScalaRepl.MetaPhase))
    def phaseMs: List[Long] = updates.flatMap(_.get(ScalaRepl.MetaPhaseMs)).map(_.toLong)

  private def evaluate(repl: ScalaRepl, sink: RecordingSink, timeoutMs: Option[Int] = None)(using
      Async
  ): ToolResult =
    given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll, progress = sink)
    EvalScala(repl).execute(EvalScalaParams("1 + 1", timeoutMs))

  private def assertAnswered(result: ToolResult): Unit =
    assert(!result.isError, result.output)
    assert(result.output.contains("val x: Int = 2"), result.output)

  // -- watching a live eval --------------------------------------------------------

  test("a working eval is reported as it runs, with the counter advancing"):
    Async.fromSync:
      val repl = replOf(ticking(TwoPolls))
      val sink = RecordingSink()
      try
        val result = evaluate(repl, sink)
        assertAnswered(result)
        // The result itself is untouched by having been watched.
        assertEquals(result.metadata("timedOut"), "false")
        assertEquals(result.metadata("restarted"), "false")
        assertEquals(result.metadata.get("stateVersion"), Some("1"))
        assert(!result.metadata.contains(ScalaRepl.MetaLiveness), result.metadata.toString)
        assert(!result.output.contains("liveness"), result.output)

        assert(sink.updates.size >= 2, s"only ${sink.updates.size} update(s): ${sink.updates}")
        assertEquals(sink.states.distinct, List("working"))
        // Every poll reports, changed state or not: that is what makes the
        // elapsed counter live rather than a single stale reading.
        assert(sink.elapsed.head > 0, sink.elapsed.toString)
        assert(sink.elapsed.last > sink.elapsed.head, sink.elapsed.toString)
        // A working worker's last signal is recent by definition; there is no
        // silence worth naming.
        assert(sink.updates.forall(!_.contains(ScalaRepl.MetaSilentMs)), sink.updates.toString)
        assert(
          sink.updates.forall(_.get(ScalaRepl.MetaWorkerPid).exists(_.toInt > 0)),
          sink.updates.toString
        )
        // This worker names no stage, so no stage and no stage clock are
        // reported for it — there is nothing to time.
        assert(sink.updates.forall(!_.contains(ScalaRepl.MetaPhase)), sink.updates.toString)
        assert(sink.updates.forall(!_.contains(ScalaRepl.MetaPhaseMs)), sink.updates.toString)
      finally repl.close()

  test("a stage the worker names travels with every report that follows it"):
    Async.fromSync:
      val repl = replOf(phased(TwoPolls))
      val sink = RecordingSink()
      try
        assertAnswered(evaluate(repl, sink))
        assert(sink.updates.size >= 2, s"only ${sink.updates.size} update(s): ${sink.updates}")
        assertEquals(sink.states.distinct, List("working"))
        // The phase was named before the first poll and never changed, so every
        // report carries it — a watcher never has to ask what it was doing.
        assertEquals(sink.phases.distinct, List("compiling"))
        assertEquals(sink.phases.size, sink.updates.size)
        // And it brings a clock of its own, which is a clock: it advances
        // between two reports of the same unchanged stage.
        assertEquals(sink.phaseMs.size, sink.phases.size)
        assert(sink.phaseMs.last > sink.phaseMs.head, sink.phaseMs.toString)
        // The stage is younger than the request that contains it: its clock
        // starts when the stage does, not when the eval was sent.
        assert(
          sink.phaseMs.zip(sink.elapsed).forall(_ < _),
          s"${sink.phaseMs} against ${sink.elapsed}"
        )
      finally repl.close()

  test("a worker gone silent is reported blocked, with the silence it was judged on"):
    Async.fromSync:
      val repl = replOf(goesSilent)
      val sink = RecordingSink()
      try
        val result = evaluate(repl, sink, Some(TwoPolls))
        assert(result.isError, result.output)
        assertEquals(sink.states.distinct, List("blocked"))
        val silent = sink.updates.last.get(ScalaRepl.MetaSilentMs).map(_.toLong)
        assert(
          silent.exists(_ >= ScalaRepl.BlockedAfterMissedTicks * 25),
          s"blocked was reported after only $silent ms of silence"
        )
        // The state that led there is also the one the diagnosis describes.
        assert(result.output.contains("event loop"), result.output)
      finally repl.close()

  // -- what a death is rendered as -------------------------------------------------

  test("a timeout carries the worker's own account of what it was doing"):
    Async.fromSync:
      val repl = replOf(neverAnswers)
      val sink = RecordingSink()
      try
        val result = evaluate(repl, sink, Some(TwoPolls))
        assert(result.isError, result.output)
        assert(result.output.contains(s"timed out after ${TwoPolls}ms"), result.output)
        // Its own line, under the failure it explains.
        assert(result.output.contains("\nliveness: "), result.output)
        assert(result.output.contains("heartbeating"), result.output)
        assertEquals(result.metadata("timedOut"), "true")
        val diagnosis =
          result.metadata.getOrElse(ScalaRepl.MetaLiveness, fail("no diagnosis in the metadata"))
        assert(result.output.endsWith(diagnosis), result.output)

        // The reports that led there say the same thing the diagnosis does.
        assert(sink.updates.nonEmpty, "a wedged v2 worker was never reported on")
        assertEquals(sink.states.distinct, List("working"))
      finally repl.close()

  // -- a legacy worker is untouched by any of it -----------------------------------

  test("a legacy worker's eval reports nothing at all, and renders as it always did"):
    Async.fromSync:
      val repl = replOf(legacySlow(TwoPolls))
      val sink = RecordingSink()
      try
        val result = evaluate(repl, sink)
        assertAnswered(result)
        // Polls happened — the eval outlasted two of them — and every one of
        // them found nothing to say.
        assertEquals(sink.updates, Nil)
        assert(!result.output.contains("liveness"), result.output)
        assertEquals(result.metadata.keySet, Set("stateVersion", "timedOut", "restarted"))
      finally repl.close()

  test("a legacy worker's timeout is rendered with nothing added to it"):
    Async.fromSync:
      val repl = replOf(legacyMute)
      val sink = RecordingSink()
      try
        val result = evaluate(repl, sink, Some(TwoPolls))
        assert(result.isError, result.output)
        assert(result.output.contains("timed out"), result.output)
        assertEquals(sink.updates, Nil)
        assert(!result.output.contains("liveness"), result.output)
        assertEquals(result.metadata.keySet, Set("timedOut", "restarted"))
      finally repl.close()

  // -- the poller is bounded by the eval -------------------------------------------

  test("no report arrives after execute returns, even with an eval still running"):
    Async.fromSync:
      val repl = replOf(ticksThenWedges(EvalScala.LivenessPollMs + 600))
      val sink = RecordingSink()
      try
        assertAnswered(evaluate(repl, sink))
        val reported = sink.updates.size
        assert(reported >= 1, "the first eval was never reported on, so nothing is being proven")

        // A second eval, run WITHOUT the tool: liveness is readable again and
        // stays readable, so a poller that outlived the first call would have
        // every chance to speak. This sink must not grow by one.
        val running = Future(repl.eval("still going", Some(TwoPolls + 1_000)))
        Interop.sleep(TwoPolls.toDouble)
        assertEquals(sink.updates.size, reported)
        val _ = running.await
      finally repl.close()
