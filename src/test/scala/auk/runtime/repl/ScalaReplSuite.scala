package auk.runtime.repl

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js

import gears.async.{Async, Future}
import gears.async.AsyncOperations.sleep
import gears.async.default.given

import auk.platform.js.ReplArtifacts

/** [[ScalaRepl]] against fake workers: small `node -e` scripts that speak the
  * JSONL protocol and misbehave on purpose. Nothing here compiles Scala — the
  * subject is the parent's own conduct (what it records, what it concludes,
  * when it kills), which a real worker would only make slower and less
  * repeatable to observe.
  *
  * Timing is designed out rather than waited out. Every scenario that needs the
  * worker booted first spends a warm-up eval on it, so a wedge's timeout budget
  * pays for the wedge alone; heartbeats are 25ms, so "blocked" is reached in
  * under a tenth of a second while the timeouts guarding it are an order of
  * magnitude larger; and the one event whose arrival order matters — a killed
  * worker's `close` — is pinned by holding its stdout pipe open past its death,
  * rather than by hoping the parent respawns first.
  */
class ScalaReplSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 120.seconds

  // -- fake workers --------------------------------------------------------------

  /** The worker's heartbeat period. Small enough that [[ScalaRepl]]'s
    * three-missed-ticks threshold is crossed in ~75ms — two orders of magnitude
    * inside every timeout below. */
  private val TickMs = 25

  /** The node that runs this suite also runs the fakes; `node` need not be on
    * PATH. */
  private def nodeExec: String = js.Dynamic.global.process.execPath.asInstanceOf[String]

  private def spawnOf(script: String): ReplArtifacts.Spawn =
    ReplArtifacts.Spawn(List(nodeExec, "-e", script), Map.empty)

  /** A fake worker. `onRequest` is the body of the request handler, with `n` as
    * the 1-based request number and `reply()` as a successful eval response.
    * Every fake answers `shutdown` at once, so `close()` never waits out its
    * grace period; `hello` and the tick/ack chatter are the scenario's choice.
    *
    * `tickMs` is the worker's own claim about its heartbeat, and so sets the
    * silence [[ScalaRepl]] will call blocked. A scenario about being blocked
    * declares a tiny one to get there fast; a scenario that must still look
    * healthy at the moment it dies declares a large one, so no plausible stall
    * on the machine running this can turn its verdict into the other. */
  private def workerScript(
      hello: Boolean,
      onRequest: String,
      tickMs: Int = TickMs,
      prelude: String = ""
  ): String =
    val greeting =
      if hello then s"say({ op: 'hello', protocol: 2, tickMs: $tickMs, pid: process.pid });" else ""
    s"""$prelude
       |const rl = require('readline').createInterface({ input: process.stdin });
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

  /** Acks, ticks once, answers. */
  private val healthy = workerScript(hello = true, """
    say({ op: 'received' });
    say({ op: 'tick', seq: 1 });
    reply();""")

  /** Answers the first request, then acks and heartbeats forever. Its ticks are
    * far enough apart that "blocked" would need 600ms of silence, so the
    * verdict on it at the timeout is Working beyond argument. */
  private val wedged = workerScript(
    hello = true,
    """
    say({ op: 'received' });
    if (n === 1) reply();
    else setInterval(() => say({ op: 'tick', seq: ++n }), 200);""",
    tickMs = 200
  )

  /** Answers the first request, then acks and goes completely silent — the
    * shape of a worker whose event loop has stopped turning. */
  private val blocked = workerScript(hello = true, """
    say({ op: 'received' });
    if (n === 1) reply();""")

  /** Answers the first request, then ignores the second entirely: no ack, no
    * ticks, as if the request never arrived. */
  private val deaf = workerScript(hello = true, """
    if (n === 1) { say({ op: 'received' }); reply(); }""")

  /** Acks, names each stage as it reaches it, then answers. */
  private val phasing = workerScript(hello = true, """
    say({ op: 'received' });
    say({ op: 'progress', phase: 'compiling' });
    say({ op: 'progress', phase: 'running' });
    reply();""")

  /** Answers the first request; on the second it acks and then reports the same
    * phase over and over WITHOUT ever ticking. Its declared heartbeat is long
    * enough that nothing but those progress lines can keep it out of Blocked —
    * which is the point: they are signals in exactly the sense ticks are. */
  private val progressOnly = workerScript(
    hello = true,
    """
    say({ op: 'received' });
    if (n === 1) reply();
    else setInterval(() => say({ op: 'progress', phase: 'compiling' }), 25);""",
    tickMs = 200
  )

  /** Like [[progressOnly]], but it renames the stage it is in partway through —
    * a worker moving down its pipeline while staying just as alive. */
  private val changingPhase = workerScript(
    hello = true,
    """
    say({ op: 'received' });
    if (n === 1) reply();
    else {
      let phase = 'compiling';
      setTimeout(() => { phase = 'running'; }, 300);
      setInterval(() => say({ op: 'progress', phase }), 25);
    }""",
    tickMs = 200
  )

  /** Answers the first request; on the second it acks, says what it is doing,
    * and then stops dead — a synchronous compile as seen from outside. */
  private val silentInPhase = workerScript(hello = true, """
    say({ op: 'received' });
    if (n === 1) reply();
    else say({ op: 'progress', phase: 'compiling' });""")

  /** Names a phase far longer than any line could hold. Phase tokens are the
    * worker's free choice, so this is as legal as any other. */
  private val shoutyPhase = workerScript(hello = true, """
    say({ op: 'received' });
    if (n === 1) reply();
    else say({ op: 'progress', phase: 'compiling ' + 'x'.repeat(500) });""")

  /** Slips an op from a newer protocol between the ack and the answer. */
  private val chatty = workerScript(hello = true, """
    say({ op: 'received' });
    say({ op: 'wat', note: 'from a newer worker' });
    reply();""")

  /** Corrupts the stream mid-eval with something that is not a protocol
    * message at all. */
  private val garbled = workerScript(hello = true, """
    say({ op: 'received' });
    console.log('Segmentation fault (core dumped)');""")

  /** Acks, then dies with a distinctive code while the request is outstanding.
    * A long declared heartbeat keeps its dying state unambiguously Working, no
    * matter how long the exit takes to reach the parent. */
  private val crashing = workerScript(
    hello = true,
    """
    say({ op: 'received' });
    process.exit(7);""",
    tickMs = 2_000
  )

  /** A pre-liveness worker: no hello, no ack, no ticks — just answers, slowly
    * enough that a reader can look at it while it works. */
  private val legacySlow = workerScript(hello = false, "  setTimeout(reply, 250);")

  /** A pre-liveness worker that never answers at all. */
  private val legacyMute = workerScript(hello = false, "  ;")

  /** Wedges like [[blocked]] from the first request, and hands its stdout to a
    * grandchild that outlives it: node reports `close` only once the pipe is
    * empty of writers, so this worker's exit is guaranteed to be observed well
    * after its replacement has been spawned. */
  private val wedgedPastDeath = workerScript(
    hello = true,
    "  say({ op: 'received' });",
    prelude =
      "require('child_process').spawn(process.execPath, ['-e', 'setTimeout(() => {}, 1500)'], " +
        "{ stdio: ['ignore', 'inherit', 'inherit'] });"
  )

  /** Answers with megabytes of stdout on one line — a grep that matched the
    * session's own logs, which is how a 40 MB reply reached this parent on
    * 2026-08-15 and took its heap with it. */
  private val floody = workerScript(hello = true, s"""
    say({ op: 'received' });
    say({ op: 'eval', ok: true, output: 'val x: Int = 2', stdout: 'x'.repeat(${4 * 1024 * 1024}),
          stderr: '', stateVersion: 1 });""")

  // -- harness -------------------------------------------------------------------

  private def repl(logs: Logs, scripts: String*): ScalaRepl =
    var spawns = 0
    ScalaRepl(
      spawnSpec = () =>
        val script = scripts(math.min(spawns, scripts.size - 1))
        spawns += 1
        Right(spawnOf(script)),
      preamble = "",
      makeLog = logs.factory
    )

  /** Poll until `probe` answers. Pull-based liveness is meant to be read this
    * way; the deadline is far longer than any delay under test, so it fails
    * only when the state genuinely never arrives. */
  private def awaitSome[T](what: String, withinMs: Int = 8_000)(probe: => Option[T])(using
      Async
  ): T =
    val deadline = js.Date.now() + withinMs
    var got = probe
    while got.isEmpty && js.Date.now() < deadline do
      sleep(5)
      got = probe
    got.getOrElse(fail(s"gave up waiting for $what"))

  private def awaitState(r: ScalaRepl, what: String)(
      p: ScalaRepl.Liveness => Boolean
  )(using Async): ScalaRepl.Liveness =
    awaitSome(s"the worker to look $what")(r.liveness.filter(p))

  private def responseOf(result: ScalaRepl.EvalResult): ReplProtocol.Response =
    result.status match
      case ScalaRepl.Status.Completed(r) => r
      case other                         => fail(s"expected a completed eval, got $other")

  private def assertAnswered(result: ScalaRepl.EvalResult): Unit =
    val r = responseOf(result)
    assert(r.ok, r.toString)
    assertEquals(r.output, "val x: Int = 2")

  private def pidOf(rec: Recorder): Int =
    rec.spawnedEvent.flatMap(_.pid).getOrElse(fail("the spawn recorded no pid"))

  // -- a worker that speaks the liveness protocol ---------------------------------

  test("hello, ack and ticks are recorded but only the reply settles the eval"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, healthy)
      try
        val result = r.eval("1 + 1", Some(10_000))
        assertAnswered(result)
        assertEquals(result.restartedSession, false)
        assertEquals(result.diagnosis, None)

        val rec = logs.gen(1)
        assertEquals(rec.recvKinds, List("hello", "received", "tick", "reply"))
        assertEquals(
          rec.evNames,
          List("spawned", "sent", "recv", "recv", "recv", "recv", "settled")
        )
        assertEquals(rec.settles, List(("completed", "eval ok")))

        val spawned = rec.spawnedEvent.getOrElse(fail("no spawn was recorded"))
        assertEquals(spawned.generation, 1)
        assertEquals(spawned.argv.head, nodeExec)
        assert(spawned.pid.exists(_ > 0), spawned.toString)
        assertEquals(r.workerPid, spawned.pid)
        // The spawn's env goes in by key; no scenario here sets one, but the
        // parent's own environment is inherited and must still be listed by name.
        assert(spawned.envKeys.forall(_.nonEmpty))
      finally r.close()

  test("an unrecognised op mid-eval is dropped, not taken for an answer"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, chatty)
      try
        // Had the unknown op settled the request, this would be its `ok = false`
        // response rather than the eval's.
        assertAnswered(r.eval("1 + 1", Some(10_000)))
        assertEquals(logs.gen(1).recvKinds, List("hello", "received", "unknown", "reply"))
      finally r.close()

  test("a line that is not a protocol message at all still fails the eval, loudly"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, garbled)
      try
        val result = r.eval("1 + 1", Some(10_000))
        result.status match
          case ScalaRepl.Status.Failed(reason) => assert(reason.contains("unparseable"), reason)
          case other                           => fail(s"expected a failure, got $other")
        // Stream corruption is not a liveness verdict: there is nothing to
        // diagnose about a worker that was answering.
        assertEquals(result.diagnosis, None)
        val rec = logs.gen(1)
        assertEquals(rec.recvKinds, List("hello", "received", "garbage"))
        assertEquals(rec.settles.map(_._1), List("failed"))
      finally r.close()

  test("a reply of megabytes is answered, and what the parent keeps of it is bounded"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, floody)
      try
        val result = r.eval("lib.fs.cwd.grep(\"needle\").display()", Some(20_000))
        // The eval still succeeds: a worker that says too much is not an error,
        // it is a worker to hold less of.
        val response = responseOf(result)
        assert(response.ok, response.toString)
        assertEquals(response.stdout.length, ReplProtocol.MaxFieldChars)
        assert(
          response.stdout.contains(s"of ${4 * 1024 * 1024} characters kept"),
          response.stdout.takeRight(120)
        )
        // The line really did arrive whole over the pipe — so the bound was
        // applied here, on this side of the read, and not by the fake.
        val raw = logs.gen(1).events.collect { case e: Ev.Recv => e.line }
        assert(
          raw.exists(_.length > 4 * 1024 * 1024),
          s"the parent should have read a >4 MiB line; sizes were ${raw.map(_.length)}"
        )
        assertEquals(logs.gen(1).settles, List(("completed", "eval ok")))
      finally r.close()

  // -- liveness ------------------------------------------------------------------

  test("a worker that heartbeats without answering is Working, and times out saying so"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, wedged)
      try
        // Warm-up: the boot cost is paid here, so the wedged eval's timeout
        // budget is spent entirely on the wedge.
        assertAnswered(r.eval("warm up", Some(10_000)))

        val running = Future(r.eval("while (true) ()", Some(1_000)))
        awaitState(r, "working") { case _: ScalaRepl.Liveness.Working => true; case _ => false } match
          case ScalaRepl.Liveness.Working(elapsedMs) => assert(elapsedMs >= 0, elapsedMs.toString)
          case other                                 => fail(s"expected Working, got $other")

        val result = running.await
        assertEquals(result.status, ScalaRepl.Status.TimedOut(1_000))
        val diagnosis = result.diagnosis.getOrElse(fail("a v2 worker's death must be diagnosed"))
        val rec = logs.gen(1)
        assert(diagnosis.contains("heartbeating"), diagnosis)
        assert(diagnosis.contains(s"worker pid ${pidOf(rec)}"), diagnosis)

        assertEquals(rec.killCauses, List("timeout"))
        assertEquals(rec.settles.map(_._1), List("completed", "timed-out"))
        // The death is a self-contained post-mortem: the state it died in,
        // then how it was settled, then the kill that followed.
        assertEquals(rec.evNames.takeRight(3), List("liveness", "settled", "killed"))
        assertEquals(rec.livenessStates.lastOption, Some("working"))
        // Everything before that forced snapshot was logged because someone
        // READ it, and only where it had changed: many polls, no state twice
        // running.
        val polled = rec.livenessStates.init
        assert(polled.contains("working"), polled.toString)
        assert(
          polled.sliding(2).forall(w => w.distinct.size == w.size),
          s"a state was logged twice running: $polled"
        )
        assert(rec.recvKinds.count(_ == "tick") >= 1, rec.recvKinds.toString)
      finally r.close()

  test("a worker gone silent is Blocked, and the diagnosis names the silence"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, blocked)
      try
        assertAnswered(r.eval("warm up", Some(10_000)))

        val running = Future(r.eval("Thread.sleep(forever)", Some(1_500)))
        val state =
          awaitState(r, "blocked") { case _: ScalaRepl.Liveness.Blocked => true; case _ => false }
        state match
          case ScalaRepl.Liveness.Blocked(_, silentForMs) =>
            assert(
              silentForMs >= ScalaRepl.BlockedAfterMissedTicks * TickMs,
              s"blocked was declared after only ${silentForMs}ms of silence"
            )
          case other => fail(s"expected Blocked, got $other")

        val result = running.await
        assertEquals(result.status, ScalaRepl.Status.TimedOut(1_500))
        val diagnosis = result.diagnosis.getOrElse(fail("a v2 worker's death must be diagnosed"))
        assert(diagnosis.contains("silent for"), diagnosis)
        assert(diagnosis.contains("event loop"), diagnosis)

        val rec = logs.gen(1)
        // The ack arrived; the silence began after it.
        assertEquals(rec.recvKinds, List("hello", "received", "reply", "received"))
        assertEquals(rec.livenessStates.lastOption, Some("blocked"))
        assertEquals(rec.evNames.takeRight(3), List("liveness", "settled", "killed"))
        assertEquals(rec.killCauses, List("timeout"))
      finally r.close()

  test("a request the worker never acknowledges is AwaitingAck, and says it was lost"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, deaf)
      try
        assertAnswered(r.eval("warm up", Some(10_000)))

        val running = Future(r.eval("1 + 1", Some(1_000)))
        awaitState(r, "unacknowledged") {
          case _: ScalaRepl.Liveness.AwaitingAck => true
          case _                                 => false
        }

        val result = running.await
        assertEquals(result.status, ScalaRepl.Status.TimedOut(1_000))
        val diagnosis = result.diagnosis.getOrElse(fail("a v2 worker's death must be diagnosed"))
        assert(diagnosis.contains("never acknowledged"), diagnosis)

        val rec = logs.gen(1)
        // Nothing at all came back for the second request.
        assertEquals(rec.recvKinds, List("hello", "received", "reply"))
        // Once because it was read while outstanding, once forced at the death;
        // the state never changed in between, so there is nothing else.
        assertEquals(rec.livenessStates, List("awaiting-ack", "awaiting-ack"))
        assertEquals(rec.evNames.takeRight(3), List("liveness", "settled", "killed"))
      finally r.close()

  test("a worker that dies mid-eval is diagnosed from where liveness stood"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, crashing, healthy)
      try
        val result = r.eval("System.exit(7)", Some(10_000))
        result.status match
          case ScalaRepl.Status.Failed(reason) => assert(reason.contains("code 7"), reason)
          case other                           => fail(s"expected a failure, got $other")
        val diagnosis = result.diagnosis.getOrElse(fail("a v2 worker's death must be diagnosed"))
        assert(diagnosis.contains(s"worker pid ${pidOf(logs.gen(1))}"), diagnosis)

        val rec = logs.gen(1)
        assertEquals(rec.exits.map(e => (e.code, e.stale)), List((7, false)))
        assertEquals(rec.settles.map(_._1), List("failed"))
        // Nobody was watching this one, so the forced snapshot is the only
        // record of how it stood — and it is in the file, after the exit that
        // ended it and before the settle it explains.
        assertEquals(rec.livenessStates, List("working"))
        assertEquals(rec.evNames.takeRight(3), List("exit", "liveness", "settled"))
        // The session is gone; the next eval says so.
        assertEquals(r.eval("1 + 1", Some(10_000)).restartedSession, true)
      finally r.close()

  // -- phases ---------------------------------------------------------------------

  test("phases are recorded as they arrive and forgotten when the request settles"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, phasing)
      try
        assertAnswered(r.eval("1 + 1", Some(10_000)))
        // A phase describes a request in flight; this one is over.
        assertEquals(r.phase, None)
        assertEquals(logs.gen(1).recvKinds, List("hello", "received", "progress", "progress", "reply"))
      finally r.close()

  test("progress alone keeps a worker Working — a phase-reporting worker need never tick"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, progressOnly)
      try
        assertAnswered(r.eval("warm up", Some(10_000)))

        val running = Future(r.eval("compile something big", Some(1_000)))
        // The phase is awaited, not assumed: this worker names it on its first
        // interval rather than in the same breath as the ack.
        assertEquals(awaitSome("a phase")(r.phase), "compiling")
        awaitState(r, "working") { case _: ScalaRepl.Liveness.Working => true; case _ => false }

        val result = running.await
        assertEquals(result.status, ScalaRepl.Status.TimedOut(1_000))

        val rec = logs.gen(1)
        // Not one heartbeat in a second — six times the silence this worker's
        // declared 200ms tick would have been called blocked for. The progress
        // lines are the only reason the verdict at its death is Working.
        assert(!rec.recvKinds.contains("tick"), rec.recvKinds.toString)
        assert(rec.recvKinds.count(_ == "progress") >= 2, rec.recvKinds.toString)
        assertEquals(rec.livenessStates.lastOption, Some("working"))
        assert(!rec.livenessStates.contains("blocked"), rec.livenessStates.toString)
      finally r.close()

  test("a phase change is no liveness transition — it rides on the events that do fire"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, changingPhase)
      try
        assertAnswered(r.eval("warm up", Some(10_000)))

        val running = Future(r.eval("compile then run it", Some(1_000)))
        // Liveness is READ across the phase change — only a read can log a
        // transition, so a phase-sensitive one would have every chance to fire
        // here — and the state read at the change is the same Working as before.
        awaitSome("the stage to change under an unchanged state"):
          r.liveness
            .filter { case _: ScalaRepl.Liveness.Working => true; case _ => false }
            .flatMap(_ => r.phase.filter(_ == "running"))

        val result = running.await
        assertEquals(result.status, ScalaRepl.Status.TimedOut(1_000))

        val rec = logs.gen(1)
        // Two, out of a second of polling across two stages: the transition into
        // Working that a reader saw, and the forced snapshot at the death.
        val fired = rec.livenessEvents.filter(_.state == "working")
        assertEquals(fired.size, 2, s"a phase change logged a transition: ${rec.livenessEvents}")
        // And the phase rides along on the event that matters most — the death —
        // naming the stage it was actually in by then, not the one it started in.
        assertEquals(fired.last.phase, Some("running"))
      finally r.close()

  test("the stage clock times the stage, not the worker's chatter about it"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, changingPhase)
      try
        assertAnswered(r.eval("warm up", Some(10_000)))

        val running = Future(r.eval("compile then run it", Some(1_000)))
        // This worker repeats the stage it is in every 25ms. A clock restarted
        // by each of those repeats could never reach 100ms; a clock that times
        // the stage sails past it.
        val compiling = awaitSome("the compiling stage to outlive several repeats"):
          r.phase.filter(_ == "compiling").flatMap(_ => r.phaseAgeMs).filter(_ >= 100)

        // A real change of stage does start a new clock: the run it moved on to
        // is younger than the compile it left.
        awaitSome("the running stage")(r.phase.filter(_ == "running"))
        val running0 = r.phaseAgeMs.getOrElse(fail("the running stage reported no clock"))
        assert(running0 < compiling, s"the stage clock did not restart: $running0 vs $compiling")

        assertEquals(running.await.status, ScalaRepl.Status.TimedOut(1_000))
        // The stage and its clock describe a request in flight, and go with it.
        assertEquals(r.phase, None)
        assertEquals(r.phaseAgeMs, None)
      finally r.close()

  test("a worker that goes quiet inside a phase is diagnosed by the phase it went quiet in"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, silentInPhase)
      try
        assertAnswered(r.eval("warm up", Some(10_000)))

        val running = Future(r.eval("compile something enormous", Some(1_500)))
        awaitState(r, "blocked") { case _: ScalaRepl.Liveness.Blocked => true; case _ => false }
        assertEquals(r.phase, Some("compiling"))

        val result = running.await
        assertEquals(result.status, ScalaRepl.Status.TimedOut(1_500))
        val diagnosis = result.diagnosis.getOrElse(fail("a v2 worker's death must be diagnosed"))
        // The whole value of the phase: not just that it stopped, but what it
        // was in the middle of when it did.
        assert(diagnosis.contains("silent for"), diagnosis)
        assert(diagnosis.contains("during 'compiling'"), diagnosis)

        val rec = logs.gen(1)
        assertEquals(rec.recvKinds, List("hello", "received", "reply", "received", "progress"))
        assertEquals(rec.livenessStates.lastOption, Some("blocked"))
        assertEquals(rec.killCauses, List("timeout"))
        // Every blocked event says so in structure too, the read one and the
        // forced snapshot alike: a post-mortem gets the stage from the event it
        // is already reading, not by correlating the recv lines around it.
        assertEquals(
          rec.livenessEvents.filter(_.state == "blocked").map(_.phase).distinct,
          List(Some("compiling"))
        )
        assertEquals(r.phase, None)
      finally r.close()

  test("an outsized phase is cut to something a line of UI can hold"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, shoutyPhase)
      try
        assertAnswered(r.eval("warm up", Some(10_000)))

        val running = Future(r.eval("shout", Some(1_000)))
        val phase = awaitSome("a phase")(r.phase)
        assertEquals(phase.length, ScalaRepl.MaxPhaseChars)
        assert(phase.startsWith("compiling"), phase)
        val _ = running.await
      finally r.close()

  // -- a legacy worker is untouched by any of it ----------------------------------

  test("a legacy worker's eval is exactly what it was: no liveness, no diagnosis"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, legacySlow)
      try
        val running = Future(r.eval("1 + 1", Some(10_000)))
        // Read liveness repeatedly while the eval is outstanding. Every read
        // returning None is what keeps the log free of liveness events below —
        // a single Some would have logged its transition.
        (1 to 5).foreach: _ =>
          assertEquals(r.liveness, None)
          sleep(40)

        val result = running.await
        assertAnswered(result)
        assertEquals(result.diagnosis, None)
        assertEquals(result.restartedSession, false)

        val rec = logs.gen(1)
        assertEquals(rec.recvKinds, List("reply"))
        assertEquals(rec.livenessStates, Nil)
        assertEquals(rec.settles, List(("completed", "eval ok")))
      finally r.close()

  test("a legacy worker that never answers times out with nothing to say about it"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, legacyMute)
      try
        val result = r.eval("1 + 1", Some(600))
        assertEquals(result.status, ScalaRepl.Status.TimedOut(600))
        assertEquals(result.diagnosis, None)
        assertEquals(r.liveness, None)

        val rec = logs.gen(1)
        assertEquals(rec.livenessStates, Nil)
        assertEquals(rec.killCauses, List("timeout"))
        assertEquals(rec.settles, List(("timed-out", "no response within 600ms")))
      finally r.close()

  // -- generations ----------------------------------------------------------------

  test("a killed worker's exit lands in its own log, marked stale"):
    Async.fromSync:
      val logs = Logs()
      val r = repl(logs, wedgedPastDeath, healthy)
      try
        val first = r.eval("wedge", Some(600))
        assertEquals(first.status, ScalaRepl.Status.TimedOut(600))

        // No await between the timeout and this call: the replacement worker is
        // spawned before the killed one's `close` can arrive — which its stdout
        // pipe, held open by a grandchild, guarantees regardless.
        val second = r.eval("1 + 1", Some(10_000))
        assertAnswered(second)
        assertEquals(second.restartedSession, true)

        val old = logs.gen(1)
        val exited = awaitSome("the killed worker's close event")(old.exits.headOption)
        assert(exited.stale, s"the exit of generation 1 was not marked stale: $exited")
        assertEquals(old.killCauses, List("timeout"))
        // Its own file tells the whole story: acked, then silent, killed for it,
        // and only much later actually gone.
        assertEquals(old.livenessStates, List("blocked"))
        assertEquals(old.evNames.takeRight(4), List("liveness", "settled", "killed", "exit"))
        // The live worker is untouched by its predecessor's death.
        assertEquals(logs.gen(2).exits, Nil)
        assertEquals(logs.gen(2).settles.map(_._1), List("completed"))
      finally r.close()

  test("a resolve failure spawns nothing, and so logs nothing"):
    Async.fromSync:
      val logs = Logs()
      val r = ScalaRepl(
        spawnSpec = () => Left("Scala REPL artifacts not found"),
        preamble = "",
        makeLog = logs.factory
      )
      val result = r.eval("1 + 1", Some(10_000))
      assertEquals(result.status, ScalaRepl.Status.Failed("Scala REPL artifacts not found"))
      assertEquals(result.restartedSession, false)
      assertEquals(result.diagnosis, None)
      assertEquals(logs.generations, Nil)

/** One recorded worker-log event, named as [[WorkerLog]] names it. */
private enum Ev:
  case Spawned(argv: List[String], generation: Int, envKeys: List[String], pid: Option[Int])
  case SpawnFailed(error: String)
  case Sent(line: String)
  case Recv(line: String, kind: String)
  case Stderr(chunk: String)
  case Settled(status: String, detail: String)
  case Live(state: String, silentMs: Long, phase: Option[String])
  case Killed(cause: String)
  case Exited(code: Int, stale: Boolean)

  /** The event's name in a real log file's `ev` field. */
  def ev: String = this match
    case _: Spawned     => "spawned"
    case _: SpawnFailed => "spawn-failed"
    case _: Sent        => "sent"
    case _: Recv        => "recv"
    case _: Stderr      => "stderr"
    case _: Settled     => "settled"
    case _: Live        => "liveness"
    case _: Killed      => "killed"
    case _: Exited      => "exit"

/** A [[WorkerLog]] kept in memory, so a test can read back what the parent
  * recorded about one worker. */
private final class Recorder extends WorkerLog:
  private val buf = ListBuffer.empty[Ev]

  def events: List[Ev] = buf.toList
  def evNames: List[String] = events.map(_.ev)
  def recvKinds: List[String] = events.collect { case Ev.Recv(_, kind) => kind }
  def livenessStates: List[String] = events.collect { case Ev.Live(state, _, _) => state }
  def livenessEvents: List[Ev.Live] = events.collect { case e: Ev.Live => e }
  def killCauses: List[String] = events.collect { case Ev.Killed(cause) => cause }
  def settles: List[(String, String)] = events.collect { case Ev.Settled(s, d) => (s, d) }
  def exits: List[Ev.Exited] = events.collect { case e: Ev.Exited => e }
  def spawnedEvent: Option[Ev.Spawned] = events.collectFirst { case e: Ev.Spawned => e }

  def spawned(argv: List[String], generation: Int, envKeys: List[String], pid: Option[Int]): Unit =
    buf += Ev.Spawned(argv, generation, envKeys, pid)
  def spawnFailed(error: String): Unit = buf += Ev.SpawnFailed(error)
  def sent(line: String): Unit = buf += Ev.Sent(line)
  def recv(line: String, kind: String): Unit = buf += Ev.Recv(line, kind)
  def stderrChunk(chunk: String): Unit = buf += Ev.Stderr(chunk)
  def settled(status: String, detail: String): Unit = buf += Ev.Settled(status, detail)
  def liveness(state: String, silentMs: Long, phase: Option[String]): Unit =
    buf += Ev.Live(state, silentMs, phase)
  def killed(cause: String): Unit = buf += Ev.Killed(cause)
  def exit(code: Int, stale: Boolean): Unit = buf += Ev.Exited(code, stale)

/** The [[Recorder]]s of one [[ScalaRepl]]'s workers, by generation. */
private final class Logs:
  private val byGen = scala.collection.mutable.Map.empty[Int, Recorder]

  val factory: (Int, Option[Int]) => WorkerLog = (generation, _) =>
    val recorder = Recorder()
    byGen(generation) = recorder
    recorder

  def generations: List[Int] = byGen.keys.toList.sorted
  def gen(n: Int): Recorder =
    byGen.getOrElse(n, throw AssertionError(s"no worker was logged for generation $n"))
