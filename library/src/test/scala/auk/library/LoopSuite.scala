package auk.library

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** The worker side of the loop DSL, against a stub host on a real Unix-domain socket:
  * what `loop.start` puts on the wire in each of its two modes, the in-band marker that
  * makes the host capture the eval, the status mirror the handles read through, and the
  * `git diff` helper against a real repository.
  *
  * The host itself (validation, ledger, baseline) is the `LoopBridge`'s business and is
  * covered by the root suite; nothing here needs one.
  *
  * ==Testing a bridge client from the library side==
  *
  * The pattern below is worth copying for the other bridge clients (`TeamClient` and
  * `WorkflowClient` have no library-side tests at all, so their wire formats are only
  * ever asserted from the host). It rests on three pieces, none of which needs a host, a
  * REPL worker, or a packed `library.bin`:
  *
  *   - a [[StubHost]] — a `node:net` server on a temp socket that records every line a
  *     client sends and can push lines back — which lets a test assert the EXACT payload
  *     the DSL puts on the wire, and drive the client's mirror with host messages that
  *     would be awkward to provoke for real (a refusal, a phase this session never
  *     caused);
  *   - `loopsFor`/`cleanup`, which set and clear the environment variables the client
  *     reads before constructing it, so one process can host many independent sessions
  *     (the DSL opens its connection lazily on first use, which is what makes this work);
  *   - [[recordingStdout]], which swaps `process.stdout.write` for a recorder, so an
  *     in-band marker meant for the host can be asserted verbatim instead of being
  *     printed into the test runner's own output.
  *
  * Because a socket round trip only completes when the event loop gets a turn, the
  * assertions that depend on one are written as `Future`s chained off [[eventually]]
  * rather than as straight-line code, and a test that pushes to a client must first wait
  * for evidence the connection exists (its `hello`) — a push to a server with no accepted
  * socket yet goes nowhere.
  */
class LoopSuite extends LibSuite:

  /** An artifact type of the shape a real loop would report. */
  private case class Perf(p99Ms: Double, accuracy: Double) derives LibToolInput

  private val net: js.Dynamic = js.Dynamic.global.require("node:net")

  private def osTmp: String = js.Dynamic.global.require("node:os").tmpdir().asInstanceOf[String]

  private var sockCounter = 0
  private def freshSock(name: String): String =
    sockCounter += 1
    s"$osTmp/auk-loop-$name-${js.Dynamic.global.process.pid}-$sockCounter.sock"

  /** A stub `LoopBridge`: records every line a client sends and can push lines back. */
  private final class StubHost(name: String):
    val sockPath: String = freshSock(name)
    private val lines = scala.collection.mutable.ListBuffer.empty[String]
    private val sockets = scala.collection.mutable.ListBuffer.empty[js.Dynamic]

    private val server: js.Dynamic = net.createServer(((sock: js.Dynamic) =>
      sockets += sock
      sock.setEncoding("utf8")
      var buf = ""
      sock.on(
        "data",
        ((chunk: js.Any) =>
          buf += chunk.asInstanceOf[String]
          var i = buf.indexOf("\n")
          while i >= 0 do
            val line = buf.substring(0, i)
            buf = buf.substring(i + 1)
            if line.nonEmpty then lines += line
            i = buf.indexOf("\n")
        ): js.Function1[js.Any, Unit]
      )
      ()
    ): js.Function1[js.Dynamic, Unit]).asInstanceOf[js.Dynamic]

    def listen(): Future[Unit] =
      val p = Promise[Unit]()
      server.listen(sockPath, (() => { p.success(()); () }): js.Function0[Unit])
      p.future

    /** Every message received so far, parsed. */
    def received: List[js.Dynamic] = lines.toList.map(l => js.JSON.parse(l))

    def kinds: List[String] = received.map(_.t.asInstanceOf[String])

    def find(kind: String): Option[js.Dynamic] = received.find(_.t.asInstanceOf[String] == kind)

    /** Push a `status` snapshot to every connected client. */
    def pushStatus(loops: (String, String)*): Unit =
      val entries = loops.map((id, phase) => js.Dynamic.literal(id = id, phase = phase)).toJSArray
      pushLine(js.Dynamic.literal(t = "status", loops = entries))

    def pushError(loopId: String, msg: String): Unit =
      pushLine(js.Dynamic.literal(t = "error", loopId = loopId, msg = msg))

    private def pushLine(obj: js.Dynamic): Unit =
      val line = js.JSON.stringify(obj) + "\n"
      sockets.foreach(s => try { s.write(line); () } catch case _: Throwable => ())

    def close(): Unit =
      sockets.foreach(s => try { s.destroy(); () } catch case _: Throwable => ())
      try { server.close(); () } catch case _: Throwable => ()

  extension [A](xs: Seq[A]) private def toJSArray: js.Array[js.Any] = js.Array(xs.map(_.asInstanceOf[js.Any])*)

  /** Poll `pred` on the event loop until it holds (so the socket can be serviced in
    * between), failing the test if it never does. */
  private def eventually(what: String, pred: => Boolean, timeoutMs: Int = 10000): Future[Unit] =
    val p = Promise[Unit]()
    var waited = 0
    def tick(): Unit =
      if pred then p.success(())
      else if waited >= timeoutMs then p.failure(new RuntimeException(s"timed out waiting for $what"))
      else
        waited += 10
        js.Dynamic.global.setTimeout((() => tick()): js.Function0[Unit], 10)
        ()
    tick()
    p.future

  // -- environment / stdout plumbing -------------------------------------------

  private def env: js.Dynamic = js.Dynamic.global.process.env

  private def setEnv(name: String, value: String): Unit = env.updateDynamic(name)(value)
  private def clearEnv(name: String): Unit = js.special.delete(env, name)

  /** Run `body` with `process.stdout.write` recorded rather than printed, which is how
    * the host reads the loop start marker. */
  private def recordingStdout[A](body: => A): (A, String) =
    val stdout = js.Dynamic.global.process.stdout
    val original = stdout.write
    val sb = new StringBuilder
    stdout.updateDynamic("write")(((chunk: js.Any) => { sb ++= chunk.asInstanceOf[String]; true }): js.Function1[js.Any, Boolean])
    try (body, sb.result())
    finally stdout.updateDynamic("write")(original)

  /** A loop DSL bound to `host`, in normal or attach mode, with a clean registry. */
  private def loopsFor(host: StubHost, attach: Option[String] = None): LoopImpl =
    LoopRegistry.clear()
    setEnv("AUK_LOOP_SOCK", host.sockPath)
    attach match
      case Some(id) => setEnv("AUK_LOOP_ATTACH", id)
      case None     => clearEnv("AUK_LOOP_ATTACH")
    new LoopImpl(lib.shell)

  private def cleanup(host: StubHost): Unit =
    clearEnv("AUK_LOOP_SOCK")
    clearEnv("AUK_LOOP_ATTACH")
    LoopRegistry.clear()
    host.close()

  private def checker: LoopChecker[Perf] = (prev, cand) =>
    if prev.exists(_.artifact.p99Ms <= cand.artifact.p99Ms) then CheckResult.fail("p99 did not improve")
    else CheckResult.pass.withMetrics("p99Ms" -> cand.artifact.p99Ms)

  // -- CheckResult / LoopBudgets ------------------------------------------------

  test("CheckResult.pass passes with no reasons and no metrics"):
    assertEquals(CheckResult.pass, CheckResult(passed = true, reasons = Nil, metrics = Map.empty))

  test("CheckResult.fail carries its reasons in order and still passes nothing"):
    val r = CheckResult.fail("bench is red", "p99 regressed")
    assert(!r.passed)
    assertEquals(r.reasons, List("bench is red", "p99 regressed"))
    assertEquals(r.metrics, Map.empty[String, Double])

  test("withMetrics adds to either verdict, and a later key wins"):
    val passed = CheckResult.pass.withMetrics("p99Ms" -> 41.2).withMetrics("accuracy" -> 0.97, "p99Ms" -> 40.0)
    assert(passed.passed)
    assertEquals(passed.metrics, Map("p99Ms" -> 40.0, "accuracy" -> 0.97))
    val failed = CheckResult.fail("regressed").withMetrics("p99Ms" -> 61.0)
    assert(!failed.passed)
    assertEquals(failed.reasons, List("regressed"))
    assertEquals(failed.metrics, Map("p99Ms" -> 61.0))

  test("LoopBudgets defaults are 50 generations, patience 2, 3 attempts"):
    assertEquals(LoopBudgets(), LoopBudgets(50, 2, 3))
    assertEquals(LoopBudgets(maxGenerations = 10).patience, 2)

  // -- the diff helper -----------------------------------------------------------

  test("diffArgs armors git diff and path-limits only when asked"):
    val plain = LoopImpl.diffArgs("aaa", "bbb", Nil)
    assertEquals(plain.take(6), List("-c", "core.quotePath=false", "-c", "diff.mnemonicPrefix=false", "-c", "diff.noprefix=false"))
    assert(plain.contains("--no-renames"), plain.toString)
    assert(plain.contains("--no-color"), plain.toString)
    assertEquals(plain.takeRight(2), List("aaa^{tree}", "bbb^{tree}"))
    assert(!plain.contains("--"), plain.toString)
    val limited = LoopImpl.diffArgs("aaa", "bbb", List("src/main.scala", "build.sbt"))
    assertEquals(limited.takeRight(3), List("--", "src/main.scala", "build.sbt"))

  tmp.test("diff returns the patch between two commits of a real repository"): d =>
    val repo = d.dir("repo")
    repo.makedir()
    val git = lib.shell.command("git").at(repo.path)
    git.execute("init", "-b", "main")
    (repo.path / "a.txt").openAsFile.write("one\n")
    git.execute("add", "-A")
    git.execute("-c", "user.name=Test", "-c", "user.email=t@example.com", "commit", "-m", "first")
    val first = git.execute("rev-parse", "HEAD").stdout.trim
    (repo.path / "a.txt").openAsFile.write("one\ntwo\n")
    (repo.path / "b.txt").openAsFile.write("other\n")
    git.execute("add", "-A")
    git.execute("-c", "user.name=Test", "-c", "user.email=t@example.com", "commit", "-m", "second")
    val second = git.execute("rev-parse", "HEAD").stdout.trim

    val loops = new LoopImpl(lib.shell.at(repo.path))
    val patch = loops.diff(first, second)
    assert(patch.contains("+two"), patch)
    assert(patch.contains("b/b.txt"), patch)
    // Path-limited: only the named file's hunk survives.
    val limited = loops.diff(first, second, "a.txt")
    assert(limited.contains("+two"), limited)
    assert(!limited.contains("b.txt"), limited)
    // Identical trees produce an empty patch, not an error.
    assertEquals(loops.diff(second, second), "")
    // A revision git cannot resolve fails loudly rather than returning "".
    val boom = intercept[RuntimeException](loops.diff("nope", second))
    assert(boom.getMessage.contains("git diff"), boom.getMessage)

  // -- unavailable host ----------------------------------------------------------

  test("without AUK_LOOP_SOCK every operation fails with a clear 'loops are unavailable'"):
    clearEnv("AUK_LOOP_SOCK")
    clearEnv("AUK_LOOP_ATTACH")
    val loops = new LoopImpl(lib.shell)
    val e = intercept[RuntimeException](loops.start[Perf]("opt", "goal", "rubric")(checker))
    assert(e.getMessage.contains("loops are unavailable"), e.getMessage)
    assert(e.getMessage.contains("AUK_LOOP_SOCK"), e.getMessage)

  // -- normal mode ---------------------------------------------------------------

  test("start submits the whole configuration, prints the capture marker, and registers the checker"):
    val host = StubHost("hello")
    host.listen().flatMap: _ =>
      val loops = loopsFor(host)
      val (handle, printed) = recordingStdout:
        loops.start[Perf](
          id = "opt-tokenizer",
          goal = "cut p99 by 30%",
          rubric = "green suite and a faster p99",
          budgets = LoopBudgets(maxGenerations = 20, patience = 1, maxAttemptsPerGeneration = 5)
        )(checker)
      // The marker carries the loop id and nothing else, on its own line.
      assertEquals(printed, "auk:loop:start:opt-tokenizer\n")
      assertEquals(handle.id, "opt-tokenizer")
      // Before the host says anything, the handle reports the phase it echoed locally.
      assertEquals(handle.status, "validating")
      assertEquals(loops.list, List("opt-tokenizer" -> "validating"))
      // The checker is bound in this session too, so both modes leave the same registry.
      assertEquals(LoopRegistry.ids, List("opt-tokenizer"))
      eventually("the hello", host.find("hello").isDefined).map: _ =>
        val hello = host.find("hello").get
        assertEquals(hello.loopId.asInstanceOf[String], "opt-tokenizer")
        assertEquals(hello.goal.asInstanceOf[String], "cut p99 by 30%")
        assertEquals(hello.rubric.asInstanceOf[String], "green suite and a faster p99")
        assertEquals(hello.checkerRegistered.asInstanceOf[Boolean], true)
        assertEquals(hello.budgets.maxGenerations.asInstanceOf[Int], 20)
        assertEquals(hello.budgets.patience.asInstanceOf[Int], 1)
        assertEquals(hello.budgets.maxAttemptsPerGeneration.asInstanceOf[Int], 5)
        // The artifact schema travels as JSON text, and describes `Perf`.
        val schema = js.JSON.parse(hello.artifactSchema.asInstanceOf[String])
        assertEquals(schema.`type`.asInstanceOf[String], "object")
        assert(!js.isUndefined(schema.properties.p99Ms), "the schema must describe p99Ms")
        assert(!js.isUndefined(schema.properties.accuracy), "the schema must describe accuracy")
        assertEquals(host.kinds, List("hello"))
        cleanup(host)

  test("a status snapshot from the host advances every handle, and an error names the loop that failed"):
    val host = StubHost("status")
    host.listen().flatMap: _ =>
      val loops = loopsFor(host)
      val handle = recordingStdout(loops.start[Perf]("opt", "goal", "rubric")(checker))._1
      // Push only once the connection exists — the real host pushes in reply to `hello`.
      val connected = eventually("the hello", host.find("hello").isDefined)
      val snapshotSeen = connected.flatMap: _ =>
        host.pushStatus("opt" -> "running", "older" -> "parked: user requested")
        eventually("the status snapshot", handle.status == "running")
      val dropped = snapshotSeen.flatMap: _ =>
        // Loops this session never started are mirrored too — the snapshot is the host's.
        assertEquals(loops.list.toSet, Set("opt" -> "running", "older" -> "parked: user requested"))
        assertEquals(loops.get("older").status, "parked: user requested")
        // A snapshot REPLACES the mirror: a loop it stops naming is one the host no
        // longer has, and must not linger in the phase it was last seen in.
        host.pushStatus("older" -> "parked: user requested")
        eventually("the loop to drop out of the mirror", handle.status == "unknown")
      dropped.flatMap: _ =>
        assertEquals(loops.list, List("older" -> "parked: user requested"))
        host.pushError("opt", "another loop is already active")
        eventually("the error", handle.status.startsWith("failed:")).map: _ =>
          // The error is what explains a loop the host does not have.
          assertEquals(handle.status, "failed: another loop is already active")
          cleanup(host)

  test("park and resume reach the host, and toString shows the loop's phase"):
    val host = StubHost("park")
    host.listen().flatMap: _ =>
      val loops = loopsFor(host)
      val handle = recordingStdout(loops.start[Perf]("opt", "goal", "rubric")(checker))._1
      handle.park()
      handle.resume()
      assertEquals(handle.toString, "Loop(opt: validating)")
      eventually("park and resume", host.kinds.size == 3).map: _ =>
        assertEquals(host.kinds, List("hello", "park", "resume"))
        assertEquals(host.find("park").get.loopId.asInstanceOf[String], "opt")
        assertEquals(host.find("resume").get.loopId.asInstanceOf[String], "opt")
        cleanup(host)

  test("get refuses a loop this session has never heard of"):
    val host = StubHost("get")
    host.listen().map: _ =>
      val loops = loopsFor(host)
      val e = intercept[IllegalArgumentException](loops.get("ghost"))
      assert(e.getMessage.contains("unknown loop 'ghost'"), e.getMessage)
      assert(e.getMessage.contains("lib.loop.list"), e.getMessage)
      cleanup(host)

  test("start refuses an invalid id, an empty goal, or an empty rubric before touching the host"):
    val host = StubHost("invalid")
    host.listen().map: _ =>
      val loops = loopsFor(host)
      val bad = intercept[IllegalArgumentException](loops.start[Perf]("../escape", "goal", "rubric")(checker))
      assert(bad.getMessage.contains("invalid loop id"), bad.getMessage)
      assert(intercept[IllegalArgumentException](loops.start[Perf]("ok", "  ", "rubric")(checker)).getMessage.contains("goal is empty"))
      assert(intercept[IllegalArgumentException](loops.start[Perf]("ok", "goal", "")(checker)).getMessage.contains("rubric is empty"))
      assertEquals(LoopRegistry.ids, Nil)
      cleanup(host)

  // -- attach mode ---------------------------------------------------------------

  test("attach mode binds the checker and acknowledges it, without a hello or a marker"):
    val host = StubHost("attach")
    host.listen().flatMap: _ =>
      val loops = loopsFor(host, attach = Some("opt"))
      val (handle, printed) = recordingStdout(loops.start[Perf]("opt", "goal", "rubric")(checker))
      // Re-evaluating a captured definition must not look like a new loop: no marker
      // (nothing to capture) and no hello (the loop already exists).
      assertEquals(printed, "")
      assertEquals(handle.id, "opt")
      assertEquals(LoopRegistry.ids, List("opt"))
      // The registered checker is the one that was written, callable with its artifacts.
      val registered = LoopRegistry.get("opt").get.checker
      val candidate = new LoopCandidate[Any]:
        def artifact: Any = Perf(40.0, 0.98)
        def description: String = "made it faster"
        def commits: List[String] = List("abc123")
      assertEquals(registered(None, candidate), CheckResult.pass.withMetrics("p99Ms" -> 40.0))
      val previous = Some(LoopGen[Any](1, Perf(30.0, 0.98), "earlier", "def456", Map.empty))
      assertEquals(registered(previous, candidate), CheckResult.fail("p99 did not improve"))
      eventually("the bound acknowledgement", host.find("bound").isDefined).map: _ =>
        assertEquals(host.kinds, List("bound"))
        assertEquals(host.find("bound").get.loopId.asInstanceOf[String], "opt")
        cleanup(host)

  test("an attach-mode session refuses to bind a loop it was not spawned for"):
    val host = StubHost("attach-wrong")
    host.listen().map: _ =>
      val loops = loopsFor(host, attach = Some("opt"))
      val e = intercept[IllegalArgumentException](loops.start[Perf]("other", "goal", "rubric")(checker))
      assert(e.getMessage.contains("attach-mode session may only bind 'opt'"), e.getMessage)
      assertEquals(LoopRegistry.ids, Nil)
      cleanup(host)
