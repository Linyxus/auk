package auk.runtime

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

import auk.TestFs
import auk.llm.tools.{Json, RuntimeContext}
import auk.loop.{Budgets, LoopEvent, LoopStore}
import auk.platform.PathOps
import auk.session.{SessionProvider, SessionRef}
import auk.workflow.{LoopAttemptWire, LoopBudgetsWire, LoopGenerationWire, LoopStageWire, LoopWire, OrchestrationEvent, TranscriptEvent, WireCodec, WireMessage}

/** End-to-end against the real host [[WorkflowWebServer]] on an OS-picked spare
  * port (modeled on `webui-dev`'s `MockServerSuite` + the bridge's real-server
  * tests). munit awaits the returned `Future`. */
class WorkflowWebServerSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 30.seconds

  private var counter = 0
  private val http = js.Dynamic.global.require("node:http")
  private val fs = js.Dynamic.global.require("node:fs")
  private val osPath = js.Dynamic.global.require("node:path")
  private val os = js.Dynamic.global.require("node:os")

  /** Make a temp asset dir holding `index.html`, point `$AUK_WEBUI_DIR` at it for
    * the duration of `f` (the server resolves it synchronously in `ensureStarted`,
    * so restoring the env right after `f` returns its Future is safe). */
  private def withAssetDir[A](f: String => A): A =
    counter += 1
    val dir = osPath.join(os.tmpdir(), s"auk-webui-test-${js.Dynamic.global.process.pid}-$counter").asInstanceOf[String]
    fs.mkdirSync(dir, js.Dynamic.literal(recursive = true))
    fs.writeFileSync(osPath.join(dir, "index.html"), "<!doctype html><div id=\"app\"></div>", "utf8")
    val env = js.Dynamic.global.process.env
    val prev = env.AUK_WEBUI_DIR
    env.AUK_WEBUI_DIR = dir
    try f(dir)
    finally
      if js.isUndefined(prev) then js.special.delete(env.asInstanceOf[js.Object], "AUK_WEBUI_DIR")
      else env.AUK_WEBUI_DIR = prev

  private def portOf(url: String): Int = url.substring(url.lastIndexOf(':') + 1).toInt

  /** Read SSE frames from `/events` on `port`, decoding each; `onFirst` fires once
    * the first frame arrives (to publish a live event after the client is
    * registered); resolves with the decoded messages once `target` have arrived. */
  private def collectFrames(port: Int, target: Int, onFirst: () => Unit): Future[List[WireMessage]] =
    val p = Promise[List[WireMessage]]()
    val frames = ListBuffer.empty[WireMessage]
    var buf = ""
    var firedFirst = false
    http.get(
      s"http://127.0.0.1:$port/events",
      ((res: js.Dynamic) =>
        res.setEncoding("utf8")
        res.on("data", ((chunk: js.Any) =>
          buf += chunk.asInstanceOf[String]
          var idx = buf.indexOf("\n\n")
          while idx >= 0 do
            val frame = buf.substring(0, idx).trim
            buf = buf.substring(idx + 2)
            if frame.startsWith("data:") then WireCodec.decode(frame.stripPrefix("data:").trim).foreach(frames += _)
            idx = buf.indexOf("\n\n")
          if !firedFirst && frames.nonEmpty then { firedFirst = true; onFirst() }
          if frames.size >= target && !p.isCompleted then p.success(frames.toList)
          ()
        ): js.Function1[js.Any, Unit])
        ()
      ): js.Function1[js.Dynamic, Unit]
    )
    p.future

  /** GET `path`, resolving with the status and the whole body as text. */
  private def get(port: Int, path: String): Future[(Int, String)] =
    val p = Promise[(Int, String)]()
    http.get(s"http://127.0.0.1:$port$path", ((res: js.Dynamic) =>
      res.setEncoding("utf8")
      var body = ""
      res.on("data", ((c: js.Any) => { body += c.asInstanceOf[String]; () }): js.Function1[js.Any, Unit])
      res.on("end", ((_: js.Any) => { p.trySuccess((res.statusCode.asInstanceOf[Int], body)); () }): js.Function1[js.Any, Unit])
      ()
    ): js.Function1[js.Dynamic, Unit])
    p.future

  // -- loop fixtures ------------------------------------------------------------

  private val At = "2026-07-30T12:00:00Z"

  /** A fresh project directory, and a server rooted at it. */
  private def project(): String = TestFs.tempDir("auk-webui-loops")

  private def storeIn(dir: String): LoopStore = LoopStore(PathOps.join(dir, LoopStore.AukRelativePath))

  private def writeLedger(dir: String, loopId: String, events: LoopEvent*): Unit =
    val store = storeIn(dir)
    events.foreach(e => store.append(loopId, e).getOrElse(fail("the fixture ledger could not be written")))

  private def created(baseline: String) = LoopEvent.LoopCreated("opt", baseline, "head", "s0", At)
  private val attached = LoopEvent.DefAttached(1, "source", "cut p99 latency", "faster is better", Budgets(), Json.Null, At)

  private def server(dir: String, portP: Promise[Int]): WorkflowWebServer =
    WorkflowWebServer(
      onStarted = url => portP.trySuccess(portOf(url)),
      onError = msg => portP.tryFailure(new RuntimeException(msg)),
      loopContext = Some(RuntimeContext(dir))
    )

  private def generationWire(gen: Int, state: String) =
    LoopGenerationWire(gen, None, state, "", Nil, None,
      List(LoopAttemptWire(1, "a try", "{}", hasSnapshot = false, None, None, At)), At, None)

  private def loopWire(id: String, held: Boolean, generations: List[LoopGenerationWire] = Nil) =
    LoopWire(id, "running (gen 1)", "cut p99 latency", "faster is better", LoopBudgetsWire(50, 2, 3),
      "source", 1, held, None, orphaned = false, Some("gen 1, attempt 1 — working"),
      Some(LoopStageWire(1, 1, "working")), Some(LoopBridge.workerTranscriptLabel(1)), generations, At)

  /** Run `git` in `dir`, with an identity so committing works on any machine. */
  private def git(dir: String, args: String*): String =
    val cp = js.Dynamic.global.require("node:child_process")
    val argv = List("-C", dir, "-c", "user.name=auk", "-c", "user.email=auk@test") ++ args
    cp.execFileSync("git", js.Array(argv*), js.Dynamic.literal(encoding = "utf8", stdio = js.Array[js.Any]("ignore", "pipe", "pipe")))
      .asInstanceOf[String]
      .trim

  /** A repository holding two commits, one line of `f.txt` apart. */
  private def scratchRepo(dir: String): (String, String) =
    git(dir, "init", "-q")
    TestFs.write(PathOps.join(dir, "f.txt"), "one\n")
    git(dir, "add", "-A")
    git(dir, "commit", "-q", "-m", "first")
    val base = git(dir, "rev-parse", "HEAD")
    TestFs.write(PathOps.join(dir, "f.txt"), "two\n")
    git(dir, "add", "-A")
    git(dir, "commit", "-q", "-m", "second")
    (base, git(dir, "rev-parse", "HEAD"))

  test("a connecting client gets a Snapshot, replayed transcripts, then live frames"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val web = WorkflowWebServer(
        onStarted = url => portP.trySuccess(portOf(url)),
        onError = msg => portP.tryFailure(new RuntimeException(msg))
      )
      web.ensureStarted()
      // State accrued before anyone connects → must arrive as snapshot + replay.
      web.publish(WireMessage.Event(OrchestrationEvent.NodeDeclared("r", "a", None, Nil)))
      web.publish(WireMessage.Activity(TranscriptEvent.Said("r", "a", "hi")))
      portP.future.flatMap: port =>
        // Once connected (first frame seen), publish a live event the client must
        // receive. Four frames: the two snapshots, the replay, then the live event.
        collectFrames(port, target = 4, onFirst = () =>
          web.publish(WireMessage.Event(OrchestrationEvent.NodeStarted("r", "a", "go")))
        ).map: fs =>
          web.close()
          fs.head match
            case WireMessage.Snapshot(forests) =>
              assert(forests.exists((rid, f) => rid == "r" && f.nodes.exists(_.id == "a")), s"snapshot missing node: $forests")
            case other => fail(s"expected a Snapshot first, got $other")
          assert(fs.exists { case WireMessage.Activity(TranscriptEvent.Said("r", "a", t)) => t == "hi"; case _ => false },
            s"expected the replayed transcript: $fs")
          assert(fs.exists { case WireMessage.Event(_: OrchestrationEvent.NodeStarted) => true; case _ => false },
            s"expected the live event: $fs")

  test("a resumed (re-queued) interrupted node's stale transcript is not replayed to a new client"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val web = WorkflowWebServer(
        onStarted = url => portP.trySuccess(portOf(url)),
        onError = msg => portP.tryFailure(new RuntimeException(msg))
      )
      web.ensureStarted()
      // Node 'a' ran, produced partial output, was interrupted, then re-admitted on
      // resume — its discarded attempt's transcript must be dropped from the replay
      // store, so a client connecting after resume doesn't see it spliced in.
      web.publish(WireMessage.Event(OrchestrationEvent.NodeStarted("r", "a", "go")))
      web.publish(WireMessage.Activity(TranscriptEvent.Said("r", "a", "OLD-PARTIAL")))
      web.publish(WireMessage.Event(OrchestrationEvent.NodeInterrupted("r", "a")))
      web.publish(WireMessage.Event(OrchestrationEvent.NodeQueued("r", "a")))
      portP.future.flatMap: port =>
        // After the two snapshots, publish the fresh run's first line; collect all three.
        collectFrames(port, target = 3, onFirst = () =>
          web.publish(WireMessage.Activity(TranscriptEvent.Said("r", "a", "FRESH")))
        ).map: fs =>
          web.close()
          val saidTexts = fs.collect { case WireMessage.Activity(TranscriptEvent.Said(_, "a", t)) => t }
          assert(!saidTexts.contains("OLD-PARTIAL"), s"the discarded attempt's transcript was replayed: $fs")
          assert(saidTexts.contains("FRESH"), s"expected the fresh transcript frame: $fs")

  test("serves a binary asset (woff2) byte-for-byte intact with font/woff2"):
    val Buffer = js.Dynamic.global.Buffer
    // wOF2 magic + bytes a utf8 round-trip would mangle (0xC3 0x28 is invalid UTF-8).
    val original = Buffer.from(js.Array[js.Any](0x77, 0x4f, 0x46, 0x32, 0x00, 0xff, 0x80, 0x10, 0xc3, 0x28))
    withAssetDir: dir =>
      fs.writeFileSync(osPath.join(dir, "font.woff2"), original)
      val portP = Promise[Int]()
      val web = WorkflowWebServer(url => portP.trySuccess(portOf(url)), msg => portP.tryFailure(new RuntimeException(msg)))
      web.ensureStarted()
      portP.future.flatMap: port =>
        val p = Promise[(Int, String, js.Dynamic)]()
        http.get(s"http://127.0.0.1:$port/font.woff2", ((res: js.Dynamic) =>
          val chunks = js.Array[js.Any]() // no setEncoding → data chunks are Buffers
          res.on("data", ((c: js.Any) => { chunks.push(c); () }): js.Function1[js.Any, Unit])
          res.on("end", ((_: js.Any) =>
            p.trySuccess((res.statusCode.asInstanceOf[Int], res.headers.selectDynamic("content-type").asInstanceOf[String], Buffer.concat(chunks)))
            ()
          ): js.Function1[js.Any, Unit])
          ()
        ): js.Function1[js.Dynamic, Unit])
        p.future.map: (status, ctype, body) =>
          web.close()
          assertEquals(status, 200)
          assertEquals(ctype, "font/woff2")
          assertEquals(body.length.asInstanceOf[Int], original.length.asInstanceOf[Int])
          assert(Buffer.compare(body, original).asInstanceOf[Int] == 0, "served font bytes differ from the original (binary corruption)")

  test("serves index.html at / and 404s unknown paths and traversal"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val web = WorkflowWebServer(url => portP.trySuccess(portOf(url)), msg => portP.tryFailure(new RuntimeException(msg)))
      web.ensureStarted()
      portP.future.flatMap: port =>
        def status(path: String): Future[Int] =
          val p = Promise[Int]()
          http.get(s"http://127.0.0.1:$port$path", ((res: js.Dynamic) => { p.trySuccess(res.statusCode.asInstanceOf[Int]); () }): js.Function1[js.Dynamic, Unit])
          p.future
        for
          root <- status("/")
          missing <- status("/nope.xyz")
          traversal <- status("/../secret")
        yield
          web.close()
          assertEquals(root, 200)
          assertEquals(missing, 404)
          assertEquals(traversal, 404)

  test("the SSE stream sends heartbeat comments so idle connections aren't reaped"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val web = WorkflowWebServer(
        url => portP.trySuccess(portOf(url)),
        msg => portP.tryFailure(new RuntimeException(msg)),
        heartbeatMs = 40
      )
      web.ensureStarted()
      portP.future.flatMap: port =>
        val seen = Promise[Boolean]()
        http.get(s"http://127.0.0.1:$port/events", ((res: js.Dynamic) =>
          res.setEncoding("utf8")
          res.on("data", ((chunk: js.Any) =>
            if chunk.asInstanceOf[String].contains(": ping") then seen.trySuccess(true)
            ()
          ): js.Function1[js.Any, Unit])
          ()
        ): js.Function1[js.Dynamic, Unit])
        seen.future.map: ok =>
          web.close()
          assert(ok, "expected a heartbeat ': ping' comment within a few intervals")

  test("heartbeat comments do not corrupt the decoded message stream"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val web = WorkflowWebServer(
        url => portP.trySuccess(portOf(url)),
        msg => portP.tryFailure(new RuntimeException(msg)),
        heartbeatMs = 25
      )
      web.ensureStarted()
      web.publish(WireMessage.Event(OrchestrationEvent.NodeDeclared("r", "a", None, Nil)))
      portP.future.flatMap: port =>
        // Heartbeats (": ping") interleave with data frames; collectFrames keeps
        // only `data:` frames, so the decoded stream must be clean regardless.
        collectFrames(port, target = 3, onFirst = () =>
          web.publish(WireMessage.Event(OrchestrationEvent.NodeStarted("r", "a", "go")))
        ).map: fs =>
          web.close()
          assert(fs.head.isInstanceOf[WireMessage.Snapshot], s"expected a Snapshot first, got ${fs.head}")
          assert(fs.exists { case WireMessage.Event(_: OrchestrationEvent.NodeStarted) => true; case _ => false },
            s"live event missing amid heartbeats: $fs")

  // -- loops --------------------------------------------------------------------

  test("a connecting client is told the workflows, then the loops, then the transcripts"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val dir = project()
      writeLedger(dir, "opt", created("base"), attached, LoopEvent.GenerationStarted(1, None, "sess-a", At))
      val web = server(dir, portP)
      web.ensureStarted()
      web.publishLoopActivity(TranscriptEvent.Said("opt", LoopBridge.workerTranscriptLabel(1), "working"))
      portP.future.flatMap: port =>
        collectFrames(port, target = 3, onFirst = () => ()).map: fs =>
          web.close()
          // The order is a contract: a transcript names a loop the client has heard of.
          assert(fs(0).isInstanceOf[WireMessage.Snapshot], s"expected a Snapshot first, got ${fs(0)}")
          fs(1) match
            case WireMessage.LoopSnapshot(loops) => assertEquals(loops.map(_.id), List("opt"))
            case other                           => fail(s"expected a LoopSnapshot second, got $other")
          assert(fs(2).isInstanceOf[WireMessage.Activity], s"expected the replayed transcript third, got ${fs(2)}")

  test("a published loop reaches a connected client"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val web = server(project(), portP)
      web.ensureStarted()
      portP.future.flatMap: port =>
        collectFrames(port, target = 3, onFirst = () => web.publishLoop(loopWire("opt", held = true))).map: fs =>
          web.close()
          val published = fs.collect { case WireMessage.Loop(l) => l }
          assertEquals(published.map(_.id), List("opt"))
          assertEquals(published.head.goal, "cut p99 latency")

  test("a loop nobody here is driving is read off disk and arrives in the connect snapshot"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val dir = project()
      // A ledger that says a generation is running, with no session behind it.
      writeLedger(dir, "opt", created("base"), attached,
        LoopEvent.GenerationStarted(1, None, "sess-a", At),
        LoopEvent.AttemptSubmitted(1, 1, Json.Null, "a try", None, Nil, At))
      val web = server(dir, portP)
      web.ensureStarted()
      portP.future.flatMap: port =>
        collectFrames(port, target = 2, onFirst = () => ()).map: fs =>
          web.close()
          fs(1) match
            case WireMessage.LoopSnapshot(List(loop)) =>
              assertEquals(loop.id, "opt")
              assertEquals(loop.phase, LoopBridge.Orphaned)
              assertEquals(loop.held, false)
              // The whole ledger, not just its shape: attempts and all.
              assertEquals(loop.generations.map(_.gen), List(1))
              assertEquals(loop.generations.head.attempts.map(_.description), List("a try"))
            case other => fail(s"expected one disk-read loop, got $other")

  test("a loop this session holds is never overwritten by the disk read"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val dir = project()
      // On disk it reads as an orphan; the session driving it knows better.
      writeLedger(dir, "opt", created("base"), attached, LoopEvent.GenerationStarted(1, None, "sess-a", At))
      val web = server(dir, portP)
      web.ensureStarted()
      web.publishLoop(loopWire("opt", held = true))
      portP.future.flatMap: port =>
        collectFrames(port, target = 2, onFirst = () => ()).map: fs =>
          web.close()
          fs(1) match
            case WireMessage.LoopSnapshot(List(loop)) =>
              assertEquals(loop.held, true)
              assertEquals(loop.phase, "running (gen 1)")
              assertEquals(loop.activity, Some("gen 1, attempt 1 — working"))
              assertEquals(loop.stage, Some(LoopStageWire(1, 1, "working")))
            case other => fail(s"expected the held loop, got $other")

  test("a settled generation's transcripts are dropped, the one in flight kept"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val web = server(project(), portP)
      web.ensureStarted()
      web.publishLoopActivity(TranscriptEvent.Said("opt", LoopBridge.workerTranscriptLabel(1), "GEN-ONE"))
      web.publishLoopActivity(TranscriptEvent.Said("opt", LoopBridge.evalTranscriptLabel(1), "JUDGED-ONE"))
      web.publishLoopActivity(TranscriptEvent.Said("opt", LoopBridge.workerTranscriptLabel(2), "GEN-TWO"))
      // Generation 1 has settled: its logs stop changing and live on disk.
      web.publishLoop(loopWire("opt", held = true,
        generations = List(generationWire(1, "accepted"), generationWire(2, "running"))))
      portP.future.flatMap: port =>
        collectFrames(port, target = 3, onFirst = () => ()).map: fs =>
          web.close()
          val said = fs.collect { case WireMessage.Activity(TranscriptEvent.Said(_, label, text)) => (label, text) }
          assertEquals(said, List((LoopBridge.workerTranscriptLabel(2), "GEN-TWO")))

  // -- the on-demand API ---------------------------------------------------------

  test("the transcript route serves the tee file the generation's session wrote"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val dir = project()
      writeLedger(dir, "opt", created("base"), attached, LoopEvent.GenerationStarted(3, None, "sess-a", At))
      val jsonl =
        WireCodec.encode(WireMessage.Activity(TranscriptEvent.Said("opt", "gen-3-worker", "hello"))) + "\n" +
          WireCodec.encode(WireMessage.Activity(TranscriptEvent.Thought("opt", "gen-3-worker", "hmm"))) + "\n"
      TestFs.write(SessionRef.loopLog(PathOps.join(dir, SessionProvider.RelativePath), "sess-a", "opt", "gen-3-worker"), jsonl)
      val web = server(dir, portP)
      web.ensureStarted()
      portP.future.flatMap: port =>
        for
          served  <- get(port, "/api/loop/opt/transcript/gen-3-worker")
          // The generation ran in sess-a, so the evaluator's file simply is not there.
          missing <- get(port, "/api/loop/opt/transcript/gen-3-eval")
          // Neither of a generation's two labels, so nothing to look for.
          bogus   <- get(port, "/api/loop/opt/transcript/..%2Fescape")
          unknown <- get(port, "/api/loop/nobody/transcript/gen-3-worker")
        yield
          web.close()
          assertEquals(served._1, 200)
          assertEquals(served._2, jsonl)
          assertEquals(missing._1, 404)
          assertEquals(bogus._1, 404)
          assertEquals(unknown._1, 404)

  test("the diff route answers with the patch between what a generation started from and what it offered"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val dir = project()
      val (base, candidate) = scratchRepo(dir)
      writeLedger(dir, "opt", created(base), attached,
        LoopEvent.GenerationStarted(1, None, "sess-a", At),
        LoopEvent.AttemptSubmitted(1, 1, Json.Null, "a try", None, List(candidate), At))
      val web = server(dir, portP)
      web.ensureStarted()
      portP.future.flatMap: port =>
        for
          diff       <- get(port, "/api/loop/opt/diff/1/1")
          noAttempt  <- get(port, "/api/loop/opt/diff/1/2")
          noGen      <- get(port, "/api/loop/opt/diff/9/1")
          notANumber <- get(port, "/api/loop/opt/diff/one/1")
        yield
          web.close()
          assertEquals(diff._1, 200)
          assert(diff._2.contains("f.txt"), s"expected a patch naming the file: ${diff._2}")
          assert(diff._2.contains("-one"), s"expected the removed line: ${diff._2}")
          assert(diff._2.contains("+two"), s"expected the added line: ${diff._2}")
          // Nothing was snapshotted for these, so there is nothing to diff.
          assertEquals(noAttempt._1, 404)
          assertEquals(noGen._1, 404)
          assertEquals(notANumber._1, 404)

  test("an abandoned generation's diff falls back to the snapshot its rescue kept"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val dir = project()
      val (base, candidate) = scratchRepo(dir)
      // What the rescue captured before the tree was rolled back.
      val rescueId = LoopBridge.abandonedId("opt", 1)
      git(dir, "update-ref", auk.snapshot.Snapshot.RefPrefix + rescueId, candidate)
      writeLedger(dir, "opt", created(base), attached,
        LoopEvent.GenerationStarted(1, None, "sess-a", At),
        LoopEvent.GenerationAbandoned(1, 0, Some(rescueId), At))
      val web = server(dir, portP)
      web.ensureStarted()
      portP.future.flatMap: port =>
        get(port, "/api/loop/opt/diff/1/1").map: (status, body) =>
          web.close()
          assertEquals(status, 200)
          assert(body.contains("+two"), s"expected the rescued work's patch: $body")

  test("an unknown API route is a 404, and the bundle is still served beside it"):
    withAssetDir: _ =>
      val portP = Promise[Int]()
      val web = server(project(), portP)
      web.ensureStarted()
      portP.future.flatMap: port =>
        for
          nonsense <- get(port, "/api/nope")
          root     <- get(port, "/")
        yield
          web.close()
          assertEquals(nonsense._1, 404)
          assertEquals(root._1, 200)
