package auk.runtime

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

import auk.workflow.{OrchestrationEvent, TranscriptEvent, WireCodec, WireMessage}

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
        // Once connected (first frame seen), publish a live event the client must receive.
        collectFrames(port, target = 3, onFirst = () =>
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
        // After the snapshot, publish the fresh run's first line; collect both.
        collectFrames(port, target = 2, onFirst = () =>
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
        collectFrames(port, target = 2, onFirst = () =>
          web.publish(WireMessage.Event(OrchestrationEvent.NodeStarted("r", "a", "go")))
        ).map: fs =>
          web.close()
          assert(fs.head.isInstanceOf[WireMessage.Snapshot], s"expected a Snapshot first, got ${fs.head}")
          assert(fs.exists { case WireMessage.Event(_: OrchestrationEvent.NodeStarted) => true; case _ => false },
            s"live event missing amid heartbeats: $fs")
