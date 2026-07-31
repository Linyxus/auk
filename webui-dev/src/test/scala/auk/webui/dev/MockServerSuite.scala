package auk.webui.dev

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import auk.workflow.WireCodec

/** End-to-end against a real `node:http` server on an ephemeral port (modeled on
  * how WorkflowBridgeSuite spins a real `node:net` server in-test). munit awaits
  * the returned `Future`. */
class MockServerSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 30.seconds

  test("GET /events streams SSE frames that decode to wire messages"):
    val portP = Promise[Int]()
    val server = MockServer.start(".", 0, p => portP.success(p))
    portP.future.flatMap: port =>
      val framesP = Promise[List[String]]()
      val frames = scala.collection.mutable.ListBuffer.empty[String]
      var buf = ""
      NodeHttp.get(
        s"http://127.0.0.1:$port/events?scenario=fanout",
        ((res: ClientResponse) =>
          res.setEncoding("utf8")
          res.on("data", ((chunk: js.Any) =>
            buf += chunk.asInstanceOf[String]
            var idx = buf.indexOf("\n\n")
            while idx >= 0 do
              val frame = buf.substring(0, idx).trim
              buf = buf.substring(idx + 2)
              if frame.startsWith("data:") then frames += frame.stripPrefix("data:").trim
              idx = buf.indexOf("\n\n")
            if frames.size >= 2 && !framesP.isCompleted then framesP.success(frames.toList)
            ()
          ): js.Function1[js.Any, Unit])
          ()
        ): js.Function1[ClientResponse, Unit]
      )
      framesP.future.map: fs =>
        server.close()
        assert(fs.nonEmpty, "expected SSE frames")
        assert(fs.forall(f => WireCodec.decode(f).isRight), s"frames did not decode: $fs")

  test("an unknown static path returns 404"):
    val portP = Promise[Int]()
    val server = MockServer.start(".", 0, p => portP.success(p))
    portP.future.flatMap: port =>
      val codeP = Promise[Int]()
      NodeHttp.get(
        s"http://127.0.0.1:$port/definitely-not-a-real-file.xyz",
        ((res: ClientResponse) => { codeP.success(res.statusCode); () }): js.Function1[ClientResponse, Unit]
      )
      codeP.future.map: code =>
        server.close()
        assertEquals(code, 404)

  /** Read a whole response, so a body can be asserted on rather than just a code. */
  private def fetch(port: Int, path: String): Future[(Int, String)] =
    val done = Promise[(Int, String)]()
    NodeHttp.get(
      s"http://127.0.0.1:$port$path",
      ((res: ClientResponse) =>
        res.setEncoding("utf8")
        val body = new StringBuilder
        res.on("data", ((c: js.Any) => { body ++= c.asInstanceOf[String]; () }): js.Function1[js.Any, Unit])
        res.on("end", ((_: js.Any) => { done.success((res.statusCode, body.toString)); () }): js.Function1[js.Any, Unit])
        ()
      ): js.Function1[ClientResponse, Unit]
    )
    done.future

  test("the api serves a settled generation's transcript as the JSONL a tee would hold"):
    val portP = Promise[Int]()
    val server = MockServer.start(".", 0, p => portP.success(p))
    portP.future
      .flatMap(port => fetch(port, s"/api/loop/${LoopScenario.LiveId}/transcript/gen-1-worker"))
      .map: (code, body) =>
        server.close()
        assertEquals(code, 200)
        val frames = body.linesIterator.toVector
        assert(frames.nonEmpty, "expected some frames")
        assert(frames.forall(f => WireCodec.decode(f).isRight), s"frames did not decode: $frames")

  test("the api serves an attempt's patch"):
    val portP = Promise[Int]()
    val server = MockServer.start(".", 0, p => portP.success(p))
    portP.future
      .flatMap(port => fetch(port, s"/api/loop/${LoopScenario.LiveId}/diff/6/2"))
      .map: (code, body) =>
        server.close()
        assertEquals(code, 200)
        assert(body.startsWith("diff --git"), body.take(60))

  /** A 404 is a real answer — a generation whose tee was never written — and the
    * browser draws it, so it must not be dressed up as an empty success. */
  test("the api 404s a payload the scenario never recorded"):
    val portP = Promise[Int]()
    val server = MockServer.start(".", 0, p => portP.success(p))
    portP.future
      .flatMap(port => fetch(port, "/api/loop/no-such-loop/transcript/gen-1-worker"))
      .map: (code, _) =>
        server.close()
        assertEquals(code, 404)
