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
