package auk.platform.js

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** Regression coverage for [[SocketServer]]'s write-after-close guard.
  *
  * A workflow pause closes a run's connection (`conn.close()` → `socket.end()`)
  * while an in-flight sub-agent may still write its result/error. A naive
  * `socket.write` after `end()` raises `ERR_STREAM_WRITE_AFTER_END` as a
  * *listener-less* 'error' event, which Node escalates to an `uncaughtException`
  * that crashes the whole process (observed in `.auk/crash.log`). The conn must
  * absorb writes once closed. Real OS socket over an OS-picked temp UDS path. */
class SocketServerSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 30.seconds

  private val net = js.Dynamic.global.require("node:net")
  private val os = js.Dynamic.global.require("node:os")
  private val osPath = js.Dynamic.global.require("node:path")
  private var counter = 0

  private def tmpSock(): String =
    counter += 1
    osPath.join(os.tmpdir(), s"auk-sock-test-${js.Dynamic.global.process.pid}-$counter.sock").asInstanceOf[String]

  /** Sleep `ms` on the JS event loop (long enough for an async 'error' to surface). */
  private def after(ms: Int): Future[Unit] =
    val p = Promise[Unit]()
    js.Dynamic.global.setTimeout((() => { p.trySuccess(()); () }): js.Function0[Unit], ms)
    p.future

  test("write after the conn is closed is a no-op, not a process crash"):
    val sockPath = tmpSock()
    val connP = Promise[SocketServer.Conn]()
    val handle = SocketServer.listen(sockPath)(conn => connP.trySuccess(conn))
    val client = net.createConnection(sockPath).asInstanceOf[js.Dynamic]
    // Capture any uncaughtException the bad write would otherwise escalate to.
    val proc = js.Dynamic.global.process
    val crashed = Promise[String]()
    val onCrash: js.Function1[js.Any, Unit] = (e: js.Any) => { crashed.trySuccess(String.valueOf(e)); () }
    proc.on("uncaughtException", onCrash)
    connP.future
      .flatMap: conn =>
        conn.close()        // socket.end()
        conn.write("late")  // raw socket would throw write-after-end here
        conn.write("late-2")
        after(120)          // let any async 'error' surface as uncaughtException
      .map: _ =>
        proc.removeListener("uncaughtException", onCrash)
        client.end()
        handle.close()
        assert(!crashed.isCompleted, "write after close escalated to an uncaughtException (process would crash)")
