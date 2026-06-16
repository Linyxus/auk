package auk.platform.js

import scala.scalajs.js

/** A Unix-domain-socket line server: accepts connections and exchanges
  * newline-framed text. This is the host side of the workflow bridge (the worker
  * connects as a client, see `auk.library.WorkflowClient`).
  *
  * Callbacks fire on the JS event loop; bridge them into gears with channels
  * (`sendImmediately` from a callback, a consumer fiber on the reading side).
  */
object SocketServer:
  /** One accepted connection. Register [[onLine]] synchronously in the accept
    * callback (before data flows); [[write]] appends the newline. */
  trait Conn:
    def onLine(f: String => Unit): Unit
    def onClose(f: () => Unit): Unit
    def write(line: String): Unit
    def close(): Unit

  trait Handle:
    def path: String
    def close(): Unit

  /** Listen on `socketPath`. `onConn` runs for each accepted connection (use it
    * to register the line handler); `onListening` fires once the server is bound. */
  def listen(socketPath: String, onListening: () => Unit = () => ())(onConn: Conn => Unit): Handle =
    // Clear any stale socket file from a previous run so bind doesn't EADDRINUSE.
    try NodeFs.rmSync(socketPath, js.Dynamic.literal(force = true).asInstanceOf[js.Object])
    catch case _: Throwable => ()
    val server = NodeNet.createServer(((socket: NodeSocket) =>
      socket.setEncoding("utf8")
      val lines = new LineSplitter
      var handler: String => Unit = _ => ()
      var closeHandler: () => Unit = () => ()
      val conn = new Conn:
        def onLine(f: String => Unit): Unit = handler = f
        def onClose(f: () => Unit): Unit = closeHandler = f
        def write(line: String): Unit = { socket.write(line + "\n"); () }
        def close(): Unit = socket.end()
      onConn(conn)
      socket.on(
        "data",
        ((chunk: js.Any) => lines.feed(chunk.asInstanceOf[String]).foreach(l => handler(l))): js.Function1[js.Any, Unit]
      )
      socket.on("close", ((_: js.Any) => closeHandler()): js.Function1[js.Any, Unit])
      ()
    ): js.Function1[NodeSocket, Unit])
    server.listen(socketPath, (() => onListening()): js.Function0[Unit])
    new Handle:
      def path: String = socketPath
      def close(): Unit = { server.close(); () }
