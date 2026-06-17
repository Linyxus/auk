package auk.platform.js

import scala.scalajs.js

/** A minimal static-file + Server-Sent-Events HTTP server — the transport behind
  * the workflow web dashboard. Static GETs are served from `dir` (with a `..`
  * traversal guard); SSE clients connect at `ssePath`. Like [[SocketServer]],
  * callbacks fire on the JS event loop, so a caller that mutates shared state from
  * them does so on the single JS thread (no locking needed).
  *
  * The server is `unref`'d once listening, so an open SSE connection (which never
  * ends on its own) can never block process exit; [[Handle.close]] additionally
  * drops live connections.
  */
object WebServer:
  /** One connected SSE client. [[send]] frames `data` as a single SSE event and
    * silently drops writes to an already-closed socket. */
  trait Sse:
    def send(data: String): Unit
    def onClose(f: () => Unit): Unit

  trait Handle:
    def port: Int
    def close(): Unit

  /** Bind on `port` (0 = an OS-chosen spare port); `onListening` receives the
    * bound port. `onClient` runs synchronously for each new SSE connection — send
    * a snapshot and register the client there. Other GETs serve files from `dir`.
    *
    * `heartbeatMs` is the interval for an SSE keep-alive comment: an idle SSE
    * connection (one with no events flowing) is otherwise liable to be reaped by
    * the OS/proxy, after which the browser's `EventSource` silently reconnects —
    * and every reconnect makes the host replay the whole transcript. A periodic
    * comment line keeps the connection live; comments are ignored by `EventSource`,
    * so they never reach the client's `onmessage`. */
  def serve(dir: String, ssePath: String, onListening: Int => Unit, heartbeatMs: Int = 15000)(onClient: Sse => Unit): Handle =
    val server = NodeHttp.createServer(((req, res) =>
      val path = pathOf(req.url)
      if req.method != "GET" then notFound(res)
      else if path == ssePath then
        res.writeHead(200, headers(
          "Content-Type" -> "text/event-stream",
          "Cache-Control" -> "no-cache",
          "Connection" -> "keep-alive",
          "X-Accel-Buffering" -> "no"
        ))
        var open = true
        var closeHandler: () => Unit = () => ()
        val sse = new Sse:
          def send(data: String): Unit =
            if open then
              try { res.write(s"data: $data\n\n"); () }
              catch case _: Throwable => ()
          def onClose(f: () => Unit): Unit = closeHandler = f
        // Keep-alive comment so idle connections aren't reaped (see scaladoc). The
        // interval is unref'd so it can never, by itself, hold the process open —
        // matching the server's own `unref()`.
        val heartbeat = js.timers.setInterval(heartbeatMs.toDouble) {
          if open then
            try { res.write(": ping\n\n"); () }
            catch case _: Throwable => ()
        }
        try heartbeat.asInstanceOf[js.Dynamic].unref() catch case _: Throwable => ()
        req.on("close", ((_: js.Any) =>
          open = false
          js.timers.clearInterval(heartbeat)
          closeHandler()
        ): js.Function1[js.Any, Unit])
        onClient(sse)
      else serveFile(dir, path, res)
      ()
    ): js.Function2[NodeHttpRequest, NodeHttpResponse, Unit])
    server.listen(0, (() => onListening(actualPort(server))): js.Function0[Unit])
    server.unref()
    new Handle:
      def port: Int = actualPort(server)
      def close(): Unit =
        try server.closeAllConnections()
        catch case _: Throwable => ()
        server.close(); ()

  private def serveFile(dir: String, path: String, res: NodeHttpResponse): Unit =
    val rel = if path == "/" || path.isEmpty then "index.html" else path.stripPrefix("/")
    safeJoin(dir, rel) match
      case Some(p) if NodeFs.existsSync(p) =>
        res.writeHead(200, headers("Content-Type" -> contentType(p)))
        // Serve raw bytes, not a UTF-8 string: text assets are byte-identical, and
        // binary ones (woff2 fonts) would be corrupted by a utf8 round-trip.
        res.end(NodeFs.readFileSync(p))
      case _ => notFound(res)

  private def notFound(res: NodeHttpResponse): Unit =
    res.writeHead(404, headers("Content-Type" -> "text/plain; charset=utf-8"))
    res.end("not found")

  private def headers(pairs: (String, String)*): js.Object =
    js.Dynamic.literal(pairs.map((k, v) => (k, v: js.Any))*).asInstanceOf[js.Object]

  private def actualPort(server: NodeHttpServer): Int =
    val addr = server.address()
    if addr == null then 0 else addr.port.asInstanceOf[Int]

  private def pathOf(url: String): String =
    val i = url.indexOf('?')
    if i < 0 then url else url.substring(0, i)

  /** Join `rel` onto `root`, rejecting any path that escapes via `..`. */
  private[js] def safeJoin(root: String, rel: String): Option[String] =
    val segs = rel.split("/").iterator.filter(s => s.nonEmpty && s != ".").toVector
    if segs.contains("..") then None
    else Some((root.stripSuffix("/") +: segs).mkString("/"))

  private[js] def contentType(path: String): String =
    val dot = path.lastIndexOf('.')
    val ext = if dot >= 0 then path.substring(dot + 1) else ""
    ext match
      case "html"  => "text/html; charset=utf-8"
      case "js"    => "text/javascript; charset=utf-8"
      case "map"   => "application/json; charset=utf-8"
      case "css"   => "text/css; charset=utf-8"
      case "json"  => "application/json; charset=utf-8"
      case "txt"   => "text/plain; charset=utf-8"
      case "woff2" => "font/woff2"
      case "woff"  => "font/woff"
      case _       => "application/octet-stream"
