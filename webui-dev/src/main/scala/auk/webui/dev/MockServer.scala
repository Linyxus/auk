package auk.webui.dev

import scala.scalajs.js
import scala.scalajs.js.timers

/** The effectful mock server: serve static files from `rootDir` and stream a
  * scripted scenario as Server-Sent Events on `GET /events?scenario=…`. All the
  * branching logic it calls ([[Router]], [[Scenario]]) is pure and tested
  * separately; this is the thin Node glue. */
object MockServer:

  /** Bind on `port` (0 = ephemeral) and start serving; `onListening` receives the
    * actual bound port. Returns the server handle so callers can `close()`. */
  def start(rootDir: String, port: Int, onListening: Int => Unit = _ => ()): HttpServer =
    val handler: js.Function2[ServerRequest, ServerResponse, Unit] = (req, res) =>
      Router.route(req.method, req.url, rootDir) match
        case Route.Static(p)    => serveFile(p, res)
        case Route.Events(name) => serveSse(name, res)
        case Route.NotFound(_)  => respond(res, 404, "text/plain; charset=utf-8", "not found")
    val server = NodeHttp.createServer(handler)
    server.listen(port, () => onListening(actualPort(server)))
    server

  private def serveFile(path: String, res: ServerResponse): Unit =
    if NodeFs.existsSync(path) then
      // Raw bytes: text assets are byte-identical, and binary ones (woff2 fonts)
      // would be corrupted by a utf8 round-trip.
      res.writeHead(200, headers("Content-Type" -> Router.contentType(path)))
      res.end(NodeFs.readFileSync(path))
    else respond(res, 404, "text/plain; charset=utf-8", "not found")

  private def serveSse(scenarioName: String, res: ServerResponse): Unit =
    res.writeHead(200, headers("Content-Type" -> "text/event-stream", "Cache-Control" -> "no-cache", "Connection" -> "keep-alive"))
    Scenario.schedule(Scenarios.byName(scenarioName), leadSnapshot = false).foreach: sl =>
      timers.setTimeout(sl.delayMs.toDouble) { res.write(s"data: ${sl.data}\n\n"); () }

  private def respond(res: ServerResponse, status: Int, contentType: String, body: String): Unit =
    res.writeHead(status, headers("Content-Type" -> contentType))
    res.end(body)

  private def headers(pairs: (String, String)*): js.Object =
    js.Dynamic.literal(pairs.map((k, v) => (k, v: js.Any))*).asInstanceOf[js.Object]

  private def actualPort(server: HttpServer): Int =
    val addr = server.address()
    if addr == null then 0 else addr.port.asInstanceOf[Int]
