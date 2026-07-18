package auk.runtime

import scala.util.control.NonFatal

import gears.async.{Async, CancellationException, Future, UnboundedChannel}

import auk.llm.tools.Json
import auk.platform.js.SocketServer
import auk.runtime.mcp.{McpHub, McpResourceInfo, McpToolInfo}

/** The host side of the MCP bridge: a Unix-domain-socket server the short-lived
  * `--mcp-call` helper connects to for ONE request/response round-trip, so
  * `eval_scala` user code can reach MCP servers synchronously (it `spawnSync`s the
  * helper). It owns nothing itself — it is a thin request/response front for the
  * host's shared [[McpHub]].
  *
  * Unlike [[TeamBridge]] (which multiplexes a long-lived, stateful conversation
  * per connection), this bridge is stateless and correlation-free at the socket
  * layer: each inbound line is a self-contained request `{id, op, server?, tool?,
  * args?, uri?}`, and the reply — `{id, ok:true, data}` or `{id, ok:false,
  * error}`, with `id` echoed — goes back on the SAME connection. The helper reads
  * exactly one line, so no `t`-discriminator is needed. `data` is the Phase-1
  * host result types serialized via their `toJson`; see [[dataFor]] for the
  * per-op shape (the library-side proxy decodes it).
  *
  * Each request runs on its own fiber ([[dispatch]]), because an op suspends on
  * the MCP server's round-trip: serving them inline would let one slow server
  * head-of-line-block every other in-flight call across all agents. All state is
  * touched only on the single JS event loop, so there is no locking. */
final class McpBridge(val socketPath: String, hub: McpHub):
  import McpBridge.*

  private val incoming = UnboundedChannel[(SocketServer.Conn, Json)]()
  private var server: SocketServer.Handle | Null = null

  /** Bind the socket and start servicing clients. Spawns the dispatch fiber in the
    * caller's scope; returns immediately. */
  def start(onReady: () => Unit = () => ())(using Async.Spawn): Unit =
    server = SocketServer.listen(socketPath, onReady): conn =>
      conn.onLine: line =>
        Json.parse(line) match
          case Right(msg) => incoming.sendImmediately((conn, msg))
          case Left(_)    => () // ignore an unparseable line; the helper only sends JSON
    Future:
      var running = true
      while running do
        incoming.read() match
          case Right((conn, msg)) => dispatch(conn, msg)
          case Left(_)            => running = false

  /** Run one request on its own fiber so a slow MCP round-trip can't stall others. */
  private def dispatch(conn: SocketServer.Conn, msg: Json)(using Async.Spawn): Unit =
    val _ = Future(respond(conn, msg))

  /** Execute the request's op against the hub and write the correlated reply.
    *
    * This ALWAYS writes a reply. The premise of the whole feature is that a call
    * never hangs the model: the helper blocks in `spawnSync` until it reads a
    * line, so a request that produced no reply would stall the worker until its
    * ~130s timeout. A logical failure is already a `Left`; an UNEXPECTED throwable
    * (a JS-interop surprise, a future decode edge) must not silently kill the
    * fiber and swallow the reply, so it is caught and turned into an `ok:false`
    * "internal error" line. Cancellation (bridge shutdown) is the one throwable we
    * let propagate — the connection is going away, and swallowing it would break
    * cooperative teardown. */
  private def respond(conn: SocketServer.Conn, msg: Json)(using Async): Unit =
    val id = field(msg, "id").getOrElse(Json.Null)
    val reply =
      try
        runOp(msg) match
          case Right(data) => okReply(id, data)
          case Left(err)   => errReply(id, err)
      catch
        case e: CancellationException => throw e
        case NonFatal(e)              => errReply(id, s"internal error: ${describe(e)}")
    conn.write(reply.render)

  /** Dispatch on `op`, resolving the required fields and delegating to the hub.
    * Returns the op's `data` on success or a message on failure — both a missing
    * field and a hub error (unknown server, MCP failure) map to `Left`. */
  private def runOp(msg: Json)(using Async): Either[String, Json] =
    str(msg, "op") match
      case None => Left("missing 'op'")
      case Some("listServers") =>
        Right(Json.Obj(List("servers" -> Json.Arr(hub.serverNames.map(Json.Str(_))))))
      case Some("listTools") =>
        require(msg, "server").flatMap(s => hub.listTools(s)).map(toolsData)
      case Some("callTool") =>
        for
          server <- require(msg, "server")
          tool   <- require(msg, "tool")
          result <- hub.callTool(server, tool, field(msg, "args").getOrElse(Json.Obj(Nil)))
        yield result.toJson
      case Some("listResources") =>
        require(msg, "server").flatMap(s => hub.listResources(s)).map(resourcesData)
      case Some("readResource") =>
        for
          server <- require(msg, "server")
          uri    <- require(msg, "uri")
          result <- hub.readResource(server, uri)
        yield result.toJson
      case Some(other) => Left(s"unknown MCP op '$other'")

  def close(): Unit =
    if server != null then server.nn.close()

object McpBridge:
  /** The env var carrying the bridge socket path to every REPL worker (unset ⇒
    * `lib.mcp` reports MCP unavailable). Injection is Phase 4; this names the
    * contract. */
  val SockEnv: String = "AUK_MCP_SOCK"

  /** A per-process temp socket path for the bridge (the server unlinks any stale
    * file on bind), mirroring [[TeamBridge.defaultSocketPath]]. */
  def defaultSocketPath(): String =
    val os = scala.scalajs.js.Dynamic.global.require("node:os")
    val pid = scala.scalajs.js.Dynamic.global.process.pid
    s"${os.tmpdir().asInstanceOf[String]}/auk-mcp-$pid.sock"

  private def toolsData(tools: List[McpToolInfo]): Json =
    Json.Obj(List("tools" -> Json.Arr(tools.map(_.toJson))))

  private def resourcesData(resources: List[McpResourceInfo]): Json =
    Json.Obj(List("resources" -> Json.Arr(resources.map(_.toJson))))

  private def okReply(id: Json, data: Json): Json =
    Json.Obj(List("id" -> id, "ok" -> Json.Bool(true), "data" -> data))

  private def errReply(id: Json, error: String): Json =
    Json.Obj(List("id" -> id, "ok" -> Json.Bool(false), "error" -> Json.Str(error)))

  /** A throwable's message, or a stand-in when it carries none (getMessage is
    * nullable under `-Yexplicit-nulls`). */
  private def describe(e: Throwable): String =
    val m = e.getMessage
    if m != null then m else "unknown error"

  // -- request field accessors -------------------------------------------------

  private def field(msg: Json, key: String): Option[Json] = msg match
    case o: Json.Obj => o.get(key)
    case _           => None

  private def str(msg: Json, key: String): Option[String] =
    field(msg, key).collect { case Json.Str(s) => s }

  private def require(msg: Json, key: String): Either[String, String] =
    str(msg, key).toRight(s"missing '$key'")
