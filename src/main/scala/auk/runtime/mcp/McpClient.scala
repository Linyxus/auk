package auk.runtime.mcp

import scala.collection.mutable
import scala.scalajs.js.timers
import scala.util.Success
import scala.util.control.NonFatal

import gears.async.{Async, Future}

import auk.llm.tools.Json

/** A host-side, one-per-server MCP client speaking JSON-RPC 2.0 over an
  * [[McpTransport]].
  *
  * ## Why this shape
  *
  * A stdio MCP server is a single long-lived process handling MANY concurrent
  * requests, correlated only by JSON-RPC `id`. Responses arrive on the transport
  * callback (`onLine`) on the JS event loop, in no particular order, while the
  * callers awaiting them are suspended gears fibers. The bridge between the two
  * is a table of promises: each request registers a `Future.Promise` under its
  * `id`, the caller suspends on that promise's future, and `onLine` completes the
  * matching promise when the response lands. `onExit` fails every outstanding
  * promise at once. This is the same "JS callback → `Future.Promise` → gears
  * await" idiom used by `auk.platform.js.Interop` and `ScalaRepl`, generalised to
  * an id-keyed map because many requests can be in flight together.
  *
  * Everything runs on the single JS event loop, so the mutable state (the id
  * counter, the pending table, the connect gate) needs no locking — mirroring
  * [[auk.runtime.TeamBridge]].
  *
  * ## Lifecycle
  *
  * The client is lazy: the transport (and thus the child process) is not created
  * until the first operation triggers [[connect]]. `connect` runs the MCP
  * handshake once — `initialize`, record the server's reply, then the
  * `notifications/initialized` notification — and is idempotent: concurrent first
  * callers all await one shared handshake, and a completed handshake is memoised.
  * A per-request timeout guarantees a caller is never suspended forever on a
  * server that goes silent. */
final class McpClient(
    val name: String,
    makeTransport: (String => Unit, Int => Unit) => McpTransport,
    requestTimeoutMs: Double = McpClient.DefaultRequestTimeoutMs
):
  import McpClient.*

  // -- mutable host state (single JS event loop → no locking) -----------------

  private var transport: McpTransport | Null = null
  private var idCounter: Long = 0L
  private val pending = mutable.Map.empty[Long, Future.Promise[Either[String, Json]]]

  private var connected: Boolean = false
  // The in-flight handshake, shared by racing first callers; null once settled.
  private var connectGate: Future.Promise[Either[String, Unit]] | Null = null

  // Recorded from the server's `initialize` result. Kept for observability and,
  // in a later phase, capability-gated behaviour; negotiation is lenient (we do
  // not reject a differing protocol version in v1 — see the design doc).
  private var serverProtocolVersion: Option[String] = None
  private var serverCapabilities: Option[Json] = None
  private var serverInfoValue: Option[Json] = None

  /** The `serverInfo` object the server returned from `initialize`, if any. */
  def serverInfo: Option[Json] = serverInfoValue

  /** The protocol version the server echoed (may differ from ours). */
  def negotiatedProtocolVersion: Option[String] = serverProtocolVersion

  /** The server's advertised capabilities object, if any. */
  def capabilities: Option[Json] = serverCapabilities

  // -- public operations ------------------------------------------------------

  /** Run the MCP handshake if it has not run already. Idempotent and safe to call
    * concurrently: the first caller drives it and any caller that arrives while it
    * is in flight awaits the same result. */
  def connect()(using Async): Either[String, Unit] =
    if connected then Right(())
    else if connectGate != null then connectGate.nn.asFuture.await
    else
      val gate = Future.Promise[Either[String, Unit]]()
      connectGate = gate
      // handshake() reports failure as a Left, but an UNEXPECTED throwable (e.g.
      // the transport factory throwing when it spawns the child) must NOT escape
      // with the gate left pending — a later caller would `connectGate.await`
      // forever. Convert any such throw to a Left so the gate is ALWAYS resolved
      // and cleared, and the next call retries from a clean slate.
      val result =
        try handshake()
        catch case NonFatal(e) => Left(s"MCP connect to '$name' failed: ${describe(e)}")
      if result.isRight then connected = true
      connectGate = null
      gate.complete(Success(result))
      result

  /** List the server's tools. Lazily connects first. */
  def listTools()(using Async): Either[String, List[McpToolInfo]] =
    connect().flatMap(_ => rpc("tools/list", Json.Obj(Nil)).map(McpToolInfo.decodeList))

  /** Invoke a tool by name with a JSON `arguments` object. Lazily connects first.
    * A tool-level failure comes back as a successful [[McpCallResult]] with
    * `isError = true`; only a protocol/transport failure is `Left`. */
  def callTool(tool: String, arguments: Json)(using Async): Either[String, McpCallResult] =
    val params = Json.Obj(List("name" -> Json.Str(tool), "arguments" -> arguments))
    connect().flatMap(_ => rpc("tools/call", params).map(McpCallResult.decode))

  /** List the server's resources. Lazily connects first. */
  def listResources()(using Async): Either[String, List[McpResourceInfo]] =
    connect().flatMap(_ => rpc("resources/list", Json.Obj(Nil)).map(McpResourceInfo.decodeList))

  /** Read a resource by URI. Lazily connects first. */
  def readResource(uri: String)(using Async): Either[String, McpReadResult] =
    val params = Json.Obj(List("uri" -> Json.Str(uri)))
    connect().flatMap(_ => rpc("resources/read", params).map(McpReadResult.decode))

  /** Kill the transport. Best-effort; any subsequent operation fails cleanly. */
  def close(): Unit =
    if transport != null then transport.nn.kill()

  // -- handshake --------------------------------------------------------------

  private def handshake()(using Async): Either[String, Unit] =
    val params = Json.Obj(
      List(
        "protocolVersion" -> Json.Str(ProtocolVersion),
        "capabilities" -> Json.Obj(Nil),
        "clientInfo" -> Json.Obj(List("name" -> Json.Str(ClientName), "version" -> Json.Str(ClientVersion)))
      )
    )
    rpc("initialize", params) match
      case Left(err) => Left(s"MCP initialize failed for '$name': $err")
      case Right(result) =>
        recordServerInfo(result)
        // Per spec, tell the server we are ready. It is fire-and-forget: a server
        // that never reads it is fine, and there is no response to await.
        val _ = ensureTransport().writeLine(notificationLine("notifications/initialized", Json.Obj(Nil)))
        Right(())

  private def recordServerInfo(result: Json): Unit =
    serverProtocolVersion = JsonView.str(result, "protocolVersion")
    serverCapabilities = JsonView.field(result, "capabilities")
    serverInfoValue = JsonView.field(result, "serverInfo")

  // -- request/response correlation -------------------------------------------

  /** Issue one JSON-RPC request and await its correlated response, mapping a
    * JSON-RPC `error` to `Left`. Registers a promise under a fresh id, writes the
    * request, arms a timeout, and awaits; the response (or exit, or timeout)
    * resolves the promise. */
  private def rpc(method: String, params: Json)(using Async): Either[String, Json] =
    val id = nextId()
    val promise = Future.Promise[Either[String, Json]]()
    pending(id) = promise
    if !ensureTransport().writeLine(requestLine(id, method, params)) then
      pending.remove(id)
      Left(s"MCP server '$name' is not accepting input (transport closed)")
    else
      // The timeout is the backstop against a server that accepts a request but
      // never answers; without it the awaiting fiber would suspend forever.
      val timer = timers.setTimeout(requestTimeoutMs):
        resolve(id, Left(s"MCP request '$method' to '$name' timed out after ${requestTimeoutMs.toInt}ms"))
      try promise.asFuture.await
      finally timers.clearTimeout(timer)

  /** Complete the promise registered under `id`, exactly once. Removing it from
    * the table first means a later resolver (a late response after a timeout, a
    * duplicated id) is a no-op — the guard the double-completion safety rests on.
    * Single-threaded JS makes the check-then-remove race-free. */
  private def resolve(id: Long, result: Either[String, Json]): Unit =
    pending.remove(id).foreach(_.complete(Success(result)))

  /** The transport callback for every inbound line. Parses it as JSON and routes
    * it: a response completes its pending request; a server→client request gets a
    * method-not-found reply; a notification is dropped. Unparseable lines (server
    * log noise leaking onto stdout) are ignored. */
  private def onLine(line: String): Unit =
    Json.parse(line) match
      case Left(_)     => ()
      case Right(json) => route(json)

  private def route(json: Json): Unit = json match
    case o: Json.Obj =>
      o.get("method") match
        // `method` present ⇒ inbound request or notification (server → client).
        case Some(_) =>
          o.get("id") match
            case Some(reqId) => replyMethodNotFound(reqId) // a request we do not support
            case None        => ()                         // a notification we drop
        // No `method` ⇒ a response to one of our requests.
        case None =>
          idOf(o).foreach: id =>
            o.get("error") match
              case Some(err) => resolve(id, Left(rpcErrorMessage(err)))
              case None      => resolve(id, Right(o.get("result").getOrElse(Json.Null)))
    case _ => ()

  /** The transport exit callback: the server is gone, so no pending request can
    * ever be answered. Fail them all with one clear message and reset connection
    * state — including DROPPING the dead transport — so the next operation spawns
    * a fresh child (via the factory) and re-runs the handshake, mirroring
    * `ScalaRepl`'s respawn-on-death. Leaving the dead handle in place would make
    * `ensureTransport` keep handing it back, and every later call would fail with
    * "transport closed" forever. */
  private def onExit(code: Int): Unit =
    connected = false
    connectGate = null
    transport = null
    val message = s"MCP server '$name' exited (code $code)"
    val outstanding = pending.values.toList
    pending.clear()
    outstanding.foreach(_.complete(Success(Left(message))))

  private def replyMethodNotFound(reqId: Json): Unit =
    val reply = Json.Obj(
      List(
        "jsonrpc" -> Json.Str(JsonRpcVersion),
        "id" -> reqId,
        "error" -> Json.Obj(List("code" -> Json.num(MethodNotFound), "message" -> Json.Str("method not found")))
      )
    )
    val _ = ensureTransport().writeLine(reply.render)

  // -- helpers ----------------------------------------------------------------

  private def ensureTransport(): McpTransport =
    if transport == null then transport = makeTransport(onLine, onExit)
    transport.nn

  private def nextId(): Long =
    idCounter += 1
    idCounter

  /** A throwable's message, or a stand-in when it carries none (getMessage is
    * nullable under `-Yexplicit-nulls`). */
  private def describe(e: Throwable): String =
    val m = e.getMessage
    if m != null then m else "unknown error"

  /** Our request ids are always numbers; ignore a non-numeric id (which would be
    * a server bug — it can only echo an id we sent). */
  private def idOf(o: Json.Obj): Option[Long] =
    o.get("id").collect { case Json.Num(n) => n.toLong }

  private def requestLine(id: Long, method: String, params: Json): String =
    Json.Obj(
      List(
        "jsonrpc" -> Json.Str(JsonRpcVersion),
        "id" -> Json.num(id),
        "method" -> Json.Str(method),
        "params" -> params
      )
    ).render

  private def notificationLine(method: String, params: Json): String =
    Json.Obj(List("jsonrpc" -> Json.Str(JsonRpcVersion), "method" -> Json.Str(method), "params" -> params)).render

  /** Render a JSON-RPC `error` object as a human message: `message (code N)`. */
  private def rpcErrorMessage(err: Json): String = err match
    case o: Json.Obj =>
      val msg = JsonView.str(o, "message").getOrElse("unknown error")
      o.get("code") match
        case Some(Json.Num(code)) => s"$msg (code ${code.toBigInt})"
        case _                    => msg
    case other => other.render

object McpClient:
  /** The MCP protocol revision auk advertises (pinned from spec 2025-06-18). */
  val ProtocolVersion: String = "2025-06-18"

  /** Identity auk presents to servers in `clientInfo`. */
  val ClientName: String = "auk"
  val ClientVersion: String = "0.1.0"

  /** Default per-request timeout, matching the library's shell/eval default
    * (`Sh.DefaultTimeoutMs`, 120s) — the same clock the model already reasons
    * about. A hung server thus frees the awaiting fiber (and its spawned child)
    * on that familiar budget rather than a five-minute one. It is also the anchor
    * Phase 3's worker-side `spawnSync` timeout sits just ABOVE, so the host
    * resolves a clean "timed out" `Left` the helper relays instead of the worker
    * killing the helper mid-flight. Overridable per client via the constructor. */
  val DefaultRequestTimeoutMs: Double = 120_000

  private val JsonRpcVersion: String = "2.0"
  private val MethodNotFound: Int = -32601
