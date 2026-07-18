package auk.runtime.mcp

import scala.collection.mutable
import scala.scalajs.js.timers

import auk.llm.tools.Json

/** How a scripted fake server answers a given JSON-RPC method. */
enum Reply:
  case Result(json: Json)
  case Error(code: Int, message: String)
  case NoReply // a server that accepts the request but never responds

/** A scripted in-memory [[McpTransport]] shared by the MCP test suites.
  *
  * It answers requests from a `method → Reply` script asynchronously (via
  * `setTimeout(0)`, mirroring a real child process delivering on the event loop),
  * so a client's suspend/resume path is exercised for real without spawning a
  * process. It records every line written (for asserting the wire), and lets a
  * test deliver an arbitrary inbound line ([[deliver]]) or simulate the child
  * exiting ([[exit]]). */
final class ScriptedTransport(
    script: Map[String, Reply],
    onWrite: Json => Unit,
    onLine: String => Unit,
    onExit: Int => Unit
) extends McpTransport:
  val written = mutable.ListBuffer.empty[Json]
  var killed = false
  private var live = true

  def writeLine(line: String): Boolean =
    if !live then false
    else
      Json.parse(line) match
        case Right(req) =>
          written += req
          onWrite(req)
          replyFor(req).foreach(resp => timers.setTimeout(0)(if live then onLine(resp.render)))
        case Left(_) => ()
      true

  def kill(): Unit = { killed = true; live = false }
  def alive: Boolean = live

  /** Deliver an arbitrary line as if the server sent it (server→client requests). */
  def deliver(json: Json): Unit = onLine(json.render)

  /** Simulate the child process exiting. */
  def exit(code: Int): Unit = { live = false; onExit(code) }

  private def replyFor(req: Json): Option[Json] =
    for
      method <- JsonView.str(req, "method")
      id <- JsonView.field(req, "id").collect { case Json.Num(n) => n.toLong }
      reply <- script.get(method)
      body <- reply match
        case Reply.Result(r)        => Some(ScriptedTransport.responseLine(id, "result", r))
        case Reply.Error(code, msg) => Some(ScriptedTransport.responseLine(id, "error", ScriptedTransport.errorObj(code, msg)))
        case Reply.NoReply          => None
    yield body

object ScriptedTransport:
  def responseLine(id: Long, key: String, value: Json): Json =
    Json.Obj(List("jsonrpc" -> Json.Str("2.0"), "id" -> Json.num(id), key -> value))

  def errorObj(code: Int, message: String): Json =
    Json.Obj(List("code" -> Json.num(code), "message" -> Json.Str(message)))
