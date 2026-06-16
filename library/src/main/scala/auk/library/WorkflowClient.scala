package auk.library

import scala.scalajs.js
import scala.concurrent.{Future, Promise}

/** The worker side of the workflow bridge: a Unix-domain-socket client to the
  * host's `WorkflowBridge`. The host runs the actual sub-agents (it owns the LLM
  * endpoints, tools, and UI event stream); this client only marshals structure
  * and calls across the socket and correlates each `agent_call` with its reply.
  *
  * Wire format: one JSON object per line, `t` tagging the kind.
  *   worker -> host: `group` (declare), `node` (declare + deps), `call` (run an
  *                   agent), `log`.
  *   host -> worker: `result` (`ok` + `value` | `error`), keyed by node `id`.
  *
  * The socket path comes from `AUK_WF_SOCK` (injected into the orchestrator
  * worker's env by the host). Writes before the connection completes are buffered
  * by Node, so the synchronous build pass can declare/call freely.
  */
private[library] final class WorkflowClient(sockPath: String):
  import WorkflowClient.queue

  private def net: js.Dynamic = js.Dynamic.global.require("node:net")

  private val pending = scala.collection.mutable.Map.empty[String, Promise[js.Any]]
  private var buffer = ""
  private var closed = false

  private val socket: js.Dynamic = net.connect(sockPath).asInstanceOf[js.Dynamic]
  socket.setEncoding("utf8")
  socket.on("data", ((chunk: js.Any) => onData(chunk.asInstanceOf[String])): js.Function1[js.Any, Unit])
  socket.on("error", ((e: js.Any) => failAll(s"workflow socket error: $e")): js.Function1[js.Any, Unit])
  socket.on("close", ((_: js.Any) => failAll("workflow socket closed")): js.Function1[js.Any, Unit])

  private def onData(chunk: String): Unit =
    buffer += chunk
    var idx = buffer.indexOf("\n")
    while idx >= 0 do
      val line = buffer.substring(0, idx)
      buffer = buffer.substring(idx + 1)
      if line.nonEmpty then handleLine(line)
      idx = buffer.indexOf("\n")

  private def handleLine(line: String): Unit =
    try
      val msg = js.JSON.parse(line)
      if msg.t.asInstanceOf[String] == "result" then
        val id = msg.id.asInstanceOf[String]
        pending.remove(id).foreach: p =>
          if msg.ok.asInstanceOf[Boolean] then p.success(msg.value.asInstanceOf[js.Any])
          else p.failure(new RuntimeException(Option(msg.error.asInstanceOf[String]).getOrElse("agent failed")))
    catch case _: Throwable => () // ignore malformed lines

  private def failAll(reason: String): Unit =
    if !closed then
      val ps = pending.values.toList
      pending.clear()
      ps.foreach(p => if !p.isCompleted then p.failure(new RuntimeException(reason)))

  private def send(msg: js.Any): Unit =
    if !closed then socket.write(js.JSON.stringify(msg) + "\n")

  def declareGroup(id: String, name: String, desc: String, parent: String | Null): Unit =
    send(LibToolInput.jsObj(
      "t" -> "group", "id" -> id, "name" -> name, "desc" -> desc,
      "parent" -> (if parent == null then null.asInstanceOf[js.Any] else parent.asInstanceOf[js.Any])))

  def declareNode(id: String, group: String | Null, deps: List[String]): Unit =
    send(LibToolInput.jsObj(
      "t" -> "node", "id" -> id,
      "group" -> (if group == null then null.asInstanceOf[js.Any] else group.asInstanceOf[js.Any]),
      "deps" -> js.Array(deps.map(_.asInstanceOf[js.Any])*)))

  def call(id: String, prompt: String, schema: js.Any): Future[js.Any] =
    val p = Promise[js.Any]()
    pending(id) = p
    send(LibToolInput.jsObj("t" -> "call", "id" -> id, "prompt" -> prompt, "schema" -> schema))
    p.future

  def log(message: String): Unit =
    send(LibToolInput.jsObj("t" -> "log", "msg" -> message))

  /** Report the workflow's settled result to the host (it surfaces it as the
    * eval_scala tool result). `value` is the rendered result on success. */
  def sendDone(ok: Boolean, value: String, error: String): Unit =
    send(LibToolInput.jsObj("t" -> "done", "ok" -> ok.asInstanceOf[js.Any], "value" -> value, "error" -> error))

  def close(): Unit =
    if !closed then
      closed = true
      try socket.end() catch case _: Throwable => ()

private[library] object WorkflowClient:
  given queue: scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  /** Connect using the socket path from `AUK_WF_SOCK`, or fail clearly. */
  def fromEnv(): WorkflowClient =
    val env = js.Dynamic.global.process.env
    val sock = env.AUK_WF_SOCK
    if sock == null || js.isUndefined(sock) then
      throw new RuntimeException(
        "workflows are unavailable: AUK_WF_SOCK is not set (the host workflow bridge is not connected)")
    new WorkflowClient(sock.asInstanceOf[String])
