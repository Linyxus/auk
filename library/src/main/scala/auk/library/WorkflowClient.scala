package auk.library

import scala.scalajs.js
import scala.concurrent.{Future, Promise}

/** The worker side of the workflow bridge: a Unix-domain-socket client to the
  * host's `WorkflowBridge`. The host runs the actual sub-agents (it owns the LLM
  * endpoints, tools, and UI event stream); this client only marshals structure
  * and calls across the socket and correlates each `agent_call` with its reply.
  *
  * Wire format: one JSON object per line, `t` tagging the kind. Every
  * worker -> host message carries `run` (the workflow run id) so the host can
  * attribute it — runs are background and concurrent, no longer one-per-socket-
  * accept.
  *   worker -> host: `hello` (announce the run id first), `group` (declare),
  *                   `node` (declare + deps), `call` (run an agent), `log`, `done`.
  *   host -> worker: `result` (`ok` + `value` | `error`), keyed by node `id`.
  *
  * The socket path comes from `AUK_WF_SOCK` (injected into the orchestrator
  * worker's env by the host). Writes before the connection completes are buffered
  * by Node, so the synchronous build pass can declare/call freely.
  */
private[library] final class WorkflowClient(sockPath: String, run: String):
  import WorkflowClient.queue

  private def net: js.Dynamic = js.Dynamic.global.require("node:net")

  private val pending = scala.collection.mutable.Map.empty[String, Promise[js.Any]]
  private var buffer = ""
  private var closed = false
  // Set when the host sends a `paused` message. The host now KEEPS the connection
  // open while paused (it serves as the run's control channel), so this flag is the
  // sole signal: the `WorkflowRun` handle reads it to report `Paused`, and `send`
  // stops writing while it is set. The host clears it again by sending `resume`.
  private var paused = false

  /** Whether the host has paused this run. */
  def isPaused: Boolean = paused

  /** Invoked when the host sends a `resume` message: re-run the stored workflow
    * closure. Set by [[Workflow.start]] to its relaunch routine. */
  var onResume: () => Unit = () => ()

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
      msg.t.asInstanceOf[String] match
        case "result" =>
          val id = msg.id.asInstanceOf[String]
          pending.remove(id).foreach: p =>
            if msg.ok.asInstanceOf[Boolean] then p.success(msg.value.asInstanceOf[js.Any])
            else p.failure(new RuntimeException(Option(msg.error.asInstanceOf[String]).getOrElse("agent failed")))
        case "paused" =>
          // The host paused this run; mark it so the handle reports `Paused` and
          // `send` stops writing. The connection stays open as the control channel.
          paused = true
        case "resume" =>
          // The host resumed this run: clear the flag first (so the relaunch's
          // declares/calls write freely), then re-run the stored workflow closure.
          paused = false
          onResume()
        case _ => ()
    catch case _: Throwable => () // ignore malformed lines

  private def failAll(reason: String): Unit =
    if !closed then
      val ps = pending.values.toList
      pending.clear()
      ps.foreach(p => if !p.isCompleted then p.failure(new RuntimeException(reason)))

  // Every message carries the run id so the host can attribute it. Stamped here,
  // centrally, so no builder can forget it.
  private def send(pairs: (String, js.Any)*): Unit =
    if !closed && !paused then
      val withRun = ("run" -> (run: js.Any)) +: pairs
      socket.write(js.JSON.stringify(LibToolInput.jsObj(withRun*)) + "\n")

  /** Announce this run to the host as the very first message, so the host binds
    * this connection to the run id before any node/call arrives (and can resolve
    * the run even if the worker drops with no nodes). */
  def hello(): Unit =
    send("t" -> "hello")

  def declareGroup(id: String, name: String, desc: String, parent: String | Null): Unit =
    send(
      "t" -> "group", "id" -> id, "name" -> name, "desc" -> desc,
      "parent" -> (if parent == null then null.asInstanceOf[js.Any] else parent.asInstanceOf[js.Any]))

  def declareNode(id: String, group: String | Null, deps: List[String]): Unit =
    send(
      "t" -> "node", "id" -> id,
      "group" -> (if group == null then null.asInstanceOf[js.Any] else group.asInstanceOf[js.Any]),
      "deps" -> js.Array(deps.map(_.asInstanceOf[js.Any])*))

  def call(id: String, prompt: String, schema: js.Any): Future[js.Any] =
    val p = Promise[js.Any]()
    pending(id) = p
    send("t" -> "call", "id" -> id, "prompt" -> prompt, "schema" -> schema)
    p.future

  def log(message: String): Unit =
    send("t" -> "log", "msg" -> message)

  /** Report the workflow's settled result to the host (it surfaces it as a system
    * notice and drops the run's panel). `value` is the rendered result on success. */
  def sendDone(ok: Boolean, value: String, error: String): Unit =
    send("t" -> "done", "ok" -> ok.asInstanceOf[js.Any], "value" -> value, "error" -> error)

  def close(): Unit =
    if !closed then
      closed = true
      try socket.end() catch case _: Throwable => ()

private[library] object WorkflowClient:
  given queue: scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  /** Connect using the socket path from `AUK_WF_SOCK`, tagging every message with
    * `run`, or fail clearly. */
  def fromEnv(run: String): WorkflowClient =
    val env = js.Dynamic.global.process.env
    val sock = env.AUK_WF_SOCK
    if sock == null || js.isUndefined(sock) then
      throw new RuntimeException(
        "workflows are unavailable: AUK_WF_SOCK is not set (the host workflow bridge is not connected)")
    new WorkflowClient(sock.asInstanceOf[String], run)
