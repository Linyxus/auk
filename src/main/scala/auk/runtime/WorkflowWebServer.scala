package auk.runtime

import scala.collection.mutable.LinkedHashMap
import scala.scalajs.js

import auk.platform.js.{WebServer, WebUiAssets}
import auk.workflow.{Forest, Transcript, WireCodec, WireMessage}

/** The host side of the live workflow dashboard: lazily starts a [[WebServer]] on
  * a spare port when the first workflow event arrives, serves the prebuilt webui
  * bundle, and streams the forest + each sub-agent's transcript to connected
  * browsers over SSE.
  *
  * A browser that connects mid-run gets a forest [[WireMessage.Snapshot]] plus
  * every stored [[Transcript]] replayed as activity, so it converges to the live
  * state. All folds use the shared `workflow-protocol` types, so the host and the
  * browser can never drift.
  *
  * Every method is plain (no Gears `Async`): the server runs on the JS event loop
  * and all state is folded/broadcast synchronously on the single JS thread, so the
  * mutable maps and client set need no locking. */
final class WorkflowWebServer(
    onStarted: String => Unit,
    onError: String => Unit,
    heartbeatMs: Int = WorkflowWebServer.HeartbeatMs,
    onControl: (String, String) => Unit = (_, _) => ()
):
  private enum State:
    case Disabled, Starting, Failed
    case Running(port: Int)

  private var state: State = State.Disabled
  private var handle: WebServer.Handle | Null = null
  private val forests = LinkedHashMap.empty[String, Forest]
  private val transcripts = LinkedHashMap.empty[String, LinkedHashMap[String, Transcript]]
  private val clients = scala.collection.mutable.Set.empty[WebServer.Sse]

  /** Start the server on the first call (idempotent). Resolves the bundle dir; on
    * failure surfaces the reason once via `onError` and stays disabled. */
  def ensureStarted(): Unit = state match
    case State.Disabled =>
      WebUiAssets.resolve() match
        case Left(msg) =>
          state = State.Failed
          onError(msg)
        case Right(dir) =>
          state = State.Starting // guard re-entry before the async listen callback fires
          handle = WebServer.serve(
            dir,
            "/events",
            port => { state = State.Running(port); onStarted(s"http://localhost:$port") },
            heartbeatMs,
            controlPath = "/control",
            onControl = handleControl
          )(onClient)
    case _ => ()

  /** Parse a `{ "action": "pause"|"resume", "runId": "…" }` control body from the
    * browser and forward it; malformed bodies are ignored. */
  private def handleControl(body: String): Unit =
    try
      val d = js.JSON.parse(body)
      val action = if js.typeOf(d.action) == "string" then d.action.asInstanceOf[String] else ""
      val runId = if js.typeOf(d.runId) == "string" then d.runId.asInstanceOf[String] else ""
      if action.nonEmpty && runId.nonEmpty then onControl(action, runId)
    catch case _: Throwable => ()

  /** Fold an event into the dashboard state and broadcast it to every client.
    * Folding happens whether or not the server is up yet, so the snapshot is ready
    * when a client connects. A `Snapshot` is connect-only and must never be passed
    * here. */
  def publish(m: WireMessage): Unit =
    fold(m)
    val frame = WireCodec.encode(m)
    clients.foreach(_.send(frame))

  def close(): Unit =
    if handle != null then handle.nn.close()

  private def onClient(sse: WebServer.Sse): Unit =
    // Synchronous: send the forest snapshot, replay stored transcripts, then
    // register — no suspension between, so no concurrently-published event is missed.
    sse.send(WireCodec.encode(WireMessage.Snapshot(forests.toList)))
    for
      (runId, perNode) <- transcripts
      (nodeId, t) <- perNode
      ev <- t.toEvents(runId, nodeId)
    do sse.send(WireCodec.encode(WireMessage.Activity(ev)))
    clients += sse
    sse.onClose(() => { clients -= sse; () })

  private def fold(m: WireMessage): Unit = m match
    case WireMessage.Event(ev) =>
      forests(ev.runId) = forests.getOrElse(ev.runId, Forest.empty).update(ev)
    case WireMessage.Activity(ev) =>
      val perNode = transcripts.getOrElseUpdate(ev.runId, LinkedHashMap.empty)
      perNode(ev.nodeId) = perNode.getOrElse(ev.nodeId, Transcript.empty).update(ev)
    case _: WireMessage.Snapshot => ()

object WorkflowWebServer:
  /** Default SSE keep-alive interval (ms). Comfortably under the idle-connection
    * timeouts of common OSes/proxies, so the browser's `EventSource` stays put. */
  val HeartbeatMs: Int = 15000
