package auk.webui

import scala.scalajs.js
import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Browser entry point: hold an [[AppState]] in a `Var`, wire an SSE
  * `EventSource` into it (decoding each frame via [[SseClient.step]]), and mount
  * the [[WorkflowRender]] view. The scenario is taken from the `?scenario=` query
  * param so the mock server can drive different demos, and the selected run is
  * mirrored in the `#fragment` (see [[UrlHash]]) so a reload — or a link someone
  * else opens — lands on the same run. */
@main def main(): Unit =
  // No run is known at boot (the Snapshot lands after mount), so the fragment can
  // only be an intent; AppState adopts it once that run turns up.
  val state = Var(AppState(pendingRun = UrlHash.parse(dom.window.location.hash)))

  val es = new dom.EventSource(s"/events?scenario=${scenarioFromQuery()}")
  es.onopen = _ => state.update(_.withConn(ConnStatus.Open))
  es.onmessage = (e: dom.MessageEvent) => state.update(s => SseClient.step(s, e.data.asInstanceOf[String]))
  es.onerror = _ => state.update(_.withConn(ConnStatus.Error("connection lost")))

  val view = state.signal.map(WorkflowView.from)
  val onSelectRun: String => Unit = rid => state.update(_.selectRun(rid))
  val onSelectNode: String => Unit = nid => state.update(_.selectNode(nid))
  // the code button toggles: a second click (while the code is showing) closes it
  val onSelectCode: () => Unit = () => state.update(s => if s.focus == Focus.Code then s.clearFocus else s.selectCode)
  val onClose: () => Unit = () => state.update(_.clearFocus)

  // Mirror the run the state wants the URL to name (see AppState.urlRun) into the
  // fragment. replaceState (not `location.hash = …`) keeps it out of the history and
  // fires no `hashchange`, so this never loops back through the listener below.
  state.signal.map(_.urlRun).distinct.foreach { sel =>
    val loc = dom.window.location
    dom.window.history.replaceState(null, "", loc.pathname + loc.search + UrlHash.format(sel))
  }(using unsafeWindowOwner)
  // …and follow the fragment when something else changes it (a pasted URL, back).
  dom.window.addEventListener(
    "hashchange",
    (_: dom.Event) => UrlHash.parse(dom.window.location.hash).foreach(rid => state.update(_.desireRun(rid)))
  )

  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    WorkflowRender.app(view, onSelectRun, onSelectNode, onSelectCode, onClose)
  )

/** Read `scenario` from `window.location.search`, defaulting to `fanout`. */
private def scenarioFromQuery(): String =
  val raw = dom.window.location.search
  val q = if raw.startsWith("?") then raw.substring(1) else raw
  q.split("&").iterator
    .map(_.split("=", 2))
    .collectFirst { case Array("scenario", v) if v.nonEmpty => js.URIUtils.decodeURIComponent(v) }
    .getOrElse("fanout")
