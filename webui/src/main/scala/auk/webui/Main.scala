package auk.webui

import scala.scalajs.js
import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Browser entry point: hold an [[AppState]] in a `Var`, wire an SSE
  * `EventSource` into it (decoding each frame via [[SseClient.step]]), and mount
  * the [[WorkflowRender]] view. The scenario is taken from the `?scenario=` query
  * param so the mock server can drive different demos, and what is selected is
  * mirrored in the `#fragment` (see [[UrlHash]]) so a reload — or a link someone
  * else opens — lands on the same run or loop.
  *
  * The two on-demand payloads are fetched from here rather than from the render
  * layer, and the "have we asked already?" question is answered here too: the
  * render asks whenever a pane that wants one is shown, and this is the single
  * place that turns a repeated ask into at most one request. */
@main def main(): Unit =
  // Nothing is known at boot (the snapshots land after mount), so the fragment can
  // only be an intent; AppState adopts it once its target turns up.
  val state = Var(AppState(pending = UrlHash.parse(dom.window.location.hash)))

  val es = new dom.EventSource(s"/events?scenario=${scenarioFromQuery()}")
  es.onopen = _ => state.update(_.withConn(ConnStatus.Open))
  es.onmessage = (e: dom.MessageEvent) => state.update(s => SseClient.step(s, e.data.asInstanceOf[String]))
  es.onerror = _ => state.update(_.withConn(ConnStatus.Error("connection lost")))

  val view = state.signal.map(WorkflowView.from)
  val actions = Actions(
    selectRun = rid => state.update(_.select(Target.Run(rid))),
    selectLoop = id => state.update(_.select(Target.Loop(id))),
    selectNode = nid => state.update(_.selectNode(nid)),
    selectGeneration = gen => state.update(_.selectGeneration(gen)),
    selectPane = pane => state.update(_.selectPane(pane)),
    selectAttempt = n => state.update(_.selectAttempt(n)),
    // the code button toggles: a second click (while the code is showing) closes it
    selectCode = () => state.update(s => if s.focus == Focus.Code then s.clearFocus else s.selectCode),
    close = () => state.update(_.clearFocus),
    needTranscript = (loopId, label) =>
      if state.now().needsTranscript(loopId, label) then
        state.update(_.transcriptRequested(loopId, label))
        LoopApi.transcript(
          loopId,
          label,
          text => state.update(_.transcriptLoaded(loopId, label, text)),
          msg => state.update(_.transcriptFailed(loopId, label, msg))
        )
    ,
    needDiff = (loopId, gen, attempt) =>
      if state.now().needsDiff(loopId, gen, attempt) then
        state.update(_.diffRequested(loopId, gen, attempt))
        LoopApi.diff(
          loopId,
          gen,
          attempt,
          text => state.update(_.diffLoaded(loopId, gen, attempt, text)),
          msg => state.update(_.diffFailed(loopId, gen, attempt, msg))
        )
  )

  // Mirror what the state wants the URL to name (see AppState.urlTarget) into the
  // fragment. replaceState (not `location.hash = …`) keeps it out of the history and
  // fires no `hashchange`, so this never loops back through the listener below.
  state.signal.map(_.urlTarget).distinct.foreach { sel =>
    val loc = dom.window.location
    dom.window.history.replaceState(null, "", loc.pathname + loc.search + UrlHash.format(sel))
  }(using unsafeWindowOwner)
  // …and follow the fragment when something else changes it (a pasted URL, back).
  dom.window.addEventListener(
    "hashchange",
    (_: dom.Event) => UrlHash.parse(dom.window.location.hash).foreach(t => state.update(_.desire(t)))
  )

  renderOnDomContentLoaded(dom.document.getElementById("app"), WorkflowRender.app(view, actions))

/** Read `scenario` from `window.location.search`, defaulting to `fanout`. */
private def scenarioFromQuery(): String =
  val raw = dom.window.location.search
  val q = if raw.startsWith("?") then raw.substring(1) else raw
  q.split("&").iterator
    .map(_.split("=", 2))
    .collectFirst { case Array("scenario", v) if v.nonEmpty => js.URIUtils.decodeURIComponent(v) }
    .getOrElse("fanout")
