package auk.webui

import scala.scalajs.js
import org.scalajs.dom

/** Browser → host control: POST a pause/resume action to the dashboard's
  * `/control` endpoint. Fire-and-forget — the resulting state change streams back
  * over SSE, so there is nothing to await. */
object Control:
  def send(action: String, runId: String): Unit =
    val payload = js.JSON.stringify(js.Dynamic.literal(action = action, runId = runId))
    val init = js.Dynamic.literal(method = "POST", body = payload).asInstanceOf[dom.RequestInit]
    val _ = dom.fetch("/control", init)
