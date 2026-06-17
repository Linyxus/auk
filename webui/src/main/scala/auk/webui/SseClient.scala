package auk.webui

import auk.workflow.WireCodec

/** The pure core of the SSE client: decode one frame's data and fold it into the
  * state, swallowing malformed or empty lines (so a bad frame never breaks the
  * stream). The effectful `EventSource` wiring lives in [[Main]]. */
object SseClient:
  def step(state: AppState, line: String): AppState =
    if line.isEmpty then state
    else WireCodec.decode(line).map(state.reduce).getOrElse(state)
