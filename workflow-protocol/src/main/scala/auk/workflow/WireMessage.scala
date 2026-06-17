package auk.workflow

/** A single host->browser message over the SSE channel.
  *
  *   - [[Snapshot]] is sent once when a client connects: the current folded state
  *     of every active run, so a late joiner sees the world immediately without
  *     replaying the whole event history.
  *   - [[Event]] carries one [[OrchestrationEvent]] delta (forest structure) thereafter.
  *   - [[Activity]] carries one [[TranscriptEvent]] delta (a sub-agent's transcript).
  *
  * See [[WireCodec]] for the JSON encoding.
  */
enum WireMessage:
  case Snapshot(forests: List[(String, Forest)])
  case Event(event: OrchestrationEvent)
  case Activity(event: TranscriptEvent)
