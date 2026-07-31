package auk.workflow

/** A single host->browser message over the SSE channel.
  *
  *   - [[Snapshot]] is sent once when a client connects: the current folded state
  *     of every active run, so a late joiner sees the world immediately without
  *     replaying the whole event history.
  *   - [[Event]] carries one [[OrchestrationEvent]] delta (forest structure) thereafter.
  *   - [[Activity]] carries one [[TranscriptEvent]] delta (a sub-agent's transcript).
  *   - [[LoopSnapshot]] is the loops' counterpart of [[Snapshot]], sent on connect
  *     directly after it, and [[Loop]] upserts one loop thereafter.
  *
  * Loops send whole values rather than deltas ([[LoopWire]] explains why), and
  * their transcripts reuse [[Activity]] — a loop agent's frames are keyed
  * `runId = loopId`, `nodeId = <label>`, so nothing loop-specific is needed for
  * them.
  *
  * A client relies on the connect order: [[Snapshot]], then [[LoopSnapshot]], then
  * the replayed transcripts, which by then address things the client has heard of.
  *
  * See [[WireCodec]] for the JSON encoding.
  */
enum WireMessage:
  case Snapshot(forests: List[(String, Forest)])
  case Event(event: OrchestrationEvent)
  case Activity(event: TranscriptEvent)
  case LoopSnapshot(loops: List[LoopWire])
  case Loop(loop: LoopWire)
