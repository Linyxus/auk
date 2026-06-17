package auk.workflow

/** One item in a sub-agent's transcript, in occurrence order.
  *
  *   - [[Said]] is a run of assistant prose. It streams: consecutive deltas are
  *     concatenated into the open `Said` (see [[Transcript.update]]).
  *   - [[Thought]] is a run of reasoning text, streamed the same way.
  *   - [[ToolCall]] is a tool invocation: its `input` arguments are known when the
  *     call starts; `output` is `None` until the tool returns.
  */
enum TranscriptItem:
  case Said(text: String)
  case Thought(text: String)
  case ToolCall(callId: String, tool: String, input: String, output: Option[String], isError: Boolean)

/** The streamed transcript of a single sub-agent (keyed by its node id within a
  * run). Folded from [[TranscriptEvent]]s exactly like [[Forest]] is folded from
  * [[OrchestrationEvent]]s, so the host and the web UI share one definition. */
final case class Transcript(items: Vector[TranscriptItem] = Vector.empty):
  def update(ev: TranscriptEvent): Transcript =
    import TranscriptEvent.*
    ev match
      case Said(_, _, text)    => appendProse(text)
      case Thought(_, _, text) => appendThought(text)
      case ToolCalled(_, _, callId, tool, input) =>
        copy(items = items :+ TranscriptItem.ToolCall(callId, tool, input, None, isError = false))
      case ToolReturned(_, _, callId, output, isError) =>
        var filled = false
        val next = items.map:
          case t @ TranscriptItem.ToolCall(id, _, _, None, _) if !filled && id == callId =>
            filled = true
            t.copy(output = Some(output), isError = isError)
          case other => other
        copy(items = next)

  /** Append a prose delta, growing the open `Said` if the last item is one. */
  private def appendProse(text: String): Transcript =
    items.lastOption match
      case Some(TranscriptItem.Said(prev)) => copy(items = items.init :+ TranscriptItem.Said(prev + text))
      case _                               => copy(items = items :+ TranscriptItem.Said(text))

  /** Append a reasoning delta, growing the open `Thought` if the last item is one. */
  private def appendThought(text: String): Transcript =
    items.lastOption match
      case Some(TranscriptItem.Thought(prev)) => copy(items = items.init :+ TranscriptItem.Thought(prev + text))
      case _                                  => copy(items = items :+ TranscriptItem.Thought(text))

  /** Re-expand this transcript into the [[TranscriptEvent]]s that would rebuild it
    * — the inverse of [[update]] (`items.foldLeft(empty)(_.update(_))` round-trips).
    * Used by the host to replay a sub-agent's already-streamed transcript to a
    * browser that connects mid-run, since [[WireMessage.Snapshot]] carries only
    * forests. */
  def toEvents(runId: String, nodeId: String): Vector[TranscriptEvent] =
    items.flatMap:
      case TranscriptItem.Said(text)    => Vector(TranscriptEvent.Said(runId, nodeId, text))
      case TranscriptItem.Thought(text) => Vector(TranscriptEvent.Thought(runId, nodeId, text))
      case TranscriptItem.ToolCall(callId, tool, input, output, isError) =>
        TranscriptEvent.ToolCalled(runId, nodeId, callId, tool, input) +:
          output.map(o => TranscriptEvent.ToolReturned(runId, nodeId, callId, o, isError)).toVector

object Transcript:
  val empty: Transcript = Transcript()
