package auk.workflow

/** One item in a sub-agent's transcript, in occurrence order.
  *
  *   - [[Said]] is a run of assistant prose. It streams: consecutive deltas are
  *     accumulated into the open `Said` (see [[Transcript.update]]).
  *   - [[Thought]] is a run of reasoning text, streamed the same way.
  *   - [[ToolCall]] is a tool invocation: its `input` arguments are known when the
  *     call starts; `output` is `None` until the tool returns.
  *
  * Streamed prose/reasoning is stored as the **append-only chunks** that arrived,
  * never re-concatenated on each delta. Folding N deltas is then O(N) total work,
  * not O(N²): a naive `prev + delta` per delta copies the whole accumulated string
  * every time, which is quadratic in the run's length (the dominant per-event CPU
  * cost flagged by the `webui` performance audit, paid on both the host and the
  * browser). The full text is materialized only on demand — [[Said.text]] /
  * [[Thought.text]], rendering, and [[Transcript.toEvents]] replay.
  *
  * Two runs are **equal when their text is equal**, regardless of how the chunks
  * were split, so a multi-chunk streamed run equals the single-chunk run it
  * round-trips through [[Transcript.toEvents]]. [[Said.apply]]/[[Thought.apply]]
  * build a single-chunk run and the extractors yield the full text, so callers and
  * tests treat these as if they still held a plain `String`. */
sealed trait TranscriptItem

object TranscriptItem:
  /** A run of assistant prose, kept as the chunks that streamed in. */
  final class Said(val chunks: Vector[String]) extends TranscriptItem:
    /** The full prose, materialized from the chunks (O(length); call sparingly). */
    def text: String = chunks.mkString
    /** Grow the run by one delta in O(1) amortized — no re-concatenation. */
    def appended(delta: String): Said = new Said(chunks :+ delta)
    override def equals(that: Any): Boolean = that match
      case s: Said => s.text == text
      case _       => false
    override def hashCode: Int = text.hashCode
    override def toString: String = s"Said($text)"

  object Said:
    /** A single-chunk run — the common constructor for callers, replay, and tests. */
    def apply(text: String): Said = new Said(Vector(text))
    def unapply(s: Said): Some[String] = Some(s.text)

  /** A run of reasoning text, kept as the chunks that streamed in. */
  final class Thought(val chunks: Vector[String]) extends TranscriptItem:
    def text: String = chunks.mkString
    def appended(delta: String): Thought = new Thought(chunks :+ delta)
    override def equals(that: Any): Boolean = that match
      case t: Thought => t.text == text
      case _          => false
    override def hashCode: Int = text.hashCode
    override def toString: String = s"Thought($text)"

  object Thought:
    def apply(text: String): Thought = new Thought(Vector(text))
    def unapply(t: Thought): Some[String] = Some(t.text)

  /** A tool invocation; `output` is `None` until the tool returns. */
  final case class ToolCall(callId: String, tool: String, input: String, output: Option[String], isError: Boolean)
      extends TranscriptItem

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

  /** Append a prose delta as a new chunk, growing the open `Said` if the last item
    * is one. O(1) amortized — the accumulated text is never re-concatenated. */
  private def appendProse(delta: String): Transcript =
    items.lastOption match
      case Some(s: TranscriptItem.Said) => copy(items = items.updated(items.size - 1, s.appended(delta)))
      case _                            => copy(items = items :+ TranscriptItem.Said(delta))

  /** Append a reasoning delta, growing the open `Thought` if the last item is one. */
  private def appendThought(delta: String): Transcript =
    items.lastOption match
      case Some(t: TranscriptItem.Thought) => copy(items = items.updated(items.size - 1, t.appended(delta)))
      case _                               => copy(items = items :+ TranscriptItem.Thought(delta))

  /** Re-expand this transcript into the [[TranscriptEvent]]s that would rebuild it
    * — the inverse of [[update]] (`items.foldLeft(empty)(_.update(_))` round-trips).
    * Each streamed run collapses to a single fully-accumulated event (so replay is
    * linear, not a re-stream of every delta). Used by the host to replay a
    * sub-agent's already-streamed transcript to a browser that connects mid-run,
    * since [[WireMessage.Snapshot]] carries only forests. */
  def toEvents(runId: String, nodeId: String): Vector[TranscriptEvent] =
    items.flatMap:
      case s: TranscriptItem.Said    => Vector(TranscriptEvent.Said(runId, nodeId, s.text))
      case t: TranscriptItem.Thought => Vector(TranscriptEvent.Thought(runId, nodeId, t.text))
      case TranscriptItem.ToolCall(callId, tool, input, output, isError) =>
        TranscriptEvent.ToolCalled(runId, nodeId, callId, tool, input) +:
          output.map(o => TranscriptEvent.ToolReturned(runId, nodeId, callId, o, isError)).toVector

object Transcript:
  val empty: Transcript = Transcript()
