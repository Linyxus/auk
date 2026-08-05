package auk.workflow

/** One item in a sub-agent's transcript, in occurrence order.
  *
  *   - [[Said]] is a run of assistant prose. It streams: consecutive deltas are
  *     accumulated into the open `Said` (see [[Transcript.update]]).
  *   - [[Thought]] is a run of reasoning text, streamed the same way.
  *   - [[ToolCall]] is a tool invocation: its `input` arguments are known when the
  *     call starts; `output` is `None` until the tool returns.
  *   - [[Received]] is a message another agent sent *to* this one, recorded where
  *     it entered the conversation. It is atomic — it never streams or merges.
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

  /** A tool invocation; `output` is `None` until the tool returns.
    *
    * `progress` is the latest report an OPEN call made about itself, held
    * uninterpreted for a view to read ([[EvalDisplay]] is what reads it), and
    * `progressAtMs` is when the fold took that report in — the anchor a viewer
    * extrapolates a sampled counter from. Each report replaces the last, so a key
    * absent from the newest one is absent rather than stale, and both fields are
    * cleared when the call returns: a finished call is rendered from its own
    * frozen facts and has no clock left to advance.
    *
    * Neither field survives [[Transcript.toEvents]] — see [[TranscriptEvent.ToolProgressed]]
    * for why progress is live-only. */
  final case class ToolCall(
      callId: String,
      tool: String,
      input: String,
      output: Option[String],
      isError: Boolean,
      progress: Map[String, String] = Map.empty,
      progressAtMs: Option[Long] = None
  ) extends TranscriptItem

  /** A message delivered into this agent's conversation by `from`. Atomic: it
    * arrives whole, never accumulates, and never merges with a neighbouring run —
    * a prose delta landing after one opens a fresh [[Said]]. */
  final case class Received(from: String, text: String) extends TranscriptItem

/** The streamed transcript of a single sub-agent (keyed by its node id within a
  * run). Folded from [[TranscriptEvent]]s exactly like [[Forest]] is folded from
  * [[OrchestrationEvent]]s, so the host and the web UI share one definition. */
final case class Transcript(items: Vector[TranscriptItem] = Vector.empty):
  /** Fold one event in. `nowMs` is the consumer's clock at the moment the event
    * arrived, which only a progress report uses: what it means to a viewer
    * depends on when it landed, and that is knowable only here — the reading side
    * stamps it, exactly as the TUI's own `ChatState.progressToolRun` does. It is
    * deliberately not on the wire: an anchor has to be in the reader's clock
    * domain, and a host's milliseconds mean nothing in a browser's.
    *
    * A consumer with no clock (replay, tests) passes none and simply gets no
    * anchor — the reported figure is then displayed as reported, which is the
    * honest reading when nothing knows how old it is. */
  def update(ev: TranscriptEvent, nowMs: Option[Long] = None): Transcript =
    import TranscriptEvent.*
    ev match
      case Said(_, _, text)    => appendProse(text)
      case Thought(_, _, text) => appendThought(text)
      case ToolCalled(_, _, callId, tool, input) =>
        copy(items = items :+ TranscriptItem.ToolCall(callId, tool, input, None, isError = false))
      case ToolProgressed(_, _, callId, metadata) =>
        copy(items = mapOpenCall(callId)(_.copy(progress = metadata, progressAtMs = nowMs)))
      // Returning freezes the row: the output and error flag are what it says
      // from now on, and the live progress goes with the clock it belonged to.
      case ToolReturned(_, _, callId, output, isError) =>
        copy(items = mapOpenCall(callId): t =>
          t.copy(output = Some(output), isError = isError, progress = Map.empty, progressAtMs = None))
      // Atomic: appended as its own item, closing any open prose/thinking run —
      // the next delta of either starts a new one (see `appendProse`).
      case Received(_, _, from, text) =>
        copy(items = items :+ TranscriptItem.Received(from, text))

  /** Rewrite the FIRST still-open call bearing `callId`, leaving every other item
    * alone — the shape shared by every event that addresses a running call. Ids
    * are only unique in practice, so "first open one" is what makes a repeat id
    * land on the call that is still waiting rather than on a settled one. */
  private def mapOpenCall(callId: String)(
      f: TranscriptItem.ToolCall => TranscriptItem.ToolCall
  ): Vector[TranscriptItem] =
    var done = false
    items.map:
      case t: TranscriptItem.ToolCall if !done && t.output.isEmpty && t.callId == callId =>
        done = true
        f(t)
      case other => other

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
    * — the inverse of [[update]] (`items.foldLeft(empty)(_.update(_))` round-trips)
    * for everything durable. Each streamed run collapses to a single
    * fully-accumulated event (so replay is linear, not a re-stream of every delta).
    * Used by the host to replay a sub-agent's already-streamed transcript to a
    * browser that connects mid-run, since [[WireMessage.Snapshot]] carries only
    * forests.
    *
    * An open call's live progress is the one thing NOT replayed, which is what
    * keeps [[TranscriptEvent.ToolProgressed]] wire-only in the strong sense:
    * progress reaches a viewer as a live report or not at all, and can never be
    * laundered through stored state into a file. The late joiner sees the row
    * without its stage for as long as it takes the next report to arrive — about
    * a second — and a report that stale was worth little anyway. */
  def toEvents(runId: String, nodeId: String): Vector[TranscriptEvent] =
    items.flatMap:
      case s: TranscriptItem.Said    => Vector(TranscriptEvent.Said(runId, nodeId, s.text))
      case t: TranscriptItem.Thought => Vector(TranscriptEvent.Thought(runId, nodeId, t.text))
      case c: TranscriptItem.ToolCall =>
        TranscriptEvent.ToolCalled(runId, nodeId, c.callId, c.tool, c.input) +:
          c.output.map(o => TranscriptEvent.ToolReturned(runId, nodeId, c.callId, o, c.isError)).toVector
      case TranscriptItem.Received(from, text) =>
        Vector(TranscriptEvent.Received(runId, nodeId, from, text))

object Transcript:
  val empty: Transcript = Transcript()
