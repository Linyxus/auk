package auk.workflow

/** A live delta in one sub-agent's transcript, emitted by the host as the agent
  * works and folded into a [[Transcript]]. Kept separate from
  * [[OrchestrationEvent]] (which is about forest *structure*) so the [[Forest]]
  * fold stays focused; both travel over the wire as a [[WireMessage]].
  *
  * `runId` + `nodeId` identify which sub-agent's transcript this belongs to. Text
  * events carry deltas (they accumulate); tool events come in pairs sharing a
  * `callId` ([[ToolCalled]] then [[ToolReturned]]).
  */
enum TranscriptEvent:
  /** The run this activity belongs to (the `eval_scala` tool-use id). */
  def runId: String

  /** The sub-agent node this activity belongs to. */
  def nodeId: String

  /** Whether this event exists only for a viewer watching right now, and so must
    * reach live consumers and nothing else. True for [[ToolProgressed]] alone;
    * every other case is durable transcript content. A persistence tee asks this
    * (or, better, encodes through [[WireCodec.encodeDurable]]) before writing. */
  def ephemeral: Boolean = this match
    case _: TranscriptEvent.ToolProgressed => true
    case _                                 => false

  /** A chunk of assistant prose (appended to the open prose run). */
  case Said(runId: String, nodeId: String, text: String)
  /** A chunk of reasoning text (appended to the open thinking run). */
  case Thought(runId: String, nodeId: String, text: String)
  /** A tool call started; `input` is its (complete) argument JSON. */
  case ToolCalled(runId: String, nodeId: String, callId: String, tool: String, input: String)

  /** A running tool call reported on itself — the transcript's mirror of the
    * engine's `StreamEvent.ToolRunProgress`, so a sub-agent's row can show what
    * the lead's own row shows (`eval_scala` reports its worker's stage and
    * liveness this way). `metadata` is uninterpreted here; [[EvalDisplay]] is
    * what reads it.
    *
    * EPHEMERAL — THIS EVENT MUST NEVER BE PERSISTED. It arrives at roughly 1 Hz
    * for the whole life of a call, and the transcript wire is teed to the session
    * logs under `.auk/sessions/`; letting progress through would bloat every log
    * with samples of a number that was only ever interesting while it moved, and
    * a replay of that log would re-animate clocks that stopped long ago. The
    * report is a statement about *now*, and it is worth nothing later.
    *
    * Two things keep that true. [[ephemeral]] marks the event, and
    * [[WireCodec.encodeDurable]] refuses to encode it — a persistence tee that
    * goes through the latter cannot write one even by mistake. Both live here
    * because the tees themselves are outside this module (the workflow, team and
    * loop bridges), so this side can only make the contract un-missable, not
    * enforce it directly. [[Transcript.toEvents]] holds the other half of the
    * line: folded progress is never replayed back out as an event.
    *
    * Reports carry the whole picture, so each replaces the last rather than
    * merging with it — a key absent from the newest report is absent, not stale. */
  case ToolProgressed(runId: String, nodeId: String, callId: String, metadata: Map[String, String])

  /** A tool call returned `output` (an error if `isError`). */
  case ToolReturned(runId: String, nodeId: String, callId: String, output: String, isError: Boolean)

  /** A message delivered *into* this agent's conversation by `from` — the lead or
    * another member. Unlike the other cases this is not the agent's own output:
    * it is what it was asked, recorded where it entered the history. Emitted by
    * agent teams (see `TeamBridge`); workflow runs seed their sub-agents from a
    * node prompt instead and so currently emit none. */
  case Received(runId: String, nodeId: String, from: String, text: String)

object TranscriptEvent:
  /** The `from` of a message sent by the lead (the main agent) rather than by a
    * fellow member — the reserved id no member may take. It lives here, beside
    * the event that carries it over the wire, because every side needs the same
    * string: the host emits it, the TUI leaves such a message unattributed, and
    * the dashboard labels the others by sender. */
  val LeadSender: String = "lead"
