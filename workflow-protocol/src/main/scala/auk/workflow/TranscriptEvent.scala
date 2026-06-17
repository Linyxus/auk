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

  /** A chunk of assistant prose (appended to the open prose run). */
  case Said(runId: String, nodeId: String, text: String)
  /** A chunk of reasoning text (appended to the open thinking run). */
  case Thought(runId: String, nodeId: String, text: String)
  /** A tool call started; `input` is its (complete) argument JSON. */
  case ToolCalled(runId: String, nodeId: String, callId: String, tool: String, input: String)
  /** A tool call returned `output` (an error if `isError`). */
  case ToolReturned(runId: String, nodeId: String, callId: String, output: String, isError: Boolean)
