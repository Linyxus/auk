package auk.agent

import auk.workflow.{OrchestrationEvent, TranscriptEvent}
import auk.llm.endpoint.{LLMError, StreamEvent}
import auk.session.{SessionSnapshot, SessionSummary}
import auk.utils.Result

/** Events flowing from the agent loop to the UI. */
enum AgentEvent:
  /** Normal model/tool streaming output. */
  case Stream(result: Result[StreamEvent, LLMError])

  /** Available sessions for the resume picker, newest first. */
  case SessionsListed(sessions: List[SessionSummary])

  /** The active session has changed and should replace the UI transcript. */
  case SessionSwitched(snapshot: SessionSnapshot)

  /** The active model changed; carries the new display label, its context
    * window size (tokens), and the provider/model-id/endpoint details for the
    * debug panel. */
  case ModelSwitched(label: String, contextWindow: Int, provider: String, modelId: String, baseUrl: String)

  /** Context compaction has started for the current session. */
  case ContextCompactionStarted

  /** The current session's earlier context has been compacted into `summary`.
    * `estimatedTokens` is the engine's estimate of the resulting prompt size —
    * system prompt + tool schemas + the compaction message, not the summary text
    * alone — so the gauge can drop to a realistic figure at once, before the next
    * round reports exact usage. Only the engine knows all three pieces, so it
    * computes the estimate and carries it here. */
  case ContextCompacted(summary: String, estimatedTokens: Long)

  /** A workflow orchestration update — forest structure and per-node status —
    * for the eval_scala run identified by the event's `runId`. */
  case Orchestration(event: OrchestrationEvent)

  /** A workflow sub-agent transcript delta — prose, reasoning, or a tool
    * call/return — for the node identified by the event's run + node ids.
    * The TUI folds these into per-node [[auk.workflow.Transcript]]s exactly
    * as it folds [[Orchestration]] events into forests. */
  case Activity(event: TranscriptEvent)

  /** The in-flight turn was interrupted by the user: the UI should commit
    * whatever streamed so far, mark it interrupted, and return to idle. */
  case Interrupted

  /** An out-of-band, ephemeral status line for the transcript (e.g. the workflow
    * dashboard URL). Not persisted to the session. */
  case Notice(message: String)

  /** An inbox item arrived while a turn was in flight and is now queued. The UI
    * appends it to the pending-queue panel (above the input box) until a
    * matching [[InputsConsumed]] drains it. The engine is the authority on queue
    * order — including interleaved user messages and system notices. */
  case InputQueued(item: Inbox)

  /** The engine folded these queued inputs (always a FIFO prefix of the pending
    * queue) into the conversation — at a turn's start, or at a tool-calling
    * round boundary mid-turn. The UI drops the first `items.size` from its
    * pending panel and shows them in the transcript in chronological position. */
  case InputsConsumed(items: List[Inbox])
