package auk.agent

import auk.workflow.OrchestrationEvent
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

  /** A workflow orchestration update — forest structure and per-node status —
    * for the eval_scala run identified by the event's `runId`. */
  case Orchestration(event: OrchestrationEvent)

  /** The in-flight turn was interrupted by the user: the UI should commit
    * whatever streamed so far, mark it interrupted, and return to idle. */
  case Interrupted
