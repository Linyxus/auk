package auk.agent

import auk.workflow.{OrchestrationEvent, TranscriptEvent}
import auk.llm.endpoint.{LLMError, StreamEvent}
import auk.session.{SessionSnapshot, SessionSummary}
import auk.utils.Result

/** A team member as the UI sees it: identity, live status, and cumulative
  * token usage (completed turns plus the in-flight turn's running totals).
  * Snapshots arrive via [[AgentEvent.Team]]; the member's live transcript
  * arrives separately as [[AgentEvent.Activity]] events keyed
  * `("team", memberId)`. */
final case class TeamMemberView(
    id: String,
    desc: String,
    working: Boolean,
    inputTokens: Long,
    outputTokens: Long,
    /** The lead retired this member: it runs nothing further, but stays on the
      * roster with its transcript and totals intact. Never `working`. */
    retired: Boolean = false
)

/** One configured MCP server as the UI sees it: identity, the command line it
  * launches, where its startup tool discovery stands, and — once the handshake
  * has run — the facts the server reported. Snapshots arrive via
  * [[AgentEvent.McpUpdated]]: once at startup (every server still
  * [[McpServerState.Pending]]) and again as each server's discovery settles. */
final case class McpServerView(
    name: String,
    command: String,
    /** The names (never the values — they may be secrets) of the env vars the
      * config sets for this server, sorted. */
    env: Vector[String],
    state: McpServerState,
    /** The failure that ended discovery; `Some` exactly when [[state]] is
      * [[McpServerState.Failed]]. */
    error: Option[String],
    /** The server's self-reported version (`serverInfo.version`), if given. */
    version: Option[String],
    /** The protocol version the server echoed from `initialize`, if any. */
    protocolVersion: Option[String],
    tools: Vector[McpToolView]
)

/** Where a server stands in startup tool discovery: awaiting its `tools/list`
  * ([[Pending]]), answered ([[Ready]]), or failed to spawn / handshake / list
  * ([[Failed]] — the error rides on the [[McpServerView]]). */
enum McpServerState:
  case Pending, Ready, Failed

/** One tool an MCP server contributed: its own name, the (possibly
  * disambiguated) wire name the model calls it by, and its description. */
final case class McpToolView(name: String, wireName: String, description: String)

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

  /** A full team-roster snapshot, emitted by the [[auk.runtime.TeamBridge]] on
    * every change (member created, status flip, a turn's round completing with
    * fresh token totals). Snapshots rather than deltas: the roster is small and
    * a snapshot can never leave the UI out of sync. */
  case Team(members: Vector[TeamMemberView])

  /** A full MCP server-status snapshot, pushed by the host whenever a server's
    * startup discovery settles (and once before any does). Snapshots rather
    * than deltas, exactly like [[Team]]: the set is small and a snapshot can
    * never leave the UI out of sync. */
  case McpUpdated(servers: Vector[McpServerView])

  /** The in-flight turn was interrupted by the user: the UI should commit
    * whatever streamed so far, mark it interrupted, and return to idle. */
  case Interrupted

  /** An out-of-band, ephemeral status line for the transcript. Not persisted to
    * the session. */
  case Notice(message: String)

  /** The live workflow dashboard came up at `url`. Deliberately not a [[Notice]]:
    * the UI stores the URL and opens it on demand (`o` on the workflow page)
    * instead of printing it. */
  case Dashboard(url: String)

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
