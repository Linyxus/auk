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

/** How one generation of a refinement loop settled. A generation that is still
  * being worked on is [[Running]]; there is at most one of those per loop. */
enum LoopGenerationState:
  case Accepted, Abandoned, Running

/** One generation in a loop's lineage as the UI draws it: its number, how it
  * settled, and what the loop's checker measured on it.
  *
  * Only an accepted generation carries `metrics` and a `description` — an
  * abandoned one produced nothing worth keeping and an in-flight one has not been
  * judged yet, so both are a bare number and a marker.
  */
final case class LoopGenerationView(
    gen: Int,
    state: LoopGenerationState,
    /** The checker's measurements, in key order so the strip does not reshuffle
      * between frames. */
    metrics: Vector[(String, Double)],
    description: String
):
  def accepted: Boolean = state == LoopGenerationState.Accepted

  /** Every metric as `key value`, in key order — the whole map, for the view that
    * has room for it. Empty when nothing was measured. */
  def metricsText: String =
    metrics.map((k, v) => s"$k ${LoopView.number(v)}").mkString(" · ")

/** One metric picked out of a lineage: its newest accepted value, and the value the
  * accepted generation before it had for the same key (`None` when there was no
  * earlier one, or it did not measure this key). The direction is deliberately not
  * interpreted here — whether a number going up is good is the loop's business, so
  * the UI draws which way it moved and says nothing about whether that is progress. */
final case class LoopMetric(key: String, value: Double, previous: Option[Double])

/** A refinement loop as the UI sees it: where the host has it, what it is for, the
  * lineage it has built so far, and what it is doing right now.
  *
  * Snapshots arrive via [[AgentEvent.Loops]] on every phase and stage change, and
  * cover BOTH the loops this session drives and the ones its `.auk/loops` holds from
  * earlier sessions — a loop outlives its session, and one nobody can see is one
  * nobody picks up. The live agent's transcript arrives separately as
  * [[AgentEvent.Activity]] events keyed `(id, liveLabel)`.
  */
final case class LoopView(
    id: String,
    /** The host's phase string, verbatim: `validating`, `adopting`,
      * `running (gen 3)`, `parked: <reason>`, `orphaned (dead session)`. */
    phase: String,
    /** The loop's goal, first line only. */
    goal: String,
    /** Every generation the loop has STARTED, in order — the accepted lineage with
      * the abandoned numbers still in their places, so the strip reads as the loop's
      * actual history rather than as its successes. */
    generations: Vector[LoopGenerationView],
    /** What the loop is doing right now — `gen 3, attempt 2 — evaluating` — or
      * `None` when no generation is in flight. */
    activity: Option[String],
    /** Where the live agent's transcript is filed, e.g. `gen-3-worker`: the
      * transcript itself is at `transcripts((id, liveLabel))`. `None` for a loop
      * with no agent running, which is every loop read off disk. */
    liveLabel: Option[String],
    /** Why the loop stopped, without the phase's `parked: ` prefix; `None` while it
      * is live. */
    parked: Option[String],
    /** A loop the project's ledger records as running that no session is driving —
      * what a session that ended mid-generation left behind. Not a park: nothing
      * decided to stop it, so it reads as work waiting to be picked up. */
    orphaned: Boolean
):
  /** Whether some session is driving this loop right now. */
  def live: Boolean = parked.isEmpty && !orphaned

  /** The newest accepted generation, which is the tree the next one starts from. */
  def latestAccepted: Option[LoopGenerationView] = generations.findLast(_.accepted)

  /** The loop's headline number: the first metric of the newest accepted
    * generation, carrying whatever the accepted generation before it measured for
    * the same key. `None` for a loop that has accepted nothing, or whose checker
    * measures nothing. */
  def headline: Option[LoopMetric] =
    val lineage = generations.filter(_.accepted)
    lineage.lastOption.flatMap(_.metrics.headOption).map: (key, value) =>
      LoopMetric(key, value, lineage.dropRight(1).lastOption.flatMap(_.metrics.find(_._1 == key)).map(_._2))

object LoopView:
  /** A measurement as text: whole numbers without a trailing `.0`, everything else
    * as it is. The same reading the host's own loop notices give. */
  def number(v: Double): String =
    if v == v.floor && v.abs < 1e15 then v.toLong.toString else v.toString

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

  /** A full refinement-loop snapshot, pushed by the [[auk.runtime.LoopBridge]] on
    * every phase change and on every stage of the drive cycle, so the panel
    * breathes with the generation. Snapshots rather than deltas, exactly like
    * [[Team]] — and they include the loops sitting in `.auk/loops` that no session
    * is driving, so a loop left behind by an earlier session is visible without
    * anyone having to ask for it. */
  case Loops(loops: Vector[LoopView])

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
