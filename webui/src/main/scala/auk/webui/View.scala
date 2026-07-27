package auk.webui

import auk.workflow.NodeStatus

/** A status kind decoupled from glyph/CSS, so the view-model stays pure data and
  * the Laminar binding maps kind -> class mechanically. Mirrors [[NodeStatus]],
  * plus `Paused` for a whole run (no per-node equivalent). */
enum StatusKind:
  case Pending, Queued, Running, Done, Failed, Interrupted, Paused

object StatusKind:
  def of(s: NodeStatus): StatusKind = s match
    case NodeStatus.Pending     => Pending
    case NodeStatus.Queued      => Queued
    case NodeStatus.Running     => Running
    case NodeStatus.Done        => Done
    case NodeStatus.Failed      => Failed
    case NodeStatus.Interrupted => Interrupted

  def cssClass(k: StatusKind): String = k match
    case Pending     => "is-pending"
    case Queued      => "is-queued"
    case Running     => "is-running"
    case Done        => "is-done"
    case Failed      => "is-failed"
    case Interrupted => "is-interrupted"
    case Paused      => "is-paused"

  /** The lowercase name used for the `data-status` attribute and for badges. */
  def name(k: StatusKind): String = k match
    case Pending     => "pending"
    case Queued      => "queued"
    case Running     => "running"
    case Done        => "done"
    case Failed      => "failed"
    case Interrupted => "interrupted"
    case Paused      => "paused"

/** A run switcher entry (the dropdown is only shown when more than one run is
  * live). `statusKind` is the run's overall state (for the menu's status dot) and
  * `settled`/`total` are its finished / declared sub-agent counts. */
final case class RunTab(
    runId: String,
    label: String,
    selected: Boolean,
    statusKind: StatusKind,
    settled: Int,
    total: Int
)

/** One sub-agent card on the canvas. `tokensText`/`toolText`/`promptHint` are ""
  * when absent; `selected` highlights the card whose transcript is open in the
  * floating window. */
final case class AgentCard(
    id: String,
    statusKind: StatusKind,
    tokensText: String,
    toolText: String,
    promptHint: String,
    selected: Boolean
)

/** One group's card on the canvas; `name` is None for the trailing ungrouped
  * card. `key` is a stable identity for keyed rendering ("g:<id>" or
  * "~ungrouped"); `settled`/`total` are the card's finished / declared agent
  * counts (the header's progress figure — the filmstrip itself is derived from
  * `agents`). */
final case class GroupCard(
    key: String,
    name: Option[String],
    description: String,
    agents: Vector[AgentCard],
    settled: Int,
    total: Int
)

/** The canvas: one card per non-empty group (declared order, ungrouped last) and
  * any workflow-level log lines. `nodeCount` distinguishes a run with no agents
  * yet from no run at all. */
final case class CanvasView(cards: Vector[GroupCard], nodeCount: Int, logs: Vector[String])

/** The top bar's "workflow code" button, present when the run has source code. */
final case class CodeButton(selected: Boolean)

/** The selected run's at-a-glance figures for the top bar's metrics strip:
  * finished/declared agents, currently running agents, and the run's total
  * output tokens (compact-formatted). */
final case class RunStats(settled: Int, total: Int, running: Int, tokensText: String)

/** One rendered line of a sub-agent's transcript.
  *
  * `Prose`/`Thought` carry the streamed text as its append-only `chunks` (mirroring
  * [[auk.workflow.TranscriptItem]]), so the Laminar binding can render one node per
  * chunk and a streaming delta only appends a node rather than re-rendering the
  * whole run. The string-taking `apply` overloads keep the common single-chunk case
  * (and tests) concise; equality is by chunks, so appending a chunk makes the row
  * unequal and the binding updates. */
sealed trait TranscriptRow
object TranscriptRow:
  final case class Prose(chunks: Vector[String]) extends TranscriptRow
  object Prose:
    def apply(text: String): Prose = Prose(Vector(text))
  /** A reasoning block. `done` is true once the agent has moved on (a later row
    * exists, or the agent stopped streaming), so the binding folds it; a still-
    * active thought (the last row of a streaming agent) stays open. */
  final case class Thought(chunks: Vector[String], done: Boolean) extends TranscriptRow
  object Thought:
    def apply(text: String, done: Boolean): Thought = Thought(Vector(text), done)
  /** A tool call card: `input` is the syntax-highlighted argument source
    * (Scala tokens for `eval_scala`, a single plain token otherwise) and
    * `output` is None while the tool is still running.
    *
    * `title` and `hint` are the projected display strings — the human-readable
    * name and a one-line argument digest. They are computed here rather than in
    * the DOM layer so the binding stays branch-free and the copy has one home;
    * `name` is kept as the raw wire name, since that is the identity. */
  final case class Tool(
      callId: String,
      name: String,
      title: String,
      hint: String,
      input: Vector[HlToken],
      output: Option[String],
      isError: Boolean
  ) extends TranscriptRow

  /** A message another agent sent *to* this one, shown where it entered the
    * conversation. Atomic (it never streams), so it holds plain text rather than
    * chunks. `from` is the raw sender id; the binding names it only when it is
    * not the lead, whose messages are the expected kind. */
  final case class Received(from: String, text: String) extends TranscriptRow

/** The header + streamed transcript of the selected sub-agent. `streaming` is true
  * while the node is still running, so the binding can show a live caret. */
final case class AgentView(
    id: String,
    statusKind: StatusKind,
    tokensText: String,
    toolText: String,
    prompt: Option[String],
    summary: Option[String],
    rows: Vector[TranscriptRow],
    streaming: Boolean
)

/** What fills the right-hand drawer. */
enum PanelView:
  /** Nothing focused — the drawer is closed. */
  case Closed
  /** Show the selected agent's transcript. */
  case Agent(view: AgentView)
  /** Show the workflow's source code (syntax-highlighted Scala). */
  case Code(tokens: Vector[HlToken])

/** The whole rendered page, as pure data. Split into independent sub-models
  * (`runs`/`canvas`/`panel`) so the Laminar layer can bind each to its own signal
  * and only rebuild the part that changed (the transcript streams without
  * rebuilding the canvas). */
final case class View(
    conn: ConnStatus,
    runs: Vector[RunTab],
    codeButton: Option[CodeButton],
    stats: Option[RunStats],
    canvas: CanvasView,
    panel: PanelView
)
