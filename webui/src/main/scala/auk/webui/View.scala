package auk.webui

import auk.workflow.NodeStatus

/** A status kind decoupled from glyph/CSS, so the view-model stays pure data and
  * the Laminar binding maps kind -> class mechanically. Mirrors [[NodeStatus]]. */
enum StatusKind:
  case Pending, Queued, Running, Done, Failed

object StatusKind:
  def of(s: NodeStatus): StatusKind = s match
    case NodeStatus.Pending => Pending
    case NodeStatus.Queued  => Queued
    case NodeStatus.Running => Running
    case NodeStatus.Done    => Done
    case NodeStatus.Failed  => Failed

  /** A static glyph per kind. Running's pulse is done in CSS; the glyph is a steady
    * dot so it can animate. */
  def glyph(k: StatusKind): String = k match
    case Pending => "○"
    case Queued  => "◔"
    case Running => "●"
    case Done    => "●"
    case Failed  => "●"

  def cssClass(k: StatusKind): String = k match
    case Pending => "is-pending"
    case Queued  => "is-queued"
    case Running => "is-running"
    case Done    => "is-done"
    case Failed  => "is-failed"

  /** The lowercase name used for the `data-status` attribute and for badges. */
  def name(k: StatusKind): String = k match
    case Pending => "pending"
    case Queued  => "queued"
    case Running => "running"
    case Done    => "done"
    case Failed  => "failed"

/** A run switcher entry (only rendered when more than one run is live). */
final case class RunTab(runId: String, label: String, selected: Boolean)

/** One sub-agent row in the sidebar tree. `tokensText`/`toolText` are "" when
  * absent; `selected` highlights the row whose transcript is shown. */
final case class NodeRow(
    id: String,
    statusKind: StatusKind,
    glyph: String,
    tokensText: String,
    toolText: String,
    selected: Boolean
)

/** A group's section in the sidebar; `id`/`name` are None for the trailing
  * ungrouped section. */
final case class GroupSection(id: Option[String], name: Option[String], nodes: Vector[NodeRow])

/** The left bar: the agent tree plus any workflow-level log lines. */
final case class SidebarView(sections: Vector[GroupSection], nodeCount: Int, logs: Vector[String])

/** One rendered line of a sub-agent's transcript. */
enum TranscriptRow:
  case Prose(text: String)
  case Thought(text: String)
  /** A tool call card: `input` is the syntax-highlighted argument source
    * (Scala tokens for `eval_scala`, a single plain token otherwise) and
    * `output` is None while the tool is still running. */
  case Tool(callId: String, name: String, input: Vector[HlToken], output: Option[String], isError: Boolean)

/** The header + streamed transcript of the selected sub-agent. `streaming` is true
  * while the node is still running, so the binding can show a live caret. */
final case class AgentView(
    id: String,
    statusKind: StatusKind,
    glyph: String,
    tokensText: String,
    toolText: String,
    prompt: Option[String],
    summary: Option[String],
    rows: Vector[TranscriptRow],
    streaming: Boolean
)

/** What fills the main panel. */
enum MainView:
  /** No run yet — waiting for a workflow to start. */
  case Waiting
  /** A run exists but no agent is selected. */
  case Unselected
  /** Show the selected agent's transcript. */
  case Agent(view: AgentView)

/** The whole rendered page, as pure data. Split into independent sub-models
  * (`runs`/`sidebar`/`main`) so the Laminar layer can bind each to its own signal
  * and only rebuild the part that changed (the transcript streams without
  * rebuilding the tree). */
final case class View(
    conn: ConnStatus,
    runs: Vector[RunTab],
    sidebar: SidebarView,
    main: MainView
)
