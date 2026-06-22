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

  /** A static glyph per kind. Running's pulse is done in CSS; the glyph is a steady
    * dot so it can animate. */
  def glyph(k: StatusKind): String = k match
    case Pending     => "○"
    case Queued      => "◔"
    case Running     => "●"
    case Done        => "●"
    case Failed      => "●"
    case Interrupted => "❚"
    case Paused      => "❚"

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

/** The sidebar's "workflow code" tab, present when the run has source code. */
final case class CodeTab(selected: Boolean)

/** The left bar: an optional code tab, the agent tree, and any workflow-level
  * log lines. */
final case class SidebarView(codeTab: Option[CodeTab], sections: Vector[GroupSection], nodeCount: Int, logs: Vector[String])

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
    * `output` is None while the tool is still running. */
  final case class Tool(callId: String, name: String, input: Vector[HlToken], output: Option[String], isError: Boolean)
      extends TranscriptRow

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
  /** A run exists but nothing is selected. */
  case Unselected
  /** Show the selected agent's transcript. */
  case Agent(view: AgentView)
  /** Show the workflow's source code (syntax-highlighted Scala). */
  case Code(tokens: Vector[HlToken])

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
