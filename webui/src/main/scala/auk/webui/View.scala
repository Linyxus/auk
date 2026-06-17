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

  /** A static glyph per kind. Running's live spinner is done in CSS, so its glyph
    * here is a steady placeholder. Pending/Queued/Done/Failed mirror the TUI. */
  def glyph(k: StatusKind): String = k match
    case Pending => "·"
    case Queued  => "◌"
    case Running => "◍"
    case Done    => "✓"
    case Failed  => "✗"

  def cssClass(k: StatusKind): String = k match
    case Pending => "node-pending"
    case Queued  => "node-queued"
    case Running => "node-running"
    case Done    => "node-done"
    case Failed  => "node-failed"

  /** The lowercase name used for the `data-status` attribute. */
  def name(k: StatusKind): String = k match
    case Pending => "pending"
    case Queued  => "queued"
    case Running => "running"
    case Done    => "done"
    case Failed  => "failed"

/** One sub-agent row. `tokensText`/`toolText` are "" when absent; the binding adds
  * separators. */
final case class NodeRow(
    id: String,
    statusKind: StatusKind,
    glyph: String,
    tokensText: String,
    toolText: String,
    summary: Option[String]
)

/** A group's section; `id`/`name` are None for the trailing ungrouped section. */
final case class GroupSection(id: Option[String], name: Option[String], nodes: Vector[NodeRow])

final case class RunTab(runId: String, label: String, selected: Boolean)

final case class ForestView(
    tabs: Vector[RunTab],
    sections: Vector[GroupSection],
    logs: Vector[String],
    nodeCount: Int
)

/** What the page renders right now. */
enum View:
  case Empty(conn: ConnStatus)
  case Forest(view: ForestView, conn: ConnStatus)
