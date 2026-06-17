package auk.webui

import auk.workflow.{Forest, ForestNode}

/** Pure projection `AppState -> View`, mirroring the TUI's
  * `ChatApp.forestLines`/`forestNodeLine` decisions exactly: groups render in
  * declaration order, the ungrouped section comes last and only when non-empty,
  * empty sections are dropped, and tokens are formatted with the TUI's `fmtTokens`.
  */
object WorkflowView:
  def from(state: AppState): View =
    state.selected.flatMap(rid => state.forests.get(rid)) match
      case None => View.Empty(state.conn)
      case Some(f) =>
        val sel = state.selected.getOrElse("")
        val tabs = state.order.map(r => RunTab(r, tabLabel(r), r == sel))
        View.Forest(ForestView(tabs, sectionsOf(f), f.logs, f.nodes.size), state.conn)

  private def sectionsOf(f: Forest): Vector[GroupSection] =
    val byGroup = f.nodes.groupBy(_.group)
    val declared = f.groups.map(g =>
      GroupSection(Some(g.id), Some(g.name), byGroup.getOrElse(Some(g.id), Vector.empty).map(nodeRow)))
    val ungrouped = byGroup.get(None).map(ns => GroupSection(None, None, ns.map(nodeRow))).toVector
    (declared ++ ungrouped).filter(_.nodes.nonEmpty)

  private def nodeRow(n: ForestNode): NodeRow =
    val kind = StatusKind.of(n.status)
    NodeRow(
      id = n.id,
      statusKind = kind,
      glyph = StatusKind.glyph(kind),
      tokensText = if n.outputTokens > 0 then s"${fmtTokens(n.outputTokens)} tokens" else "",
      toolText = n.currentTool.getOrElse(""),
      summary = n.summary
    )

  /** Byte-identical to `ChatApp.fmtTokens`/`oneDecimal` (round-half-up to tenths). */
  private[webui] def fmtTokens(n: Long): String =
    if n >= 1000 then s"${oneDecimal((n + 50) / 100)}k" else n.toString
  private def oneDecimal(scaled: Long): String = s"${scaled / 10}.${scaled % 10}"

  private def tabLabel(runId: String): String = if runId.length > 8 then runId.take(8) else runId
