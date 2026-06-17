package auk.webui

import scala.scalajs.js

import auk.workflow.{Forest, ForestNode, NodeStatus, Transcript, TranscriptItem}

/** Pure projection `AppState -> View`. The sidebar mirrors the TUI's grouping
  * decisions (declared-group order, ungrouped section last, empty sections
  * dropped); the main panel projects the selected node's streamed [[Transcript]].
  * All copy and formatting lives here so the Laminar binding is branch-free. */
object WorkflowView:
  def from(state: AppState): View =
    val sel = state.selectedRun
    val runs = state.order.map(r => RunTab(r, tabLabel(r), sel.contains(r)))
    val forest = sel.flatMap(state.forests.get)
    View(
      conn = state.conn,
      runs = runs,
      sidebar = sidebarOf(forest, state.selectedNode),
      main = mainOf(state, forest)
    )

  // -- sidebar -----------------------------------------------------------------

  private def sidebarOf(forest: Option[Forest], selectedNode: Option[String]): SidebarView =
    forest match
      case None    => SidebarView(Vector.empty, 0, Vector.empty)
      case Some(f) => SidebarView(sectionsOf(f, selectedNode), f.nodes.size, f.logs)

  private def sectionsOf(f: Forest, selectedNode: Option[String]): Vector[GroupSection] =
    val byGroup = f.nodes.groupBy(_.group)
    val declared = f.groups.map(g =>
      GroupSection(Some(g.id), Some(g.name), byGroup.getOrElse(Some(g.id), Vector.empty).map(nodeRow(_, selectedNode))))
    val ungrouped = byGroup.get(None).map(ns => GroupSection(None, None, ns.map(nodeRow(_, selectedNode)))).toVector
    (declared ++ ungrouped).filter(_.nodes.nonEmpty)

  private def nodeRow(n: ForestNode, selectedNode: Option[String]): NodeRow =
    val kind = StatusKind.of(n.status)
    NodeRow(
      id = n.id,
      statusKind = kind,
      glyph = StatusKind.glyph(kind),
      tokensText = if n.outputTokens > 0 then fmtTokens(n.outputTokens) else "",
      toolText = n.currentTool.getOrElse(""),
      selected = selectedNode.contains(n.id)
    )

  // -- main panel --------------------------------------------------------------

  private def mainOf(state: AppState, forest: Option[Forest]): MainView =
    forest match
      case None => MainView.Waiting
      case Some(f) =>
        state.selectedNode.flatMap(nid => f.nodes.find(_.id == nid)) match
          case None       => MainView.Unselected
          case Some(node) => MainView.Agent(agentView(node, state.selectedTranscript))

  private def agentView(n: ForestNode, transcript: Transcript): AgentView =
    val kind = StatusKind.of(n.status)
    AgentView(
      id = n.id,
      statusKind = kind,
      glyph = StatusKind.glyph(kind),
      tokensText = if n.outputTokens > 0 then s"${fmtTokens(n.outputTokens)} tokens" else "",
      toolText = n.currentTool.getOrElse(""),
      prompt = n.prompt,
      summary = n.summary,
      rows = transcript.items.map(transcriptRow),
      streaming = n.status == NodeStatus.Running
    )

  private def transcriptRow(item: TranscriptItem): TranscriptRow = item match
    case TranscriptItem.Said(text)    => TranscriptRow.Prose(text)
    case TranscriptItem.Thought(text) => TranscriptRow.Thought(text)
    case TranscriptItem.ToolCall(callId, tool, input, output, isError) =>
      TranscriptRow.Tool(callId, tool, highlightInput(tool, input), output, isError)

  /** `eval_scala`'s input is `{"code": "<scala>"}` — pull the code out and
    * highlight it as Scala; any other tool's input is shown verbatim. */
  private def highlightInput(tool: String, input: String): Vector[HlToken] =
    if tool == "eval_scala" then Highlight.scala(extractCode(input))
    else Vector(HlToken(HlKind.Plain, input))

  private def extractCode(input: String): String =
    try
      val d = js.JSON.parse(input).asInstanceOf[js.Dynamic]
      val c = d.code
      if js.typeOf(c) == "string" then c.asInstanceOf[String] else input
    catch case _: Throwable => input

  // -- formatting --------------------------------------------------------------

  /** Byte-identical to `ChatApp.fmtTokens`/`oneDecimal` (round-half-up to tenths). */
  private[webui] def fmtTokens(n: Long): String =
    if n >= 1000 then s"${oneDecimal((n + 50) / 100)}k" else n.toString
  private def oneDecimal(scaled: Long): String = s"${scaled / 10}.${scaled % 10}"

  private def tabLabel(runId: String): String = if runId.length > 8 then runId.take(8) else runId
