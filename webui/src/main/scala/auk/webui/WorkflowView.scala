package auk.webui

import scala.scalajs.js

import auk.workflow.{Forest, ForestNode, NodeStatus, RunStatus, Transcript, TranscriptItem}

/** Projection `AppState -> View`. The sidebar mirrors the TUI's grouping
  * decisions (declared-group order, ungrouped section last, empty sections
  * dropped); the main panel projects the selected node's streamed [[Transcript]].
  * All copy and formatting lives here so the Laminar binding is branch-free.
  *
  * `from` runs on every SSE frame, so syntax highlighting is memoized (see
  * [[highlightScala]]) — the only state here. That both avoids re-lexing unchanged
  * `eval_scala`/workflow source each frame and, by returning a stable token-vector
  * instance per source, lets the render layer's `.distinct` gates short-circuit on
  * reference equality instead of walking every token each delta. */
object WorkflowView:
  def from(state: AppState): View =
    val sel = state.selectedRun
    val runs = state.order.map { r =>
      val f = state.forests.getOrElse(r, Forest.empty)
      RunTab(r, tabLabel(r), sel.contains(r), runStatusKind(f), settledCount(f), f.nodes.size)
    }
    val forest = sel.flatMap(state.forests.get)
    View(
      conn = state.conn,
      runs = runs,
      sidebar = sidebarOf(forest, state.focus),
      main = mainOf(state, forest)
    )

  // -- sidebar -----------------------------------------------------------------

  private def sidebarOf(forest: Option[Forest], focus: Focus): SidebarView =
    forest match
      case None => SidebarView(None, Vector.empty, 0, Vector.empty)
      case Some(f) =>
        val codeTab = f.code.map(_ => CodeTab(focus == Focus.Code))
        SidebarView(codeTab, sectionsOf(f, focusedNode(focus)), f.nodes.size, f.logs)

  private def focusedNode(focus: Focus): Option[String] = focus match
    case Focus.Node(id) => Some(id)
    case _              => None

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
        state.focus match
          case Focus.Code =>
            f.code match
              case Some(c) => MainView.Code(highlightScala(c))
              case None    => MainView.Unselected
          case Focus.Node(id) =>
            f.nodes.find(_.id == id) match
              case Some(node) => MainView.Agent(agentView(node, state.selectedTranscript))
              case None       => MainView.Unselected
          case Focus.Unfocused => MainView.Unselected

  private def agentView(n: ForestNode, transcript: Transcript): AgentView =
    val kind = StatusKind.of(n.status)
    val streaming = n.status == NodeStatus.Running
    val items = transcript.items
    AgentView(
      id = n.id,
      statusKind = kind,
      glyph = StatusKind.glyph(kind),
      tokensText = if n.outputTokens > 0 then s"${fmtTokens(n.outputTokens)} tokens" else "",
      toolText = n.currentTool.getOrElse(""),
      prompt = n.prompt,
      summary = n.summary,
      rows = items.zipWithIndex.map((item, i) => transcriptRow(item, isLast = i == items.size - 1, streaming = streaming)),
      streaming = streaming
    )

  private def transcriptRow(item: TranscriptItem, isLast: Boolean, streaming: Boolean): TranscriptRow = item match
    case s: TranscriptItem.Said    => TranscriptRow.Prose(s.chunks)
    // A thought is "active" only while it is the agent's last word and the agent
    // is still streaming; otherwise it is done and folds.
    case t: TranscriptItem.Thought => TranscriptRow.Thought(t.chunks, done = !(streaming && isLast))
    case TranscriptItem.ToolCall(callId, tool, input, output, isError) =>
      TranscriptRow.Tool(callId, tool, highlightInput(tool, input), output, isError)

  /** `eval_scala`'s input is `{"code": "<scala>"}` — pull the code out and
    * highlight it as Scala; any other tool's input is shown verbatim. */
  private def highlightInput(tool: String, input: String): Vector[HlToken] =
    if tool == "eval_scala" then highlightScala(extractCode(input))
    else Vector(HlToken(HlKind.Plain, input))

  private def extractCode(input: String): String =
    try
      val d = js.JSON.parse(input).asInstanceOf[js.Dynamic]
      val c = d.code
      if js.typeOf(c) == "string" then c.asInstanceOf[String] else input
    catch case _: Throwable => input

  /** Syntax-highlight Scala source, memoized for the session. `eval_scala` inputs
    * and the workflow code are re-projected on every SSE frame but their source is
    * immutable, so each unique source is lexed at most once; returning the **same**
    * token-vector instance for a repeated source also lets the render layer's
    * `.distinct` gates short-circuit on reference equality. Both paths feed
    * [[Highlight.scala]], so a shared entry for identical source is always correct.
    * (Only the expensive Scala lex is cached — never the plain-input fallback — so
    * there is no key collision between highlighted and verbatim tokenizations. If
    * transcript eviction is added later, clear this alongside it.) */
  private val highlightCache = scala.collection.mutable.Map.empty[String, Vector[HlToken]]
  private[webui] def highlightScala(code: String): Vector[HlToken] =
    highlightCache.getOrElseUpdate(code, Highlight.scala(code))

  // -- formatting --------------------------------------------------------------

  /** A one-line preview of a folded block's content, shown next to its heading:
    * whitespace collapsed to single spaces, trimmed, and truncated with an ellipsis.
    * Pure, so it is unit-tested. */
  private[webui] def preview(text: String, max: Int = 80): String =
    val s = text.trim.replaceAll("\\s+", " ")
    if s.length > max then s.take(max).trim + "…" else s

  /** [[preview]] over chunked text, scanning only enough leading chunks to fill
    * `max` visible characters. A streaming block's hint therefore costs O(max), not
    * O(length) — it never materializes the whole accumulated run on each delta. */
  private[webui] def previewChunks(chunks: Vector[String], max: Int = 80): String =
    val b = new StringBuilder
    val it = chunks.iterator
    while it.hasNext && b.length <= max do b.append(it.next())
    preview(b.toString, max)

  /** Byte-identical to `ChatApp.fmtTokens`/`oneDecimal` (round-half-up to tenths). */
  private[webui] def fmtTokens(n: Long): String =
    if n >= 1000 then s"${oneDecimal((n + 50) / 100)}k" else n.toString
  private def oneDecimal(scaled: Long): String = s"${scaled / 10}.${scaled % 10}"

  private def tabLabel(runId: String): String = if runId.length > 8 then runId.take(8) else runId

  /** A run's finished (terminal) sub-agent count — the switcher's progress
    * numerator (`total` is `f.nodes.size`). */
  private def settledCount(f: Forest): Int =
    f.nodes.count(n => n.status == NodeStatus.Done || n.status == NodeStatus.Failed)

  /** A run's overall status for the switcher dot: failed if any sub-agent failed,
    * else running/queued if any is active, else done once all have settled, else
    * pending. Mirrors the at-a-glance status the TUI shows per run. */
  private def runStatusKind(f: Forest): StatusKind =
    if f.status == RunStatus.Paused then StatusKind.Paused
    else if f.nodes.exists(_.status == NodeStatus.Failed) then StatusKind.Failed
    else if f.nodes.exists(_.status == NodeStatus.Running) then StatusKind.Running
    else if f.nodes.exists(_.status == NodeStatus.Queued) then StatusKind.Queued
    else if f.nodes.nonEmpty && f.nodes.forall(_.status == NodeStatus.Done) then StatusKind.Done
    else StatusKind.Pending
