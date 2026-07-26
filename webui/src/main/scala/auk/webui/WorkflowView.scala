package auk.webui

import scala.scalajs.js

import auk.workflow.{Forest, ForestNode, NodeStatus, RunStatus, ToolDisplay, Transcript, TranscriptItem}

/** Projection `AppState -> View`. The canvas mirrors the TUI's grouping
  * decisions (declared-group order, ungrouped card last, empty groups dropped);
  * the drawer projects the selected node's streamed [[Transcript]] (or the
  * workflow code). All copy and formatting lives here so the Laminar binding is
  * branch-free.
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
      codeButton = forest.flatMap(_.code).map(_ => CodeButton(state.focus == Focus.Code)),
      stats = forest.map(statsOf),
      canvas = canvasOf(forest, focusedNode(state.focus)),
      panel = panelOf(state, forest)
    )

  /** The selected run's top-bar figures. */
  private def statsOf(f: Forest): RunStats =
    RunStats(
      settled = settledCount(f),
      total = f.nodes.size,
      running = f.nodes.count(_.status == NodeStatus.Running),
      tokensText = fmtTokens(f.nodes.map(_.outputTokens).sum)
    )

  // -- canvas ------------------------------------------------------------------

  private def canvasOf(forest: Option[Forest], selectedNode: Option[String]): CanvasView =
    forest match
      case None    => CanvasView(Vector.empty, 0, Vector.empty)
      case Some(f) => CanvasView(cardsOf(f, selectedNode), f.nodes.size, f.logs)

  private def focusedNode(focus: Focus): Option[String] = focus match
    case Focus.Node(id) => Some(id)
    case _              => None

  private def cardsOf(f: Forest, selectedNode: Option[String]): Vector[GroupCard] =
    val byGroup = f.nodes.groupBy(_.group)
    val declared = f.groups.map(g =>
      groupCard(s"g:${g.id}", Some(g.name), g.description, byGroup.getOrElse(Some(g.id), Vector.empty), selectedNode))
    val ungrouped = byGroup.get(None).map(ns => groupCard("~ungrouped", None, "", ns, selectedNode)).toVector
    (declared ++ ungrouped).filter(_.agents.nonEmpty)

  private def groupCard(
      key: String,
      name: Option[String],
      description: String,
      nodes: Vector[ForestNode],
      selectedNode: Option[String]
  ): GroupCard =
    GroupCard(
      key = key,
      name = name,
      description = description,
      agents = nodes.map(agentCard(_, selectedNode)),
      settled = nodes.count(n => n.status == NodeStatus.Done || n.status == NodeStatus.Failed),
      total = nodes.size
    )

  private def agentCard(n: ForestNode, selectedNode: Option[String]): AgentCard =
    AgentCard(
      id = n.id,
      statusKind = StatusKind.of(n.status),
      tokensText = if n.outputTokens > 0 then fmtTokens(n.outputTokens) else "",
      toolText = n.currentTool.map(ToolDisplay.prettyName).getOrElse(""),
      promptHint = n.prompt.map(preview(_, 110)).getOrElse(""),
      selected = selectedNode.contains(n.id)
    )

  // -- drawer panel --------------------------------------------------------------

  private def panelOf(state: AppState, forest: Option[Forest]): PanelView =
    forest match
      case None => PanelView.Closed
      case Some(f) =>
        state.focus match
          case Focus.Code =>
            f.code match
              case Some(c) => PanelView.Code(highlightScala(c))
              case None    => PanelView.Closed
          case Focus.Node(id) =>
            f.nodes.find(_.id == id) match
              case Some(node) => PanelView.Agent(agentView(node, state.selectedTranscript))
              case None       => PanelView.Closed
          case Focus.Unfocused => PanelView.Closed

  private def agentView(n: ForestNode, transcript: Transcript): AgentView =
    val kind = StatusKind.of(n.status)
    val streaming = n.status == NodeStatus.Running
    val items = transcript.items
    AgentView(
      id = n.id,
      statusKind = kind,
      tokensText = if n.outputTokens > 0 then s"${fmtTokens(n.outputTokens)} tokens" else "",
      toolText = n.currentTool.map(ToolDisplay.prettyName).getOrElse(""),
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
      TranscriptRow.Tool(
        callId,
        tool,
        title = ToolDisplay.prettyName(tool),
        hint = ToolDisplay.compactArgs(input, ToolHintBudget).getOrElse(""),
        input = highlightInput(tool, input),
        output = output,
        isError = isError
      )

  /** How much of a tool call's arguments fits the card's one-line hint. Wider
    * than the TUI's budget: a browser card has the room. */
  private val ToolHintBudget = 100

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
