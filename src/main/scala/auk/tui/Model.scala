package auk.tui

import auk.agent.{AgentEvent, Inbox, McpServerView, TeamMemberView, TokenEstimate}
import auk.workflow.{Forest, ForestNode, OrchestrationEvent, RunStatus, Transcript, TranscriptEvent}
import auk.llm.endpoint.{Content, Usage}
import auk.tui.markdown.MarkdownDocument
import auk.session.{SessionEvent, SessionSnapshot, SessionSummary}

/** Who authored a line in the transcript. */
enum Role:
  case You
  case Auk

/** One segment of an assistant turn, in the order it streamed in.
  *
  * A turn is a sequence of these: the model may reason, call tools, and answer,
  * possibly across several tool rounds, so we keep them ordered rather than
  * folding everything into two strings.
  */
enum Block:
  /** Reasoning. While it streams, `durationMs` is None and the [[Typewriter]]
    * reveals smoothly as text arrives; once the model moves on (answers, calls a
    * tool, or finishes) it is collapsed to a fixed duration — its text fully
    * settled — and rendered as "Thought for Xs". */
  case Thinking(typed: Typewriter, startedMs: Long, durationMs: Option[Long])

  /** A tool the model invoked. `rawArgs` accumulates the streamed JSON argument
    * text; the label (e.g. "Reading foo.scala") is derived from it at render.
    *
    * Execution timing is tracked once the tool actually runs: `startedMs` is set
    * when it begins, and while `elapsedMs` is still None the call is ongoing (its
    * live duration is `clock - startedMs`). On completion `elapsedMs` freezes the
    * duration, `tokens` carries the total tokens spent when the tool reports
    * them (e.g. a sub-agent), and `output`/`isError` record the result text for
    * tools whose result the view renders (e.g. eval_scala's REPL reply). */
  case Tool(
      id: String,
      name: String,
      rawArgs: String,
      startedMs: Option[Long] = None,
      elapsedMs: Option[Long] = None,
      tokens: Option[Long] = None,
      output: Option[String] = None,
      isError: Boolean = false
  )

  /** Answer text addressed to the user. Held as a [[Typewriter]] so the live
    * region can reveal it smoothly, a little per animation tick, rather than in
    * the bursts the deltas arrive in. Committed and loaded answers are fully
    * shown (`Typewriter.shown`).
    *
    * `doc` is the answer's Markdown, parsed incrementally to track the revealed
    * text: it is fed `typed.visible` as the reveal advances, so finalised blocks
    * are parsed once and only the open tail re-parses (see [[MarkdownDocument]]). */
  case Answer(typed: Typewriter, doc: MarkdownDocument)

  /** A queued input (user message or system notice) the engine folded into this
    * turn at a round boundary, shown inline in stream order — after the work
    * already done, before what it triggers. Held as the raw [[Inbox]] so the
    * renderer picks the right marker (cyan `›` for a steer, dim `◆` for a
    * notice). Inert across reveal/settle (it is already fully shown) and carried
    * into the committed transcript like any other block. */
  case Injected(item: Inbox)

object Block:
  /** A streaming answer whose Markdown is parsed up to its currently-revealed
    * text. Feed the existing document forward with [[advanceAnswer]] rather than
    * rebuilding from here, so parsing stays incremental. */
  def answer(typed: Typewriter): Answer =
    Answer(typed, MarkdownDocument.empty.feedTo(typed.visible))

  /** A fully-shown answer (committed reply / resumed history): the whole text is
    * parsed at once. */
  def shownAnswer(text: String): Answer =
    Answer(Typewriter.shown(text), MarkdownDocument.parse(text))

  /** Append newly-arrived text. The reveal (and thus the parsed document) is
    * unchanged — only the backlog grows; the document catches up as the reveal
    * advances. */
  def appendAnswer(a: Answer, text: String): Answer =
    a.copy(typed = a.typed.append(text))

  /** Reveal a little more and feed the document up to the new visible text. */
  def advanceAnswer(a: Answer): Answer =
    val t = a.typed.advance
    Answer(t, a.doc.feedTo(t.visible))

  /** Reveal everything at once and finalise the document. */
  def settleAnswer(a: Answer): Answer =
    val t = a.typed.settle
    Answer(t, a.doc.feedTo(t.visible).close())

  /** Finalise an answer's Markdown when its turn commits, so the committed
    * transcript renders without re-parsing the open tail each frame. Non-answer
    * blocks pass through. */
  def closeAnswer(b: Block): Block = b match
    case a: Answer => a.copy(doc = a.doc.close())
    case other     => other

/** A committed entry in the chat transcript. */
enum Entry:
  case User(text: String)

  /** A system notice that was folded into the conversation while idle (it woke
    * the agent). Rendered as a dim `◆`-led interjection, distinct from a user
    * line. (Mid-turn notices appear as an inline [[Block.Injected]] instead.) */
  case System(text: String)

  case Assistant(blocks: Vector[Block])
  case Error(text: String)

  /** A dim marker showing a turn was cut short by the user (`Ctrl+C k`). */
  case Interrupted

  /** A checkpoint where older model context was replaced by `summary`. */
  case ContextCompacted(summary: String)

/** The live countdown shown while a transiently-failed API request waits out
  * its backoff: which attempt just died out of how many the schedule allows,
  * and the wall-clock instant the next attempt fires (so the working line can
  * tick down against [[ChatState.clockMs]]). */
final case class RetryState(attempt: Int, maxAttempts: Int, nextAtMs: Long)

/** What auk is doing right now — drives which animation the view shows. */
enum Phase:
  /** Waiting for the user to type and submit a line. */
  case Idle

  /** Command submitted, no reply event has arrived yet (spinner). */
  case Waiting

  /** A manual context compaction is running. */
  case Compacting

  /** A reply is in the live region, accumulated as ordered [[Block]]s (the last
    * is the one currently growing).
    *
    * `closed` is false while the engine may still deliver deltas. It flips true
    * once the turn's final event has arrived: the blocks then stop changing, but
    * the answer's [[Typewriter]] keeps draining until it has caught up, at which
    * point the turn is committed to the transcript (see
    * [[ChatState.commitIfDrained]]). */
  case Streaming(blocks: Vector[Block], closed: Boolean = false)

/** One selectable model in the model picker, flattened across providers. */
final case class ModelChoice(
    providerName: String, // display, e.g. "OpenRouter"
    providerKey: String,  // value written to config, e.g. "openrouter"
    modelId: String,      // wire id, e.g. "z-ai/glm-5.1"
    modelLabel: String,   // display, e.g. "GLM 5.1"
    contextWindow: Int
)

object ModelChoice:
  def matchesQuery(choice: ModelChoice, query: String): Boolean =
    val terms = searchTerms(query)
    if terms.isEmpty then true
    else
      val fields = Vector(
        choice.modelLabel,
        choice.modelId,
        choice.providerName,
        choice.providerKey,
        choice.contextWindow.toString
      )
      val searchable = fields.mkString(" ")
      val normalized = normalizeSearchText(searchable)
      val compacted = compactSearchText(searchable)
      val compactQuery = compactSearchText(query)

      compactQuery.nonEmpty && compacted.contains(compactQuery) ||
        terms.forall(term => normalized.contains(term) || compacted.contains(term))

  private def searchTerms(query: String): Vector[String] =
    normalizeSearchText(query).split(" ").toVector.filter(_.nonEmpty)

  private def normalizeSearchText(text: String): String =
    text.toLowerCase.map(ch => if ch.isLetterOrDigit then ch else ' ').mkString

  private def compactSearchText(text: String): String =
    text.toLowerCase.filter(_.isLetterOrDigit)

/** The floating panel currently shown over the live region. */
enum Overlay:
  case None
  case KeyBindings
  case DebugInfo
  case ResumeLoading(message: String)
  case SessionPicker(sessions: Vector[SessionSummary], selected: Int)
  case ModelPicker(choices: Vector[ModelChoice], query: String, selected: Int)

  /** The slash-command palette, opened by typing `/` into an empty input. The
    * typed text lives entirely in [[ChatState.input]] — the palette is a pure
    * completion helper that reacts to it. Holds only the `selected` row; the
    * command list itself is owned by the [[ChatApp]] and read live at render /
    * dispatch (as [[WorkflowList]] reads the running runs), so it stays in sync
    * with whatever commands are registered. */
  case SlashPalette(selected: Int)

  /** The workflow menu: pick one of the running `wf.start` runs to view. Holds
    * only the selection index; the run list itself is read live from
    * [[ChatState.activeWorkflows]] at render time, so a finishing run drops out
    * on its own. */
  case WorkflowList(selected: Int)

  /** A single run's forest, keyed by run id (looked up in
    * [[ChatState.activeWorkflows]] each frame). `cursor` selects a node in
    * display order (see [[ChatState.displayNodes]]); the body auto-scrolls at
    * render to keep it visible, and — on a wide terminal — a live tail preview
    * of the selected node's transcript is shown beside the forest. */
  case WorkflowDetail(runId: String, cursor: Int)

  /** The fullscreen transcript of one team member (Enter on the subagent
    * panel), reading its live transcript from `transcripts(("team", memberId))`.
    * `offset` is the same bottom-anchored scroll as [[WorkflowTranscript]]:
    * 0 follows the tail, each unit reveals one older row, upper-clamped at
    * render. Esc returns to the chat with the panel focus restored. */
  case TeamTranscript(memberId: String, offset: Int)

  /** One sub-agent's full transcript, keyed by run + node id (looked up in
    * [[ChatState.transcripts]] each frame). `offset` is a **bottom-anchored**
    * scroll position: the number of rows to reveal above the tail. `offset == 0`
    * means pinned to the tail (following live output); each ↑ moves one row older,
    * ↓ one row newer. Kept viewport-free so the pure update loop can adjust it
    * without knowing the body height: `offset` is only floored at 0 here and is
    * clamped against the content length at render (so a huge offset shows the
    * top). */
  case WorkflowTranscript(runId: String, nodeId: String, offset: Int)

  /** The MCP server inspector (`/mcp`, Ctrl+C s): every configured server with
    * its discovery status. Holds only the selection index; the server list is
    * read live from [[ChatState.mcpServers]] each frame, so a snapshot landing
    * while the panel is open updates it in place. */
  case McpServers(selected: Int)

  /** One MCP server's detail page (Enter on the inspector): identity, command,
    * handshake facts, and its full tool list. Keyed by server name (looked up
    * in [[ChatState.mcpServers]] each frame). Unlike the transcript views the
    * content is a document read from the top, so `offset` is **top-anchored**:
    * 0 shows the page top, each unit hides one more leading row. Same
    * viewport-free contract as the others — floored at 0 in the update loop,
    * upper-clamped against the content length at render. */
  case McpServerDetail(name: String, offset: Int)

/** A drag-selection over the fullscreen transcript, in CONTENT space: absolute
  * laid-line indices (the same line space as [[ChatState.chatScroll]]) and
  * 0-based display-cell columns. `anchor` is where the drag began (the fixed
  * end); `head` is the moving end.
  *
  * `width` is the render width the selection was made at. The highlight is shown
  * and the text extracted ONLY at that width, so a resize simply hides it (and
  * the next press replaces it) — no coordinate remapping is ever needed.
  */
final case class Selection(anchorLine: Int, anchorCol: Int, headLine: Int, headCol: Int, width: Int):
  /** True when anchor and head coincide — a plain click that selects nothing. */
  def isEmpty: Boolean = anchorLine == headLine && anchorCol == headCol

  /** The two endpoints ordered so `start <= end`, comparing line first, then
    * column: `((startLine, startCol), (endLine, endCol))`. */
  def normalized: ((Int, Int), (Int, Int)) =
    val anchor = (anchorLine, anchorCol)
    val head = (headLine, headCol)
    if anchorLine < headLine || (anchorLine == headLine && anchorCol <= headCol) then (anchor, head)
    else (head, anchor)

/** The full immutable state of the TUI.
  *
  * @param inputHistory submitted user inputs, oldest first.
  * @param histNav      cursor into [[inputHistory]]; equal to its size when
  *                     editing a fresh line rather than recalling a past one.
  * @param draft        the in-progress line, stashed while recalling history.
  */
final case class ChatState(
    history: Vector[Entry],
    input: String,
    phase: Phase,
    frame: Int,
    clockMs: Long = 0,
    turnStartMs: Long = 0,
    anchoredOutputTokens: Long = 0,
    anchorChars: Long = 0,
    /** The in-flight round's rewind point: how many live-region blocks existed
      * when its `RoundStart` marker arrived. A `Retrying` truncates the blocks
      * back to it — everything past the mark is the dead attempt's partial
      * output, which the retry re-streams from the start. */
    roundMark: Int = 0,
    /** Set while the turn waits out a retry backoff after a transient API
      * failure: the working line shows its countdown instead of "auk is
      * thinking". Cleared when the next attempt opens (`RoundStart`) or the
      * turn settles. */
    retry: Option[RetryState] = None,
    inputHistory: Vector[String] = Vector.empty,
    histNav: Int = 0,
    draft: String = "",
    cursor: Int = 0,
    overlay: Overlay = Overlay.None,
    transcriptEpoch: Long = 0,
    modelName: String = "",
    contextWindow: Int = 0,
    contextTokens: Long = 0,
    provider: String = "",
    modelId: String = "",
    baseUrl: String = "",
    notices: Vector[String] = Vector.empty,
    /** The live workflow dashboard's URL once its server is up. Not a notice —
      * `o` on the workflow page opens it in the browser. */
    dashboardUrl: Option[String] = None,
    pendingQueue: Vector[Inbox] = Vector.empty,
    activeWorkflows: Vector[(String, Forest)] = Vector.empty,
    transcripts: Map[(String, String), Transcript] = Map.empty, // (runId, nodeId) → transcript
    /** The team roster shown in the subagent panel below the prompt, in creation
      * order (the panel is absent while empty). Replaced wholesale by each
      * [[auk.agent.AgentEvent.Team]] snapshot. */
    team: Vector[TeamMemberView] = Vector.empty,
    /** Every configured MCP server (config order), replaced wholesale by each
      * [[auk.agent.AgentEvent.McpUpdated]] snapshot. Empty when the project
      * configures no servers — the `/mcp` inspector then shows how to add one. */
    mcpServers: Vector[McpServerView] = Vector.empty,
    /** The subagent panel's focus: `Some(i)` while the panel holds the keyboard
      * (↓ on a fresh input line entered it) with member `i` selected; `None`
      * while the input box has focus. */
    teamSel: Option[Int] = None,
    /** The panel's first visible grid ROW while its rows overflow the visible
      * cap — adjusted by [[moveTeamSel]] to keep the selection visible, reset on
      * exit. The unfocused panel always shows from row 0. */
    teamScroll: Int = 0,
    /** The fullscreen chat's vertical scroll position. `None` follows the tail
      * (the newest line pinned just above the bottom stack); `Some(top)` is
      * detached, `top` being the ABSOLUTE index of the first visible line in the
      * laid transcript — the committed lines (the header banner then every
      * finalized entry) followed by the streaming turn's lines. Absolute, not
      * bottom-anchored like [[Overlay.WorkflowTranscript.offset]], so the view
      * does not slide as streamed lines append: committed lines are append-only
      * per (width, epoch) and the streaming turn materializes strictly after
      * them. Kept viewport-free like that offset — floored at 0 in the update
      * loop and upper-clamped at render against the content height, so a stale
      * anchor simply pins at the tail. Reset to `None` only on a transcript-epoch
      * bump (see [[switchedTo]]); the inline render never reads it. */
    chatScroll: Option[Int] = None,
    /** The active fullscreen drag-selection, in content space (see [[Selection]]).
      * `None` when nothing is selected. Set on a left press in the transcript
      * body, extended on drag, finalized (and copied) on release; cleared by a
      * plain click, a press outside the body, or a transcript-epoch bump (see
      * [[switchedTo]]). Only the fullscreen view reads it. */
    selection: Option[Selection] = None,
    /** The copy-feedback chip shown in the fullscreen footer after a drag-selection
      * is copied, e.g. `Some("copied 3 lines")`. Its lifetime is tied to
      * [[selection]] — set only when a copy completes and cleared wherever the
      * selection clears or is replaced, so the invariant holds: `copied` is only
      * ever `Some` while `selection` is `Some`. Never touches [[notices]]. */
    copied: Option[String] = None
):
  def idle: Boolean = phase == Phase.Idle

  /** Record the token usage of the most recently completed LLM round. The input
    * plus output tokens of the latest round approximate how full the context
    * window now is: the next request resends this whole history, so the prompt
    * the model just saw is the best estimate of current context occupancy.
    *
    * Applied after *every* round (on `RoundComplete`), not only at the turn's
    * terminal `Done`, so the gauge tracks occupancy round-by-round through a long
    * agentic turn and stays truthful when a turn is interrupted — no `Done` ever
    * arrives then, yet the last completed round already reported its usage here. */
  def withContextUsage(usage: Option[Usage]): ChatState =
    usage match
      case Some(u) => copy(contextTokens = u.inputTokens + u.outputTokens)
      case None    => this

  /** Percentage of the active model's context window consumed so far, once the
    * window size is known (capped at 100). Reads 0% before any tokens are spent,
    * and is absent only while the active model's window is still unknown. */
  def contextPercentUsed: Option[Int] =
    Option.when(contextWindow > 0):
      math.min(100, math.round(contextTokens * 100.0 / contextWindow).toInt)

  def showKeyBindings: ChatState = copy(overlay = Overlay.KeyBindings)
  def showDebugInfo: ChatState = copy(overlay = Overlay.DebugInfo)
  def hideOverlay: ChatState = copy(overlay = Overlay.None)
  def showResumeLoading(message: String): ChatState =
    copy(overlay = Overlay.ResumeLoading(message))
  def showSessionPicker(sessions: Vector[SessionSummary]): ChatState =
    copy(overlay = Overlay.SessionPicker(sessions, selected = 0))
  def moveSessionSelection(delta: Int): ChatState =
    overlay match
      case Overlay.SessionPicker(sessions, selected) if sessions.nonEmpty =>
        val next = math.max(0, math.min(sessions.length - 1, selected + delta))
        copy(overlay = Overlay.SessionPicker(sessions, next))
      case _ => this
  def selectedSessionId: Option[String] =
    overlay match
      case Overlay.SessionPicker(sessions, selected) => sessions.lift(selected).map(_.id)
      case _                                         => None

  def showModelPicker(choices: Vector[ModelChoice]): ChatState =
    val initial = choices.indexWhere(_.modelLabel == modelName).max(0)
    copy(overlay = Overlay.ModelPicker(choices, query = "", selected = initial))
  def moveModelSelection(delta: Int): ChatState =
    overlay match
      case Overlay.ModelPicker(choices, query, selected) =>
        val filtered = ChatState.filteredModelChoices(choices, query)
        if filtered.isEmpty then copy(overlay = Overlay.ModelPicker(choices, query, selected = 0))
        else
          val next = math.max(0, math.min(filtered.length - 1, selected + delta))
          copy(overlay = Overlay.ModelPicker(choices, query, next))
      case _ => this
  def appendModelSearch(c: Char): ChatState =
    overlay match
      case Overlay.ModelPicker(choices, query, _) =>
        updateModelSearch(choices, query + c)
      case _ => this
  def backspaceModelSearch: ChatState =
    overlay match
      case Overlay.ModelPicker(choices, query, _) if query.nonEmpty =>
        updateModelSearch(choices, query.dropRight(1))
      case _ => this
  def clearModelSearch: ChatState =
    overlay match
      case Overlay.ModelPicker(choices, _, _) =>
        updateModelSearch(choices, "")
      case _ => this
  def selectedModel: Option[ModelChoice] =
    overlay match
      case Overlay.ModelPicker(choices, query, selected) =>
        ChatState.filteredModelChoices(choices, query).lift(selected)
      case _ => None

  private def updateModelSearch(choices: Vector[ModelChoice], query: String): ChatState =
    copy(overlay = Overlay.ModelPicker(choices, query, selected = 0))

  /* ---- Slash-command palette ---- */

  /** Open the slash palette (selection starts at the first row). The typed
    * text stays in [[input]]; this only opens the completion helper. */
  def openSlashPalette: ChatState = copy(overlay = Overlay.SlashPalette(selected = 0))

  /** The current slash-query text — everything in [[input]] after the leading
    * `/`. The palette's filtered list is derived from this (see
    * [[isSlashPrefix]]). */
  def slashQuery: String = if input.startsWith("/") then input.drop(1) else ""

  /** True when [[input]] is a slash command prefix: it starts with `/` and the
    * rest has no whitespace. The palette is open only while this holds (typing
    * a space, or deleting the `/`, closes it — see [[reconcileSlashPalette]]). */
  def isSlashPrefix: Boolean =
    input.startsWith("/") && !slashQuery.exists(_.isWhitespace)

  /** Whether the slash palette overlay is currently open. */
  def slashPaletteOpen: Boolean = overlay match
    case Overlay.SlashPalette(_) => true
    case _                       => false

  /** Post-processing hook run after every event: close the palette when the
    * input is no longer a slash prefix (e.g. a space was typed, or the `/`
    * itself was backspaced). Does NOT reset the selection — that is done
    * explicitly in the `KeyChar`/`Backspace` handlers so arrow navigation and
    * ticks leave it untouched. */
  def reconcileSlashPalette: ChatState =
    overlay match
      case Overlay.SlashPalette(_) if !isSlashPrefix => hideOverlay
      case _                                          => this

  /* ---- Workflow menu (read-only view of the live background runs) ---- */

  /** Open the workflow menu — always the list, for a predictable Enter→detail /
    * Esc→close mental model even with a single run. The run set is read live
    * from [[activeWorkflows]], so an empty list renders its own empty state. */
  def showWorkflowList: ChatState = copy(overlay = Overlay.WorkflowList(selected = 0))

  /** Move the list selection, clamped to the live run count. */
  def moveWorkflowSelection(delta: Int): ChatState =
    overlay match
      case Overlay.WorkflowList(selected) if activeWorkflows.nonEmpty =>
        val next = math.max(0, math.min(activeWorkflows.length - 1, selected + delta))
        copy(overlay = Overlay.WorkflowList(next))
      case _ => this

  /** List → detail for the selected run (no-op if the list is empty). The
    * selection is re-clamped in case the run set shrank since it was set. The
    * node cursor starts at the first node in display order. */
  def openSelectedWorkflow: ChatState =
    overlay match
      case Overlay.WorkflowList(selected) if activeWorkflows.nonEmpty =>
        val idx = math.max(0, math.min(activeWorkflows.length - 1, selected))
        copy(overlay = Overlay.WorkflowDetail(activeWorkflows(idx)._1, cursor = 0))
      case _ => this

  /** Detail → list, restoring the selection to the run we were viewing (or 0 if
    * it has since finished and dropped out). */
  def backToWorkflowList: ChatState =
    overlay match
      case Overlay.WorkflowDetail(runId, _) =>
        copy(overlay = Overlay.WorkflowList(math.max(0, activeWorkflows.indexWhere(_._1 == runId))))
      case _ => this

  /** The forest of a run currently in the panel (live or recently finished). */
  private def forestOf(runId: String): Option[Forest] =
    activeWorkflows.collectFirst { case (id, f) if id == runId => f }

  /** Move the detail node cursor, clamped to the run's display-order nodes.
    * A no-op when the run is missing or has no nodes. */
  def moveWorkflowCursor(delta: Int): ChatState =
    overlay match
      case Overlay.WorkflowDetail(runId, cursor) =>
        forestOf(runId) match
          case Some(forest) =>
            val nodes = ChatState.displayNodes(forest)
            if nodes.isEmpty then this
            else copy(overlay = Overlay.WorkflowDetail(runId, math.max(0, math.min(nodes.length - 1, cursor + delta))))
          case None => this
      case _ => this

  /** Detail → transcript for the node under the cursor, pinned to the tail
    * (`offset == 0`). A no-op when the cursor is out of range or the run has gone. */
  def openSelectedNode: ChatState =
    overlay match
      case Overlay.WorkflowDetail(runId, cursor) =>
        forestOf(runId).flatMap(f => ChatState.displayNodes(f).lift(cursor)) match
          case Some(node) => copy(overlay = Overlay.WorkflowTranscript(runId, node.id, offset = 0))
          case None       => this
      case _ => this

  /** Transcript → detail, restoring the cursor to the node's current display
    * index (0 if the node has since vanished). */
  def backToWorkflowDetail: ChatState =
    overlay match
      case Overlay.WorkflowTranscript(runId, nodeId, _) =>
        val cursor = forestOf(runId).map(f => math.max(0, ChatState.displayNodes(f).indexWhere(_.id == nodeId))).getOrElse(0)
        copy(overlay = Overlay.WorkflowDetail(runId, cursor))
      case _ => this

  /** Adjust the transcript's bottom-anchored scroll `offset` (rows above the
    * tail): a positive `delta` reveals older content, negative moves back toward
    * the tail. Floored at 0 here (following); the upper clamp is applied at render
    * against the content length, so `offset` may grow past the top and simply
    * pins there — matching the old top-anchored "clamp at render" convention. */
  def scrollTranscript(delta: Int): ChatState =
    overlay match
      case Overlay.WorkflowTranscript(runId, nodeId, offset) =>
        copy(overlay = Overlay.WorkflowTranscript(runId, nodeId, math.max(0, offset + delta)))
      case _ => this

  /** Re-pin the transcript view to the tail (`offset = 0`). */
  def followTranscript: ChatState =
    overlay match
      case Overlay.WorkflowTranscript(runId, nodeId, _) =>
        copy(overlay = Overlay.WorkflowTranscript(runId, nodeId, offset = 0))
      case _ => this

  /* ---- Subagent (team) panel ---- */

  /** Fold a roster snapshot in, clamping a live selection to the new length
    * (members are only ever appended today, but the clamp keeps a stale
    * snapshot harmless). */
  def applyTeam(members: Vector[TeamMemberView]): ChatState =
    val sel =
      if members.isEmpty then None
      else teamSel.map(s => math.min(s, members.length - 1))
    copy(team = members, teamSel = sel)

  /** ↓ on a fresh input line: move focus into the subagent panel, selecting the
    * first member. A no-op without members. */
  def enterTeamPanel: ChatState =
    if team.isEmpty then this else copy(teamSel = Some(0), teamScroll = 0)

  /** Return focus to the input box. */
  def exitTeamPanel: ChatState = copy(teamSel = None, teamScroll = 0)

  /** Move the panel selection by `(dCol, dRow)` on a `cols`-wide grid, clamped
    * to the roster; ↑ from the top row exits back to the input. [[teamScroll]]
    * follows the selection so it stays inside a `visRows`-tall window. The
    * geometry comes from the last render (the update loop has no viewport). */
  def moveTeamSel(dCol: Int, dRow: Int, cols: Int, visRows: Int): ChatState =
    teamSel match
      case None => this
      case Some(sel) =>
        val n = team.length
        if n == 0 then exitTeamPanel
        else
          val c = math.max(1, cols)
          if dRow < 0 && sel / c == 0 then exitTeamPanel
          else
            val next = math.max(0, math.min(n - 1, sel + dCol + dRow * c))
            val totalRows = (n + c - 1) / c
            val vis = math.max(1, visRows)
            val nrow = next / c
            val base = math.min(teamScroll, math.max(0, totalRows - vis))
            val scroll =
              if nrow < base then nrow
              else if nrow >= base + vis then nrow - vis + 1
              else base
            copy(teamSel = Some(next), teamScroll = scroll)

  /** Enter on the panel: open the selected member's fullscreen transcript,
    * pinned to the tail. */
  def openSelectedMember: ChatState =
    teamSel.flatMap(team.lift) match
      case Some(m) => copy(overlay = Overlay.TeamTranscript(m.id, offset = 0))
      case None    => this

  /** Esc from the member transcript: back to the chat with the panel focused on
    * that member (index 0 if the roster has since changed shape). */
  def closeTeamTranscript: ChatState =
    overlay match
      case Overlay.TeamTranscript(memberId, _) =>
        val sel = math.max(0, team.indexWhere(_.id == memberId))
        copy(overlay = Overlay.None, teamSel = if team.isEmpty then None else Some(sel))
      case _ => this

  /** Adjust the member transcript's bottom-anchored offset — same semantics as
    * [[scrollTranscript]] (floored at 0 here, upper-clamped at render). */
  def scrollTeamTranscript(delta: Int): ChatState =
    overlay match
      case Overlay.TeamTranscript(id, offset) =>
        copy(overlay = Overlay.TeamTranscript(id, math.max(0, offset + delta)))
      case _ => this

  /** Re-pin the member transcript to the tail. */
  def followTeamTranscript: ChatState =
    overlay match
      case Overlay.TeamTranscript(id, _) => copy(overlay = Overlay.TeamTranscript(id, offset = 0))
      case _ => this

  /* ---- MCP server inspector (read-only view of the host's MCP status) ---- */

  /** Fold a server-status snapshot in. No selection clamp needed: the set of
    * configured servers never changes within a run, only their states do. */
  def applyMcp(servers: Vector[McpServerView]): ChatState = copy(mcpServers = servers)

  /** Open the MCP inspector — always the list, mirroring [[showWorkflowList]]'s
    * Enter→detail / Esc→close mental model. An empty list (no servers
    * configured) renders its own how-to-configure empty state. */
  def showMcpServers: ChatState = copy(overlay = Overlay.McpServers(selected = 0))

  /** Move the list selection, clamped to the configured servers. */
  def moveMcpSelection(delta: Int): ChatState =
    overlay match
      case Overlay.McpServers(selected) if mcpServers.nonEmpty =>
        val next = math.max(0, math.min(mcpServers.length - 1, selected + delta))
        copy(overlay = Overlay.McpServers(next))
      case _ => this

  /** List → detail for the selected server, opened at the page top (a no-op on
    * an empty list). */
  def openSelectedMcpServer: ChatState =
    overlay match
      case Overlay.McpServers(selected) if mcpServers.nonEmpty =>
        val idx = math.max(0, math.min(mcpServers.length - 1, selected))
        copy(overlay = Overlay.McpServerDetail(mcpServers(idx).name, offset = 0))
      case _ => this

  /** Detail → list, restoring the selection to the server we were viewing. */
  def backToMcpList: ChatState =
    overlay match
      case Overlay.McpServerDetail(name, _) =>
        copy(overlay = Overlay.McpServers(math.max(0, mcpServers.indexWhere(_.name == name))))
      case _ => this

  /** Adjust the detail page's top-anchored offset: positive `delta` scrolls
    * toward the page bottom. Floored at 0 here (the top); the upper clamp is
    * applied at render against the content length. */
  def scrollMcpDetail(delta: Int): ChatState =
    overlay match
      case Overlay.McpServerDetail(name, offset) =>
        copy(overlay = Overlay.McpServerDetail(name, math.max(0, offset + delta)))
      case _ => this

  /** Jump the detail page back to its top. */
  def topMcpDetail: ChatState =
    overlay match
      case Overlay.McpServerDetail(name, _) => copy(overlay = Overlay.McpServerDetail(name, offset = 0))
      case _ => this

  /* ---- Line editing. `cursor` is an index in [0, input.length]. ---- */

  /** Insert a character at the cursor. */
  def insert(c: Char): ChatState =
    copy(input = input.take(cursor) + c + input.drop(cursor), cursor = cursor + 1)

  /** Delete the character before the cursor (Backspace). */
  def backspace: ChatState =
    if cursor <= 0 then this
    else copy(input = input.take(cursor - 1) + input.drop(cursor), cursor = cursor - 1)

  /** Delete the character under the cursor (Delete / Ctrl+D). */
  def deleteForward: ChatState =
    if cursor >= input.length then this
    else copy(input = input.take(cursor) + input.drop(cursor + 1))

  def cursorLeft: ChatState = copy(cursor = math.max(0, cursor - 1))
  def cursorRight: ChatState = copy(cursor = math.min(input.length, cursor + 1))

  /** Move to the start of the current logical line (Ctrl+A / Home) — just after
    * the preceding newline, or column 0 on the first line. */
  def cursorHome: ChatState = copy(cursor = input.lastIndexOf('\n', cursor - 1) + 1)

  /** Move to the end of the current logical line (Ctrl+E / End) — just before
    * the next newline, or the end of the input on the last line. */
  def cursorEnd: ChatState =
    val nextNl = input.indexOf('\n', cursor)
    copy(cursor = if nextNl < 0 then input.length else nextNl)

  /** True when the cursor sits on the first logical line of [[input]] — there is
    * no newline before it. Used to decide whether Up edits or recalls history. */
  def onFirstLine: Boolean = input.lastIndexOf('\n', cursor - 1) < 0

  /** True when the cursor sits on the last logical line — no newline at or after
    * it. Used to decide whether Down edits or recalls history. */
  def onLastLine: Boolean = input.indexOf('\n', cursor) < 0

  /** Move the cursor up one logical line, keeping its column where possible
    * (clamped to the shorter line). A no-op on the first line. */
  def cursorUp: ChatState =
    val lineStart = input.lastIndexOf('\n', cursor - 1) + 1
    if lineStart == 0 then this // already on the first line
    else
      val col = cursor - lineStart
      val prevStart = input.lastIndexOf('\n', lineStart - 2) + 1
      val prevLen = (lineStart - 1) - prevStart
      copy(cursor = prevStart + math.min(col, prevLen))

  /** Move the cursor down one logical line, keeping its column where possible
    * (clamped to the shorter line). A no-op on the last line. */
  def cursorDown: ChatState =
    val nextNl = input.indexOf('\n', cursor)
    if nextNl < 0 then this // already on the last line
    else
      val lineStart = input.lastIndexOf('\n', cursor - 1) + 1
      val col = cursor - lineStart
      val nextStart = nextNl + 1
      val nextNlAfter = input.indexOf('\n', nextStart)
      val nextEnd = if nextNlAfter < 0 then input.length else nextNlAfter
      copy(cursor = nextStart + math.min(col, nextEnd - nextStart))

  /** Delete from the cursor to the end of the line (Ctrl+K). */
  def killToEnd: ChatState = copy(input = input.take(cursor))

  /** Delete from the start of the line to the cursor (Ctrl+U). */
  def killToStart: ChatState = copy(input = input.drop(cursor), cursor = 0)

  /** Delete the word before the cursor (Ctrl+W): the run of spaces just left
    * of the cursor, then the non-space word before that. */
  def deleteWordBack: ChatState =
    if cursor <= 0 then this
    else
      var i = cursor
      while i > 0 && input(i - 1) == ' ' do i -= 1
      while i > 0 && input(i - 1) != ' ' do i -= 1
      copy(input = input.take(i) + input.drop(cursor), cursor = i)

  /* ---- History ---- */

  /** Record a submitted line: append it to the transcript and the input
    * history (collapsing an immediate repeat), clear the input, and reset
    * history navigation. Leaves [[phase]] for the caller to set. */
  def submitted(text: String): ChatState =
    val nextInputs =
      if inputHistory.lastOption.contains(text) then inputHistory
      else inputHistory :+ text
    copy(
      history = history :+ Entry.User(text),
      inputHistory = nextInputs,
      histNav = nextInputs.size,
      draft = "",
      input = "",
      cursor = 0
    )

  /** Clear the input and record it for ↑/↓ recall, WITHOUT appending to the
    * transcript — the engine echoes the message back via [[inputsConsumed]] at
    * its correct chronological position (and the right ordering relative to
    * system notices), so the UI renders it from that single source of truth. */
  def clearedInput(text: String): ChatState =
    val nextInputs =
      if inputHistory.lastOption.contains(text) then inputHistory
      else inputHistory :+ text
    copy(inputHistory = nextInputs, histNav = nextInputs.size, draft = "", input = "", cursor = 0)

  /** Recall the previous (older) input, stashing the live draft on the first
    * step back. No-op at the oldest entry or with no history. */
  def recallPrev: ChatState =
    if histNav <= 0 then this
    else
      val stash = if histNav >= inputHistory.size then input else draft
      val pos = histNav - 1
      val recalled = inputHistory(pos)
      copy(input = recalled, cursor = recalled.length, histNav = pos, draft = stash)

  /** Recall the next (newer) input; stepping past the newest restores the
    * stashed draft. No-op while already editing the draft. */
  def recallNext: ChatState =
    if histNav >= inputHistory.size then this
    else
      val pos = histNav + 1
      val recalled = if pos >= inputHistory.size then draft else inputHistory(pos)
      val nav = math.min(pos, inputHistory.size)
      copy(input = recalled, cursor = recalled.length, histNav = nav)

  /* ---- Streaming a reply (driven by the engine's events) ---- */

  /** The blocks accumulated so far this turn (empty before the first event). */
  def streamingBlocks: Vector[Block] = phase match
    case Phase.Streaming(bs, _) => bs
    case _                      => Vector.empty

  /** Total characters of model output streamed so far this turn — the full
    * thinking and answer text plus the tool-call argument JSON, across every
    * block. Counts received text, not yet-revealed text, so it tracks what the
    * model has actually produced. Used to estimate live token throughput on the
    * working indicator (no exact usage is available mid-turn). */
  def streamedOutputChars: Long =
    streamingBlocks.foldLeft(0L): (acc, b) =>
      b match
        case Block.Thinking(typed, _, _) => acc + typed.full.length
        case Block.Answer(typed, _)      => acc + typed.full.length
        case t: Block.Tool               => acc + t.rawArgs.length
        case Block.Injected(_)           => acc // queued input, not model output

  /** Begin a fresh assistant turn: stamp the start clock and clear the live
    * token accounting (the exact-usage anchor and the estimate baseline) so the
    * working indicator measures this turn alone. */
  def startingTurn(now: Long): ChatState =
    copy(turnStartMs = now, clockMs = now, anchoredOutputTokens = 0, anchorChars = 0)

  /** Enter the compacting phase, using the same clock fields as a normal turn so
    * the TUI can render elapsed time with the spinner. */
  def startCompaction(now: Long): ChatState =
    copy(
      phase = Phase.Compacting,
      overlay = Overlay.None,
      turnStartMs = now,
      clockMs = now,
      anchoredOutputTokens = 0,
      anchorChars = 0
    )

  /** Anchor the live token tally to a completed round's exact output tokens.
    * The real figure supersedes that round's character estimate: its output
    * tokens join the running exact total, and the estimate baseline advances to
    * the current streamed length, so only the *next* round's characters are
    * estimated. Between rounds (while tools run) the baseline already equals the
    * streamed length, so the tally rests on the exact anchor. */
  def anchorRoundUsage(usage: Usage): ChatState =
    copy(anchoredOutputTokens = anchoredOutputTokens + usage.outputTokens, anchorChars = streamedOutputChars)

  /** Collapse a still-open trailing thinking block into a fixed duration. Its
    * reveal is settled at once, since the collapsed form shows only the duration,
    * not the text. */
  private def closeThinking(bs: Vector[Block], now: Long): Vector[Block] =
    bs.lastOption match
      case Some(Block.Thinking(typed, start, None)) =>
        bs.init :+ Block.Thinking(typed.settle, start, Some(now - start))
      case _ => bs

  /** Append reasoning text, starting the thinking clock on the first delta. */
  def appendThinking(text: String, now: Long): ChatState =
    val bs = streamingBlocks
    val updated = bs.lastOption match
      case Some(Block.Thinking(typed, start, None)) =>
        bs.init :+ Block.Thinking(typed.append(text), start, None)
      case _ =>
        bs :+ Block.Thinking(Typewriter.empty.append(text), now, None)
    copy(phase = Phase.Streaming(updated))

  /** Append answer text. Reasoning in progress is collapsed first (its duration
    * fixed at `now`), since the model has moved on to answering. */
  def appendReply(text: String, now: Long): ChatState =
    val bs = closeThinking(streamingBlocks, now)
    val updated = bs.lastOption match
      case Some(a: Block.Answer) => bs.init :+ Block.appendAnswer(a, text)
      case _                     => bs :+ Block.answer(Typewriter.empty.append(text))
    copy(phase = Phase.Streaming(updated))

  /** Begin a tool call (the model has started emitting it). Any reasoning in
    * progress is collapsed first. */
  def startTool(id: String, name: String, now: Long): ChatState =
    copy(phase = Phase.Streaming(closeThinking(streamingBlocks, now) :+ Block.Tool(id, name, "")))

  /** Append streamed JSON argument text to the most recent tool call. */
  def appendToolArgs(delta: String): ChatState =
    val bs = streamingBlocks
    val updated = bs.lastOption match
      case Some(t: Block.Tool) => bs.init :+ t.copy(rawArgs = t.rawArgs + delta)
      case _                   => bs
    copy(phase = Phase.Streaming(updated))

  /** Mark the tool call `id` as running (its execution has begun). */
  def startToolRun(id: String, now: Long): ChatState =
    mapTool(id)(_.copy(startedMs = Some(now)))

  /** Fold a running tool's live progress into its block: update the token total
    * when the update carries one, leaving the duration to keep ticking from
    * `startedMs`. The tool stays running (no `elapsedMs`); this only refreshes
    * what's shown mid-flight, e.g. a streaming sub-agent's climbing token count. */
  def progressToolRun(id: String, metadata: Map[String, String]): ChatState =
    mapTool(id)(t => t.copy(tokens = ChatState.totalTokens(metadata).orElse(t.tokens)))

  /** Mark the tool call `id` as finished: freeze its duration and record its
    * result — the output text, error flag, and total tokens if reported. */
  def endToolRun(
      id: String,
      isError: Boolean,
      metadata: Map[String, String],
      output: String,
      now: Long
  ): ChatState =
    mapTool(id) { t =>
      t.copy(
        elapsedMs = t.startedMs.map(now - _).orElse(t.elapsedMs),
        tokens = ChatState.totalTokens(metadata).orElse(t.tokens),
        output = Some(output).filter(_.nonEmpty),
        isError = isError
      )
    }

  /** Apply `f` to the streaming tool block with the given id, if present. */
  private def mapTool(id: String)(f: Block.Tool => Block.Tool): ChatState =
    phase match
      case Phase.Streaming(bs, closed) =>
        copy(phase = Phase.Streaming(
          bs.map {
            case t: Block.Tool if t.id == id => f(t)
            case other                       => other
          },
          closed
        ))
      case _ => this

  /** Fold a workflow orchestration event into the workflow panel. Each run (a
    * background `wf.start`, no longer tied to the eval that launched it) keeps its
    * own forest keyed by run id; `activeWorkflows` holds live *and recently
    * finished* runs, in first-seen order, so a transcript stays readable right
    * after a run settles. `WorkflowFinished` folds like any other event (it settles
    * the run's status); settled runs are then capped by [[capFinishedRuns]].
    *
    * A resumed sub-agent re-runs from scratch, so its interrupted attempt's
    * transcript is dropped before the fresh one streams in (mirrors
    * [[auk.runtime.WorkflowWebServer]]). */
  def applyOrchestration(ev: OrchestrationEvent): ChatState =
    val runId = ev.runId
    val isFinish = ev match
      case _: OrchestrationEvent.WorkflowFinished => true
      case _                                      => false
    activeWorkflows.indexWhere(_._1 == runId) match
      case -1 =>
        // Unknown run: a terminal event for a run we never tracked is ignored (no
        // phantom settled forest); anything else opens the run.
        if isFinish then this
        else copy(activeWorkflows = activeWorkflows :+ (runId -> Forest.empty.update(ev))).capFinishedRuns
      case i =>
        val forest = activeWorkflows(i)._2
        val cleared = forest.restartsInterrupted(ev) match
          case Some(nodeId) => transcripts - ((runId, nodeId))
          case None         => transcripts
        copy(
          activeWorkflows = activeWorkflows.updated(i, runId -> forest.update(ev)),
          transcripts = cleared
        ).capFinishedRuns

  /** Cap the retained settled (Done/Failed) runs at [[ChatState.MaxFinishedRuns]],
    * evicting the oldest (front-most) beyond the cap along with their transcripts.
    * Running/Paused runs are never evicted, and the surviving runs keep their
    * relative order. */
  private def capFinishedRuns: ChatState =
    val settledIdx = activeWorkflows.zipWithIndex.collect {
      case ((_, f), idx) if f.status == RunStatus.Done || f.status == RunStatus.Failed => idx
    }
    if settledIdx.length <= ChatState.MaxFinishedRuns then this
    else
      val evictIdx = settledIdx.dropRight(ChatState.MaxFinishedRuns).toSet
      val evictedRunIds = evictIdx.map(i => activeWorkflows(i)._1)
      copy(
        activeWorkflows = activeWorkflows.zipWithIndex.collect { case (rf, idx) if !evictIdx.contains(idx) => rf },
        transcripts = transcripts.filterNot { case ((rid, _), _) => evictedRunIds.contains(rid) }
      )

  /** Fold a workflow sub-agent transcript delta into its per-node [[Transcript]],
    * keyed by (runId, nodeId) — the exact fold the web dashboard uses. */
  def applyActivity(ev: TranscriptEvent): ChatState =
    val key = (ev.runId, ev.nodeId)
    copy(transcripts = transcripts.updated(key, transcripts.getOrElse(key, Transcript.empty).update(ev)))

  /** The engine signalled the turn is over. Finalize the blocks — collapse any
    * open reasoning, and append the model's final text as an answer if none
    * streamed as deltas — and mark the live region `closed`. The blocks stop
    * changing here, but the answer's reveal may still be catching up; the turn
    * is committed to the transcript later, once [[revealSettled]], by
    * [[commitIfDrained]]. `fallback` is the model's final text, used when no
    * answer streamed as deltas (an endpoint that only delivers the full reply on
    * Done); it animates in just like streamed text would. */
  def finishReply(fallback: String, now: Long): ChatState =
    val blocks0 = closeThinking(streamingBlocks, now)
    val hasAnswer = blocks0.exists { case _: Block.Answer => true; case _ => false }
    val finalBlocks =
      if !hasAnswer && fallback.nonEmpty then blocks0 :+ Block.answer(Typewriter.empty.append(fallback))
      else blocks0
    if finalBlocks.isEmpty then copy(phase = Phase.Idle)
    else copy(phase = Phase.Streaming(finalBlocks, closed = true))

  /** Reveal a little more of every animating answer — one [[Typewriter]] step.
    * Driven by the UI's animation tick; a no-op outside a streaming turn. */
  def advanceReveal: ChatState = phase match
    case Phase.Streaming(blocks, closed) =>
      copy(phase = Phase.Streaming(blocks.map(revealMore), closed))
    case _ => this

  private def revealMore(b: Block): Block = b match
    case a: Block.Answer                => Block.advanceAnswer(a)
    case Block.Thinking(typed, s, None) => Block.Thinking(typed.advance, s, None)
    case other                          => other

  /** True when a `closed` turn's reveal has fully caught up and it is ready to
    * be committed to the transcript. (Collapsed reasoning is already settled, so
    * in practice this waits only on the answer.) */
  def revealSettled: Boolean = phase match
    case Phase.Streaming(blocks, closed) => closed && blocks.forall(blockSettled)
    case _                               => false

  private def blockSettled(b: Block): Boolean = b match
    case Block.Answer(t, _)      => t.settled
    case Block.Thinking(t, _, _) => t.settled
    case _                       => true

  /** Commit a fully-revealed closed turn to the transcript and idle; otherwise
    * leave the state untouched (the reveal is still draining, or no turn is
    * closing). Called after every fold and every tick, so a turn with nothing
    * left to reveal commits at once and a lagging one commits as it catches up. */
  def commitIfDrained: ChatState =
    if revealSettled then commitReply else this

  private def commitReply: ChatState = phase match
    case Phase.Streaming(blocks, _) if blocks.nonEmpty =>
      copy(history = history :+ Entry.Assistant(blocks.map(Block.closeAnswer)), phase = Phase.Idle)
    case _ => copy(phase = Phase.Idle)

  /** A model round is about to be requested (or re-requested after a failure):
    * remember the rewind point for a possible retry, and clear any retry
    * countdown — the backoff wait is over, the new attempt is in flight. */
  def roundStarted: ChatState =
    copy(roundMark = streamingBlocks.length, retry = None)

  /** The in-flight round's request died transiently and will be re-issued after
    * `delayMs`. Rewind the dead attempt's partial blocks to the round marker
    * (the retry re-streams the round from the start — keeping them would render
    * it twice) and start the working line's countdown. A first-round rewind
    * empties the turn entirely, so drop back to the plain waiting spinner. */
  def retrying(attempt: Int, maxAttempts: Int, delayMs: Long, now: Long): ChatState =
    val rewound = phase match
      case Phase.Streaming(_, _) if roundMark == 0 => copy(phase = Phase.Waiting)
      case Phase.Streaming(bs, closed) if bs.length > roundMark =>
        copy(phase = Phase.Streaming(bs.take(roundMark), closed))
      case _ => this
    rewound.copy(retry = Some(RetryState(attempt, maxAttempts, now + delayMs)))

  /** Abort the turn with an error line in the transcript — but first commit
    * whatever streamed, exactly like [[interrupted]], so an API failure late in
    * a long turn doesn't wipe the steps already shown. */
  def failed(message: String): ChatState =
    val committed = phase match
      case Phase.Streaming(blocks, _) if blocks.nonEmpty =>
        copy(history = history :+ Entry.Assistant(blocks.map(settleBlock)))
      case _ => this
    committed.copy(
      history = committed.history :+ Entry.Error(message),
      phase = Phase.Idle,
      overlay = Overlay.None,
      retry = None
    )

  /** Record a sticky system notice (e.g. an MCP config error). It is shown
    * pinned just above the input box rather than appended to the scrolling
    * transcript, so it stays readable. Deduplicated and capped; leaves
    * `phase`/`transcriptEpoch` untouched, and never persisted to a session. */
  def notice(message: String): ChatState =
    if notices.contains(message) then this
    else copy(notices = (notices :+ message).takeRight(4))

  /** The workflow dashboard server came up at `url`. Stored, not announced: the
    * workflow status line hints at `ctrl+c w o`, which opens it. */
  def dashboardReady(url: String): ChatState = copy(dashboardUrl = Some(url))

  /** The engine compacted older model context into `summary`. Show a durable
    * marker in the transcript and reset the context gauge to the engine's
    * `estimatedTokens` for the resulting prompt — system prompt + tool schemas +
    * the compaction message, not the summary text alone — until the next model
    * round reports exact usage. The estimate is the engine's because only it sees
    * all three pieces (see [[auk.agent.AgentEvent.ContextCompacted]]). */
  def contextCompacted(summary: String, estimatedTokens: Long): ChatState =
    copy(
      history = history :+ Entry.ContextCompacted(summary),
      phase = Phase.Idle,
      overlay = Overlay.None,
      contextTokens = estimatedTokens
    )

  /* ---- Steering queue (pending inbox, mirrored from the engine) ---- */

  /** An input arrived while a turn was in flight and is now queued: append it to
    * the pending panel. The engine is the authority on order (including
    * interleaved user messages and system notices), so we just append in the
    * arrival order it echoes. */
  def inputQueued(item: Inbox): ChatState =
    copy(pendingQueue = pendingQueue :+ item)

  /** The engine folded a FIFO prefix of the queue into the conversation. Drop
    * that prefix from the panel and surface the items at their chronological
    * position: leading the turn (as transcript entries) when none has streamed
    * yet, or inline within the live turn (as [[Block.Injected]]) once it has —
    * inline is required because intermediate round `Done`s are held back, so the
    * current turn lives in the live region, not committed history. */
  def inputsConsumed(items: List[Inbox]): ChatState =
    val drained = copy(pendingQueue = pendingQueue.drop(items.size))
    phase match
      case Phase.Streaming(blocks, closed) =>
        drained.copy(phase = Phase.Streaming(blocks ++ items.map(Block.Injected(_)), closed))
      case _ =>
        // Turn start (idle): lead the turn with these entries and show the working
        // indicator at once — submit no longer enters Waiting optimistically.
        // (`turnStartMs` is stamped by the idle→non-idle guard in update.)
        drained.copy(history = history ++ items.map(ChatState.entryFor), phase = Phase.Waiting)

  /** The turn was interrupted: commit whatever streamed so far (settled at once,
    * since there is no more to reveal), append a dim interruption marker, and
    * return to idle. */
  def interrupted: ChatState =
    val committed = phase match
      case Phase.Streaming(blocks, _) if blocks.nonEmpty =>
        copy(history = history :+ Entry.Assistant(blocks.map(settleBlock)))
      case _ => this
    committed.copy(history = committed.history :+ Entry.Interrupted, phase = Phase.Idle, overlay = Overlay.None, retry = None)

  /** Reveal a block's text in full at once (used when committing immediately
    * rather than letting the typewriter drain). */
  private def settleBlock(b: Block): Block = b match
    case a: Block.Answer                   => Block.settleAnswer(a)
    case Block.Thinking(typed, s, None)    => Block.Thinking(typed.settle, s, Some(0L))
    case other                             => other

  /** Replace the visible transcript and input state with a loaded session. */
  def switchedTo(snapshot: SessionSnapshot): ChatState =
    val inputs = ChatState.inputHistoryFrom(snapshot.events)
    copy(
      history = ChatState.historyFrom(snapshot.events),
      input = "",
      phase = Phase.Idle,
      roundMark = 0,
      retry = None,
      inputHistory = inputs,
      histNav = inputs.size,
      draft = "",
      cursor = 0,
      overlay = Overlay.None,
      pendingQueue = Vector.empty,
      // A loaded session has no live runs; drop any panel from the prior session.
      activeWorkflows = Vector.empty,
      transcripts = Map.empty,
      transcriptEpoch = transcriptEpoch + 1,
      // A new transcript replaces the scroll frame of reference; re-pin to the
      // tail (this epoch bump is the only thing that resets the anchor) and drop
      // any drag-selection, whose line indices belong to the old transcript.
      chatScroll = None,
      selection = None,
      copied = None,
      // Restore the gauge from the last reply's persisted usage; if the session
      // predates usage logging the figure stays 0 until the next turn's Done.
      contextTokens = ChatState.contextTokensFrom(snapshot.events).getOrElse(0L)
    )

object ChatState:
  val initial: ChatState =
    ChatState(history = Vector.empty, input = "", phase = Phase.Idle, frame = 0)

  /** How many settled (Done/Failed) workflow runs to keep in the panel so a
    * transcript stays readable after a run finishes; older ones are evicted. */
  val MaxFinishedRuns: Int = 5

  /** A forest's nodes in display order: declared groups first (in declaration
    * order, each group's nodes in declaration order), ungrouped nodes last. The
    * detail view renders — and its cursor indexes — exactly this order. */
  def displayNodes(forest: Forest): Vector[ForestNode] =
    val byGroup = forest.nodes.groupBy(_.group)
    val order: List[Option[String]] =
      forest.groups.map(g => Some(g.id)).toList ++ (if byGroup.contains(None) then List(None) else Nil)
    order.toVector.flatMap(gid => byGroup.getOrElse(gid, Vector.empty))

  def filteredModelChoices(choices: Vector[ModelChoice], query: String): Vector[ModelChoice] =
    choices.filter(ModelChoice.matchesQuery(_, query))

  /** The transcript entry for a queued input folded in at a turn's start (a user
    * message leads like a normal prompt; a system notice as a dim interjection). */
  def entryFor(item: Inbox): Entry = item match
    case Inbox.UserMessage(text)  => Entry.User(text)
    case Inbox.SystemNotice(text) => Entry.System(text)

  /** Sum the input/output token counts from a tool's metadata, if either is
    * present (sub-agents report them; most tools do not). */
  def totalTokens(metadata: Map[String, String]): Option[Long] =
    val in = metadata.get("inputTokens").flatMap(_.toLongOption)
    val out = metadata.get("outputTokens").flatMap(_.toLongOption)
    Option.when(in.isDefined || out.isDefined)(in.getOrElse(0L) + out.getOrElse(0L))

  /** The context occupancy to show on resume, as a forward fold over the log.
    *
    * An *anchor* is a point with a trustworthy size: an assistant reply that
    * carried usage (its input + output is the whole history the model saw), or a
    * compaction checkpoint (its persisted estimate, else an estimate of the
    * summary). The latest anchor is the base figure. But events logged *after* the
    * last anchor were never measured — the trailing user line, the tool results
    * fed back, or a cut-off interrupt-partial reply persisted without usage (see
    * `Engine.reconcileInterrupted`) — and a "last usage wins" reading would drop
    * them, under-counting a session resumed mid-turn. So we add an estimate of
    * each such trailing event on top of the anchor, resetting the trailing tally
    * whenever a new anchor supersedes them.
    *
    * `None` only when no anchor ever appears (sessions logged before usage was
    * persisted), leaving the gauge at zero until the next turn's exact usage. */
  def contextTokensFrom(events: List[SessionEvent]): Option[Long] =
    var anchor: Option[Long] = None
    var trailing: Long = 0L
    events.foreach:
      case SessionEvent.AssistantResponded(r, _) if r.usage.isDefined =>
        anchor = r.usage.map(u => u.inputTokens + u.outputTokens)
        trailing = 0L
      case SessionEvent.ContextCompacted(summary, _, est) =>
        anchor = Some(est.getOrElse(estimatedTokens(summary)))
        trailing = 0L
      case SessionEvent.ToolResultsReceived(results, _) if anchor.isDefined =>
        trailing += results.map(r => estimatedTokens(r.content)).sum
      case SessionEvent.UserSubmitted(text) if anchor.isDefined =>
        trailing += estimatedTokens(text)
      case SessionEvent.AssistantResponded(r, _) if anchor.isDefined =>
        // A usage-free reply after an anchor is the persisted interrupt-partial;
        // its text is in context but was never measured.
        trailing += estimatedTokens(r.message.text)
      case _ => ()
    anchor.map(_ + trailing)

  /** A rough token count for `text`, delegating to the shared CJK-aware
    * [[auk.agent.TokenEstimate]] so the gauge estimates identically wherever it
    * has no exact usage figure (post-compaction checkpoints, resumed sessions'
    * trailing events). */
  def estimatedTokens(text: String): Long =
    TokenEstimate.estimate(text)

  def inputHistoryFrom(events: List[SessionEvent]): Vector[String] =
    events.collect { case SessionEvent.UserSubmitted(text) => text }.toVector

  def historyFrom(events: List[SessionEvent]): Vector[Entry] =
    // Tool results arrive in a later event than the calls they answer, so
    // collect them up front and attach each to its call's block by id.
    val results: Map[String, Content.ToolResult] =
      events
        .collect { case SessionEvent.ToolResultsReceived(rs, _) => rs }
        .flatten
        .map(r => r.toolUseId -> r)
        .toMap
    events.flatMap:
      case SessionEvent.UserSubmitted(text) => Some(Entry.User(text))
      case SessionEvent.AssistantResponded(response, _) =>
        val blocks = response.message.content.flatMap:
          case Content.Text(text) if text.nonEmpty =>
            Some(Block.shownAnswer(text))
          case Content.Thinking(text, _) if text.nonEmpty =>
            Some(Block.Thinking(Typewriter.shown(text), startedMs = 0L, durationMs = Some(0L)))
          case Content.Reasoning(blocks) =>
            val text = blocks.flatMap(_.displayText).mkString
            Option.when(text.nonEmpty)(Block.Thinking(Typewriter.shown(text), startedMs = 0L, durationMs = Some(0L)))
          case Content.ToolUse(id, name, input) =>
            val result = results.get(id)
            Some(Block.Tool(
              id,
              name,
              input,
              elapsedMs = Some(0L),
              output = result.map(_.content).filter(_.nonEmpty),
              isError = result.exists(_.isError)
            ))
          case _ =>
            None
        Option.when(blocks.nonEmpty)(Entry.Assistant(blocks.toVector))
      case SessionEvent.ToolResultsReceived(_, _) => None
      case SessionEvent.Interrupted               => Some(Entry.Interrupted)
      case SessionEvent.ApiErrored(message)       => Some(Entry.Error(s"⚠ $message"))
      case SessionEvent.SystemNotice(text)        => Some(Entry.System(text))
      case SessionEvent.ContextCompacted(summary, _, _) => Some(Entry.ContextCompacted(summary))
    .toVector

/** Messages that drive the Elm-style update loop. */
enum Event:
  case KeyChar(c: Char)
  case ShowKeyBindings
  case HideOverlay
  case RunCommand(key: String)
  case SessionPickerUp
  case SessionPickerDown
  case ResumeSelected
  case ModelPickerUp
  case ModelPickerDown
  case ModelPickerSearchChar(c: Char)
  case ModelPickerSearchBackspace
  case ModelPickerSearchClear
  case ModelSelected

  /** Slash-command palette: navigate, run or complete the selection. Typing and
    * backspace go through the normal input events (the palette is a pure
    * completion helper that reacts to [[ChatState.input]]). */
  case SlashPaletteUp
  case SlashPaletteDown
  case SlashSelected
  /** Tab: complete the selected command name into the input box without running. */
  case SlashComplete

  /** Workflow overlays: list select / open; detail node-cursor move / open the
    * selected node's transcript / pause / resume; transcript scroll / follow-tail;
    * and back (transcript → detail when a transcript is open, detail → list
    * otherwise). */
  case WorkflowListUp
  case WorkflowListDown
  case WorkflowOpen
  case WorkflowOpenDashboard
  case WorkflowBack
  case WorkflowCursorUp
  case WorkflowCursorDown
  case WorkflowNodeOpen
  /** Scroll the workflow transcript by `delta` rows of its bottom-anchored
    * offset: one row per arrow key, three per wheel notch, a near-page for
    * PageUp/Down. Positive reveals older content, negative moves toward the tail. */
  case WorkflowTranscriptScroll(delta: Int)
  case WorkflowFollow
  case WorkflowPause
  case WorkflowResume

  /** Subagent panel: grid selection moves (deltas are grid-relative; the update
    * loop resolves the column count from the last render's geometry), Enter
    * opening the selected member's fullscreen transcript, and Esc/↑-from-the-top
    * returning focus to the input. Focus ENTERS via [[HistoryNext]]: ↓ on a
    * fresh input line with members live. */
  case TeamMove(dCol: Int, dRow: Int)
  case TeamOpen
  case TeamExit

  /** Member transcript scroll/follow/back — the same bottom-anchored offset
    * semantics as [[WorkflowTranscriptScroll]]/[[WorkflowFollow]]. Back returns
    * to the chat with the panel focused. */
  case TeamTranscriptScroll(delta: Int)
  case TeamTranscriptFollow
  case TeamTranscriptBack

  /** MCP inspector: list select / open the selected server's detail page;
    * detail page scroll (TOP-anchored offset — positive `delta` moves toward
    * the page bottom) / jump back to the top; and back (detail → list). The
    * list itself closes via [[HideOverlay]]. */
  case McpListUp
  case McpListDown
  case McpOpen
  case McpBack
  case McpDetailScroll(delta: Int)
  case McpDetailTop

  /** Fullscreen chat viewport scrolling. [[ChatScroll]] moves by a line delta
    * (wheel notches, ±3); [[ChatScrollPage]] by one page in `direction` (±1),
    * the page height resolved in the update loop from the last render's
    * snapshot; [[ChatFollow]] re-pins to the tail. */
  case ChatScroll(deltaLines: Int)
  case ChatScrollPage(direction: Int)
  case ChatFollow

  /** Fullscreen drag-selection, carrying RAW 1-based SCREEN coordinates; the
    * update loop translates them to content space via the last render's scroll
    * snapshot. [[MouseDown]] starts (or clears) a selection, [[MouseDragTo]]
    * extends the moving head, [[MouseUp]] finalizes it (copying on release). */
  case MouseDown(col: Int, row: Int)
  case MouseDragTo(col: Int, row: Int)
  case MouseUp(col: Int, row: Int)

  case Backspace
  case Newline
  case Submit

  /** Cursor movement and line editing (arrows, Home/End, Ctrl+A/E/K/U/W, Delete). */
  case CursorLeft
  case CursorRight
  case CursorHome
  case CursorEnd
  case DeleteForward
  case KillToEnd
  case KillToStart
  case DeleteWordBack

  /** Recall older / newer submitted input (Up / Down arrows). */
  case HistoryPrev
  case HistoryNext

  /** One engine event, delivered by the runtime's gears-channel subscription
    * (`Sub.onChannel`). Render coalescing comes from the runtime's frame cap, so
    * events no longer need to be drained into a batch. */
  case Inbound1(event: AgentEvent)

  /** The engine channel closed; the subscription stops delivering. */
  case InboundClosed

  /** The while-active animation clock: advances the spinner frame and the live
    * render clock (`clockMs`) so a running tool's duration ticks up. Engine
    * events arrive separately via [[Inbound1]]. */
  case Tick
