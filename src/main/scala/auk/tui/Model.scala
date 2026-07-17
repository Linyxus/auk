package auk.tui

import auk.agent.{AgentEvent, Inbox}
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

  /** One sub-agent's full transcript, keyed by run + node id (looked up in
    * [[ChatState.transcripts]] each frame). `offset` is a **bottom-anchored**
    * scroll position: the number of rows to reveal above the tail. `offset == 0`
    * means pinned to the tail (following live output); each ↑ moves one row older,
    * ↓ one row newer. Kept viewport-free so the pure update loop can adjust it
    * without knowing the body height: `offset` is only floored at 0 here and is
    * clamped against the content length at render (so a huge offset shows the
    * top). */
  case WorkflowTranscript(runId: String, nodeId: String, offset: Int)

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
    pendingQueue: Vector[Inbox] = Vector.empty,
    activeWorkflows: Vector[(String, Forest)] = Vector.empty,
    transcripts: Map[(String, String), Transcript] = Map.empty // (runId, nodeId) → transcript
):
  def idle: Boolean = phase == Phase.Idle

  /** Record the token usage of the most recently completed LLM round. The input
    * plus output tokens of the latest round approximate how full the context
    * window now is: the next request resends this whole history, so the prompt
    * the model just saw is the best estimate of current context occupancy. */
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

  /** Abort the turn with an error line in the transcript. */
  def failed(message: String): ChatState =
    copy(history = history :+ Entry.Error(message), phase = Phase.Idle, overlay = Overlay.None)

  /** Record a sticky system notice (e.g. the workflow dashboard URL). It is shown
    * pinned just above the input box rather than appended to the scrolling
    * transcript, so it stays readable. Deduplicated and capped; leaves
    * `phase`/`transcriptEpoch` untouched, and never persisted to a session. */
  def notice(message: String): ChatState =
    if notices.contains(message) then this
    else copy(notices = (notices :+ message).takeRight(4))

  /** The engine compacted older model context into `summary`. Show a durable
    * marker in the transcript and reset the context gauge to the checkpoint's
    * approximate size until the next model round reports exact usage. */
  def contextCompacted(summary: String): ChatState =
    copy(
      history = history :+ Entry.ContextCompacted(summary),
      phase = Phase.Idle,
      overlay = Overlay.None,
      contextTokens = ChatState.estimatedTokens(summary)
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
    committed.copy(history = committed.history :+ Entry.Interrupted, phase = Phase.Idle, overlay = Overlay.None)

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

  /** The context occupancy to show on resume: the input + output tokens of the
    * last assistant reply that carried a usage figure. That round's input is the
    * whole history resent up to that point, so it is the best estimate of how
    * full the window is — and the next turn's `Done` refreshes it exactly.
    * `None` for sessions logged before usage was persisted. */
  def contextTokensFrom(events: List[SessionEvent]): Option[Long] =
    events.reverseIterator
      .collectFirst:
        case SessionEvent.AssistantResponded(r, _) if r.usage.isDefined =>
          r.usage.map(u => u.inputTokens + u.outputTokens)
        case SessionEvent.ContextCompacted(summary, _) =>
          Some(estimatedTokens(summary))
      .flatten

  def estimatedTokens(text: String): Long =
    math.max(1L, math.round(text.length / 4.0))

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
      case SessionEvent.SystemNotice(text)        => Some(Entry.System(text))
      case SessionEvent.ContextCompacted(summary, _) => Some(Entry.ContextCompacted(summary))
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
  case WorkflowBack
  case WorkflowCursorUp
  case WorkflowCursorDown
  case WorkflowNodeOpen
  case WorkflowTranscriptUp
  case WorkflowTranscriptDown
  case WorkflowFollow
  case WorkflowPause
  case WorkflowResume

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
