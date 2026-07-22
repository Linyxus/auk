package auk.tui

import auk.tui.app.*
import auk.tui.render.{Ansi, Attr, Color, Style, StyledLine}
import gears.async.{ReadableChannel, UnboundedChannel}
import auk.agent.{AgentEvent, TeamMemberView, UserCommand, Inbox}
import auk.workflow.{Forest, ForestNode, NodeStatus, RunStatus, Transcript, TranscriptItem}
import auk.tui.render.Width
import auk.llm.endpoint.{StreamEvent, LLMError}
import auk.llm.provider.Providers
import auk.llm.tools.Json
import auk.tui.markdown.render.{AnswerRenderCache, MarkdownRender}
import auk.session.SessionSummary
import auk.utils.Result

object ChatApp:
  /** A single command, reachable two ways: by its single-char hotkey(s) after
    * `Ctrl+C`, and — when given one or more [[names]] via [[Command.named]] — as a
    * slash command (`/exit`) typed into the input box. Both paths run the exact
    * same [[run]], so gating and effects are shared. */
  final class Command private (
      val keys: Vector[String],
      val names: Vector[String],
      val description: String,
      val run: ChatState => (ChatState, Cmd[Event])
  ):
    /** Also reach this command as `/name` (one or more) from the slash palette.
      * Names are lowercased; the first is the primary one shown in the panel. */
    def named(first: String, more: String*): Command =
      new Command(keys, (first +: more.toVector).map(_.toLowerCase), description, run)

  object Command:
    def apply(key: String, description: String)(run: ChatState => (ChatState, Cmd[Event])): Command =
      new Command(Vector(key), Vector.empty, description, run)

    def apply(keys: Iterable[String], description: String)(run: ChatState => (ChatState, Cmd[Event])): Command =
      new Command(keys.toVector, Vector.empty, description, run)

    def quit(firstKey: String, moreKeys: String*): Command =
      Command(firstKey +: moreKeys.toVector, "exit")(state => (state, Cmd.quit)).named("exit", "quit")

    def resume(commands: UnboundedChannel[UserCommand]): Command =
      Command("r", "resume session") { state =>
        if state.idle then
          (
            state.showResumeLoading("Loading sessions"),
            Cmd.fire(commands.sendImmediately(UserCommand.ListSessions))
          )
        else (state.hideOverlay, Cmd.none)
      }.named("resume")

    def newSession(commands: UnboundedChannel[UserCommand]): Command =
      Command("n", "new session") { state =>
        if state.idle then
          (
            state.showResumeLoading("Starting new session"),
            Cmd.fire(commands.sendImmediately(UserCommand.NewSession))
          )
        else (state.hideOverlay, Cmd.none)
      }.named("new")

    def switchModel(choices: Vector[ModelChoice]): Command =
      Command("m", "switch model") { state =>
        if state.idle then (state.showModelPicker(choices), Cmd.none)
        else (state.hideOverlay, Cmd.none)
      }.named("model")

    def compact(commands: UnboundedChannel[UserCommand]): Command =
      Command("p", "compact context") { state =>
        if state.idle then
          val now = System.currentTimeMillis()
          val next = if state.history.nonEmpty then state.startCompaction(now) else state.hideOverlay
          (next, Cmd.fire(commands.sendImmediately(UserCommand.CompactContext(now))))
        else (state.hideOverlay, Cmd.none)
      }.named("compact")

    /** `Ctrl+C k` while a turn is in flight: signal the engine to cancel it.
      * Gated to normal assistant generation; context compaction has its own
      * non-interruptible phase and ignores duplicate compaction requests. */
    def interrupt(interrupts: UnboundedChannel[Unit]): Command =
      Command("k", "interrupt") { state =>
        state.phase match
          case Phase.Waiting | _: Phase.Streaming => (state.hideOverlay, Cmd.fire(interrupts.sendImmediately(())))
          case _                                  => (state.hideOverlay, Cmd.none)
      }.named("interrupt")

  def defaultCommands(
      commands: UnboundedChannel[UserCommand],
      interrupts: UnboundedChannel[Unit],
      modelChoices: Vector[ModelChoice]
  ): Vector[Command] =
    Vector(
      Command.quit("c", "q"),
      Command.resume(commands),
      Command.newSession(commands),
      Command.switchModel(modelChoices),
      Command.compact(commands),
      Command("w", "view workflows")(state => (state.showWorkflowList, Cmd.none)).named("workflows"),
      Command("b", "debug info")(state => (state.showDebugInfo, Cmd.none)).named("debug"),
      // The escape hatch when the terminal's real grid diverges from the diff
      // model (a terminal bug, a rogue writer on the tty): repaint everything.
      Command("l", "repaint screen")(state => (state, Cmd.refresh)).named("repaint"),
      Command.interrupt(interrupts)
    )

  /** Named commands whose name matches `query` (substring, case-insensitive), in
    * registration order — the slash palette's filtered list. Only commands given a
    * `/name` via [[Command.named]] appear; an empty query lists them all. */
  def slashMatches(commands: Vector[Command], query: String): Vector[Command] =
    val q = query.trim.toLowerCase
    commands.filter(_.names.nonEmpty).filter(c => q.isEmpty || c.names.exists(_.contains(q)))

  /** Every model from every catalog provider, flattened for the picker. */
  def catalogChoices: Vector[ModelChoice] =
    Providers.all.toVector.flatMap { p =>
      p.models.map(m => ModelChoice(p.name, p.name.toLowerCase, m.id, m.name, m.contextWindow))
    }

  /** Columns a runaway indent must leave for content, so deeply-indented text
    * never wraps into a useless sliver (the indent is clamped to `width - this`). */
  private val WrapMinContent = 8

  /** Greedy word-wrap `text` to `width` display columns: split on `\n` first, then
    * pack space-separated tokens, hard-breaking any token wider than the width.
    * Display-width aware (transcripts contain CJK), so a wrapped line never
    * overflows its column. Empty text → one empty line.
    *
    * Each logical line's **leading indentation is preserved** and re-applied to
    * every row it wraps into, so printed code keeps its shape; an indent deeper
    * than `width - [[WrapMinContent]]` is clamped to that so content still gets a
    * usable column. Runs of spaces *within* a line collapse to one (word wrapping),
    * which is fine for prose and code alike. */
  def wrap(text: String, width: Int): Vector[String] =
    val w = math.max(1, width)
    if text.isEmpty then Vector("")
    else text.split("\n", -1).toVector.flatMap(line => wrapLine(line, w))

  private def wrapLine(line: String, width: Int): Vector[String] =
    if line.isEmpty then Vector("")
    else
      // Split off the leading-space run and re-apply it to every wrapped row.
      val indentLen = line.indexWhere(_ != ' ') match
        case -1 => line.length // an all-space line
        case i  => i
      val prefixLen = if indentLen <= width - WrapMinContent then indentLen else math.max(0, width - WrapMinContent)
      val prefix = " " * prefixLen
      val contentWidth = math.max(1, width - prefixLen)
      wrapTokens(line.drop(indentLen), contentWidth).map(prefix + _)

  /** Greedy token packing at `width` columns: space-separated tokens are packed
    * left to right (collapsing internal space runs), hard-breaking any token wider
    * than the width. Empty text → one empty row. */
  private def wrapTokens(text: String, width: Int): Vector[String] =
    val out = Vector.newBuilder[String]
    val cur = new StringBuilder
    var curW = 0
    def flush(): Unit =
      out += cur.toString
      cur.setLength(0)
      curW = 0
    for token <- text.split(" ", -1) do
      // A token wider than the width is hard-broken; each full chunk is emitted
      // on its own line and the last continues as the current line.
      val pieces = hardChunks(token, width)
      pieces.zipWithIndex.foreach: (piece, pi) =>
        val pw = Width.stringWidth(piece)
        if curW == 0 then { cur.append(piece); curW = pw }
        else if pi > 0 then { flush(); cur.append(piece); curW = pw }
        else if curW + 1 + pw <= width then { cur.append(' ').append(piece); curW += 1 + pw }
        else { flush(); cur.append(piece); curW = pw }
    flush()
    out.result()

  /** Break `s` into pieces each at most `width` display columns wide (CJK-aware). */
  private def hardChunks(s: String, width: Int): Vector[String] =
    if s.isEmpty then Vector.empty
    else
      val out = Vector.newBuilder[String]
      val cur = new StringBuilder
      var curW = 0
      var i = 0
      while i < s.length do
        val cp = s.codePointAt(i)
        val cw = Width.displayWidth(cp)
        if curW > 0 && curW + cw > width then
          out += cur.toString
          cur.setLength(0)
          curW = 0
        cur.append(new String(Character.toChars(cp)))
        curW += cw
        i += Character.charCount(cp)
      if cur.nonEmpty then out += cur.toString
      out.result()

/** An animated chat-style TUI for auk, driven by the engine channels.
  *
  * A pure [[auk.tui.app.App]]: it consumes the engine's event stream (via a
  * gears-channel subscription) and pushes user commands back. The streaming
  * assistant turn, input box, and footer form the live region; finalized
  * transcript entries are committed once into the terminal's scrollback.
  *
  * @param events   agent events from the engine (engine → UI), subscribed to
  *                 as a first-class event source.
  * @param commands user commands to the engine (UI → engine); concrete
  *                 `UnboundedChannel` so we can `sendImmediately` from a `Cmd`.
  */
final class ChatApp(
    events: ReadableChannel[AgentEvent],
    commands: UnboundedChannel[UserCommand],
    interrupts: UnboundedChannel[Unit],
    inbox: UnboundedChannel[Inbox],
    keyCommands: Vector[ChatApp.Command] = Vector.empty,
    modelName: String = "",
    contextWindow: Int = 0,
    provider: String = "",
    modelId: String = "",
    baseUrl: String = "",
    modelChoices: Vector[ModelChoice] = ChatApp.catalogChoices,
    // Note the default differs from `Tui`/`ChatTui.run` (Fullscreen): the app
    // itself defaults to Inline so the existing view suite and the inline render
    // contract exercise today's behavior without threading a mode everywhere.
    // The product default (Fullscreen) is chosen once, in `ChatTui.run`.
    mode: DisplayMode = DisplayMode.Inline,
    // Sink for a completed drag-selection's text (fullscreen copy-on-release).
    // `ChatTui.run` passes `terminal.copyToClipboard` (an OSC 52 write); the
    // default no-op keeps the view suite and inline mode side-effect free.
    copyToClipboard: String => Unit = _ => ()
) extends App[ChatState, Event]:

  /** Spinner / live-clock cadence while waiting for the first event. */
  private val SpinnerMs: Long = 100

  /** Rough characters-per-token used to estimate live token throughput on the
    * working indicator (no exact usage is available until the turn ends). */
  private val CharsPerToken: Double = 4.0

  /** Typewriter-reveal cadence while a reply is in the live region. Fast enough
    * to look continuous; paired with `Typewriter`'s adaptive drain it yields a
    * smooth ~150 ms catch-up regardless of how the deltas burst in. */
  private val RevealMs: Long = 30

  /** Subagent panel geometry: the visible cap on grid rows (the selection
    * scrolls through the rest), the narrowest a cell may go before the panel
    * drops a column, and the gap between adjacent cells. */
  private val TeamPanelMaxRows = 4
  private val TeamMinCellW = 32
  private val TeamCellGap = 2
  private val registeredKeyCommands: Vector[ChatApp.Command] =
    if keyCommands.isEmpty then ChatApp.defaultCommands(commands, interrupts, modelChoices) else keyCommands
  private val commandByKey: Map[String, ChatApp.Command] =
    registeredKeyCommands.flatMap(command => command.keys.map(key => normalizeCommandKey(key) -> command)).toMap

  /* ---- Elm architecture: init / update / subscriptions / view ---- */

  def init: (ChatState, Cmd[Event]) =
    (
      ChatState.initial.copy(
        modelName = modelName,
        contextWindow = contextWindow,
        provider = provider,
        modelId = modelId,
        baseUrl = baseUrl
      ),
      Cmd.none
    )

  def update(event: Event, state: ChatState): (ChatState, Cmd[Event]) =
    // An input-editing key while the subagent panel holds focus returns focus to
    // the input box first (the panel's key handler falls unhandled keys through
    // to the normal bindings), so typing resumes without an explicit Esc.
    val based = if state.teamSel.isDefined && editsInput(event) then state.exitTeamPanel else state
    val (next, cmd) = updateRaw(event, based)
    val reconciled = next.reconcileSlashPalette
    // Every switch between screens rides a full repaint: if the terminal's grid
    // ever diverged from the diff baseline (a stray tty writer, line noise),
    // the next screen change heals it instead of letting the smear persist.
    if screenOf(reconciled.overlay) != screenOf(state.overlay)
    then (reconciled, Cmd.batch(cmd, Cmd.refresh))
    else (reconciled, cmd)

  /** Which screen a state's overlay shows: -1 for the chat frame (embedded
    * overlays — menus, pickers, the palette — render inside it, so they don't
    * count as a switch), or the overlay's ordinal for the views that render as
    * their own screen (the [[workflowFullscreen]] four). Parameter changes
    * within one screen — a scroll offset, a cursor — never trip a repaint. */
  private def screenOf(overlay: Overlay): Int =
    overlay match
      case Overlay.WorkflowList(_) | Overlay.WorkflowDetail(_, _) |
          Overlay.WorkflowTranscript(_, _, _) | Overlay.TeamTranscript(_, _) =>
        overlay.ordinal
      case _ => -1

  /** Events that edit the input line: a live subagent-panel focus is dropped
    * before these apply (see [[update]]), so a keystroke aimed at the input box
    * always lands there. */
  private def editsInput(event: Event): Boolean =
    event match
      case Event.KeyChar(_) | Event.Backspace | Event.DeleteForward | Event.Newline |
          Event.KillToEnd | Event.KillToStart | Event.DeleteWordBack |
          Event.CursorLeft | Event.CursorRight | Event.CursorHome | Event.CursorEnd |
          Event.Submit =>
        true
      case _ => false

  /** The raw event handler, before slash-palette reconciliation. */
  private def updateRaw(event: Event, state: ChatState): (ChatState, Cmd[Event]) =
    event match
      case Event.ShowKeyBindings => (state.showKeyBindings, Cmd.none)
      case Event.HideOverlay => (state.hideOverlay, Cmd.none)
      case Event.RunCommand(key) =>
        commandByKey.get(normalizeCommandKey(key)) match
          case Some(command) => command.run(state.hideOverlay)
          case None          => (state.hideOverlay, Cmd.none)
      case Event.SessionPickerUp   => (state.moveSessionSelection(-1), Cmd.none)
      case Event.SessionPickerDown => (state.moveSessionSelection(1), Cmd.none)
      case Event.ModelPickerUp     => (state.moveModelSelection(-1), Cmd.none)
      case Event.ModelPickerDown   => (state.moveModelSelection(1), Cmd.none)
      case Event.WorkflowListUp        => (state.moveWorkflowSelection(-1), Cmd.none)
      case Event.WorkflowListDown      => (state.moveWorkflowSelection(1), Cmd.none)
      case Event.WorkflowOpen          => (state.openSelectedWorkflow, Cmd.none)
      // Back steps transcript → detail when a transcript is open, else detail → list.
      case Event.WorkflowBack =>
        state.overlay match
          case _: Overlay.WorkflowTranscript => (state.backToWorkflowDetail, Cmd.none)
          case _                             => (state.backToWorkflowList, Cmd.none)
      case Event.WorkflowCursorUp      => (state.moveWorkflowCursor(-1), Cmd.none)
      case Event.WorkflowCursorDown    => (state.moveWorkflowCursor(1), Cmd.none)
      case Event.WorkflowNodeOpen      => (state.openSelectedNode, Cmd.none)
      // Bottom-anchored offset: ↑ reveals older rows (offset + 1), ↓ moves back
      // toward the live tail (offset - 1, floored at 0).
      case Event.WorkflowTranscriptScroll(delta) => (state.scrollTranscript(delta), Cmd.none)
      case Event.WorkflowFollow         => (state.followTranscript, Cmd.none)
      case Event.WorkflowPause =>
        state.overlay match
          case Overlay.WorkflowDetail(runId, _) => (state, Cmd.fire(commands.sendImmediately(UserCommand.PauseWorkflow(runId))))
          case _                                => (state, Cmd.none)
      case Event.WorkflowResume =>
        state.overlay match
          case Overlay.WorkflowDetail(runId, _) => (state, Cmd.fire(commands.sendImmediately(UserCommand.ResumeWorkflow(runId))))
          case _                                => (state, Cmd.none)

      // Subagent panel. Grid moves resolve their column count from the last
      // render ([[lastTeamCols]]), like the chat page-scroll reads [[lastScroll]].
      case Event.TeamMove(dCol, dRow) => (state.moveTeamSel(dCol, dRow, lastTeamCols, TeamPanelMaxRows), Cmd.none)
      case Event.TeamOpen             => (state.openSelectedMember, Cmd.none)
      case Event.TeamExit             => (state.exitTeamPanel, Cmd.none)
      case Event.TeamTranscriptScroll(delta) => (state.scrollTeamTranscript(delta), Cmd.none)
      case Event.TeamTranscriptFollow => (state.followTeamTranscript, Cmd.none)
      case Event.TeamTranscriptBack   => (state.closeTeamTranscript, Cmd.none)

      // Fullscreen chat scrolling. Distinct event types from the workflow
      // scroll cases above, so their order never shadows either. `bodyHeight`
      // and `maxTop` come from the last render's snapshot (see [[ScrollSnapshot]]).
      case Event.ChatScroll(delta)  => (scrollChat(state, delta), Cmd.none)
      case Event.ChatScrollPage(dir) => (scrollChat(state, dir * math.max(1, lastScroll.bodyHeight - 1)), Cmd.none)
      case Event.ChatFollow          => (state.copy(chatScroll = None), Cmd.none)

      // Fullscreen drag-selection. Screen coordinates are translated to content
      // space against the last render's snapshot (single-fiber precedent, like
      // the scroll cases above). A press starts or clears a selection; a drag
      // moves the head (auto-scrolling at the body edges); a release finalizes it,
      // copying the extracted text on a non-empty selection.
      case Event.MouseDown(col, row) =>
        // A fresh press always clears the previous copy chip (its selection is
        // being replaced or dropped), preserving the `copied`⇒`selection` invariant.
        screenToContent(col, row) match
          case Some((line, c)) =>
            (state.copy(selection = Some(Selection(line, c, line, c, lastScroll.width)), copied = None), Cmd.none)
          case None => (state.copy(selection = None, copied = None), Cmd.none)
      case Event.MouseDragTo(col, row) => (dragSelection(state, col, row), Cmd.none)
      case Event.MouseUp(col, row)     => finishSelection(state, col, row)
      case Event.ModelPickerSearchChar(c) if state.idle =>
        (state.appendModelSearch(c), Cmd.none)
      case Event.ModelPickerSearchBackspace if state.idle =>
        (state.backspaceModelSearch, Cmd.none)
      case Event.ModelPickerSearchClear if state.idle =>
        (state.clearModelSearch, Cmd.none)
      case Event.ModelSelected if state.idle =>
        state.selectedModel match
          case Some(choice) =>
            // Ask the engine to switch live; it confirms via AgentEvent.ModelSwitched.
            (
              state.hideOverlay,
              Cmd.fire(commands.sendImmediately(UserCommand.SwitchModel(choice.providerKey, choice.modelId)))
            )
          case None => (state, Cmd.none)
      case Event.ResumeSelected if state.idle =>
        state.selectedSessionId match
          case Some(id) =>
            (
              state.showResumeLoading("Opening session"),
              Cmd.fire(commands.sendImmediately(UserCommand.ResumeSession(id)))
            )
          case None => (state, Cmd.none)

      // Slash palette: typing/backspace go through normal input events; these
      // handle navigation, running, and completing the selection. Running a
      // command reuses its `run` (the same one `RunCommand` invokes), so gating
      // and effects are identical to the Ctrl-C hotkey path.
      case Event.SlashPaletteUp   => (moveSlashSelection(state, -1), Cmd.none)
      case Event.SlashPaletteDown => (moveSlashSelection(state, 1), Cmd.none)
      case Event.SlashSelected =>
        state.overlay match
          // Enter with nothing typed is a no-op (the palette stays open) — so a
          // bare `/` then Enter never fires the pre-selected first command.
          case Overlay.SlashPalette(_) if state.slashQuery.trim.isEmpty => (state, Cmd.none)
          case Overlay.SlashPalette(selected) =>
            ChatApp.slashMatches(registeredKeyCommands, state.slashQuery).lift(selected) match
              // Clear the typed `/query` from the input (a command dispatch is not a
              // message, so it is not recorded in input history).
              case Some(command) => command.run(state.copy(input = "", cursor = 0).hideOverlay)
              case None          => (state.hideOverlay, Cmd.none)
          case _ => (state, Cmd.none)
      // Tab completes the selected command name into the input box without running.
      case Event.SlashComplete =>
        state.overlay match
          case Overlay.SlashPalette(selected) =>
            ChatApp.slashMatches(registeredKeyCommands, state.slashQuery).lift(selected) match
              case Some(command) =>
                val name = command.names.head
                val completed = s"/$name"
                (state.copy(input = completed, cursor = completed.length), Cmd.none)
              case None => (state, Cmd.none)
          case _ => (state, Cmd.none)

      // Line editing works in every phase — you can compose your next message
      // while a reply is in flight. Only sending (Submit) waits for idle.
      // A `/` typed into an empty input inserts the `/` AND opens the slash
      // palette — the typed text stays in the input box, and the palette is a
      // pure completion helper that reacts to it.
      case Event.KeyChar('/') if state.input.isEmpty && state.overlay == Overlay.None =>
        (state.insert('/').openSlashPalette, Cmd.none)
      // Typing while the palette is open edits the input AND resets the
      // selection to the first row (the filtered list changed).
      case Event.KeyChar(c) if state.slashPaletteOpen =>
        (state.insert(c).copy(overlay = Overlay.SlashPalette(0)), Cmd.none)
      case Event.KeyChar(c)     => (state.insert(c), Cmd.none)
      // Backspace while the palette is open edits the input and resets the
      // selection. Reconciliation closes the palette if the `/` itself is gone.
      case Event.Backspace if state.slashPaletteOpen =>
        (state.backspace.copy(overlay = Overlay.SlashPalette(0)), Cmd.none)
      case Event.Backspace      => (state.backspace, Cmd.none)
      case Event.DeleteForward  => (state.deleteForward, Cmd.none)
      case Event.Newline        => (state.insert('\n'), Cmd.none)
      case Event.CursorLeft     => (state.cursorLeft, Cmd.none)
      case Event.CursorRight    => (state.cursorRight, Cmd.none)
      case Event.CursorHome     => (state.cursorHome, Cmd.none)
      case Event.CursorEnd      => (state.cursorEnd, Cmd.none)
      case Event.KillToEnd      => (state.killToEnd, Cmd.none)
      case Event.KillToStart    => (state.killToStart, Cmd.none)
      case Event.DeleteWordBack => (state.deleteWordBack, Cmd.none)

      // Enter sends in every phase now: it always queues the line on the inbox
      // and clears the input. The engine decides what happens — start a turn when
      // idle, or queue it (steering) while one runs — and echoes the result back
      // (InputsConsumed / InputQueued), which is what the UI renders. We never
      // optimistically touch the transcript, so interleaved user/system ordering
      // stays the engine's single source of truth.
      case Event.Submit if state.input.trim.nonEmpty =>
        val text = state.input.trim
        // sendImmediately is non-blocking and needs no Async context.
        (state.clearedInput(text), Cmd.fire(inbox.sendImmediately(Inbox.UserMessage(text))))

      // Up/Down move the cursor between the lines of a multi-line draft; only
      // when the cursor is already on the boundary line (top for Up, bottom for
      // Down) do they step through input history. Like all line editing, both
      // work in every phase — the input box behaves identically whether or not
      // Auk is working; only Submit waits for idle.
      case Event.HistoryPrev =>
        if !state.onFirstLine then (state.cursorUp, Cmd.none)
        else (state.recallPrev, Cmd.none)
      case Event.HistoryNext =>
        // ↓ steps: lines of a multi-line draft, then newer history; on a fresh
        // line (nothing newer to recall) it moves focus into the subagent panel
        // when members are live — exactly the position where ↓ was a no-op.
        if !state.onLastLine then (state.cursorDown, Cmd.none)
        else if state.histNav >= state.inputHistory.size && state.team.nonEmpty then
          (state.enterTeamPanel, Cmd.none)
        else (state.recallNext, Cmd.none)

      case Event.Inbound1(agentEvent) =>
        // One engine event: fold it with a fresh clock so a running tool's
        // duration reflects arrival time. `commitIfDrained` lets a turn whose
        // reveal is already caught up (e.g. a short or empty reply) commit at
        // once; a lagging one stays live and the tick loop drains it.
        val now = System.currentTimeMillis()
        val folded = applyAgentEvent(state, agentEvent, now)
        // Submit no longer optimistically enters Waiting, so stamp the turn clock
        // the first time an event moves us out of idle (the turn-start
        // InputsConsumed, which enters Waiting, or the first stream event).
        val started = if state.idle && !folded.idle then folded.startingTurn(now) else folded
        (started.copy(clockMs = now).commitIfDrained, Cmd.none)

      case Event.InboundClosed =>
        // The engine channel closed. Idle => normal shutdown, nothing to do.
        // Mid-turn => the engine stopped without finishing (crash / cancellation);
        // surface it and return to idle rather than spinning forever.
        if state.idle then (state, Cmd.none)
        else (state.failed("⚠ the agent stopped unexpectedly"), Cmd.none)

      case Event.Tick =>
        // Advance the typewriter reveal and commit a closed turn once it has
        // caught up; also bump the spinner frame and live render clock.
        val now = System.currentTimeMillis()
        val next = state.advanceReveal.commitIfDrained.copy(frame = state.frame + 1, clockMs = now)
        (next, Cmd.none)

      case _ => (state, Cmd.none)

  def subscriptions(state: ChatState): Sub[Event] =
    val keys = Sub.onKeyPress { key =>
      state.overlay match
        case Overlay.KeyBindings         => commandOverlayEvent(key)
        case Overlay.DebugInfo           => debugInfoEvent(key)
        case Overlay.SessionPicker(_, _) => sessionPickerEvent(key)
        case Overlay.ModelPicker(_, _, _) => modelPickerEvent(key)
        case Overlay.SlashPalette(_)  => slashPaletteEvent(key)
        case Overlay.WorkflowList(_)     => workflowListEvent(key)
        case Overlay.WorkflowDetail(_, _) => workflowDetailEvent(key)
        case Overlay.WorkflowTranscript(_, _, _) => workflowTranscriptEvent(key)
        case Overlay.TeamTranscript(_, _) => teamTranscriptEvent(key)
        case Overlay.ResumeLoading(_)    => loadingOverlayEvent(key)
        case Overlay.None =>
          if state.teamSel.isDefined then teamPanelEvent(key) else normalKeyEvent(key)
    }
    // Engine events are consumed natively as a gears channel — active in every
    // phase so deltas keep folding. The spinner clock runs while a turn is live,
    // and also while a background workflow is running or a team member is working
    // (so the workflow panel and the subagent badge animate even though the
    // agent itself is idle); a fully idle screen stays a static frame.
    val engine = Sub.onChannel(events)(Event.Inbound1.apply, Event.InboundClosed)
    if state.idle && state.activeWorkflows.isEmpty && !state.team.exists(_.working) then
      Sub.batch(keys, engine)
    else
      // A reply in flight reveals character-by-character (fast cadence); merely
      // waiting on the first event (or animating a background run's panel) only
      // needs the slower spinner cadence.
      val tickMs = state.phase match
        case _: Phase.Streaming => RevealMs
        case _                  => SpinnerMs
      Sub.batch(Sub.time.everyMs(tickMs, Event.Tick), keys, engine)

  /** Map an emacs-style Ctrl chord to a line-editing event. */
  private def ctrlEvent(c: Char): Option[Event] =
    c.toUpper match
      case 'A' => Some(Event.CursorHome)
      case 'E' => Some(Event.CursorEnd)
      case 'K' => Some(Event.KillToEnd)
      case 'U' => Some(Event.KillToStart)
      case 'W' => Some(Event.DeleteWordBack)
      case 'J' => Some(Event.Newline)
      case 'B' => Some(Event.CursorLeft)
      case 'F' => Some(Event.CursorRight)
      case 'D' => Some(Event.DeleteForward)
      case _   => None

  private def normalKeyEvent(key: Key): Option[Event] =
    key match
      case Key.Char(c)   => Some(Event.KeyChar(c))
      case Key.Backspace => Some(Event.Backspace)
      case Key.Delete    => Some(Event.DeleteForward)
      case Key.Enter     => Some(Event.Submit)
      case Key.Newline   => Some(Event.Newline)
      case Key.Up        => Some(Event.HistoryPrev)
      case Key.Down      => Some(Event.HistoryNext)
      case Key.Left      => Some(Event.CursorLeft)
      case Key.Right     => Some(Event.CursorRight)
      case Key.Home      => Some(Event.CursorHome)
      case Key.End       => Some(Event.CursorEnd)
      case Key.Ctrl('C') => Some(Event.ShowKeyBindings)
      case Key.Ctrl(c)   => ctrlEvent(c)
      // Fullscreen chat scrolling: the wheel is primary, PageUp/Down a complement.
      // Inline mode leaves these unbound so native scroll/selection keep working.
      case Key.WheelUp(_, _)   if mode == DisplayMode.Fullscreen => Some(Event.ChatScroll(-3))
      case Key.WheelDown(_, _) if mode == DisplayMode.Fullscreen => Some(Event.ChatScroll(3))
      case Key.PageUp          if mode == DisplayMode.Fullscreen => Some(Event.ChatScrollPage(-1))
      case Key.PageDown        if mode == DisplayMode.Fullscreen => Some(Event.ChatScrollPage(1))
      // Fullscreen left-button drag-selection (button 0 only). Middle/right
      // buttons stay inert; inline mode leaves these unbound so the terminal's
      // native selection keeps working. Shift-drag bypasses mouse reporting
      // entirely, reaching the terminal's own selection.
      case Key.MousePress(0, col, row)   if mode == DisplayMode.Fullscreen => Some(Event.MouseDown(col, row))
      case Key.MouseDrag(0, col, row)    if mode == DisplayMode.Fullscreen => Some(Event.MouseDragTo(col, row))
      case Key.MouseRelease(0, col, row) if mode == DisplayMode.Fullscreen => Some(Event.MouseUp(col, row))
      case _             => None

  /** Interpret the key after Ctrl-C. Unknown keys dismiss the overlay and are
    * swallowed so a failed chord does not edit the prompt — but a stray wheel or
    * click (this menu has nothing to scroll) is ignored, not treated as a failed
    * chord that closes it. */
  private def commandOverlayEvent(key: Key): Option[Event] =
    if isMouseKey(key) then None
    else
      commandKey(key) match
        case Some(key) if commandByKey.contains(normalizeCommandKey(key)) => Some(Event.RunCommand(key))
        case _                                                           => Some(Event.HideOverlay)

  private def sessionPickerEvent(key: Key): Option[Event] =
    key match
      case Key.Up                => Some(Event.SessionPickerUp)
      case Key.Down              => Some(Event.SessionPickerDown)
      case Key.WheelUp(_, _)     => Some(Event.SessionPickerUp)
      case Key.WheelDown(_, _)   => Some(Event.SessionPickerDown)
      case Key.Enter             => Some(Event.ResumeSelected)
      case Key.Esc               => Some(Event.HideOverlay)
      case _                     => None

  private def modelPickerEvent(key: Key): Option[Event] =
    key match
      case Key.Up              => Some(Event.ModelPickerUp)
      case Key.Down            => Some(Event.ModelPickerDown)
      case Key.WheelUp(_, _)   => Some(Event.ModelPickerUp)
      case Key.WheelDown(_, _) => Some(Event.ModelPickerDown)
      case Key.Enter           => Some(Event.ModelSelected)
      case Key.Backspace       => Some(Event.ModelPickerSearchBackspace)
      case Key.Delete          => Some(Event.ModelPickerSearchBackspace)
      case Key.Ctrl('U')       => Some(Event.ModelPickerSearchClear)
      case Key.Char(c)         => Some(Event.ModelPickerSearchChar(c))
      case Key.Esc             => Some(Event.HideOverlay)
      case _                   => None

  /** The slash palette while open: ↑/↓ navigate, Enter runs the selection, Tab
    * completes it into the input box, Esc cancels. EVERYTHING ELSE — typing,
    * backspace, arrows, Ctrl chords — delegates to [[normalKeyEvent]], so the
    * input box keeps working normally while the palette is a pure completion
    * helper. (`Ctrl+C` opening the command menu is suppressed by the same empty-
    * input guard as the normal path, so it does not fire here either.) */
  private def slashPaletteEvent(key: Key): Option[Event] =
    key match
      case Key.Up    => Some(Event.SlashPaletteUp)
      case Key.Down  => Some(Event.SlashPaletteDown)
      case Key.Enter => Some(Event.SlashSelected)
      case Key.Tab   => Some(Event.SlashComplete)
      case Key.Esc   => Some(Event.HideOverlay)
      // The wheel steps the completion selection; page keys have no target here
      // and are swallowed rather than delegated on to the transcript scroll.
      case Key.WheelUp(_, _)         => Some(Event.SlashPaletteUp)
      case Key.WheelDown(_, _)       => Some(Event.SlashPaletteDown)
      case Key.PageUp | Key.PageDown => None
      // Drag-selection is inert while the palette (an overlay) is open; swallow the
      // button/drag keys rather than delegating them to the normal selection path.
      case Key.MousePress(_, _, _) | Key.MouseDrag(_, _, _) | Key.MouseRelease(_, _, _) => None
      case _         => normalKeyEvent(key)

  /** Clamp the slash-palette selection against the live filtered command count
    * (owned here, not in `ChatState`, since the filter runs over the registered
    * commands). The query is derived from [[ChatState.slashQuery]]. */
  private def moveSlashSelection(state: ChatState, delta: Int): ChatState =
    state.overlay match
      case Overlay.SlashPalette(selected) =>
        val n = ChatApp.slashMatches(registeredKeyCommands, state.slashQuery).length
        if n == 0 then state.copy(overlay = Overlay.SlashPalette(0))
        else
          val next = math.max(0, math.min(n - 1, selected + delta))
          state.copy(overlay = Overlay.SlashPalette(next))
      case _ => state

  /** Fold a fullscreen chat scroll by `delta` laid lines against the last
    * render's snapshot: reaching the tail (top `>= maxTop`) re-enters follow mode
    * (`chatScroll = None` — a fixed requirement); otherwise the absolute top is
    * floored at 0 here and upper-clamped again at render. */
  private def scrollChat(state: ChatState, delta: Int): ChatState =
    val top = state.chatScroll.getOrElse(lastScroll.maxTop) + delta
    if top >= lastScroll.maxTop then state.copy(chatScroll = None)
    else state.copy(chatScroll = Some(math.max(0, top)))

  /* ---- Fullscreen drag-selection (translation, extension, copy) ---- */

  /** Translate a raw 1-based SCREEN position to a content-space `(absoluteLine,
    * col)` using the last render's [[ScrollSnapshot]]. Body rows are screen rows
    * `stickyRows + 1 .. stickyRows + bodyHeight`; `line = top + (row - 1 -
    * stickyRows)`, `col = col - 1` (0-based). `Some` only when the row is a body
    * row AND it maps to real content (`line < total`) — a press on the sticky
    * header, the bottom stack, or past the content is `None`. */
  private def screenToContent(col: Int, row: Int): Option[(Int, Int)] =
    val s = lastScroll
    val firstBodyRow = s.stickyRows + 1
    val lastBodyRow = s.stickyRows + s.bodyHeight
    if row < firstBodyRow || row > lastBodyRow then None
    else
      val line = s.top + (row - firstBodyRow)
      if line >= s.total then None
      else Some((line, math.max(0, col - 1)))

  /** The content-space head position for a drag/release at a raw 1-based screen
    * position, translated against the last snapshot and clamped to real content:
    * `line` into `[0, total - 1]`, `col` to `>= 0`. Unlike [[screenToContent]]
    * this never returns `None` — a drag past the body edges still yields a clamped
    * head, letting the selection extend while the edge auto-scroll reveals more. */
  private def clampedHead(col: Int, row: Int): (Int, Int) =
    val s = lastScroll
    val rawLine = s.top + (row - 1 - s.stickyRows)
    val line = math.max(0, math.min(math.max(0, s.total - 1), rawLine))
    (line, math.max(0, col - 1))

  /** Extend the active selection's head to a drag position, then auto-scroll when
    * the drag is at or beyond a body edge (top reveals older lines, bottom newer),
    * so a drag can run past a single screen. A no-op when there is no selection or
    * it was made at a different width (a resize invalidated it). */
  private def dragSelection(state: ChatState, col: Int, row: Int): ChatState =
    state.selection match
      case Some(sel) if sel.width == lastScroll.width =>
        val (line, c) = clampedHead(col, row)
        val moved = state.copy(selection = Some(sel.copy(headLine = line, headCol = c)))
        val s = lastScroll
        if row <= s.stickyRows + 1 then scrollChat(moved, -1)
        else if row >= s.stickyRows + s.bodyHeight then scrollChat(moved, 1)
        else moved
      case _ => state

  /** Finalize a drag on release: move the head like a drag, then either clear a
    * plain click (empty selection, no copy) or extract the selected text, copy it,
    * keep the selection highlighted, and set the footer copy chip. A no-op when
    * there is no selection or it was made at a different width. */
  private def finishSelection(state: ChatState, col: Int, row: Int): (ChatState, Cmd[Event]) =
    state.selection match
      case Some(sel) if sel.width == lastScroll.width =>
        val (line, c) = clampedHead(col, row)
        val finalSel = sel.copy(headLine = line, headCol = c)
        if finalSel.isEmpty then (state.copy(selection = None, copied = None), Cmd.none)
        else
          val text = extractSelection(state, finalSel)
          val nLines = text.count(_ == '\n') + 1
          val noun = if nLines == 1 then "line" else "lines"
          (
            state.copy(selection = Some(finalSel), copied = Some(s"copied $nLines $noun")),
            Cmd.fire(copyToClipboard(text))
          )
      case _ => (state, Cmd.none)

  /** The plain text of a selection, laid at its own `width`: committed lines via
    * the [[transcriptIndex]]/[[laySlice]] machinery, live lines from the streaming
    * turn. The first and last lines are sliced by display cell (end column
    * inclusive of the cell under the cursor); whole middle lines are taken in full.
    * Each line is right-stripped of trailing spaces and the lines joined with
    * `"\n"`. A range past the current content clamps silently. */
  private def extractSelection(state: ChatState, sel: Selection): String =
    val width = sel.width
    val ((startLine, startCol), (endLine, endCol)) = sel.normalized
    val divider = hr('─', FrameBlue)
    val elements = headerBlock +: committedEntries(state, divider)
    transcriptIndex.refresh(elements, state.history, state.transcriptEpoch, width)
    val starts = transcriptIndex.starts
    val committedTotal = transcriptIndex.committedTotal
    val liveLines = Layout.lay(inProgress(state), width)
    val total = committedTotal + liveLines.length
    val a = math.max(0, startLine)
    val b = math.min(total - 1, endLine)
    if b < a then ""
    else
      def plainAt(idx: Int): String =
        if idx < committedTotal then
          laySlice(elements, starts, width, idx, idx + 1).headOption.map(_.plain).getOrElse("")
        else liveLines.lift(idx - committedTotal).map(_.plain).getOrElse("")
      (a to b).map: idx =>
        val full = plainAt(idx)
        val fromCell = if idx == startLine then startCol else 0
        val untilCell = if idx == endLine then endCol + 1 else Width.stringWidth(full)
        rstripSpaces(sliceByCells(full, fromCell, untilCell))
      .mkString("\n")

  /** The substring of `text` covering display cells `[fromCell, untilCell)`. A
    * wide glyph straddling either boundary is included whole; cells past the text's
    * width contribute nothing. Mirrors [[StyledLine.restyleCells]]'s cell walk. */
  private def sliceByCells(text: String, fromCell: Int, untilCell: Int): String =
    if untilCell <= fromCell then ""
    else
      val sb = new StringBuilder
      var cell = 0
      var i = 0
      val n = text.length
      while i < n && cell < untilCell do
        val cp = text.codePointAt(i)
        val w = Width.displayWidth(cp)
        val cc = Character.charCount(cp)
        if cell + math.max(w, 1) > fromCell then sb.append(new String(Character.toChars(cp)))
        cell += w
        i += cc
      sb.toString

  /** Drop trailing ASCII spaces from `s` (so a padded laid line copies clean). */
  private def rstripSpaces(s: String): String =
    var end = s.length
    while end > 0 && s.charAt(end - 1) == ' ' do end -= 1
    s.take(end)

  /** The workflow menu: ↑/↓ (or the wheel) pick a run, Enter opens its detail,
    * Esc closes. */
  private def workflowListEvent(key: Key): Option[Event] =
    key match
      case Key.Up              => Some(Event.WorkflowListUp)
      case Key.Down            => Some(Event.WorkflowListDown)
      case Key.WheelUp(_, _)   => Some(Event.WorkflowListUp)
      case Key.WheelDown(_, _) => Some(Event.WorkflowListDown)
      case Key.Enter           => Some(Event.WorkflowOpen)
      case Key.Esc             => Some(Event.HideOverlay)
      case _                   => None

  /** One run's forest: ↑/↓ (or the wheel) move the node cursor, Enter opens the
    * selected node's transcript, p/r pause/resume, Esc returns to the list. */
  private def workflowDetailEvent(key: Key): Option[Event] =
    key match
      case Key.Up              => Some(Event.WorkflowCursorUp)
      case Key.Down            => Some(Event.WorkflowCursorDown)
      case Key.WheelUp(_, _)   => Some(Event.WorkflowCursorUp)
      case Key.WheelDown(_, _) => Some(Event.WorkflowCursorDown)
      case Key.Enter           => Some(Event.WorkflowNodeOpen)
      case Key.Char('p')       => Some(Event.WorkflowPause)
      case Key.Char('r')       => Some(Event.WorkflowResume)
      case Key.Esc             => Some(Event.WorkflowBack)
      case _                   => None

  /** One node's full transcript: ↑/↓ step one row, the wheel three, PageUp/Down
    * a near-page (against the last render's body height); End or g/G pins to the
    * tail, Esc returns to the detail forest. Older content is "up" (a higher
    * bottom-anchored offset), matching the ±1 arrow steps. */
  private def workflowTranscriptEvent(key: Key): Option[Event] =
    key match
      case Key.Up                    => Some(Event.WorkflowTranscriptScroll(1))
      case Key.Down                  => Some(Event.WorkflowTranscriptScroll(-1))
      case Key.WheelUp(_, _)         => Some(Event.WorkflowTranscriptScroll(3))
      case Key.WheelDown(_, _)       => Some(Event.WorkflowTranscriptScroll(-3))
      case Key.PageUp                => Some(Event.WorkflowTranscriptScroll(transcriptPageStep))
      case Key.PageDown              => Some(Event.WorkflowTranscriptScroll(-transcriptPageStep))
      case Key.End                   => Some(Event.WorkflowFollow)
      case Key.Char('g' | 'G')       => Some(Event.WorkflowFollow)
      case Key.Esc                   => Some(Event.WorkflowBack)
      case _                         => None

  /** A near-page step for the workflow transcript, from the last render's body
    * height (see [[lastTranscriptBody]]) so it tracks the terminal size; one row
    * of overlap is kept, and it never drops below 1. */
  private def transcriptPageStep: Int = math.max(1, lastTranscriptBody - 1)

  /** Keys while the subagent panel holds focus: arrows move the grid selection
    * (↑ from the top row returns to the input — resolved in the update loop),
    * Enter opens the selected member's fullscreen transcript, Esc leaves the
    * panel. Anything else falls through to the normal input bindings; the
    * update loop drops the panel focus on an editing key, so typing resumes
    * seamlessly. */
  private def teamPanelEvent(key: Key): Option[Event] =
    key match
      case Key.Up    => Some(Event.TeamMove(0, -1))
      case Key.Down  => Some(Event.TeamMove(0, 1))
      case Key.Left  => Some(Event.TeamMove(-1, 0))
      case Key.Right => Some(Event.TeamMove(1, 0))
      case Key.Enter => Some(Event.TeamOpen)
      case Key.Esc   => Some(Event.TeamExit)
      case _         => normalKeyEvent(key)

  /** The member transcript mirrors [[workflowTranscriptEvent]]'s scroll keys;
    * Esc returns to the chat with the panel focus restored. */
  private def teamTranscriptEvent(key: Key): Option[Event] =
    key match
      case Key.Up              => Some(Event.TeamTranscriptScroll(1))
      case Key.Down            => Some(Event.TeamTranscriptScroll(-1))
      case Key.WheelUp(_, _)   => Some(Event.TeamTranscriptScroll(3))
      case Key.WheelDown(_, _) => Some(Event.TeamTranscriptScroll(-3))
      case Key.PageUp          => Some(Event.TeamTranscriptScroll(transcriptPageStep))
      case Key.PageDown        => Some(Event.TeamTranscriptScroll(-transcriptPageStep))
      case Key.End             => Some(Event.TeamTranscriptFollow)
      case Key.Char('g' | 'G') => Some(Event.TeamTranscriptFollow)
      case Key.Esc             => Some(Event.TeamTranscriptBack)
      case _                   => None

  private def loadingOverlayEvent(key: Key): Option[Event] =
    key match
      case Key.Esc => Some(Event.HideOverlay)
      case _       => None

  /** The debug panel is read-only: Esc closes it; other keys are swallowed so a
    * stray press neither edits the prompt nor lingers in a half-typed chord. */
  private def debugInfoEvent(key: Key): Option[Event] =
    key match
      case Key.Esc => Some(Event.HideOverlay)
      case _       => None

  private def commandKey(key: Key): Option[String] =
    key match
      case Key.Char(c) => Some(c.toLower.toString)
      case _           => None

  /** Mouse-reporting keys (wheel notches, button press/release/drag) and the page
    * keys — the inputs an overlay that has no scroll target should neither act on
    * nor be dismissed by. */
  private def isMouseKey(key: Key): Boolean =
    key match
      case Key.WheelUp(_, _) | Key.WheelDown(_, _) | Key.MousePress(_, _, _) | Key.MouseRelease(_, _, _) |
          Key.MouseDrag(_, _, _) | Key.PageUp | Key.PageDown =>
        true
      case _ => false

  private def normalizeCommandKey(key: String): String =
    key.toLowerCase

  // Memo of the per-entry committed Elements. `view` runs every dirty frame, but
  // `renderEntry` is a pure function of `(Entry, divider)` and `history` is
  // append-only between `transcriptEpoch` bumps, so re-rendering already-flushed
  // entries every frame is wasted O(history) work (markdown serialize + wrap).
  // We rebuild only the appended tail. The cache holds width-agnostic Elements,
  // NOT laid lines — `Runtime.render` still calls `Layout.lay(_, curWidth)` fresh
  // every frame, so the resize repaint reflows the cached Elements correctly (see
  // the width-agnostic invariant on `Element`). The single render fiber calls
  // `view` (single-threaded JS), so a plain `var` needs no synchronization.
  //
  // Each entry is wrapped in a `MemoNode`: `Layout.lay` then serves a single-slot
  // per-width memo, so the fullscreen frame — which lays overlapping entries every
  // frame to slice the viewport — pays the layout cost once per (entry, width).
  // Harmless inline, where each committed entry is laid exactly once anyway.
  private var cachedEpoch: Long = -1L
  private var cachedEntries: Vector[Element] = Vector.empty

  private def memoized(e: Entry, divider: Element): Element =
    Element.MemoNode(renderEntry(e, divider), LayMemo())

  private def committedEntries(state: ChatState, divider: Element): Vector[Element] =
    val n = state.history.length
    if state.transcriptEpoch != cachedEpoch || n < cachedEntries.length then
      // New transcript (session switch) or a shorter history: rebuild from scratch.
      cachedEpoch = state.transcriptEpoch
      cachedEntries = state.history.map(memoized(_, divider))
    else if n > cachedEntries.length then
      // Steady state: only the appended tail is new.
      cachedEntries = cachedEntries ++ state.history.drop(cachedEntries.length).map(memoized(_, divider))
    cachedEntries

  /* ---- Fullscreen transcript index + scroll snapshot ---- */

  /** A prefix-sum index over the fullscreen transcript's laid lines, so a scroll
    * position maps to the entries it overlaps without laying the whole history.
    * Keyed on `(epoch, width, historyLen)`: a transcript-epoch bump, a width
    * change, or a shrunk history forces a full rebuild; a grown history extends
    * the tail in place. The single render fiber is the only toucher, so plain
    * vars (mirrors [[cachedEntries]]).
    *
    * `elements` is `headerBlock +: committedEntries(...)`, so element 0 is the
    * header banner and element `i (>= 1)` renders `history(i - 1)`. `starts` is
    * the prefix sum of laid heights (length `elements.length + 1`): `starts(i)`
    * is the absolute first line of element `i`, and `starts.last` the whole
    * committed height. `userAt` holds the element indices of [[Entry.User]] rows
    * (ascending); `userFirstLine` their one-line message text (parallel to
    * `userAt`) for the sticky round header. */
  private final class TranscriptIndex:
    private var epoch: Long = -1L
    private var width: Int = -1
    private var len: Int = -1
    var starts: Vector[Int] = Vector(0)
    var userAt: Vector[Int] = Vector.empty
    var userFirstLine: Vector[String] = Vector.empty

    /** The total laid height of the committed transcript (header + all entries). */
    def committedTotal: Int = starts.last

    def refresh(elements: Vector[Element], history: Vector[Entry], epoch: Long, w: Int): Unit =
      val historyLen = history.length
      if epoch != this.epoch || w != this.width || historyLen < this.len then
        val startsB = Vector.newBuilder[Int]
        val userAtB = Vector.newBuilder[Int]
        val lineB = Vector.newBuilder[String]
        startsB += 0
        var acc = 0
        var e = 0
        while e < elements.length do
          acc += Layout.lay(elements(e), w).length
          startsB += acc
          if e >= 1 then
            history(e - 1) match
              case Entry.User(text) => userAtB += e; lineB += flattenLine(text)
              case _                => ()
          e += 1
        starts = startsB.result()
        userAt = userAtB.result()
        userFirstLine = lineB.result()
        this.epoch = epoch; this.width = w; this.len = historyLen
      else if historyLen > this.len then
        // Append-only extension: lay only the new tail elements, extending the
        // prefix sums and the user-row lists.
        var acc = starts.last
        val startsB = Vector.newBuilder[Int]
        val userAtB = Vector.newBuilder[Int]
        val lineB = Vector.newBuilder[String]
        var e = starts.length - 1 // first not-yet-indexed element index
        while e < elements.length do
          acc += Layout.lay(elements(e), w).length
          startsB += acc
          history(e - 1) match
            case Entry.User(text) => userAtB += e; lineB += flattenLine(text)
            case _                => ()
          e += 1
        starts = starts ++ startsB.result()
        userAt = userAt ++ userAtB.result()
        userFirstLine = userFirstLine ++ lineB.result()
        this.len = historyLen

  private val transcriptIndex: TranscriptIndex = new TranscriptIndex

  /** A one-frame snapshot of the fullscreen scroll geometry, written by the view
    * ([[chatFullscreen]]) and read by the update loop ([[scrollChat]]). The pure
    * alternative would thread viewport data through `Sub`/`Runtime`; instead this
    * follows the [[cachedEntries]] single-fiber precedent — the update runs on the
    * same render fiber, at worst one frame stale, and the update floors while the
    * next render clamps, so a stale read only ever costs a frame. */
  private final case class ScrollSnapshot(
      total: Int,
      bodyHeight: Int,
      maxTop: Int,
      top: Int,
      stickyRows: Int,
      width: Int
  )
  private var lastScroll: ScrollSnapshot = ScrollSnapshot(0, 0, 0, 0, 0, 0)

  /** The body height of the last-rendered workflow transcript, so PageUp/Down
    * there can step a near-page without threading the viewport through the key
    * handler (same single-fiber precedent as [[lastScroll]]). */
  private var lastTranscriptBody: Int = 0

  /** The subagent panel's column count from the last render, read by the update
    * loop to turn ↑/↓ into row steps (the pure update has no viewport — the
    * same recorded-geometry idiom as [[lastScroll]]/[[lastTranscriptBody]]). */
  private var lastTeamCols: Int = 1

  /** A user message as a single sticky-header line: newlines flattened to spaces
    * (the model still receives the verbatim text; this is display only). */
  private def flattenLine(text: String): String = text.replace('\n', ' ')

  // Render caches for the streaming turn's answer documents (the glowing last
  // block and any settled answers before it), one per block position. Positions
  // are stable — a turn's block list is append-only — so slot i always sees the
  // same document lineage; a new turn's documents fail the cache's identity
  // check and rebuild, so a stale slot degrades to the uncached cost, never to
  // wrong output. Cleared once the turn commits and the phase returns to Idle.
  private val answerCaches = scala.collection.mutable.ArrayBuffer.empty[AnswerRenderCache]

  private def answerCacheAt(i: Int): AnswerRenderCache =
    while answerCaches.length <= i do answerCaches += AnswerRenderCache()
    answerCaches(i)

  def view(state: ChatState, viewport: Viewport): Screen =
    mode match
      case DisplayMode.Inline => inlineScreen(state, viewport)
      case DisplayMode.Fullscreen =>
        // The whole chat owns the alt screen; the workflow views still take
        // precedence over it. Nothing is committed to native scrollback, so the
        // header banner is the transcript's first line and scrolls away with it.
        val fs = workflowFullscreen(state, viewport).getOrElse(chatFullscreen(state, viewport))
        Screen(Vector.empty, Empty, committedEpoch = state.transcriptEpoch, fullscreen = Some(fs))

  /** Today's inline hybrid screen: the header and every finalized entry printed
    * once into native scrollback, the live region cell-diffed each frame, and the
    * three workflow views still taking over the alt screen when open. */
  private def inlineScreen(state: ChatState, viewport: Viewport): Screen =
    val divider = hr('─', FrameBlue)
    // Committed: the header (printed once) and every finalized transcript entry,
    // each laid out and flushed to native scrollback exactly once.
    val committed: Vector[Element] = headerBlock +: committedEntries(state, divider)
    // Live: the still-changing turn, the input box, and the footer.
    val live: Element = layout(
      emptyHint(state),
      inProgress(state),
      br,
      overlayBlock(state, viewport),
      noticesBlock(state),
      workflowNotice(state),
      queueBlock(state, viewport.width),
      divider,
      prompt(state),
      divider,
      footer(state),
      teamPanel(state, viewport.width)
    )
    // The three workflow views take over the whole screen via the alternate
    // buffer; the inline committed/live are frozen and not painted while one is
    // open (the runtime ignores them when `fullscreen` is set).
    workflowFullscreen(state, viewport) match
      case Some(el) => Screen(committed, live, committedEpoch = state.transcriptEpoch, fullscreen = Some(el))
      case None     => Screen(committed, live, committedEpoch = state.transcriptEpoch)

  /** The fullscreen chat frame: exactly `viewport.rows` pre-laid lines — an
    * optional sticky round header, the scrollable transcript body (the committed
    * entries then the live/streaming turn), and a bottom stack (overlays,
    * notices, the input box, footer) pinned to the screen bottom.
    *
    * The transcript is virtualized through [[transcriptIndex]]: only the entries
    * overlapping the viewport are laid (memo hits), plus the one live turn. The
    * scroll anchor is [[ChatState.chatScroll]]; the geometry the update loop needs
    * is recorded in [[lastScroll]] before returning. */
  private def chatFullscreen(state: ChatState, viewport: Viewport): Element =
    val width = viewport.width
    val rows = viewport.rows
    val divider = hr('─', FrameBlue)
    val elements = headerBlock +: committedEntries(state, divider)
    transcriptIndex.refresh(elements, state.history, state.transcriptEpoch, width)
    val starts = transcriptIndex.starts
    val committedTotal = transcriptIndex.committedTotal

    // The live/streaming turn lives in the scrollable body, laid fresh each frame
    // (its glow/typewriter render is frame-dependent and already answer-cached).
    val liveLines = Layout.lay(inProgress(state), width)
    val total = committedTotal + liveLines.length

    // The bottom stack: today's live region minus the streaming turn (which moved
    // into the body), clipped to the last `rows - 1` lines like the inline
    // `maxLive` clamp — an overlay taller than the screen degrades identically.
    // The footer is split off because its text depends on the scroll geometry we
    // are about to compute, while its line count (always one) does not.
    val preFooter = Layout.lay(
      layout(
        emptyHint(state),
        overlayBlock(state, viewport),
        noticesBlock(state),
        workflowNotice(state),
        queueBlock(state, width),
        divider,
        prompt(state),
        divider
      ),
      width
    )
    // The subagent panel docks below the footer, at the very bottom of the
    // frame; its line count joins the bottom stack's before the body height
    // is derived.
    val teamLines = Layout.lay(teamPanel(state, width), width)
    val maxBottom = math.max(1, rows - 1)
    val bottomCount = math.min(preFooter.length + 1 + teamLines.length, maxBottom)
    val bodyH0 = rows - bottomCount

    // Follow (`None`) pins the tail; a detached anchor is floored at 0 and clamped
    // to the content height.
    def clampTop(bodyH: Int): (Int, Int) =
      val maxTop = math.max(0, total - bodyH)
      val top = state.chatScroll.fold(maxTop)(t => math.min(math.max(0, t), maxTop))
      (top, maxTop)
    val (top0, _) = clampTop(bodyH0)

    // Reserve one row for the sticky round header when the round's user line is
    // above the viewport and the body has room (>= 3 rows). Recompute the anchor
    // once with the reduced height: in follow mode this only moves the top down,
    // which keeps the sticky condition true — so one recompute, no oscillation.
    val stickyText0 = if bodyH0 >= 3 then stickyRound(transcriptIndex, top0) else None
    val stickyRows = if stickyText0.isDefined then 1 else 0
    val bodyH = bodyH0 - stickyRows
    val (top, maxTop) = clampTop(bodyH)
    val stickyText = if stickyRows == 1 then stickyRound(transcriptIndex, top) else None

    lastScroll = ScrollSnapshot(
      total = total,
      bodyHeight = bodyH,
      maxTop = maxTop,
      top = top,
      stickyRows = stickyRows,
      width = width
    )

    // Body: the laid lines at absolute [top, top + bodyH). Committed lines come
    // from the overlapping entries (memo hits, sliced); the live turn's tail
    // follows. When the content is shorter than the body it sits at the top and
    // blank lines pad below, keeping the bottom stack pinned to the screen bottom.
    val visEnd = math.min(total, top + bodyH)
    val committedSlice = laySlice(elements, starts, width, math.min(top, committedTotal), math.min(visEnd, committedTotal))
    val liveSlice = liveLines.slice(math.max(0, top - committedTotal), math.max(0, visEnd - committedTotal))
    val bodyContent = committedSlice ++ liveSlice
    val bodyLines0 =
      if bodyContent.length >= bodyH then bodyContent.take(bodyH)
      else bodyContent ++ Vector.fill(bodyH - bodyContent.length)(StyledLine.empty)

    // Paint the drag-selection where it intersects the visible body, but only at
    // the width it was made at (a resize hides it until the next press replaces
    // it). Each body row's absolute content line is `top + i`; the normalized
    // range decides its highlighted column span (end column inclusive). Whole
    // middle lines cover `0 until line.width`; the sticky row and bottom stack
    // are never touched.
    val bodyLines = state.selection match
      case Some(sel) if sel.width == width =>
        val ((selStartLine, selStartCol), (selEndLine, selEndCol)) = sel.normalized
        bodyLines0.zipWithIndex.map: (line, i) =>
          val abs = top + i
          if abs < selStartLine || abs > selEndLine then line
          else
            val fromCell = if abs == selStartLine then selStartCol else 0
            val untilCell = if abs == selEndLine then selEndCol + 1 else line.width
            StyledLine.restyleCells(line, fromCell, untilCell, _ => SelectionStyle)
      case _ => bodyLines0

    // Emit exactly `stickyRows` lines. When the row was reserved (from `top0`)
    // but the recomputed `top` happens to land on a user line's own start, the
    // round's user line is now the first body line and `stickyRound` yields None;
    // a blank chrome bar keeps the frame's line count exact for that one frame
    // rather than duplicating the visible user line above itself.
    val stickyLines =
      if stickyRows == 0 then Vector.empty
      else
        val content = stickyText.map(t => s" › $t").getOrElse("")
        Layout.lay(barRow(content, StickyStyle, width), width)

    // Footer: today's, plus a `↕ a-b of n` range and the re-follow hint while the
    // transcript is scrolled off its tail.
    val visible = math.max(0, visEnd - top)
    val detached = state.chatScroll.isDefined && top < maxTop
    val footerLines = Layout.lay(fullscreenFooter(state, detached, top, visible, total), width)
    val bottomAll = preFooter ++ footerLines ++ teamLines
    val bottomLines = if bottomAll.length > maxBottom then bottomAll.takeRight(maxBottom) else bottomAll

    Element.RawLines(stickyLines ++ bodyLines ++ bottomLines)

  /** The current round's user-message line for the viewport `top`, when it has
    * scrolled above the top edge — the sticky header text, else `None`. The
    * streaming turn belongs to the round of the last user entry; a `top` still
    * inside the header banner (before any user line) has no round. Shown whenever
    * the round's user line index is `< top` — true both when detached mid-round
    * and while following the tail of a long streaming answer. */
  private def stickyRound(index: TranscriptIndex, top: Int): Option[String] =
    val k =
      if index.userAt.isEmpty then -1
      else if top >= index.committedTotal then index.userAt.length - 1
      else rightmostLE(index.userAt, elementAt(index.starts, top), floor = -1)
    if k < 0 then None
    else
      val u = index.userAt(k)
      if index.starts(u) < top then Some(index.userFirstLine(k)) else None

  /** The element index whose laid range contains the absolute line `top`
    * (`top` in `[0, committedTotal)`): the rightmost element start `<= top`,
    * clamped to a real element index. */
  private def elementAt(starts: Vector[Int], top: Int): Int =
    math.min(rightmostLE(starts, top, floor = 0), starts.length - 2)

  /** The rightmost index `i` into ascending `xs` with `xs(i) <= value`, or
    * `floor` when none qualifies. */
  private def rightmostLE(xs: Vector[Int], value: Int, floor: Int): Int =
    var lo = 0
    var hi = xs.length - 1
    var ans = floor
    while lo <= hi do
      val mid = (lo + hi) >>> 1
      if xs(mid) <= value then { ans = mid; lo = mid + 1 }
      else hi = mid - 1
    ans

  /** Lay the committed lines at absolute indices `[a, b)` by laying only the
    * entries that overlap (memo hits) and slicing to the exact range. */
  private def laySlice(elements: Vector[Element], starts: Vector[Int], width: Int, a: Int, b: Int): Vector[StyledLine] =
    if b <= a then Vector.empty
    else
      val e0 = elementAt(starts, a)
      val base = starts(e0)
      val buf = Vector.newBuilder[StyledLine]
      var acc = base
      var e = e0
      while acc < b && e < elements.length do
        val laid = Layout.lay(elements(e), width)
        buf ++= laid
        acc += laid.length
        e += 1
      buf.result().slice(a - base, b - base)

  /* ---- View helpers ---- */

  /** A dim, collapsed reasoning marker, e.g. "✻ Thought for 3.4s". */
  private def thoughtLabel(millis: Long): String =
    s"✻ Thought for ${oneDecimal((millis + 50) / 100)}s"

  /** The left-bar glyph that marks reasoning and tool-call blocks. */
  private val Bar = "│"

  /** A soft, elegant light blue used to frame the input and user messages. */
  private val FrameBlue: Color = Color.True(135, 206, 235)

  // Constant SGR sequences hoisted out of per-frame builders. Each equals its
  // old inline `.setSequence` expression — byte-identical, just built once.
  // Declared after FrameBlue (and Glow's colour vals) so those init first.
  private val DimSeq: String = Style.Dim.setSequence
  private val WordmarkSeq: String = Style(fg = FrameBlue, attrs = Attr.Bold).setSequence
  private val EvalOkSeq: String = Style.fg(Color.Green).setSequence
  private val EvalErrSeq: String = Style.fg(Color.Red).setSequence
  private val ThinkBarSeq: String = Style.fg(Glow.ThinkBar).setSequence
  private val ThinkNormSeq: String = Style.fg(Glow.ThinkNormal).setSequence

  // Static labels: the rendered ANSI never changes, so cache the `.render`.
  private val PromptArrow: String = Color.Cyan("›").render
  private val YouHeader: Element = Text(s"  ${Color.Cyan("You").style(Style.Bold).render}")
  private val AukHeader: Element = Text(s"  ${Color.Green("Auk").style(Style.Bold).render}")

  private val header: Element =
    val logoRows = Vector(
      "██████████",
      "     ▄██▀",
      "  ▄██▀",
      "██████████",
    )
    val logoColors = Vector(
      Color.True(90, 240, 255),
      Color.True(107, 212, 252),
      Color.True(123, 183, 248),
      Color.True(140, 155, 245),
    )
    val wordmark = Style(fg = Color.Cyan, attrs = Attr.Bold).setSequence
    // The identity column to the right of the logo: the wordmark, then the
    // version and the working directory, dim. Every setSequence re-establishes
    // its style from a reset, so the logo colour never bleeds into the labels.
    val labels = Vector(
      s"${wordmark}Auk",
      s"${DimSeq}v${auk.generated.BuildInfo.version}",
      s"${DimSeq}${tildeify(auk.platform.Platform.cwd())}",
      "",
    )
    val logoWidth = logoRows.map(_.length).max
    val lines = logoRows.lazyZip(logoColors).lazyZip(labels).map: (row, col, label) =>
      val art = s"  ${Style.fg(col).setSequence}$row"
      if label.isEmpty then art
      else art + " " * (logoWidth - row.length + 2) + label
    Text(lines.mkString("\n"))

  /** `path` with a leading `$HOME` shortened to `~`, for compact display. */
  private def tildeify(path: String): String =
    auk.platform.Platform.env.get("HOME") match
      case Some(home) if home.nonEmpty && path == home => "~"
      case Some(home) if home.nonEmpty && path.startsWith(s"$home/") => s"~${path.drop(home.length)}"
      case _ => path

  /** The header committed once at startup: two blank lines of breathing room
    * above the banner, one below. */
  private val headerBlock: Element = layout(br, br, header, br)

  /** The footer's model/context lead — everything before the keyboard hint. Ends
    * with a `· ` separator when non-empty, so a following segment reads cleanly. */
  private def footerLead(state: ChatState): String =
    val prefix = if state.modelName.isEmpty then "" else s"${state.modelName} · "
    val context = state.contextPercentUsed.map(p => s"$p% context used · ").getOrElse("")
    s"  ${prefix}${context}"

  /** The footer's keyboard-hint segment, chosen by phase. (The subagent
    * panel's hints live on its own anchor rule, not here.) */
  private def footerHint(state: ChatState): String =
    state.phase match
      case Phase.Idle       => "ctrl+c or / for commands"
      case Phase.Compacting => "compacting context"
      case _                => "ctrl+c k to interrupt"

  private def footerText(state: ChatState): String =
    s"${footerLead(state)}${footerHint(state)}"

  private def footer(state: ChatState): Element = dim(footerText(state))

  /** The fullscreen copy chip: `✓ copied N lines · ` shown right after the
    * model/context lead while a drag-selection's copy is live. Empty otherwise.
    * Fullscreen only — the inline footer builds from [[footerText]], which omits
    * it. `copied` is `Some` only while `selection` is `Some`, so the chip's
    * lifetime tracks the highlight it belongs to (no timers). */
  private def copiedChip(state: ChatState): String =
    state.copied.map(msg => s"✓ $msg · ").getOrElse("")

  /** The fullscreen footer. A copy chip ([[copiedChip]]) leads right after the
    * model/context lead in both variants when a copy is live. While the transcript
    * is detached (scrolled off its tail), the keyboard-hint segment is REPLACED by
    * a `↕ a-b of n` range and the re-follow hint: that hint is the only actionable
    * thing in this state, so it takes the command hints' place rather than trailing
    * an already-long line where it is the first thing a narrow terminal clips. The
    * model/context lead is kept; the follow-mode footer is otherwise unchanged.
    * Always one line — a plain [[Element.TextNode]] never wraps — so the frame's
    * line count stays exact regardless of the chip. */
  private def fullscreenFooter(state: ChatState, detached: Boolean, top: Int, visible: Int, total: Int): Element =
    val chip = copiedChip(state)
    if !detached then dim(s"${footerLead(state)}$chip${footerHint(state)}")
    else dim(s"${footerLead(state)}$chip↕ ${top + 1}-${top + visible} of $total · scroll to bottom to follow")

  private val OverlayHeaderStyle: Style =
    Style(fg = FrameBlue, bg = Color.Indexed(236), attrs = Attr.Bold)
  private val OverlayBodyStyle: Style =
    Style(fg = Color.White, bg = Color.Indexed(236))
  private val OverlayFrameStyle: Style =
    Style(fg = FrameBlue, bg = Color.Indexed(236), attrs = Attr.Bold)
  private val OverlayMutedStyle: Style =
    Style(fg = Color.Indexed(250), bg = Color.Indexed(236))
  private val OverlaySelectedStyle: Style =
    Style(fg = Color.Black, bg = FrameBlue, attrs = Attr.Bold)

  /** The fullscreen sticky round-header chrome: the soft-blue accent on a subtle
    * dark bg, distinct from transcript body text so the pinned user line reads as
    * a rail rather than as content (mirrors the `barRow`/overlay bg precedent). */
  private val StickyStyle: Style =
    Style(fg = FrameBlue, bg = Color.Indexed(236))

  /** The fullscreen drag-selection highlight: black text on the soft-blue accent,
    * readable and clearly distinct from both transcript body text (default bg) and
    * the sticky round-header bar (blue on dark). */
  private val SelectionStyle: Style =
    Style(fg = Color.Black, bg = FrameBlue)

  // Workflow-forest row styles, all on the overlay's dark bg. Each forest row is
  // a single uniform style picked by node status, so active rows pop and settled
  // ones recede without per-span SGR punching holes in the dark background.
  private val OverlayGroupStyle: Style =
    Style(fg = FrameBlue, bg = Color.Indexed(236), attrs = Attr.Bold)
  private val OverlayDoneStyle: Style =
    Style(fg = Color.Green, bg = Color.Indexed(236))
  private val OverlayFailStyle: Style =
    Style(fg = Color.Red, bg = Color.Indexed(236))
  // Interrupted (paused mid-flight): amber, distinct from a red failure.
  private val OverlayInterruptStyle: Style =
    Style(fg = Color.Yellow, bg = Color.Indexed(236))

  private val KeyBindingsInnerWidth = 46
  private val SessionPickerInnerWidth = 68
  private val KeyColumnWidth = 6

  // Debug panel: a fixed label column with the value beside it. The inner width
  // leaves room for a full endpoint URL; longer values are ellipsis-truncated.
  private val DebugInfoInnerWidth = 48
  private val DebugLabelWidth = 10

  // Model-picker columns. The leading " marker " is 3 cols; with single-space
  // gaps the row is 3 + 18+1 + 11+1 + 26+1 + 7 = 68 = SessionPickerInnerWidth.
  private val ModelNameW = 18
  private val ModelProvW = 11
  private val ModelIdW = 26
  private val ModelCtxW = 7

  private val keyBindingsPanelLines: Vector[Element] =
    val top = s"┌${"─" * KeyBindingsInnerWidth}┐"
    val bottom = s"└${"─" * KeyBindingsInnerWidth}┘"
    val title = framed(" Commands", OverlayHeaderStyle, KeyBindingsInnerWidth)
    val rows = registeredKeyCommands.filter(_.keys.nonEmpty).map { command =>
      framed(keyBindingLine(command.keys.mkString(", "), command.description), OverlayBodyStyle, KeyBindingsInnerWidth)
    }
    Vector(Text(top).style(OverlayFrameStyle), title, framed("", OverlayBodyStyle, KeyBindingsInnerWidth)) ++
      rows :+ Text(bottom).style(OverlayFrameStyle)

  private val keyBindingsPanel: Element =
    layout(keyBindingsPanelLines*)

  private def overlayBlock(state: ChatState, viewport: Viewport): Element =
    overlayElement(state, viewport) match
      case Some(panel) => layout(panel, br)
      case None        => Empty

  private def overlayElement(state: ChatState, viewport: Viewport): Option[Element] =
    state.overlay match
      case Overlay.None =>
        None
      case Overlay.KeyBindings =>
        Some(keyBindingsPanel)
      case Overlay.DebugInfo =>
        Some(debugInfoPanel(state))
      case Overlay.ResumeLoading(message) =>
        Some(resumeLoadingPanel(message))
      case Overlay.SessionPicker(sessions, selected) =>
        Some(sessionPickerPanel(sessions, selected))
      case Overlay.ModelPicker(choices, query, selected) =>
        Some(modelPickerPanel(choices, query, selected))
      case Overlay.SlashPalette(selected) =>
        Some(slashPalettePanel(state.slashQuery, selected))
      // The workflow and team-transcript views are fullscreen (see
      // workflowFullscreen), not inline overlays.
      case Overlay.WorkflowList(_) | Overlay.WorkflowDetail(_, _) | Overlay.WorkflowTranscript(_, _, _)
          | Overlay.TeamTranscript(_, _) =>
        None

  /** The fullscreen (alt-screen) element for the three workflow views, or None
    * for any other overlay. Each builds exactly `viewport.rows` full-bleed lines
    * (header bar, body, footer bar) — no floating frame. */
  private def workflowFullscreen(state: ChatState, viewport: Viewport): Option[Element] =
    state.overlay match
      case Overlay.WorkflowList(selected) =>
        Some(workflowListFullscreen(state.activeWorkflows, selected, state.clockMs, viewport))
      case Overlay.WorkflowDetail(runId, cursor) =>
        Some(workflowDetailFullscreen(runId, state.activeWorkflows, state.transcripts, cursor, state.clockMs, viewport))
      case Overlay.WorkflowTranscript(runId, nodeId, offset) =>
        Some(workflowTranscriptFullscreen(runId, nodeId, state.activeWorkflows, state.transcripts, offset, state.clockMs, viewport))
      case Overlay.TeamTranscript(memberId, offset) =>
        Some(teamTranscriptFullscreen(memberId, state.team, state.transcripts, offset, state.clockMs, viewport))
      case _ => None

  private def keyBindingLine(key: String, action: String): String =
    s" ${padRight(key, KeyColumnWidth)}  $action"

  /** One `label   value` row in the debug panel, sharing the overlay styling and
    * the same fixed-column layout as the key-bindings rows. */
  private def debugRow(label: String, value: String): Element =
    val room = math.max(0, DebugInfoInnerWidth - DebugLabelWidth - 3)
    framed(s" ${padRight(label, DebugLabelWidth)}  ${truncate(value, room)}", OverlayBodyStyle, DebugInfoInnerWidth)

  /** The Ctrl-C b debug overlay: a read-only snapshot of the live model, the
    * provider/endpoint actually in use, and the current context occupancy. */
  private def debugInfoPanel(state: ChatState): Element =
    val used =
      val count = withThousands(state.contextTokens)
      state.contextPercentUsed match
        case Some(p) => s"$count · $p%"
        case None    => count
    val context = if state.contextWindow > 0 then s"${contextLabel(state.contextWindow)} tokens" else "unknown"
    val status = state.phase match
      case Phase.Idle         => "Idle"
      case Phase.Waiting      => "Waiting"
      case Phase.Compacting   => "Compacting"
      case _: Phase.Streaming => "Streaming"
    val rows = Vector(
      framed(" Debug info", OverlayHeaderStyle, DebugInfoInnerWidth),
      framed("", OverlayBodyStyle, DebugInfoInnerWidth),
      debugRow("Model", orUnknown(state.modelName)),
      debugRow("Model ID", orUnknown(state.modelId)),
      debugRow("Provider", orUnknown(state.provider)),
      debugRow("Endpoint", orUnknown(state.baseUrl)),
      debugRow("Context", context),
      debugRow("Used", used),
      debugRow("Messages", state.history.length.toString),
      debugRow("Status", status),
      framed("", OverlayBodyStyle, DebugInfoInnerWidth),
      framed(" Esc to close", OverlayMutedStyle, DebugInfoInnerWidth)
    )
    framedPanel(DebugInfoInnerWidth, rows)

  private def orUnknown(s: String): String = if s.isEmpty then "unknown" else s

  /** Group an integer's digits in threes (e.g. 12345 → "12,345"). Done by hand
    * rather than via `%,d` so it stays locale-independent on Scala.js. */
  private def withThousands(n: Long): String =
    val digits = math.abs(n).toString
    val grouped = digits.reverse.grouped(3).map(_.reverse).toVector.reverse.mkString(",")
    if n < 0 then s"-$grouped" else grouped

  private def resumeLoadingPanel(message: String): Element =
    val rows = Vector(
      framed(" Resume session", OverlayHeaderStyle, SessionPickerInnerWidth),
      framed("", OverlayBodyStyle, SessionPickerInnerWidth),
      framed(s" ${message}...", OverlayMutedStyle, SessionPickerInnerWidth)
    )
    framedPanel(SessionPickerInnerWidth, rows)

  private def sessionPickerPanel(sessions: Vector[SessionSummary], selected: Int): Element =
    val rows =
      if sessions.isEmpty then
        Vector(
          framed(" Resume session", OverlayHeaderStyle, SessionPickerInnerWidth),
          framed("", OverlayBodyStyle, SessionPickerInnerWidth),
          framed(" No saved sessions yet", OverlayMutedStyle, SessionPickerInnerWidth),
          framed(" Press Esc to return", OverlayMutedStyle, SessionPickerInnerWidth)
        )
      else
        val maxVisible = 8
        val start = math.max(0, math.min(selected - maxVisible + 1, sessions.length - maxVisible))
        val visibleSessions = sessions.zipWithIndex.slice(start, start + maxVisible)
        val visible = visibleSessions.map: (session, idx) =>
          val marker = if idx == selected then "›" else " "
          val id = shortId(session.id)
          val age = relativeTime(session.modifiedAtMs)
          val count = s"${session.messageCount} msg"
          val left = s" $marker $id  $age  $count"
          val room = SessionPickerInnerWidth - 2 - left.length
          val preview = truncate(session.preview, math.max(0, room))
          val content = s"$left  $preview"
          val style = if idx == selected then OverlaySelectedStyle else OverlayBodyStyle
          framed(content, style, SessionPickerInnerWidth)
        val range =
          if sessions.length > maxVisible then s"  ${start + 1}-${start + visibleSessions.length} of ${sessions.length}"
          else ""
        Vector(
          framed(" Resume session", OverlayHeaderStyle, SessionPickerInnerWidth),
          framed("", OverlayBodyStyle, SessionPickerInnerWidth)
        ) ++ visible :+
          framed(s" ↑/↓ select  Enter resume  Esc cancel$range", OverlayMutedStyle, SessionPickerInnerWidth)
    framedPanel(SessionPickerInnerWidth, rows)

  /** One model-picker row: a fixed-width, ellipsis-truncated grid so columns
    * stay aligned regardless of how long any field is. Shared by the column
    * header (blank marker) and each model row. */
  private def modelRow(marker: String, name: String, provider: String, id: String, ctx: String): String =
    s" $marker ${cell(name, ModelNameW)} ${cell(provider, ModelProvW)} " +
      s"${cell(id, ModelIdW)} ${cell(ctx, ModelCtxW)}"

  private def modelPickerPanel(choices: Vector[ModelChoice], query: String, selected: Int): Element =
    val title = framed(" Switch model", OverlayHeaderStyle, SessionPickerInnerWidth)
    val search = framed(s" Search: ${truncate(query, SessionPickerInnerWidth - 10)}", OverlayBodyStyle, SessionPickerInnerWidth)
    val filtered = ChatState.filteredModelChoices(choices, query)
    val rows =
      if choices.isEmpty then
        Vector(
          title,
          search,
          framed("", OverlayBodyStyle, SessionPickerInnerWidth),
          framed(" No models configured", OverlayMutedStyle, SessionPickerInnerWidth),
          framed(" Press Esc to return", OverlayMutedStyle, SessionPickerInnerWidth)
        )
      else if filtered.isEmpty then
        Vector(
          title,
          search,
          framed("", OverlayBodyStyle, SessionPickerInnerWidth),
          framed(" No models match", OverlayMutedStyle, SessionPickerInnerWidth),
          framed(" Backspace edit  Esc cancel", OverlayMutedStyle, SessionPickerInnerWidth)
        )
      else
        val header = framed(modelRow(" ", "Model", "Provider", "Model id", "Context"), OverlayMutedStyle, SessionPickerInnerWidth)
        val maxVisible = 10
        val start = math.max(0, math.min(selected - maxVisible + 1, filtered.length - maxVisible))
        val visibleChoices = filtered.zipWithIndex.slice(start, start + maxVisible)
        val visible = visibleChoices.map: (choice, idx) =>
          val marker = if idx == selected then "›" else " "
          val content = modelRow(marker, choice.modelLabel, choice.providerName, choice.modelId, contextLabel(choice.contextWindow))
          val style = if idx == selected then OverlaySelectedStyle else OverlayBodyStyle
          framed(content, style, SessionPickerInnerWidth)
        val range =
          if filtered.length > maxVisible then s"  ${start + 1}-${start + visibleChoices.length} of ${filtered.length}"
          else ""
        Vector(title, search, header) ++ visible :+
          framed(s" Type search  ↑/↓ select  Enter switch  Esc cancel$range", OverlayMutedStyle, SessionPickerInnerWidth)
    framedPanel(SessionPickerInnerWidth, rows)

  /** Column width for the `/name` in the slash palette — fits the longest name
    * (`/interrupt`, `/workflows`) with a column for the description beside it. */
  private val SlashNameWidth = 11

  private def slashRow(marker: String, name: String, description: String): String =
    s" $marker ${cell(name, SlashNameWidth)}  $description"

  /** The slash-command palette: a live-filtered command list (name + summary),
    * the selection marked with `›`. The typed `/query` lives in the input box, so
    * it is not repeated here. Shares the "Commands" framing/width with the Ctrl-C
    * key-bindings panel. */
  private def slashPalettePanel(query: String, selected: Int): Element =
    val W = KeyBindingsInnerWidth
    val matches = ChatApp.slashMatches(registeredKeyCommands, query)
    val title = framed(" Commands", OverlayHeaderStyle, W)
    val rows =
      if matches.isEmpty then
        Vector(
          title,
          framed("", OverlayBodyStyle, W),
          framed(" No commands match", OverlayMutedStyle, W),
          framed(" Esc to cancel", OverlayMutedStyle, W)
        )
      else
        val maxVisible = 10
        val start = math.max(0, math.min(selected - maxVisible + 1, matches.length - maxVisible))
        val visibleMatches = matches.zipWithIndex.slice(start, start + maxVisible)
        val visible = visibleMatches.map: (command, idx) =>
          val marker = if idx == selected then "›" else " "
          val content = slashRow(marker, "/" + command.names.head, command.description)
          val style = if idx == selected then OverlaySelectedStyle else OverlayBodyStyle
          framed(content, style, W)
        val range =
          if matches.length > maxVisible then s"  ${start + 1}-${start + visibleMatches.length} of ${matches.length}"
          else ""
        (title +: visible) :+
          framed(s" ↑/↓ select  Enter run  Tab complete  Esc cancel$range", OverlayMutedStyle, W)
    framedPanel(W, rows)

  private def contextLabel(tokens: Int): String =
    if tokens >= 1_000_000 then f"${tokens / 1_000_000.0}%.1fM"
    else s"${tokens / 1000}k"

  private def framedPanel(innerWidth: Int, rows: Vector[Element]): Element =
    val top = Text(s"┌${"─" * innerWidth}┐").style(OverlayFrameStyle)
    val bottom = Text(s"└${"─" * innerWidth}┘").style(OverlayFrameStyle)
    layout((top +: rows :+ bottom)*)

  private def framed(content: String, style: Style, innerWidth: Int): Element =
    val body = padRight(content.take(innerWidth), innerWidth)
    Text(s"│$body│").style(style)

  private def padRight(s: String, width: Int): String =
    if s.length >= width then s else s + (" " * (width - s.length))

  /** A fixed-width table cell: truncate (with an ellipsis) past `width`, else
    * pad with spaces — so columns line up no matter the content length. */
  private def cell(s: String, width: Int): String =
    padRight(truncate(s, width), width)

  private def shortId(id: String): String =
    id.take(8)

  private def relativeTime(modifiedAtMs: Option[Long]): String =
    modifiedAtMs match
      case None => "unknown"
      case Some(ms) =>
        val ageSeconds = math.max(0L, (System.currentTimeMillis() - ms) / 1000L)
        if ageSeconds < 60 then "just now"
        else if ageSeconds < 3600 then s"${ageSeconds / 60}m ago"
        else if ageSeconds < 86400 then s"${ageSeconds / 3600}h ago"
        else s"${ageSeconds / 86400}d ago"

  private def truncate(text: String, max: Int): String =
    if max <= 0 then ""
    else if text.length <= max then text
    else if max == 1 then "…"
    else text.take(max - 1) + "…"

  /** The empty-transcript hint: lives in the live region so it vanishes once
    * the first message lands. For now, it is always empty. */
  private def emptyHint(state: ChatState): Element = Empty

  /** Sticky system notices (e.g. the workflow dashboard URL), pinned just above
    * the input box in the live region so they stay readable instead of scrolling
    * away into the transcript. */
  private def noticesBlock(state: ChatState): Element =
    if state.notices.isEmpty then Empty
    else layout(state.notices.map(n => Text(s"  ${Color.Cyan(s"◆ $n").render}"))*)

  /** How many queued items to list before collapsing the rest into a count. */
  private val MaxQueuedShown = 6

  /** The pending steering queue, drawn as a soft-blue rail card pinned just above
    * the input box: queued user messages marked with a cyan `›`, system notices
    * with a dim `◆`. Each item stays on exactly one line — flattened and
    * ellipsis-truncated at the layout width — so a long message never swells
    * the card. Empty ⇒ the panel is absent (the live stack collapses, like the
    * notices/overlay blocks). The header count is the true total even when the
    * listed rows are capped. */
  private def queueBlock(state: ChatState, width: Int): Element =
    if state.pendingQueue.isEmpty then Empty
    else
      val rail = Style.fg(FrameBlue).setSequence
      val plain = Ansi.Reset
      val n = state.pendingQueue.length
      val header = Text(s"  $rail╭─ $plain${WordmarkSeq}queued$plain$DimSeq · $n$plain")
      val rows = state.pendingQueue.take(MaxQueuedShown).map(queueRow(_, rail, plain, width))
      val more =
        if n > MaxQueuedShown then Vector(Text(s"  $rail$Bar$plain $DimSeq… +${n - MaxQueuedShown} more$plain"))
        else Vector.empty
      val footer = Text(s"  $rail╰─$plain")
      layout(((header +: rows) ++ more :+ footer)*)

  /** A single compact line standing in for the live background workflows: a
    * soft-blue braille spinner, the run count, and the hint to open the menu.
    * The forest itself now lives in the `ctrl+c w` overlay, so a large workflow
    * no longer dominates the live region. Empty ⇒ the line is absent (the live
    * stack collapses, like the notices/queue blocks). The spinner animates off
    * the render clock — the same tick that already runs while a workflow is
    * active — so the line reads as alive without pulling in the whole forest. */
  private def workflowNotice(state: ChatState): Element =
    if state.activeWorkflows.isEmpty then Empty
    else
      val plain = Ansi.Reset
      val blue = Style.fg(FrameBlue).setSequence
      val n = state.activeWorkflows.length
      val glyph = EvalSpinner.charAt(math.floorMod((state.clockMs / 100).toInt, EvalSpinner.length))
      val word = if n == 1 then "workflow" else "workflows"
      Text(s"  $blue$glyph$plain ${WordmarkSeq}$n $word$plain$DimSeq running · press ctrl+c w to view$plain")

  /** One queued row: a soft-blue rail, a kind marker, then the message on one
    * line — newlines flattened, then ellipsis-truncated to the width left of
    * the 6-column rail prefix — so the queue stays scannable however long an
    * item runs (the model still receives the verbatim text). */
  private def queueRow(item: Inbox, rail: String, plain: String, width: Int): Element =
    val marker = item match
      case Inbox.UserMessage(_)  => PromptArrow           // cyan ›
      case Inbox.SystemNotice(_) => s"$DimSeq◆$plain"     // dim ◆
    val text = truncateW(item.text.replace('\n', ' '), math.max(1, width - 6))
    Text(s"  $rail$Bar$plain $marker $text")

  private def renderEntry(e: Entry, divider: Element): Element = e match
    case Entry.User(text) => layout(divider, roleHeader(Role.You), textBlock(text), divider)
    case Entry.System(text) =>
      // A folded-in system notice (it woke an idle agent): a dim ◆-led
      // interjection, frameless so it doesn't masquerade as a user turn.
      systemInterjection(text)
    case Entry.Assistant(blocks) =>
      // Committed: every tool has finished, so no live clock is needed.
      layout((roleHeader(Role.Auk) +: blocks.map(renderBlock(_, liveNow = None)))*)
    case Entry.Error(text) => Text(s"  ${Color.Red(text).render}")
    case Entry.Interrupted => dim("  ⊘ Interrupted")
    case Entry.ContextCompacted(_) =>
      systemInterjection("Context Compacted")

  /** Render one assistant block. Reasoning and tool calls get a dim left bar;
    * answer text is plain, under the "Auk" header. `liveNow` is the render
    * clock, supplied while streaming so a running tool's duration ticks. */
  private def renderBlock(b: Block, liveNow: Option[Long]): Element = b match
    case Block.Thinking(_, _, Some(ms))  => barBlock(thoughtLabel(ms))
    case Block.Thinking(typed, _, None)  => barBlock(s"thinking ▸ ${typed.visible}")
    case t: Block.Tool if t.name == "eval_scala" => scalaEvalBlock(t, liveNow)
    case t: Block.Tool                  => barBlock(toolLabel(t, liveNow))
    case Block.Answer(_, doc)           => MarkdownRender.answerBlock(doc, glow = None)
    case Block.Injected(item)           => injectedBlock(item)

  /** A queued input the engine folded into the turn mid-stream, shown inline in
    * the block stream so it sits in chronological order (after the work already
    * done, before what it triggers). A user steer reads like a prompt — a
    * soft-blue rail and cyan `›`, bright text; a system notice as a dim ◆
    * interjection. Mirrors the queue panel's visual language. */
  private def injectedBlock(item: Inbox): Element =
    val rail = Style.fg(FrameBlue).setSequence
    val plain = Ansi.Reset
    item match
      case Inbox.UserMessage(text) =>
        wrapText(s"  $rail$Bar$plain $PromptArrow ", s"  $rail$Bar$plain   ", text.replace('\n', ' '))
      case Inbox.SystemNotice(text) =>
        systemInterjection(text)

  /** A dim `◆`-led system-notice interjection — one source of truth for both a
    * turn-start [[Entry.System]] and a mid-turn [[Block.Injected]] notice. */
  private def systemInterjection(text: String): Element =
    layout(splitLines(text).zipWithIndex.map((l, i) => dim(s"  ${if i == 0 then "◆" else " "} $l"))*)

  /** Animated activity indicator — shown at the tail of live work, just above the
    * input box. */
  private def activityLine(state: ChatState, label: String, stats: String): Element =
    // A dim braille spinner leads the shimmering label: the spinner spins on the
    // frame counter, the highlight sweeps the text on wall-clock time.
    val glyph = EvalSpinner.charAt(math.floorMod(state.frame, EvalSpinner.length))
    val spin = DimSeq + glyph + " " + Ansi.Reset
    Text("  " + spin + Glow.sweep(label, state.clockMs) + DimSeq + stats + Ansi.Reset)

  /** The "auk is thinking" working indicator. */
  private def workingLine(state: ChatState): Element =
    activityLine(state, "auk is thinking", thinkingStats(state))

  /** The context compaction indicator. */
  private def compactingLine(state: ChatState): Element =
    activityLine(state, "auk is compacting context", elapsedStats(state))

  /** A dim parenthetical readout trailing "auk is thinking": elapsed wall-clock
    * time, the output-token count, and the implied throughput.
    *
    * Tokens are a hybrid: every completed round contributes its exact usage (via
    * `RoundComplete`, anchored in [[ChatState.anchoredOutputTokens]]), and only
    * the round still streaming is estimated from its character count at
    * [[CharsPerToken]] chars each. So the figure rests on real usage and the
    * estimate covers just the open tail — collapsing to zero between rounds. */
  private def thinkingStats(state: ChatState): String =
    val elapsedMs = math.max(0L, state.clockMs - state.turnStartMs)
    val secs = elapsedMs / 1000.0
    val pendingChars = math.max(0L, state.streamedOutputChars - state.anchorChars)
    val tokens = state.anchoredOutputTokens + math.round(pendingChars / CharsPerToken)
    val rate = if elapsedMs > 0 then math.round(tokens / secs) else 0L
    f" ($secs%.1fs, $tokens tokens, $rate token/s)"

  private def elapsedStats(state: ChatState): String =
    val elapsedMs = math.max(0L, state.clockMs - state.turnStartMs)
    f" (${elapsedMs / 1000.0}%.1fs)"

  private def inProgress(state: ChatState): Element =
    state.phase match
      case Phase.Waiting =>
        layout(roleHeader(Role.Auk), workingLine(state))

      case Phase.Compacting =>
        layout(roleHeader(Role.Auk), compactingLine(state))

      case Phase.Streaming(blocks, _) =>
        val rendered = blocks.zipWithIndex.map: (b, i) =>
          // Freshly-revealed text glows and the breathing cursor rides the tail
          // of whichever block is still streaming in (the answer being written,
          // or the reasoning while it is still open — both are always last).
          b match
            case Block.Answer(typed, doc) if i == blocks.length - 1 =>
              // The freshly-revealed tail still glows; the breathing cursor rides
              // the very end. `hot` is how many trailing code points haven't cooled.
              val hot = typed.visible.length - typed.coolPrefixLen
              MarkdownRender.answerBlock(doc, glow = Some((hot, state.frame)), answerCacheAt(i))
            case Block.Answer(_, doc) =>
              // A settled answer earlier in the turn (text before a tool call):
              // the same cached render, without the glow.
              MarkdownRender.answerBlock(doc, glow = None, answerCacheAt(i))
            case Block.Thinking(typed, _, None) =>
              thinkingLive(typed, state.frame)
            case other => renderBlock(other, liveNow = Some(state.clockMs))
        // Keep the working indicator pinned to the end of the turn, right above
        // the input box, for the whole generation — with a blank line between it
        // and the generated text for readability.
        layout(((roleHeader(Role.Auk) +: rendered) ++ Vector(br, workingLine(state)))*)

      case Phase.Idle =>
        // The turn is over (its entry render is cached separately, per entry in
        // `cachedEntries`): drop the streaming render caches.
        if answerCaches.nonEmpty then answerCaches.clear()
        Empty

  /** A frameless prompt line with an underline cursor at [[ChatState.cursor]]
    * (a `_`-like underline under the cell, so it can sit mid-line over a
    * character). Steady (non-blinking) so the idle view stays static. */
  private def prompt(state: ChatState): Element =
    // The input is always editable — even while a reply streams — so you can
    // compose your next message; Enter just won't send until idle.
    val before = state.input.take(state.cursor)
    val (atCursor, after) =
      if state.cursor < state.input.length then
        val ch = state.input(state.cursor)
        if ch == '\n' then (" ", "\n" + state.input.drop(state.cursor + 1))
        else (ch.toString, state.input.drop(state.cursor + 1))
      else (" ", "")
    // Byte-identical to Text(atCursor).style(Style.Underline).render: a single
    // styled span renders as setSequence + text + trailing reset.
    val cell = Style.Underline.setSequence + atCursor + Ansi.Reset
    // A single space before the arrow; the 3-column continuation prefix keeps
    // wrapped input aligned under the first typed character.
    wrapText(s"$PromptArrow ", "  ", s"$before$cell$after")

  /** The "You" / "Auk" header line that sits above an entry's content. */
  private def roleHeader(role: Role): Element =
    role match
      case Role.You => YouHeader
      case Role.Auk => AukHeader

  /** Plain, indented content; one rendered line per source line. */
  private def textBlock(text: String): Element =
    layout(splitLines(text).map(l => Text(s"  $l"))*)

  /** A dim, left-barred block; one barred line per source line. */
  private def barBlock(text: String): Element =
    layout(splitLines(text).map(l => dim(s"  $Bar $l"))*)

  /** Caps for the eval_scala card: enough to read what happened, not enough to
    * flood the chat (the model still gets the full text). */
  private val MaxEvalCodeLines = 20
  private val MaxEvalOutputLines = 12

  /** Local copy of the layout's spinner frames, indexed by the live clock so a
    * running eval's footer spins without threading the frame counter here. */
  private val EvalSpinner = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

  /** An eval_scala call drawn as a rounded card — a REPL cell in the chat. The
    * code the model wrote sits bright inside a dim rail under the soft-blue
    * `execution` wordmark; a `├─` rule separates it from the session's reply,
    * which keeps the dim tool colour (softly red when the evaluation failed).
    * The footer carries the verdict: a spinner and ticking duration while it
    * runs, then ✓/✗ and the time it took.
    *
    * {{{
    *   ╭─ execution
    *   │ val xs = (1 to 5).toList
    *   │ xs.sum
    *   ├─
    *   │ val xs: List[Int] = List(1, 2, 3, 4, 5)
    *   │ val res0: Int = 15
    *   ╰─ ✓ 0.4s
    * }}}
    *
    * While the arguments are still streaming the body is a lone `⋯`; the rule
    * and reply appear when the run finishes. */
  private def scalaEvalBlock(t: Block.Tool, liveNow: Option[Long]): Element =
    val rail = DimSeq
    val plain = Ansi.Reset
    val wordmark = WordmarkSeq

    val header = Text(s"  ${rail}╭─ $plain${wordmark}execution$plain")

    val codeLines = jsonField(t.rawArgs, "code").map(splitLines).getOrElse(Nil)
    val code =
      if codeLines.isEmpty then List(dim(s"  $Bar ⋯")) // arguments still streaming
      else
        codeLines
          .take(MaxEvalCodeLines)
          .map(l => Text(s"  $rail$Bar $plain$l"))
          ++ moreMarker(codeLines.length - MaxEvalCodeLines)

    val outputStyle =
      if t.isError then Style(fg = Color.Red, attrs = Attr.Dim) else Style.Dim
    val outputLines =
      t.output.map(o => splitLines(o.stripSuffix("\n"))).getOrElse(Nil)
    val output =
      if outputLines.isEmpty then Nil
      else
        dim(s"  ├─") +:
          (outputLines
            .take(MaxEvalOutputLines)
            .map(l => Text(s"  $Bar $l").style(outputStyle))
            ++ moreMarker(outputLines.length - MaxEvalOutputLines))

    layout(((header +: code) ++ output :+ evalFooter(t, liveNow))*)

  /** The eval card's bottom edge: `╰─` plus a verdict. Running shows the
    * spinner and the live duration; finished shows ✓ (green) or ✗ (red) and
    * the time taken — omitted when unknown, e.g. a call loaded from a saved
    * session, whose footer is just the bare verdict. */
  private def evalFooter(t: Block.Tool, liveNow: Option[Long]): Element =
    val rail = DimSeq
    val plain = Ansi.Reset
    val running = t.startedMs.isDefined && t.elapsedMs.isEmpty
    val badge =
      if running then
        liveNow.map: now =>
          s"${EvalSpinner.charAt(math.floorMod((now / 100).toInt, EvalSpinner.length))}"
      else
        t.output.map: _ =>
          if t.isError then s"${EvalErrSeq}✗$plain"
          else s"${EvalOkSeq}✓$plain"
    val time = t.elapsedMs
      .orElse(for s <- t.startedMs; now <- liveNow yield now - s)
      .filter(ms => running || ms > 0)
      .map(fmtDuration)
    val badgePart = badge.map(b => s" $b").getOrElse("")
    val timePart = time.map(d => s" $rail$d$plain").getOrElse("")
    Text(s"  ${rail}╰─$plain$badgePart$timePart")

  /* ---- Fullscreen workflow views (the `ctrl+c w` list, per-run detail, transcript) ---- */

  private val WorkflowBarW = 8       // progress-bar cells in the list
  private val WorkflowListIdW = 14   // run-id column in the list
  private val WorkflowIdW = 24       // node-id column in the single-pane forest
  private val WorkflowTokW = 6       // token column in the single-pane forest
  private val TwoPaneTokW = 5        // token column in the two-pane forest
  private val MaxWorkflowLogLines = 5 // log lines tailed in the single-pane detail
  private val MaxToolErrLines = 3     // error output lines shown under a failed tool

  /** Count a run's settled (terminal) sub-agents — the progress numerator. */
  private def settledNodes(f: Forest): Int =
    f.nodes.count(n => n.status == NodeStatus.Done || n.status == NodeStatus.Failed)

  /** A `▰▰▱▱` bar of `width` cells, filled in proportion to settled/total
    * (all-empty when nothing is declared yet). */
  private def progressBar(settled: Int, total: Int, width: Int): String =
    if total <= 0 then "▱" * width
    else
      val filled = math.max(0, math.min(width, math.round(settled.toDouble / total * width).toInt))
      ("▰" * filled) + ("▱" * (width - filled))

  /** The status glyph for a sub-agent row: a spinner while running (animated off
    * the render clock), else a static pending/queued/done/failed mark. */
  private def workflowNodeGlyph(status: NodeStatus, clockMs: Long): String =
    status match
      case NodeStatus.Pending => "·"
      case NodeStatus.Queued  => "◌"
      case NodeStatus.Running => EvalSpinner.charAt(math.floorMod((clockMs / 100).toInt, EvalSpinner.length)).toString
      case NodeStatus.Done    => "✓"
      case NodeStatus.Failed  => "✗"
      case NodeStatus.Interrupted => "❚"

  /** The uniform row style for a sub-agent, by status: active rows stay bright,
    * settled rows take their verdict colour, waiting rows recede. */
  private def workflowNodeStyle(status: NodeStatus): Style =
    status match
      case NodeStatus.Done        => OverlayDoneStyle
      case NodeStatus.Failed      => OverlayFailStyle
      case NodeStatus.Interrupted => OverlayInterruptStyle
      case NodeStatus.Running     => OverlayBodyStyle
      case _                      => OverlayMutedStyle

  /** A one-word status label for a sub-agent, shown in the transcript headers. */
  private def statusWord(status: NodeStatus): String =
    status match
      case NodeStatus.Pending     => "pending"
      case NodeStatus.Queued      => "queued"
      case NodeStatus.Running     => "running"
      case NodeStatus.Done        => "done"
      case NodeStatus.Failed      => "failed"
      case NodeStatus.Interrupted => "interrupted"

  /* ---- Display-width-aware framing (transcripts contain CJK) ---- */

  /** Right-pad `s` to `width` display columns (not char count). */
  private def padRightW(s: String, width: Int): String =
    val w = Width.stringWidth(s)
    if w >= width then s else s + (" " * (width - w))

  /** Truncate `s` to `max` display columns, appending `…` when it overflows. */
  private def truncateW(s: String, max: Int): String =
    if max <= 0 then ""
    else if Width.stringWidth(s) <= max then s
    else
      val sb = new StringBuilder
      val budget = max - 1 // room for the ellipsis
      var w = 0
      var i = 0
      var done = false
      while i < s.length && !done do
        val cp = s.codePointAt(i)
        val cw = Width.displayWidth(cp)
        if w + cw > budget then done = true
        else { sb.append(new String(Character.toChars(cp))); w += cw; i += Character.charCount(cp) }
      sb.append('…').toString

  /** Truncate then pad `s` to exactly `width` display columns. */
  private def fitW(s: String, width: Int): String = padRightW(truncateW(s, width), width)

  /* ---- Full-bleed (fullscreen) row composition — bar style, no box frame ---- */

  /** A full-width row in one uniform style, sized by display columns (CJK-safe). */
  private def barRow(content: String, style: Style, width: Int): Element =
    Text(fitW(content, width)).style(style)

  /** A full-width row of left-to-right styled segments, each re-asserting its own
    * style (bg included) inline so colours never bleed, the tail padded to `width`
    * in `fill`. The one place mixed-style rows are built — the two-pane split and
    * the list's done/failed tag. */
  private def barSegments(segments: Vector[(String, Style)], width: Int, fill: Style): Element =
    val sb = new StringBuilder
    var used = 0
    for (text, style) <- segments do
      val room = width - used
      if room > 0 then
        val t = truncateW(text, room)
        if t.nonEmpty then
          sb.append(style.setSequence).append(t)
          used += Width.stringWidth(t)
    if used < width then sb.append(fill.setSequence).append(" " * (width - used))
    sb.append(Ansi.Reset)
    Text(sb.toString)

  /** A header/footer bar: `left` text with `right` pushed to the right edge, one
    * uniform style at full width (right is kept, left truncated to fit). */
  private def barLR(left: String, right: String, style: Style, width: Int): Element =
    val rw = Width.stringWidth(right)
    val leftFit = truncateW(left, math.max(0, width - rw - 1))
    val gap = math.max(1, width - Width.stringWidth(leftFit) - rw)
    barRow(leftFit + (" " * gap) + right, style, width)

  /** Assemble a fullscreen view — `header` bar, a body padded/clipped to
    * `rows - 2` rows (blank body rows fill the tail), and a `footer` bar — as
    * exactly `rows` full-bleed lines. */
  private def fullscreenFrame(header: Element, body: Vector[Element], footer: Element, width: Int, rows: Int): Element =
    val bodyHeight = math.max(0, rows - 2)
    val filled =
      if body.length >= bodyHeight then body.take(bodyHeight)
      else body ++ Vector.fill(bodyHeight - body.length)(barRow("", OverlayBodyStyle, width))
    layout((header +: filled :+ footer)*)

  /* ---- Transcript rendering (shared by the preview pane and the full view) ---- */

  /** Render a transcript as wrapped, styled rows at `width` columns. Only ever
    * called for the one selected node (materializing a transcript's text is O(n)). */
  private def transcriptRows(t: Transcript, width: Int, clockMs: Long): Vector[(String, Style)] =
    val w = math.max(1, width)
    if t.items.isEmpty then Vector(("(no activity yet)", OverlayMutedStyle))
    else
      t.items.flatMap:
        case TranscriptItem.Thought(text) =>
          ChatApp.wrap(text, math.max(1, w - 2)).map(l => (s"$Bar $l", OverlayMutedStyle))
        case TranscriptItem.Said(text) =>
          ChatApp.wrap(text, w).map(l => (l, OverlayBodyStyle))
        case TranscriptItem.ToolCall(_, tool, input, output, isError) =>
          val firstLine = input.split("\n", -1).headOption.getOrElse("")
          val status = output match
            case None               => EvalSpinner.charAt(math.floorMod((clockMs / 100).toInt, EvalSpinner.length)).toString
            case Some(_) if isError => "✗"
            case Some(_)            => "✓"
          val head = truncateW(s"▸ $tool $firstLine".stripTrailing, math.max(1, w - 2))
          val callRow = (s"$head $status", if isError then OverlayFailStyle else OverlayBodyStyle)
          val errLines =
            if isError then
              output.toVector.flatMap(o => splitLines(o).take(MaxToolErrLines).map(l => (s"  ${truncateW(l, math.max(1, w - 2))}", OverlayMutedStyle)))
            else Vector.empty
          callRow +: errLines

  /* ---- Detail logs ---- */

  /** The `── logs ──` divider and a tail of the most recent log lines, as (text,
    * style) rows (empty when the run has logged nothing yet). */
  private def workflowLogRows(forest: Forest, innerWidth: Int): Vector[(String, Style)] =
    if forest.logs.isEmpty then Vector.empty
    else
      val label = " ── logs "
      val divider = (label + "─" * math.max(0, innerWidth - label.length), OverlayMutedStyle)
      val lines = forest.logs.takeRight(MaxWorkflowLogLines).map(l => (s"   ${truncate(l, innerWidth - 3)}", OverlayMutedStyle))
      ("", OverlayBodyStyle) +: divider +: lines

  /** A run's forest as (text, style) rows with the cursor node highlighted (`›` +
    * selected style) and group headers interleaved. The cursor indexes
    * [[ChatState.displayNodes]] (headers are not selectable); the returned Int is
    * the cursor node's row index, used to keep it in the auto-scroll window. */
  private def forestCursorRows(forest: Forest, cursor: Int, clockMs: Long, idW: Int, tokW: Int): (Vector[(String, Style)], Int) =
    val names = forest.groups.map(g => g.id -> g.name).toMap
    val nodes = ChatState.displayNodes(forest)
    val rows = Vector.newBuilder[(String, Style)]
    var cursorRow = 0
    var lastGroup: Option[String] = None
    var started = false
    var rowIdx = 0
    nodes.zipWithIndex.foreach: (n, i) =>
      if !started || n.group != lastGroup then
        n.group.flatMap(names.get).foreach: name =>
          rows += ((s" ▸ $name", OverlayGroupStyle))
          rowIdx += 1
        lastGroup = n.group
        started = true
      val selected = i == cursor
      if selected then cursorRow = rowIdx
      val marker = if selected then "›" else " "
      val glyph = workflowNodeGlyph(n.status, clockMs)
      val toks = if n.outputTokens > 0 then fmtTokens(n.outputTokens) else ""
      val tool = n.currentTool.getOrElse("")
      val style = if selected then OverlaySelectedStyle else workflowNodeStyle(n.status)
      rows += ((s" $marker $glyph ${cell(n.id, idW)}  ${cell(toks, tokW)}  $tool", style))
      rowIdx += 1
    (rows.result(), cursorRow)

  /** The first visible row so `cursorRow` stays in a `visible`-tall window. */
  private def windowStart(cursorRow: Int, total: Int, visible: Int): Int =
    if total <= visible then 0
    else math.max(0, math.min(cursorRow - visible + 1, total - visible))

  /** The fullscreen workflow menu: a header bar (`Workflows · N`), one full-width
    * row per `wf.start` run (live and recently finished) — marker, run id, progress
    * bar, `settled/total`, and a `✓ done` / `✗ failed` / `paused` tag — the selected
    * row inverted, and a footer key-hint bar. ↑/↓ select, Enter opens the detail. */
  private def workflowListFullscreen(workflows: Vector[(String, Forest)], selected: Int, clockMs: Long, viewport: Viewport): Element =
    val width = viewport.width
    val rows = viewport.rows
    val bodyHeight = math.max(4, rows - 2)
    val header = barLR(s" Workflows · ${workflows.length}", "", OverlayHeaderStyle, width)
    if workflows.isEmpty then
      val body = Vector(
        barRow("", OverlayBodyStyle, width),
        barRow("   No workflows running", OverlayMutedStyle, width),
        barRow("   Press Esc to return", OverlayMutedStyle, width)
      )
      fullscreenFrame(header, body, barLR(" Esc close", "", OverlayMutedStyle, width), width, rows)
    else
      val sel = math.max(0, math.min(workflows.length - 1, selected))
      val start = windowStart(sel, workflows.length, bodyHeight)
      val visible = workflows.zipWithIndex.slice(start, start + bodyHeight)
      val body = visible.map: (entry, idx) =>
        val (runId, forest) = entry
        val total = forest.nodes.length
        val marker = if idx == sel then "›" else " "
        val bar = progressBar(settledNodes(forest), total, WorkflowBarW)
        val rowStyle = if idx == sel then OverlaySelectedStyle else OverlayBodyStyle
        val lead = s" $marker ${cell(runId, WorkflowListIdW)}  $bar  ${settledNodes(forest)}/$total"
        forest.status match
          // Settled runs stay listed, tagged in their verdict colour (unless the
          // row is selected, which owns the whole row's styling).
          case RunStatus.Done | RunStatus.Failed =>
            val (word, tagStyle) =
              if forest.status == RunStatus.Done then ("✓ done", OverlayDoneStyle) else ("✗ failed", OverlayFailStyle)
            val effTag = if idx == sel then rowStyle else tagStyle
            barSegments(Vector((lead + "  ", rowStyle), (word, effTag)), width, rowStyle)
          case RunStatus.Paused  => barRow(lead + "  paused", rowStyle, width)
          case RunStatus.Running => barRow(lead, rowStyle, width)
      val range = if workflows.length > bodyHeight then s"${start + 1}-${start + visible.length} of ${workflows.length}" else ""
      fullscreenFrame(header, body, barLR(" ↑/↓ select  Enter view  Esc close", range, OverlayMutedStyle, width), width, rows)

  /** The fullscreen per-run detail, keyed by run id (looked up live each frame).
    * ↑/↓ move the node cursor, Enter opens the selected node's transcript, Esc
    * returns to the list. On a wide terminal (≥ 88 cols) a live tail preview of the
    * selected node's transcript is shown beside the forest; narrow terminals show
    * the forest alone. If the run has been evicted, a short fallback is shown. */
  private def workflowDetailFullscreen(
      runId: String,
      workflows: Vector[(String, Forest)],
      transcripts: Map[(String, String), Transcript],
      cursor: Int,
      clockMs: Long,
      viewport: Viewport
  ): Element =
    val width = viewport.width
    val rows = viewport.rows
    val bodyHeight = math.max(4, rows - 2)
    workflows.collectFirst { case (id, f) if id == runId => f } match
      case None =>
        val body = Vector(
          barRow("", OverlayBodyStyle, width),
          barRow("   This workflow has finished", OverlayMutedStyle, width),
          barRow("   Press Esc to return", OverlayMutedStyle, width)
        )
        fullscreenFrame(barLR(s" $runId", "", OverlayHeaderStyle, width), body, barLR(" Esc back", "", OverlayMutedStyle, width), width, rows)
      case Some(forest) =>
        val total = forest.nodes.length
        val tag = forest.status match
          case RunStatus.Paused => " · paused"
          case _                => ""
        val header = barLR(s" $runId$tag", s"${settledNodes(forest)}/$total · ${fmtTokens(forest.nodes.map(_.outputTokens).sum)} tokens ", OverlayHeaderStyle, width)
        val control = forest.status match
          case RunStatus.Paused  => "r resume  "
          case RunStatus.Running => "p pause  "
          case _                 => ""
        val footerLeft = s" ↑/↓ select  Enter transcript  ${control}Esc back"
        // Two-pane once wide enough to carry a legible preview.
        if width >= 88 then twoPaneDetailBody(runId, forest, transcripts, cursor, clockMs, width, rows, bodyHeight, header, footerLeft)
        else singlePaneDetailBody(forest, cursor, clockMs, width, rows, bodyHeight, header, footerLeft)

  /** The narrow (forest-only) detail body: the cursor'd forest plus a logs tail,
    * auto-scrolled to keep the cursor visible. */
  private def singlePaneDetailBody(
      forest: Forest, cursor: Int, clockMs: Long, width: Int, rows: Int, bodyHeight: Int, header: Element, footerLeft: String
  ): Element =
    val (forestRows, cursorRow) = forestCursorRows(forest, cursor, clockMs, WorkflowIdW, WorkflowTokW)
    val allPairs = forestRows ++ workflowLogRows(forest, width)
    val start = windowStart(cursorRow, allPairs.length, bodyHeight)
    val visiblePairs =
      if allPairs.isEmpty then Vector((" Starting…", OverlayMutedStyle))
      else allPairs.slice(start, start + bodyHeight)
    val body = visiblePairs.map((c, s) => barRow(c, s, width))
    val range = if allPairs.length > bodyHeight then s"${start + 1}-${start + visiblePairs.length} of ${allPairs.length}" else ""
    fullscreenFrame(header, body, barLR(footerLeft, range, OverlayMutedStyle, width), width, rows)

  /** The wide (two-pane) detail body: the cursor'd forest on the left, a live tail
    * preview of the selected node's transcript on the right. Left pane inner width
    * scales with the terminal (`min(48, max(38, width/3))`). */
  private def twoPaneDetailBody(
      runId: String, forest: Forest, transcripts: Map[(String, String), Transcript],
      cursor: Int, clockMs: Long, width: Int, rows: Int, bodyHeight: Int, header: Element, footerLeft: String
  ): Element =
    val leftW = math.min(48, math.max(38, width / 3))
    val rw = math.max(1, width - leftW - 3) // " │ " separator
    val (forestRows, cursorRow) = forestCursorRows(forest, cursor, clockMs, math.max(10, leftW - 22), TwoPaneTokW)
    val leftStart = windowStart(cursorRow, forestRows.length, bodyHeight)
    val leftWindow = forestRows.slice(leftStart, leftStart + bodyHeight)
    // Right pane: a header for the selected node, then the tail of its transcript.
    val rightRows: Vector[(String, Style)] =
      ChatState.displayNodes(forest).lift(cursor) match
        case Some(n) =>
          val hdr = (s"${n.id} · ${statusWord(n.status)} · ${fmtTokens(n.outputTokens)} tokens", OverlayHeaderStyle)
          val body = transcripts.get((runId, n.id)) match
            case Some(t) => transcriptRows(t, rw, clockMs)
            case None    => Vector(("(no activity yet)", OverlayMutedStyle))
          hdr +: body.takeRight(math.max(0, bodyHeight - 1))
        case None => Vector((" no node selected", OverlayMutedStyle))
    val body = (0 until bodyHeight).toVector.map: k =>
      val (lc, ls) = leftWindow.lift(k).getOrElse(("", OverlayBodyStyle))
      val (rc, rs) = rightRows.lift(k).getOrElse(("", OverlayBodyStyle))
      barSegments(Vector((fitW(lc, leftW), ls), (" │ ", OverlayMutedStyle), (fitW(rc, rw), rs)), width, OverlayBodyStyle)
    fullscreenFrame(header, body, barLR(footerLeft, "", OverlayMutedStyle, width), width, rows)

  /** The fullscreen full transcript of one sub-agent, keyed by run + node id
    * (looked up live each frame). Header bar, then a prompt block (≤ 2 dim lines +
    * divider), then the bottom-anchored body: `offset` rows are revealed above the
    * tail (`offset == 0` follows the live tail), clamped here against the content
    * height so a huge offset shows the top. If the run or node has been evicted, a
    * short fallback is shown. */
  private def workflowTranscriptFullscreen(
      runId: String,
      nodeId: String,
      workflows: Vector[(String, Forest)],
      transcripts: Map[(String, String), Transcript],
      offset: Int,
      clockMs: Long,
      viewport: Viewport
  ): Element =
    val width = viewport.width
    val rows = viewport.rows
    val forest = workflows.collectFirst { case (id, f) if id == runId => f }
    val node = forest.flatMap(f => f.nodes.find(_.id == nodeId))
    (forest, node) match
      case (None, _) =>
        val body = Vector(
          barRow("", OverlayBodyStyle, width),
          barRow("   This workflow has finished", OverlayMutedStyle, width),
          barRow("   Press Esc to return", OverlayMutedStyle, width)
        )
        fullscreenFrame(barLR(s" $runId", "", OverlayHeaderStyle, width), body, barLR(" Esc back", "", OverlayMutedStyle, width), width, rows)
      case (Some(_), None) =>
        val body = Vector(
          barRow("", OverlayBodyStyle, width),
          barRow("   Transcript no longer available", OverlayMutedStyle, width),
          barRow("   Press Esc to return", OverlayMutedStyle, width)
        )
        fullscreenFrame(barLR(s" $runId / $nodeId", "", OverlayHeaderStyle, width), body, barLR(" Esc back", "", OverlayMutedStyle, width), width, rows)
      case (Some(_), Some(n)) =>
        val header = barLR(s" $runId / $nodeId", s"${statusWord(n.status)} · ${fmtTokens(n.outputTokens)} tokens ", OverlayHeaderStyle, width)
        // The prompt above the transcript (up to two dim lines + divider), mirroring the web UI.
        val promptEls: Vector[Element] = n.prompt.filter(_.nonEmpty) match
          case Some(p) =>
            val wrapped = ChatApp.wrap(p, width - 2)
            val shown = wrapped.take(2)
            val withEllipsis = if wrapped.length > 2 && shown.nonEmpty then shown.init :+ (shown.last + "…") else shown
            withEllipsis.map(l => barRow(s" $l", OverlayMutedStyle, width)) :+
              barRow(" " + "─" * math.max(0, width - 1), OverlayMutedStyle, width)
          case None => Vector.empty
        val bodyHeight = math.max(4, rows - 2 - promptEls.length)
        // Record the body height so a PageUp/Down on this view steps a near-page.
        lastTranscriptBody = bodyHeight
        val trAll = transcripts.get((runId, nodeId)) match
          case Some(t) => transcriptRows(t, width, clockMs)
          case None    => Vector(("(no activity yet)", OverlayMutedStyle))
        // Bottom-anchored: the window's last row is the tail when offset == 0, and
        // each unit of offset reveals one older row until it pins at the top.
        val maxOffset = math.max(0, trAll.length - bodyHeight)
        val start = maxOffset - math.min(offset, maxOffset)
        val visiblePairs = trAll.slice(start, start + bodyHeight)
        val bodyRows = visiblePairs.map((c, s) => barRow(c, s, width))
        val range = if trAll.length > bodyHeight then s"${start + 1}-${start + visiblePairs.length} of ${trAll.length}" else ""
        fullscreenFrame(header, promptEls ++ bodyRows, barLR(" ↑/↓ scroll  G follow  Esc back", range, OverlayMutedStyle, width), width, rows)

  /* ---- Subagent (team) panel ---- */

  /** The working badge's frames: a braille tide that swells from ⣀ to ⣿ and
    * recedes, the two dot-columns half a step out of phase so the crest flows
    * across the cell — one full wave every ~1.2s. Glyph-only, so the selected
    * cell can animate the same shape under its inverted style; the unselected
    * badge colours it in the frame blue. Sampled off the render clock like
    * [[EvalSpinner]], one frame per 150ms. */
  private val TideGlyphs: Vector[String] =
    Vector("⣀", "⣄", "⣦", "⣷", "⣿", "⣾", "⣴", "⣠")

  private val TideSeq: String = Style.fg(FrameBlue).setSequence

  private def tideGlyph(clockMs: Long): String =
    TideGlyphs(math.floorMod((clockMs / 150).toInt, TideGlyphs.length))

  /** The selected panel cell's inverted style (the inline cousin of
    * [[OverlaySelectedStyle]], without the overlay's dark backdrop). */
  private val TeamSelectedStyle: Style = Style(fg = Color.Black, bg = FrameBlue, attrs = Attr.Bold)
  private val TeamNameSeq: String = Style(fg = Color.Cyan, attrs = Attr.Bold).setSequence
  private val TeamOrdSeq: String = Style.fg(FrameBlue).setSequence
  private val TeamPlainSeq: String = Style().setSequence
  private val TeamTokSeq: String = Style(fg = FrameBlue, attrs = Attr.Dim).setSequence

  /** The subagent panel, docked below the footer line: a dim labelled rule —
    * `─ subagents · 2 working · ↓ browse ────` — anchoring the block and
    * carrying the contextual key hints and overflow counts, then the roster as
    * a multi-column grid, one cell per member. The column count adapts to the
    * width (a cell never narrower than [[TeamMinCellW]]); rows are capped at
    * [[TeamPanelMaxRows]], the focused selection scrolling through the
    * overflow. Absent entirely while the team is empty. Records
    * [[lastTeamCols]] for the update loop's ↑/↓ row steps. */
  private def teamPanel(state: ChatState, width: Int): Element =
    if state.team.isEmpty then Empty
    else
      val avail = math.max(TeamMinCellW, width - 2)
      val fitCols = math.max(1, (avail + TeamCellGap) / (TeamMinCellW + TeamCellGap))
      val cols = math.max(1, math.min(fitCols, state.team.length))
      val cellW = (avail - TeamCellGap * (cols - 1)) / cols
      lastTeamCols = cols
      val nameW = math.min(12, math.max(4, state.team.map(m => Width.stringWidth(m.id)).max))
      val totalRows = (state.team.length + cols - 1) / cols
      val focused = state.teamSel.isDefined
      val scroll =
        if !focused then 0
        else math.max(0, math.min(state.teamScroll, totalRows - TeamPanelMaxRows))
      val visRows = math.min(TeamPanelMaxRows, totalRows - scroll)
      val gap = " " * TeamCellGap
      val lines = (scroll until scroll + visRows).toVector.map: r =>
        val cells = (0 until cols).flatMap: c =>
          val i = r * cols + c
          state.team.lift(i).map(m => teamCell(state, m, i, cellW, nameW, state.teamSel.contains(i)))
        Text("  " + cells.mkString(gap))
      layout((teamPanelRule(state, width, focused, cols, totalRows, scroll, visRows) +: lines)*)

  /** The panel's labelled anchor rule: `subagents` in the frame blue, then dim
    * live counts, the mode's key hint, the overflow tally, and a dash fill to
    * the full width. All the panel's chrome lives here, so the grid below stays
    * pure content. */
  private def teamPanelRule(
      state: ChatState,
      width: Int,
      focused: Boolean,
      cols: Int,
      totalRows: Int,
      scroll: Int,
      visRows: Int
  ): Element =
    val working = state.team.count(_.working)
    val status = if working > 0 then s"$working working · " else ""
    val hint = if focused then "enter open · esc back" else "↓ browse"
    val range =
      if totalRows <= TeamPanelMaxRows then ""
      else if !focused then s" · +${state.team.length - visRows * cols} more"
      else s" · ${scroll + 1}-${scroll + visRows}/$totalRows"
    val label = "subagents"
    // Prefer the full chrome; on narrow terminals drop the working count, then
    // compact the key hint, then drop it, so the label and the overflow tally
    // always survive intact.
    val hintShort = if focused then "enter · esc" else "↓"
    val room = width - 4 - label.length
    val meta = List(s" · $status$hint$range ", s" · $hint$range ", s" · $hintShort$range ", s"$range ")
      .find(m => Width.stringWidth(m) <= room)
      .getOrElse(" ")
    val fill = math.max(0, room - Width.stringWidth(meta))
    Text(s"$DimSeq  ─ $WordmarkSeq$label$DimSeq$meta${"─" * fill}${Ansi.Reset}")

  /** One grid cell, exactly `cellW` display columns: the ordinal in the frame
    * blue, the badge (braille tide while working, resting dot idle), the member
    * id (cyan-bold while working, plain idle), its latest action filling the
    * middle dim, and output tokens right-aligned in dim blue. A selected cell
    * renders in one inverted style; otherwise each segment re-asserts its own
    * colour and the cell ends reset, so nothing bleeds into the gaps. */
  private def teamCell(
      state: ChatState,
      m: TeamMemberView,
      idx: Int,
      cellW: Int,
      nameW: Int,
      selected: Boolean
  ): String =
    val ord = f"${idx + 1}%03d"
    val name = fitW(m.id, nameW)
    val toks = if m.outputTokens > 0 then fmtTokens(m.outputTokens) else ""
    val tokW = 6
    val tokPad = (" " * math.max(0, tokW - Width.stringWidth(toks))) + toks
    val actionW = math.max(1, cellW - 3 - 1 - 1 - 1 - nameW - 2 - 2 - tokW)
    val action = fitW(teamLatestAction(state, m), actionW)
    if selected then
      val glyph = if m.working then tideGlyph(state.clockMs) else "·"
      s"${TeamSelectedStyle.setSequence}${fitW(s"$ord $glyph $name  $action  $tokPad", cellW)}${Ansi.Reset}"
    else
      val badge = if m.working then s"$TideSeq${tideGlyph(state.clockMs)}" else s"${DimSeq}·"
      val nameSeq = if m.working then TeamNameSeq else TeamPlainSeq
      s"$TeamOrdSeq$ord $badge $nameSeq$name  $DimSeq$action  $TeamTokSeq$tokPad${Ansi.Reset}"

  /** The freshest thing a member did, for its panel cell: the tail of its live
    * transcript — the last tool call, or the last non-blank line of prose or
    * reasoning — falling back to the member's description before any activity
    * arrives. Reads only tail chunks, never materializing the whole transcript
    * (the panel renders every member every frame). */
  private def teamLatestAction(state: ChatState, m: TeamMemberView): String =
    state.transcripts.get(("team", m.id)).flatMap(_.items.lastOption) match
      case Some(TranscriptItem.ToolCall(_, tool, input, _, _)) =>
        val nl = input.indexOf('\n')
        val firstLine = (if nl < 0 then input else input.take(nl)).trim
        s"▸ $tool $firstLine".stripTrailing
      case Some(said: TranscriptItem.Said)       => tailSnippet(said.chunks)
      case Some(thought: TranscriptItem.Thought) => s"✻ ${tailSnippet(thought.chunks)}"
      case None                                  => m.desc

  /** The last non-blank line of a streamed run, reading only enough tail chunks
    * to cover ~120 characters (the cell truncates far shorter anyway). */
  private def tailSnippet(chunks: Vector[String]): String =
    var i = chunks.length - 1
    var acc = ""
    while i >= 0 && acc.length < 120 do
      acc = chunks(i) + acc
      i -= 1
    var last = ""
    acc.linesIterator.foreach(l => if l.trim.nonEmpty then last = l.trim)
    last

  /** The fullscreen transcript of one team member (Enter on the panel): a
    * header bar (`id — desc`, live status + tokens), the transcript body with
    * the same bottom-anchored scroll as the workflow node view, and the
    * key-hint footer. Esc returns to the chat (see [[ChatState.closeTeamTranscript]]). */
  private def teamTranscriptFullscreen(
      memberId: String,
      team: Vector[TeamMemberView],
      transcripts: Map[(String, String), Transcript],
      offset: Int,
      clockMs: Long,
      viewport: Viewport
  ): Element =
    val width = viewport.width
    val rows = viewport.rows
    team.find(_.id == memberId) match
      case None =>
        val body = Vector(
          barRow("", OverlayBodyStyle, width),
          barRow("   This team member is gone", OverlayMutedStyle, width),
          barRow("   Press Esc to return", OverlayMutedStyle, width)
        )
        fullscreenFrame(barLR(s" $memberId", "", OverlayHeaderStyle, width), body, barLR(" Esc back", "", OverlayMutedStyle, width), width, rows)
      case Some(m) =>
        val status = if m.working then "working" else "idle"
        val header = barLR(s" $memberId — ${m.desc}", s"$status · ${fmtTokens(m.outputTokens)} tokens ", OverlayHeaderStyle, width)
        val bodyHeight = math.max(4, rows - 2)
        // Record the body height so PageUp/Down steps a near-page here too.
        lastTranscriptBody = bodyHeight
        val trAll = transcripts.get(("team", memberId)) match
          case Some(t) => transcriptRows(t, width, clockMs)
          case None    => Vector(("(no activity yet)", OverlayMutedStyle))
        val maxOffset = math.max(0, trAll.length - bodyHeight)
        val start = maxOffset - math.min(offset, maxOffset)
        val visiblePairs = trAll.slice(start, start + bodyHeight)
        val bodyRows = visiblePairs.map((c, s) => barRow(c, s, width))
        val range = if trAll.length > bodyHeight then s"${start + 1}-${start + visiblePairs.length} of ${trAll.length}" else ""
        fullscreenFrame(header, bodyRows, barLR(" ↑/↓ scroll  G follow  Esc back", range, OverlayMutedStyle, width), width, rows)

  /** A dim "… +N more lines" bar line, or nothing when nothing was hidden. */
  private def moreMarker(hidden: Int): List[Element] =
    if hidden > 0 then List(dim(s"  $Bar … +$hidden more lines")) else Nil

  /** Open reasoning while it streams: a dim "│ thinking ▸" frame whose content
    * glows just behind the reveal (newest words brightest), with the breathing
    * cursor at the tail. The frame and the normal content colour are re-asserted
    * on every wrapped line so styling never leaks across a line break. */
  private def thinkingLive(typed: Typewriter, frame: Int): Element =
    val barSeq = ThinkBarSeq
    val normSeq = ThinkNormSeq
    val content = Glow.trail(typed.visible, typed.coolPrefixLen, Glow.ThinkHot, Glow.ThinkCool) + Glow.cursor(frame)
    val lines = splitLines(content).zipWithIndex.map: (l, idx) =>
      val head = if idx == 0 then s"$barSeq$Bar thinking ▸ $normSeq" else s"$barSeq$Bar $normSeq"
      Text(s"  $head$l")
    layout(lines*)

  private def splitLines(text: String): List[String] =
    if text.isEmpty then List("") else text.split("\n", -1).toList

  /** Fold a single agent event into the chat state (`now` is this tick's clock). */
  private def applyAgentEvent(
      state: ChatState,
      event: AgentEvent,
      now: Long
  ): ChatState =
    event match
      case AgentEvent.Stream(result) =>
        applyStreamEvent(state, result, now)
      case AgentEvent.SessionsListed(sessions) =>
        state.showSessionPicker(sessions.toVector)
      case AgentEvent.SessionSwitched(snapshot) =>
        state.switchedTo(snapshot)
      case AgentEvent.ModelSwitched(label, window, provider, modelId, baseUrl) =>
        state.copy(modelName = label, contextWindow = window, provider = provider, modelId = modelId, baseUrl = baseUrl)
      case AgentEvent.ContextCompactionStarted =>
        state.startCompaction(now)
      case AgentEvent.ContextCompacted(summary, estimatedTokens) =>
        state.contextCompacted(summary, estimatedTokens)
      case AgentEvent.Orchestration(ev) =>
        state.applyOrchestration(ev)
      case AgentEvent.Activity(ev) =>
        state.applyActivity(ev)
      case AgentEvent.Team(members) =>
        state.applyTeam(members)
      case AgentEvent.Interrupted =>
        state.interrupted
      case AgentEvent.Notice(message) =>
        state.notice(message)
      case AgentEvent.InputQueued(item) =>
        state.inputQueued(item)
      case AgentEvent.InputsConsumed(items) =>
        state.inputsConsumed(items)

  /** Fold a single LLM stream event into the chat state. */
  private def applyStreamEvent(
      state: ChatState,
      result: Result[StreamEvent, LLMError],
      now: Long
  ): ChatState =
    result match
      case Left(err)                            => state.failed(s"⚠ ${err.description}")
      case Right(StreamEvent.ThinkingDelta(t))  => state.appendThinking(t, now)
      case Right(StreamEvent.Delta(t))          => state.appendReply(t, now)
      case Right(StreamEvent.ToolCallStart(_, id, name)) => state.startTool(id, name, now)
      case Right(StreamEvent.ToolCallDelta(_, d))        => state.appendToolArgs(d)
      case Right(StreamEvent.ToolRunStart(id, _))        => state.startToolRun(id, now)
      case Right(StreamEvent.ToolRunProgress(id, md))    => state.progressToolRun(id, md)
      case Right(StreamEvent.ToolRunEnd(id, isErr, md, out)) => state.endToolRun(id, isErr, md, out, now)
      case Right(StreamEvent.RoundComplete(usage)) => state.anchorRoundUsage(usage).withContextUsage(Some(usage))
      case Right(StreamEvent.Done(response)) =>
        state.finishReply(response.message.text, now).withContextUsage(response.usage)

  /** A human label for a tool call, e.g. "Reading foo.scala", followed by a
    * timing/token annotation while or after it runs (see [[toolStatus]]). */
  private def toolLabel(t: Block.Tool, liveNow: Option[Long]): String =
    toolBase(t.name, t.rawArgs) + toolStatus(t, liveNow)

  /** The descriptive part of a tool label, derived from its streamed JSON
    * arguments; until they parse, just the verb is shown. */
  private def toolBase(name: String, rawArgs: String): String =
    name match
      case "read"         => labeled("Reading", "path", rawArgs)
      case "edit"         => labeled("Editing", "path", rawArgs)
      case "write"        => labeled("Writing", "path", rawArgs)
      case "eval_scala"   => "Scala"
      case other => labeled(other, "path", rawArgs)

  /** `verb arg` when the named field has streamed in, else just `verb`. */
  private def labeled(verb: String, field: String, rawArgs: String): String =
    jsonField(rawArgs, field) match
      case Some(value) => s"$verb $value"
      case None        => verb

  /** A dim " · 3.2s · 1.2k tokens" suffix describing a tool's execution. */
  private def toolStatus(t: Block.Tool, liveNow: Option[Long]): String =
    val running = t.startedMs.isDefined && t.elapsedMs.isEmpty
    val duration: Option[Long] =
      t.elapsedMs.orElse(for s <- t.startedMs; now <- liveNow yield now - s)
    val showDuration = duration.filter(ms => running || t.tokens.isDefined || ms >= 1000)
    val parts = showDuration.map(fmtDuration).toList ++ t.tokens.map(tk => s"${fmtTokens(tk)} tokens")
    if parts.isEmpty then "" else parts.mkString(" · ", " · ", "")

  // Hand-rolled round-half-up one-decimal, avoiding java.util.Formatter (slow
  // under Scala.js). `scaled` is the value in tenths; for ms/tokens (always >= 0)
  // `(x + 50) / 100` is round-half-up to tenths, byte-identical to the old
  // `%.1f` on the Scala.js target (verified by FmtSuite over the full range).
  private def oneDecimal(scaled: Long): String = s"${scaled / 10}.${scaled % 10}"
  private def fmtDuration(ms: Long): String = s"${oneDecimal((ms + 50) / 100)}s"

  private def fmtTokens(n: Long): String =
    if n >= 1000 then s"${oneDecimal((n + 50) / 100)}k" else n.toString

  /** Best-effort string-field lookup from streamed JSON arguments. */
  private def jsonField(rawArgs: String, field: String): Option[String] =
    Json.parse(rawArgs).toOption.collect { case o: Json.Obj => o }.flatMap { o =>
      o.get(field).collect { case Json.Str(s) => s }
    }

  private def dim(s: String): Element = Text(s).style(Style.Dim)
