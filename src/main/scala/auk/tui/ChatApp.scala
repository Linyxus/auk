package auk.tui

import auk.tui.app.*
import auk.tui.render.{Ansi, Attr, Color, Style}
import gears.async.{ReadableChannel, UnboundedChannel}
import auk.agent.{AgentEvent, UserCommand, Inbox}
import auk.workflow.{Forest, ForestNode, NodeStatus, RunStatus}
import auk.llm.endpoint.{StreamEvent, LLMError}
import auk.llm.provider.Providers
import auk.llm.tools.Json
import auk.tui.markdown.render.MarkdownRender
import auk.session.SessionSummary
import auk.utils.Result

object ChatApp:
  final class Command private (
      val keys: Vector[String],
      val description: String,
      val run: ChatState => (ChatState, Cmd[Event])
  )

  object Command:
    def apply(key: String, description: String)(run: ChatState => (ChatState, Cmd[Event])): Command =
      new Command(Vector(key), description, run)

    def apply(keys: Iterable[String], description: String)(run: ChatState => (ChatState, Cmd[Event])): Command =
      new Command(keys.toVector, description, run)

    def quit(firstKey: String, moreKeys: String*): Command =
      Command(firstKey +: moreKeys.toVector, "exit")(state => (state, Cmd.quit))

    def resume(commands: UnboundedChannel[UserCommand]): Command =
      Command("r", "resume session"): state =>
        if state.idle then
          (
            state.showResumeLoading("Loading sessions"),
            Cmd.fire(commands.sendImmediately(UserCommand.ListSessions))
          )
        else (state.hideOverlay, Cmd.none)

    def newSession(commands: UnboundedChannel[UserCommand]): Command =
      Command("n", "new session"): state =>
        if state.idle then
          (
            state.showResumeLoading("Starting new session"),
            Cmd.fire(commands.sendImmediately(UserCommand.NewSession))
          )
        else (state.hideOverlay, Cmd.none)

    def switchModel(choices: Vector[ModelChoice]): Command =
      Command("m", "switch model"): state =>
        if state.idle then (state.showModelPicker(choices), Cmd.none)
        else (state.hideOverlay, Cmd.none)

    /** `Ctrl+C k` while a turn is in flight: signal the engine to cancel it.
      * Gated opposite to the others — meaningful only when *not* idle; when idle
      * there is nothing to interrupt, so it just dismisses the palette. */
    def interrupt(interrupts: UnboundedChannel[Unit]): Command =
      Command("k", "interrupt"): state =>
        if !state.idle then (state.hideOverlay, Cmd.fire(interrupts.sendImmediately(())))
        else (state.hideOverlay, Cmd.none)

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
      Command("w", "view workflows")(state => (state.showWorkflowList, Cmd.none)),
      Command("b", "debug info")(state => (state.showDebugInfo, Cmd.none)),
      Command.interrupt(interrupts)
    )

  /** Every model from every catalog provider, flattened for the picker. */
  def catalogChoices: Vector[ModelChoice] =
    Providers.all.toVector.flatMap { p =>
      p.models.map(m => ModelChoice(p.name, p.name.toLowerCase, m.id, m.name, m.contextWindow))
    }

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
    modelChoices: Vector[ModelChoice] = ChatApp.catalogChoices
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
      case Event.WorkflowListUp     => (state.moveWorkflowSelection(-1), Cmd.none)
      case Event.WorkflowListDown   => (state.moveWorkflowSelection(1), Cmd.none)
      case Event.WorkflowOpen       => (state.openSelectedWorkflow, Cmd.none)
      case Event.WorkflowBack       => (state.backToWorkflowList, Cmd.none)
      case Event.WorkflowScrollUp   => (state.scrollWorkflowDetail(-1), Cmd.none)
      case Event.WorkflowScrollDown => (state.scrollWorkflowDetail(1), Cmd.none)
      case Event.WorkflowPause =>
        state.overlay match
          case Overlay.WorkflowDetail(runId, _) => (state, Cmd.fire(commands.sendImmediately(UserCommand.PauseWorkflow(runId))))
          case _                                => (state, Cmd.none)
      case Event.WorkflowResume =>
        state.overlay match
          case Overlay.WorkflowDetail(runId, _) => (state, Cmd.fire(commands.sendImmediately(UserCommand.ResumeWorkflow(runId))))
          case _                                => (state, Cmd.none)
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

      // Line editing works in every phase — you can compose your next message
      // while a reply is in flight. Only sending (Submit) waits for idle.
      case Event.KeyChar(c)     => (state.insert(c), Cmd.none)
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
        if !state.onLastLine then (state.cursorDown, Cmd.none)
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
        case Overlay.WorkflowList(_)     => workflowListEvent(key)
        case Overlay.WorkflowDetail(_, _) => workflowDetailEvent(key)
        case Overlay.ResumeLoading(_)    => loadingOverlayEvent(key)
        case Overlay.None                => normalKeyEvent(key)
    }
    // Engine events are consumed natively as a gears channel — active in every
    // phase so deltas keep folding. The spinner clock runs while a turn is live,
    // and also while a background workflow is running (so its panel animates even
    // though the agent itself is idle); a fully idle screen stays a static frame.
    val engine = Sub.onChannel(events)(Event.Inbound1.apply, Event.InboundClosed)
    if state.idle && state.activeWorkflows.isEmpty then Sub.batch(keys, engine)
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
      case _             => None

  /** Interpret the key after Ctrl-C. Unknown keys dismiss the overlay and are
    * swallowed so a failed chord does not edit the prompt. */
  private def commandOverlayEvent(key: Key): Option[Event] =
    commandKey(key) match
      case Some(key) if commandByKey.contains(normalizeCommandKey(key)) => Some(Event.RunCommand(key))
      case _                                                           => Some(Event.HideOverlay)

  private def sessionPickerEvent(key: Key): Option[Event] =
    key match
      case Key.Up    => Some(Event.SessionPickerUp)
      case Key.Down  => Some(Event.SessionPickerDown)
      case Key.Enter => Some(Event.ResumeSelected)
      case Key.Esc   => Some(Event.HideOverlay)
      case _         => None

  private def modelPickerEvent(key: Key): Option[Event] =
    key match
      case Key.Up        => Some(Event.ModelPickerUp)
      case Key.Down      => Some(Event.ModelPickerDown)
      case Key.Enter     => Some(Event.ModelSelected)
      case Key.Backspace => Some(Event.ModelPickerSearchBackspace)
      case Key.Delete    => Some(Event.ModelPickerSearchBackspace)
      case Key.Ctrl('U') => Some(Event.ModelPickerSearchClear)
      case Key.Char(c)   => Some(Event.ModelPickerSearchChar(c))
      case Key.Esc       => Some(Event.HideOverlay)
      case _             => None

  /** The workflow menu: ↑/↓ pick a run, Enter opens its detail, Esc closes. */
  private def workflowListEvent(key: Key): Option[Event] =
    key match
      case Key.Up    => Some(Event.WorkflowListUp)
      case Key.Down  => Some(Event.WorkflowListDown)
      case Key.Enter => Some(Event.WorkflowOpen)
      case Key.Esc   => Some(Event.HideOverlay)
      case _         => None

  /** One run's forest: ↑/↓ scroll, p/r pause/resume, Esc returns to the list. */
  private def workflowDetailEvent(key: Key): Option[Event] =
    key match
      case Key.Up         => Some(Event.WorkflowScrollUp)
      case Key.Down       => Some(Event.WorkflowScrollDown)
      case Key.Char('p')  => Some(Event.WorkflowPause)
      case Key.Char('r')  => Some(Event.WorkflowResume)
      case Key.Esc        => Some(Event.WorkflowBack)
      case _              => None

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
  private var cachedEpoch: Long = -1L
  private var cachedEntries: Vector[Element] = Vector.empty

  private def committedEntries(state: ChatState, divider: Element): Vector[Element] =
    val n = state.history.length
    if state.transcriptEpoch != cachedEpoch || n < cachedEntries.length then
      // New transcript (session switch) or a shorter history: rebuild from scratch.
      cachedEpoch = state.transcriptEpoch
      cachedEntries = state.history.map(renderEntry(_, divider))
    else if n > cachedEntries.length then
      // Steady state: only the appended tail is new.
      cachedEntries = cachedEntries ++ state.history.drop(cachedEntries.length).map(renderEntry(_, divider))
    cachedEntries

  def view(state: ChatState): Screen =
    val divider = hr('─', FrameBlue)
    // Committed: the header (printed once) and every finalized transcript entry,
    // each laid out and flushed to native scrollback exactly once.
    val committed: Vector[Element] = headerBlock +: committedEntries(state, divider)
    // Live: the still-changing turn, the input box, and the footer.
    val live: Element = layout(
      emptyHint(state),
      inProgress(state),
      br,
      overlayBlock(state),
      noticesBlock(state),
      workflowNotice(state),
      queueBlock(state),
      divider,
      prompt(state),
      divider,
      footer(state)
    )
    Screen(committed, live, committedEpoch = state.transcriptEpoch)

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
    layout(
      Color.Cyan("  Auk").style(Style.Bold),
      dim("  a coding agent")
    )

  /** The header committed once at startup (with a trailing blank line). */
  private val headerBlock: Element = layout(header, br)

  private def footer(state: ChatState): Element =
    val prefix = if state.modelName.isEmpty then "" else s"${state.modelName} · "
    val context = state.contextPercentUsed.map(p => s"$p% context used · ").getOrElse("")
    val hint = if state.idle then "ctrl+c for commands · ctrl+q quit" else "ctrl+c k to interrupt · ctrl+q quit"
    dim(s"  ${prefix}${context}$hint")

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

  // Workflow-forest row styles, all on the overlay's dark bg. Each forest row is
  // a single uniform style picked by node status, so active rows pop and settled
  // ones recede without per-span SGR punching holes in the dark background.
  private val OverlayGroupStyle: Style =
    Style(fg = FrameBlue, bg = Color.Indexed(236), attrs = Attr.Bold)
  private val OverlayDoneStyle: Style =
    Style(fg = Color.Green, bg = Color.Indexed(236))
  private val OverlayFailStyle: Style =
    Style(fg = Color.Red, bg = Color.Indexed(236))

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
    val rows = registeredKeyCommands.map { command =>
      framed(keyBindingLine(command.keys.mkString(", "), command.description), OverlayBodyStyle, KeyBindingsInnerWidth)
    }
    Vector(Text(top).style(OverlayFrameStyle), title, framed("", OverlayBodyStyle, KeyBindingsInnerWidth)) ++
      rows :+ Text(bottom).style(OverlayFrameStyle)

  private val keyBindingsPanel: Element =
    layout(keyBindingsPanelLines*)

  private def overlayBlock(state: ChatState): Element =
    overlayElement(state) match
      case Some(panel) => layout(panel, br)
      case None        => Empty

  private def overlayElement(state: ChatState): Option[Element] =
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
      case Overlay.WorkflowList(selected) =>
        Some(workflowListPanel(state.activeWorkflows, selected, state.clockMs))
      case Overlay.WorkflowDetail(runId, scroll) =>
        Some(workflowDetailPanel(runId, state.activeWorkflows, scroll, state.clockMs))

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

  /** The empty-transcript hint — lives in the live region so it vanishes once
    * the first message lands. */
  private def emptyHint(state: ChatState): Element =
    if state.history.isEmpty then layout(br, dim("  Type a message and press Enter."))
    else Empty

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
    * with a dim `◆`. Each item soft-wraps under a rail-aligned hanging indent, so
    * a long line stays inside the rail. Empty ⇒ the panel is absent (the live
    * stack collapses, like the notices/overlay blocks). The header count is the
    * true total even when the listed rows are capped. */
  private def queueBlock(state: ChatState): Element =
    if state.pendingQueue.isEmpty then Empty
    else
      val rail = Style.fg(FrameBlue).setSequence
      val plain = Ansi.Reset
      val n = state.pendingQueue.length
      val header = Text(s"  $rail╭─ $plain${WordmarkSeq}queued$plain$DimSeq · $n$plain")
      val rows = state.pendingQueue.take(MaxQueuedShown).map(queueRow(_, rail, plain))
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

  /** One queued row: a soft-blue rail, a kind marker, then the message
    * soft-wrapped with a hanging indent aligned under the text. Newlines are
    * flattened so each item is one wrapping paragraph — the queue stays scannable
    * (the model still receives the verbatim text). */
  private def queueRow(item: Inbox, rail: String, plain: String): Element =
    val marker = item match
      case Inbox.UserMessage(_)  => PromptArrow           // cyan ›
      case Inbox.SystemNotice(_) => s"$DimSeq◆$plain"     // dim ◆
    wrapText(s"  $rail$Bar$plain $marker ", s"  $rail$Bar$plain   ", item.text.replace('\n', ' '))

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

  /** The "auk is thinking" working indicator — shown at the tail of the live turn
    * the whole time a reply is being generated, so it always sits just above the
    * input box. */
  private def workingLine(state: ChatState): Element =
    // A dim braille spinner leads the shimmering label: the spinner spins on the
    // frame counter, the highlight sweeps the text on wall-clock time.
    val glyph = EvalSpinner.charAt(math.floorMod(state.frame, EvalSpinner.length))
    val spin = DimSeq + glyph + " " + Ansi.Reset
    val stats = DimSeq + thinkingStats(state) + Ansi.Reset
    Text("  " + spin + Glow.sweep("auk is thinking", state.clockMs) + stats)

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

  private def inProgress(state: ChatState): Element =
    state.phase match
      case Phase.Waiting =>
        layout(roleHeader(Role.Auk), workingLine(state))

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
              MarkdownRender.answerBlock(doc, glow = Some((hot, state.frame)))
            case Block.Thinking(typed, _, None) =>
              thinkingLive(typed, state.frame)
            case other => renderBlock(other, liveNow = Some(state.clockMs))
        // Keep the working indicator pinned to the end of the turn, right above
        // the input box, for the whole generation — with a blank line between it
        // and the generated text for readability.
        layout(((roleHeader(Role.Auk) +: rendered) ++ Vector(br, workingLine(state)))*)

      case Phase.Idle => Empty

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
    wrapText(s" $PromptArrow ", "   ", s"$before$cell$after")

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

  /* ---- Workflow menu overlays (the `ctrl+c w` list + per-run detail) ---- */

  private val WorkflowBarW = 8       // progress-bar cells in the list
  private val WorkflowListIdW = 14   // run-id column in the list
  private val WorkflowIdW = 24       // node-id column in the detail forest
  private val WorkflowTokW = 6       // token column in the detail forest
  private val WorkflowDetailRows = 12 // scrollable body rows shown at once
  private val MaxWorkflowLogLines = 5 // log lines tailed in the detail

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

  /** The uniform row style for a sub-agent, by status: active rows stay bright,
    * settled rows take their verdict colour, waiting rows recede. */
  private def workflowNodeStyle(status: NodeStatus): Style =
    status match
      case NodeStatus.Done    => OverlayDoneStyle
      case NodeStatus.Failed  => OverlayFailStyle
      case NodeStatus.Running => OverlayBodyStyle
      case _                  => OverlayMutedStyle

  /** One sub-agent row inside the detail forest: indent, status glyph, node id,
    * its live token count, and the tool it is running — all one uniform style. */
  private def workflowNodeRow(n: ForestNode, clockMs: Long, innerWidth: Int): Element =
    val glyph = workflowNodeGlyph(n.status, clockMs)
    val toks = if n.outputTokens > 0 then fmtTokens(n.outputTokens) else ""
    val tool = n.currentTool.getOrElse("")
    framed(s"   $glyph ${cell(n.id, WorkflowIdW)}  ${cell(toks, WorkflowTokW)}  $tool", workflowNodeStyle(n.status), innerWidth)

  /** The scrollable detail body: the forest grouped by group (group label then
    * its sub-agents, ungrouped last), followed — when present — by a `── logs ──`
    * divider and a tail of the most recent log lines. */
  private def workflowForestRows(forest: Forest, clockMs: Long, innerWidth: Int): Vector[Element] =
    val byGroup = forest.nodes.groupBy(_.group)
    val names = forest.groups.map(g => g.id -> g.name).toMap
    val order: List[Option[String]] =
      forest.groups.map(g => Some(g.id)).toList ++ (if byGroup.contains(None) then List(None) else Nil)
    val tree = order.toVector.flatMap: gid =>
      val ns = byGroup.getOrElse(gid, Vector.empty)
      if ns.isEmpty then Vector.empty
      else
        val head = gid.flatMap(names.get).map(name => framed(s" ▸ $name", OverlayGroupStyle, innerWidth)).toVector
        head ++ ns.map(n => workflowNodeRow(n, clockMs, innerWidth))
    val logs =
      if forest.logs.isEmpty then Vector.empty
      else
        val label = " ── logs "
        val divider = framed(label + "─" * math.max(0, innerWidth - label.length), OverlayMutedStyle, innerWidth)
        val lines = forest.logs.takeRight(MaxWorkflowLogLines).map(l => framed(s"   ${truncate(l, innerWidth - 3)}", OverlayMutedStyle, innerWidth))
        (framed("", OverlayBodyStyle, innerWidth) +: divider +: lines)
    tree ++ logs

  /** The detail header content: run id on the left, `settled/total · tokens` on
    * the right, padded to the inner width. */
  private def workflowDetailHeader(runId: String, status: RunStatus, settled: Int, total: Int, tokens: Long, innerWidth: Int): String =
    val tag = status match
      case RunStatus.Paused => " · paused"
      case _                => ""
    val left = s" $runId$tag"
    val right = s"$settled/$total · ${fmtTokens(tokens)} tokens "
    val gap = math.max(1, innerWidth - left.length - right.length)
    truncate(left + (" " * gap) + right, innerWidth)

  /** The workflow menu: one row per running `wf.start`, each a progress bar and
    * `settled/total`. ↑/↓ select, Enter opens the run's detail. The run set is
    * read live, so an empty list shows its own state. */
  private def workflowListPanel(workflows: Vector[(String, Forest)], selected: Int, clockMs: Long): Element =
    val iw = SessionPickerInnerWidth
    val rows =
      if workflows.isEmpty then
        Vector(
          framed(" Workflows", OverlayHeaderStyle, iw),
          framed("", OverlayBodyStyle, iw),
          framed(" No workflows running", OverlayMutedStyle, iw),
          framed(" Press Esc to return", OverlayMutedStyle, iw)
        )
      else
        val sel = math.max(0, math.min(workflows.length - 1, selected))
        val maxVisible = 8
        val start = math.max(0, math.min(sel - maxVisible + 1, workflows.length - maxVisible))
        val visible = workflows.zipWithIndex.slice(start, start + maxVisible)
        val listRows = visible.map: (entry, idx) =>
          val (runId, forest) = entry
          val total = forest.nodes.length
          val marker = if idx == sel then "›" else " "
          val bar = progressBar(settledNodes(forest), total, WorkflowBarW)
          val tag = if forest.status == RunStatus.Paused then "  paused" else ""
          val content = s" $marker ${cell(runId, WorkflowListIdW)}  $bar  ${settledNodes(forest)}/$total$tag"
          val style = if idx == sel then OverlaySelectedStyle else OverlayBodyStyle
          framed(content, style, iw)
        val range =
          if workflows.length > maxVisible then s"  ${start + 1}-${start + visible.length} of ${workflows.length}"
          else ""
        Vector(
          framed(s" Workflows · ${workflows.length}", OverlayHeaderStyle, iw),
          framed("", OverlayBodyStyle, iw)
        ) ++ listRows :+
          framed(s" ↑/↓ select  Enter view  Esc close$range", OverlayMutedStyle, iw)
    framedPanel(iw, rows)

  /** One run's live forest + recent logs, keyed by run id (looked up live each
    * frame). ↑/↓ scroll the body, Esc returns to the list. If the run has since
    * finished and dropped out, a short "finished" state is shown instead. */
  private def workflowDetailPanel(runId: String, workflows: Vector[(String, Forest)], scroll: Int, clockMs: Long): Element =
    val iw = SessionPickerInnerWidth
    workflows.collectFirst { case (id, f) if id == runId => f } match
      case None =>
        framedPanel(iw, Vector(
          framed(s" $runId", OverlayHeaderStyle, iw),
          framed("", OverlayBodyStyle, iw),
          framed(" This workflow has finished", OverlayMutedStyle, iw),
          framed(" Press Esc to return", OverlayMutedStyle, iw)
        ))
      case Some(forest) =>
        val total = forest.nodes.length
        val header = framed(workflowDetailHeader(runId, forest.status, settledNodes(forest), total, forest.nodes.map(_.outputTokens).sum, iw), OverlayHeaderStyle, iw)
        val body = workflowForestRows(forest, clockMs, iw)
        val maxScroll = math.max(0, body.length - WorkflowDetailRows)
        val start = math.max(0, math.min(scroll, maxScroll))
        val visible =
          if body.isEmpty then Vector(framed(" Starting…", OverlayMutedStyle, iw))
          else body.slice(start, start + WorkflowDetailRows)
        val range =
          if body.length > WorkflowDetailRows then s"  ${start + 1}-${start + visible.length} of ${body.length}"
          else ""
        // Pause is offered while running; resume while paused.
        val control = forest.status match
          case RunStatus.Paused  => "r resume  "
          case RunStatus.Running => "p pause  "
          case _                 => ""
        val rows = (header +: framed("", OverlayBodyStyle, iw) +: visible) :+
          framed(s" ↑/↓ scroll  ${control}Esc back$range", OverlayMutedStyle, iw)
        framedPanel(iw, rows)

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
      case AgentEvent.Orchestration(ev) =>
        state.applyOrchestration(ev)
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
      case Right(StreamEvent.RoundComplete(usage)) => state.anchorRoundUsage(usage)
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
      case "sub_agent"    => labeled("Sub-agent:", "description", rawArgs)
      case "write_memory" => labeled("Remembering", "key", rawArgs)
      // get_memory with a key recalls one note; without one it lists them all.
      case "get_memory" =>
        jsonField(rawArgs, "key") match
          case Some(key) => s"Recalling $key"
          case None      => "Recalling all memories"
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
