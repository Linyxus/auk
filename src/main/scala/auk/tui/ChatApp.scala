package auk.tui

import auk.tui.app.*
import auk.tui.render.{Ansi, Attr, Color, Style}
import gears.async.{ReadableChannel, UnboundedChannel}
import auk.agent.{AgentEvent, UserCommand}
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
    keyCommands: Vector[ChatApp.Command] = Vector.empty,
    modelName: String = "",
    contextWindow: Int = 0,
    modelChoices: Vector[ModelChoice] = ChatApp.catalogChoices
) extends App[ChatState, Event]:

  /** Spinner / live-clock cadence while waiting for the first event. */
  private val SpinnerMs: Long = 100

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
    (ChatState.initial.copy(modelName = modelName, contextWindow = contextWindow), Cmd.none)

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

      // Enter sends only when idle; while a reply streams it can't send — instead
      // it surfaces a hint that you must interrupt first to follow up.
      case Event.Submit if state.idle && state.input.trim.nonEmpty =>
        val text = state.input.trim
        val next = state.submitted(text).copy(phase = Phase.Waiting, busyHint = false)
        // sendImmediately is non-blocking and needs no Async context.
        (next, Cmd.fire(commands.sendImmediately(UserCommand.Submit(text))))

      case Event.Submit if !state.idle && state.input.trim.nonEmpty =>
        (state.copy(busyHint = true), Cmd.none)

      // Up/Down move the cursor between the lines of a multi-line draft; only
      // when the cursor is already on the boundary line (top for Up, bottom for
      // Down) do they step through input history — and history recall stays
      // idle-only, while in-field cursor movement works in every phase.
      case Event.HistoryPrev =>
        if !state.onFirstLine then (state.cursorUp, Cmd.none)
        else if state.idle then (state.recallPrev, Cmd.none)
        else (state, Cmd.none)
      case Event.HistoryNext =>
        if !state.onLastLine then (state.cursorDown, Cmd.none)
        else if state.idle then (state.recallNext, Cmd.none)
        else (state, Cmd.none)

      case Event.Inbound1(agentEvent) =>
        // One engine event: fold it with a fresh clock so a running tool's
        // duration reflects arrival time. `commitIfDrained` lets a turn whose
        // reveal is already caught up (e.g. a short or empty reply) commit at
        // once; a lagging one stays live and the tick loop drains it.
        val now = System.currentTimeMillis()
        (applyAgentEvent(state, agentEvent, now).copy(clockMs = now).commitIfDrained, Cmd.none)

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
        case Overlay.SessionPicker(_, _) => sessionPickerEvent(key)
        case Overlay.ModelPicker(_, _, _) => modelPickerEvent(key)
        case Overlay.ResumeLoading(_)    => loadingOverlayEvent(key)
        case Overlay.None                => normalKeyEvent(key)
    }
    // Engine events are consumed natively as a gears channel — active in every
    // phase so deltas keep folding. The spinner clock only runs while a turn is
    // live (idle stays a static frame, so the renderer never repaints it).
    val engine = Sub.onChannel(events)(Event.Inbound1.apply, Event.InboundClosed)
    if state.idle then Sub.batch(keys, engine)
    else
      // A reply in flight reveals character-by-character (fast cadence); merely
      // waiting on the first event only needs the slower spinner cadence.
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

  private def loadingOverlayEvent(key: Key): Option[Event] =
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
    if state.busyHint && !state.idle then busyHintLine
    else
      val prefix = if state.modelName.isEmpty then "" else s"${state.modelName} · "
      val context = state.contextPercentUsed.map(p => s"$p% context used · ").getOrElse("")
      val hint = if state.idle then "ctrl+c for commands · ctrl+q quit" else "ctrl+c k to interrupt · ctrl+q quit"
      dim(s"  ${prefix}${context}$hint")

  /** Shown in place of the footer when you press Enter mid-reply: a calm, dim
    * line with the interrupt chord lifted in the frame colour. */
  private val busyHintLine: Element =
    val d = Style.Dim.setSequence
    val key = Style(fg = FrameBlue, attrs = Attr.Bold).setSequence
    Text(s"  ${d}Auk is working. To follow up, press ${Ansi.Reset}${key}Ctrl+C k${Ansi.Reset}${d} to interrupt it first.${Ansi.Reset}")

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

  private val KeyBindingsInnerWidth = 46
  private val SessionPickerInnerWidth = 68
  private val KeyColumnWidth = 6

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
      case Overlay.ResumeLoading(message) =>
        Some(resumeLoadingPanel(message))
      case Overlay.SessionPicker(sessions, selected) =>
        Some(sessionPickerPanel(sessions, selected))
      case Overlay.ModelPicker(choices, query, selected) =>
        Some(modelPickerPanel(choices, query, selected))

  private def keyBindingLine(key: String, action: String): String =
    s" ${padRight(key, KeyColumnWidth)}  $action"

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

  private def renderEntry(e: Entry, divider: Element): Element = e match
    case Entry.User(text) => layout(divider, roleHeader(Role.You), textBlock(text), divider)
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

  /** The "auk is thinking" working indicator — shown at the tail of the live turn
    * the whole time a reply is being generated, so it always sits just above the
    * input box. */
  private def workingLine(state: ChatState): Element =
    // A dim braille spinner leads the shimmering label: the spinner spins on the
    // frame counter, the highlight sweeps the text on wall-clock time.
    val glyph = EvalSpinner.charAt(math.floorMod(state.frame, EvalSpinner.length))
    val spin = DimSeq + glyph + " " + Ansi.Reset
    Text("  " + spin + Glow.sweep("auk is thinking", state.clockMs))

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
      case AgentEvent.ModelSwitched(label, window) =>
        state.copy(modelName = label, contextWindow = window)
      case AgentEvent.Interrupted =>
        state.interrupted

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
