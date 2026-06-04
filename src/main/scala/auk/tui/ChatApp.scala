package auk.tui

import auk.tui.app.*
import auk.tui.render.{Attr, Color, Style}
import gears.async.{ReadableChannel, UnboundedChannel}
import auk.agent.{AgentEvent, UserCommand}
import auk.llm.endpoint.{StreamEvent, LLMError}
import auk.llm.provider.Providers
import auk.llm.tools.Json
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

  def defaultCommands(
      commands: UnboundedChannel[UserCommand],
      modelChoices: Vector[ModelChoice]
  ): Vector[Command] =
    Vector(
      Command.quit("c", "q"),
      Command.resume(commands),
      Command.newSession(commands),
      Command.switchModel(modelChoices)
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
    keyCommands: Vector[ChatApp.Command] = Vector.empty,
    modelName: String = "",
    modelChoices: Vector[ModelChoice] = ChatApp.catalogChoices
) extends App[ChatState, Event]:

  /** Spinner / live-clock cadence while waiting for the first event. */
  private val SpinnerMs: Long = 100

  /** Typewriter-reveal cadence while a reply is in the live region. Fast enough
    * to look continuous; paired with `Typewriter`'s adaptive drain it yields a
    * smooth ~150 ms catch-up regardless of how the deltas burst in. */
  private val RevealMs: Long = 30
  private val registeredKeyCommands: Vector[ChatApp.Command] =
    if keyCommands.isEmpty then ChatApp.defaultCommands(commands, modelChoices) else keyCommands
  private val commandByKey: Map[String, ChatApp.Command] =
    registeredKeyCommands.flatMap(command => command.keys.map(key => normalizeCommandKey(key) -> command)).toMap

  /* ---- Elm architecture: init / update / subscriptions / view ---- */

  def init: (ChatState, Cmd[Event]) = (ChatState.initial.copy(modelName = modelName), Cmd.none)

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

      case Event.KeyChar(c) if state.idle     => (state.insert(c), Cmd.none)
      case Event.Backspace if state.idle      => (state.backspace, Cmd.none)
      case Event.DeleteForward if state.idle  => (state.deleteForward, Cmd.none)
      case Event.Newline if state.idle        => (state.insert('\n'), Cmd.none)
      case Event.CursorLeft if state.idle     => (state.cursorLeft, Cmd.none)
      case Event.CursorRight if state.idle    => (state.cursorRight, Cmd.none)
      case Event.CursorHome if state.idle     => (state.cursorHome, Cmd.none)
      case Event.CursorEnd if state.idle      => (state.cursorEnd, Cmd.none)
      case Event.KillToEnd if state.idle      => (state.killToEnd, Cmd.none)
      case Event.KillToStart if state.idle    => (state.killToStart, Cmd.none)
      case Event.DeleteWordBack if state.idle => (state.deleteWordBack, Cmd.none)

      case Event.Submit if state.idle && state.input.trim.nonEmpty =>
        val text = state.input.trim
        val next = state.submitted(text).copy(phase = Phase.Waiting)
        // sendImmediately is non-blocking and needs no Async context.
        (next, Cmd.fire(commands.sendImmediately(UserCommand.Submit(text))))

      case Event.HistoryPrev if state.idle => (state.recallPrev, Cmd.none)
      case Event.HistoryNext if state.idle => (state.recallNext, Cmd.none)

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
        case Overlay.ModelPicker(_, _)   => modelPickerEvent(key)
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
      case Key.Up    => Some(Event.ModelPickerUp)
      case Key.Down  => Some(Event.ModelPickerDown)
      case Key.Enter => Some(Event.ModelSelected)
      case Key.Esc   => Some(Event.HideOverlay)
      case _         => None

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

  def view(state: ChatState): Screen =
    val divider = hr('─', FrameBlue)
    // Committed: the header (printed once) and every finalized transcript entry,
    // each laid out and flushed to native scrollback exactly once.
    val committed: Vector[Element] = headerBlock +: state.history.map(renderEntry(_, divider))
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
    f"✻ Thought for ${millis / 1000.0}%.1fs"

  /** The left-bar glyph that marks reasoning and tool-call blocks. */
  private val Bar = "│"

  /** A soft, elegant light blue used to frame the input and user messages. */
  private val FrameBlue: Color = Color.True(135, 206, 235)

  private val header: Element =
    layout(
      Color.Cyan("  Auk").style(Style.Bold),
      dim("  a coding agent")
    )

  /** The header committed once at startup (with a trailing blank line). */
  private val headerBlock: Element = layout(header, br)

  private def footer(state: ChatState): Element =
    val prefix = if state.modelName.isEmpty then "" else s"${state.modelName} · "
    dim(s"  ${prefix}ctrl+c for commands · ctrl+q quit")

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
      case Overlay.ModelPicker(choices, selected) =>
        Some(modelPickerPanel(choices, selected))

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

  private def modelPickerPanel(choices: Vector[ModelChoice], selected: Int): Element =
    val title = framed(" Switch model", OverlayHeaderStyle, SessionPickerInnerWidth)
    val rows =
      if choices.isEmpty then
        Vector(
          title,
          framed("", OverlayBodyStyle, SessionPickerInnerWidth),
          framed(" No models configured", OverlayMutedStyle, SessionPickerInnerWidth),
          framed(" Press Esc to return", OverlayMutedStyle, SessionPickerInnerWidth)
        )
      else
        val header = framed(modelRow(" ", "Model", "Provider", "Model id", "Context"), OverlayMutedStyle, SessionPickerInnerWidth)
        val maxVisible = 10
        val start = math.max(0, math.min(selected - maxVisible + 1, choices.length - maxVisible))
        val visibleChoices = choices.zipWithIndex.slice(start, start + maxVisible)
        val visible = visibleChoices.map: (choice, idx) =>
          val marker = if idx == selected then "›" else " "
          val content = modelRow(marker, choice.modelLabel, choice.providerName, choice.modelId, contextLabel(choice.contextWindow))
          val style = if idx == selected then OverlaySelectedStyle else OverlayBodyStyle
          framed(content, style, SessionPickerInnerWidth)
        val range =
          if choices.length > maxVisible then s"  ${start + 1}-${start + visibleChoices.length} of ${choices.length}"
          else ""
        Vector(title, header) ++ visible :+
          framed(s" ↑/↓ select  Enter switch  Esc cancel$range", OverlayMutedStyle, SessionPickerInnerWidth)
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

  /** Render one assistant block. Reasoning and tool calls get a dim left bar;
    * answer text is plain, under the "Auk" header. `liveNow` is the render
    * clock, supplied while streaming so a running tool's duration ticks. */
  private def renderBlock(b: Block, liveNow: Option[Long]): Element = b match
    case Block.Thinking(_, _, Some(ms))  => barBlock(thoughtLabel(ms))
    case Block.Thinking(typed, _, None)  => barBlock(s"thinking ▸ ${typed.visible}")
    case t: Block.Tool                  => barBlock(toolLabel(t, liveNow))
    case Block.Answer(typed)            => textBlock(typed.visible)

  private def inProgress(state: ChatState): Element =
    state.phase match
      case Phase.Waiting =>
        layout(
          roleHeader(Role.Auk),
          Text("  " + spinner(label = "auk is thinking", frame = state.frame).render)
        )

      case Phase.Streaming(blocks, _) =>
        val rendered = blocks.zipWithIndex.map: (b, i) =>
          // Freshly-revealed text glows and the breathing cursor rides the tail
          // of whichever block is still streaming in (the answer being written,
          // or the reasoning while it is still open — both are always last).
          b match
            case Block.Answer(typed) if i == blocks.length - 1 =>
              val body = Glow.trail(typed.visible, typed.coolPrefixLen, Glow.AnswerHot, Glow.AnswerCool)
              textBlock(body + Glow.cursor(state.frame))
            case Block.Thinking(typed, _, None) =>
              thinkingLive(typed, state.frame)
            case other => renderBlock(other, liveNow = Some(state.clockMs))
        layout((roleHeader(Role.Auk) +: rendered)*)

      case Phase.Idle => Empty

  /** A frameless prompt line with a reverse-video block cursor at [[ChatState.cursor]],
    * so it can sit mid-line. Steady (non-blinking) so the idle view stays static. */
  private def prompt(state: ChatState): Element =
    val arrow = Color.Cyan("›").render
    if !state.idle then Text(s"  $arrow ${dim("…").render}")
    else
      val before = state.input.take(state.cursor)
      val (atCursor, after) =
        if state.cursor < state.input.length then
          val ch = state.input(state.cursor)
          if ch == '\n' then (" ", "\n" + state.input.drop(state.cursor + 1))
          else (ch.toString, state.input.drop(state.cursor + 1))
        else (" ", "")
      val cell = Text(atCursor).style(Style.Reverse).render
      // Two-space indent aligns the prompt with role headers, the non-idle
      // prompt, and the 4-column continuation prefix below.
      wrapText(s"  $arrow ", "    ", s"$before$cell$after")

  /** The "You" / "Auk" header line that sits above an entry's content. */
  private def roleHeader(role: Role): Element =
    role match
      case Role.You => Text(s"  ${Color.Cyan("You").style(Style.Bold).render}")
      case Role.Auk => Text(s"  ${Color.Green("Auk").style(Style.Bold).render}")

  /** Plain, indented content; one rendered line per source line. */
  private def textBlock(text: String): Element =
    layout(splitLines(text).map(l => Text(s"  $l"))*)

  /** A dim, left-barred block; one barred line per source line. */
  private def barBlock(text: String): Element =
    layout(splitLines(text).map(l => dim(s"  $Bar $l"))*)

  /** Open reasoning while it streams: a dim "│ thinking ▸" frame whose content
    * glows just behind the reveal (newest words brightest), with the breathing
    * cursor at the tail. The frame and the normal content colour are re-asserted
    * on every wrapped line so styling never leaks across a line break. */
  private def thinkingLive(typed: Typewriter, frame: Int): Element =
    val barSeq = Style.fg(Glow.ThinkBar).setSequence
    val normSeq = Style.fg(Glow.ThinkNormal).setSequence
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
      case AgentEvent.ModelSwitched(label) =>
        state.copy(modelName = label)

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
      case Right(StreamEvent.ToolRunEnd(id, _, md))      => state.endToolRun(id, md, now)
      case Right(StreamEvent.Done(response))    => state.finishReply(response.message.text, now)

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
      case "bash"         => labeled("Bash", "command", rawArgs)
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

  private def fmtDuration(ms: Long): String = f"${ms / 1000.0}%.1fs"

  private def fmtTokens(n: Long): String =
    if n >= 1000 then f"${n / 1000.0}%.1fk" else n.toString

  /** Best-effort string-field lookup from streamed JSON arguments. */
  private def jsonField(rawArgs: String, field: String): Option[String] =
    Json.parse(rawArgs).toOption.collect { case o: Json.Obj => o }.flatMap { o =>
      o.get(field).collect { case Json.Str(s) => s }
    }

  private def dim(s: String): Element = Text(s).style(Style.Dim)
