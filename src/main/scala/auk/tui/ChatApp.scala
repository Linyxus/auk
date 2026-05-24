package auk.tui

import layoutz.*
import gears.async.{ReadableChannel, UnboundedChannel}
import auk.agent.UserCommand
import auk.llm.endpoint.{StreamEvent, LLMError}
import auk.utils.Result

/** An animated chat-style TUI for auk, driven by the engine channels.
  *
  * It is a pure consumer of the engine's event stream: submitted lines are
  * pushed to `commands`, and assistant replies are rendered from the
  * [[StreamEvent]]s drained off `events`. It never talks to a model directly —
  * the [[auk.agent.Engine]] on the other end of the channels does.
  *
  * @param events   LLM events from the engine (engine → UI).
  * @param commands user commands to the engine (UI → engine); concrete
  *                 `UnboundedChannel` so we can `sendImmediately` off a layoutz
  *                 callback thread (which has no Gears `Async` context).
  */
final class ChatApp(
    events: ReadableChannel[Result[StreamEvent, LLMError]],
    commands: UnboundedChannel[UserCommand]
) extends LayoutzApp[ChatState, Event]:

  /** Animation / drain cadence. */
  private val IntervalMs: Long = 45

  /* ---- Elm architecture: init / update / subscriptions / view ---- */

  def init: (ChatState, Cmd[Event]) = (ChatState.initial, Cmd.none)

  def update(event: Event, state: ChatState): (ChatState, Cmd[Event]) =
    event match
      case Event.KeyChar(c) if state.idle     => (state.insert(c), Cmd.none)
      case Event.Backspace if state.idle      => (state.backspace, Cmd.none)
      case Event.DeleteForward if state.idle  => (state.deleteForward, Cmd.none)
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
        // Hand the command to the engine. sendImmediately is non-blocking and
        // needs no Async context, so it's safe from this layoutz thread.
        (next, Cmd.fire(commands.sendImmediately(UserCommand.Submit(text))))

      case Event.HistoryPrev if state.idle =>
        (state.recallPrev, Cmd.none)

      case Event.HistoryNext if state.idle =>
        (state.recallNext, Cmd.none)

      case Event.Tick =>
        // Single clock: advance the spinner and drain whatever the engine has
        // queued, folding each event into the state.
        val advanced = state.copy(frame = state.frame + 1)
        (drainInbound().foldLeft(advanced)(applyEvent), Cmd.none)

      case _ =>
        (state, Cmd.none)

  def subscriptions(state: ChatState): Sub[Event] =
    val keys = Sub.onKeyPress {
      case Key.Char(c)   => Some(Event.KeyChar(c))
      case Key.Backspace => Some(Event.Backspace)
      case Key.Delete    => Some(Event.DeleteForward)
      case Key.Enter     => Some(Event.Submit)
      case Key.Up        => Some(Event.HistoryPrev)
      case Key.Down      => Some(Event.HistoryNext)
      case Key.Left      => Some(Event.CursorLeft)
      case Key.Right     => Some(Event.CursorRight)
      case Key.Home      => Some(Event.CursorHome)
      case Key.End       => Some(Event.CursorEnd)
      case Key.Ctrl(c)   => ctrlEvent(c)
      case _             => None
    }
    // Idle renders a static frame, so layoutz's diff never repaints it (no
    // flicker). While a turn is live, a single Tick clock both animates the
    // spinner and drains the engine channel — see Event.Tick.
    if state.idle then keys
    else Sub.batch(Sub.time.everyMs(IntervalMs, Event.Tick), keys)

  /** Map an emacs-style Ctrl chord to a line-editing event. */
  private def ctrlEvent(c: Char): Option[Event] =
    c.toUpper match
      case 'A' => Some(Event.CursorHome)
      case 'E' => Some(Event.CursorEnd)
      case 'K' => Some(Event.KillToEnd)
      case 'U' => Some(Event.KillToStart)
      case 'W' => Some(Event.DeleteWordBack)
      case 'B' => Some(Event.CursorLeft)
      case 'F' => Some(Event.CursorRight)
      case 'D' => Some(Event.DeleteForward)
      case _   => None

  def view(state: ChatState): Element =
    layout(
      header,
      br,
      transcript(state),
      inProgress(state),
      br,
      prompt(state),
      footer
    )

  /* ---- Channel bridge ---- */

  /** Drain everything currently buffered on the events channel. `poll()` is
    * non-blocking and needs no Async context (safe from a layoutz thread); an
    * empty result yields an empty list, which folds to a no-op. */
  private def drainInbound(): List[Result[StreamEvent, LLMError]] =
    val src = events.readSource
    val buf = scala.collection.mutable.ListBuffer[Result[StreamEvent, LLMError]]()
    var draining = true
    while draining do
      src.poll() match
        case Some(Right(result)) => buf += result
        case Some(Left(_))       => draining = false // channel closed
        case None                => draining = false // nothing buffered now
    buf.toList

  /** Fold a single LLM event into the chat state. */
  private def applyEvent(
      state: ChatState,
      result: Result[StreamEvent, LLMError]
  ): ChatState =
    val (thinking, reply) = state.phase match
      case Phase.Streaming(t, r) => (t, r)
      case _                     => ("", "")
    result match
      case Left(err) =>
        commit(state, s"⚠ ${err.description}")

      case Right(StreamEvent.ThinkingDelta(t)) =>
        state.copy(phase = Phase.Streaming(thinking + t, reply))

      case Right(StreamEvent.Delta(t)) =>
        state.copy(phase = Phase.Streaming(thinking, reply + t))

      case Right(StreamEvent.Done(response)) =>
        commit(state, if reply.nonEmpty then reply else response.message.text)

      // Tool-call events: not handled yet.
      case Right(_) => state

  /** Commit a finished assistant line to the transcript and return to idle. */
  private def commit(state: ChatState, text: String): ChatState =
    state.copy(
      history = state.history :+ Message(Role.Auk, text),
      phase = Phase.Idle
    )

  /* ---- View helpers ---- */

  private val header: Element =
    layout(
      Color.Cyan("  auk").style(Style.Bold),
      dim("  a coding agent")
    )

  private val footer: Element = dim("  ctrl+q to quit")

  private def transcript(state: ChatState): Element =
    if state.history.isEmpty then
      dim("  Type a message and press Enter.")
    else layout(state.history.map(renderMessage)*)

  private def renderMessage(m: Message): Element =
    Text(s"  ${label(m.role)} ${m.text}")

  private def inProgress(state: ChatState): Element =
    state.phase match
      case Phase.Waiting =>
        Text("  " + spinner(label = "auk is thinking", frame = state.frame).render)

      case Phase.Streaming(thinking, reply) =>
        // Reasoning is rendered dimmed above the answer, as ephemeral
        // scaffolding; only the answer is committed to the transcript.
        val answer = Text(s"  ${label(Role.Auk)} $reply${Color.Green("▌").render}")
        if thinking.nonEmpty then layout(dim(s"  thinking ▸ $thinking"), answer)
        else answer

      case Phase.Idle =>
        Text("")

  /** A frameless prompt line with a reverse-video block cursor at [[ChatState.cursor]],
    * so it can sit mid-line. Steady (non-blinking) so the idle view stays static
    * — see [[subscriptions]] for why that matters. */
  private def prompt(state: ChatState): Element =
    val arrow = Color.Cyan("›").render
    if !state.idle then Text(s"  $arrow ${dim("…").render}")
    else
      val before = state.input.take(state.cursor)
      val atCursor =
        if state.cursor < state.input.length then state.input(state.cursor).toString
        else " "
      val after =
        if state.cursor < state.input.length then state.input.drop(state.cursor + 1)
        else ""
      val cell = Text(atCursor).style(Style.Reverse).render
      Text(s"  $arrow $before$cell$after")

  /** Bold, color-coded role label, rendered to an ANSI string. */
  private def label(role: Role): String =
    role match
      case Role.You => Color.Cyan("you ▸").style(Style.Bold).render
      case Role.Auk => Color.Green("auk ▸").style(Style.Bold).render

  private def dim(s: String): Element = Text(s).style(Style.Dim)
