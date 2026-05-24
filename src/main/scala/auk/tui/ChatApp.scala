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
      case Event.KeyChar(c) if state.idle =>
        (state.copy(input = state.input + c), Cmd.none)

      case Event.Backspace if state.idle =>
        (state.copy(input = state.input.dropRight(1)), Cmd.none)

      case Event.Submit if state.idle && state.input.trim.nonEmpty =>
        val text = state.input.trim
        val next = state.copy(
          history = state.history :+ Message(Role.You, text),
          input = "",
          phase = Phase.Waiting
        )
        // Hand the command to the engine. sendImmediately is non-blocking and
        // needs no Async context, so it's safe from this layoutz thread.
        (next, Cmd.fire(commands.sendImmediately(UserCommand.Submit(text))))

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
      case Key.Enter     => Some(Event.Submit)
      case _             => None
    }
    // Idle renders a static frame, so layoutz's diff never repaints it (no
    // flicker). While a turn is live, a single Tick clock both animates the
    // spinner and drains the engine channel — see Event.Tick.
    if state.idle then keys
    else Sub.batch(Sub.time.everyMs(IntervalMs, Event.Tick), keys)

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
    result match
      case Left(err) =>
        commit(state, s"⚠ ${err.description}")

      case Right(StreamEvent.Delta(text)) =>
        state.phase match
          case Phase.Streaming(reply) =>
            state.copy(phase = Phase.Streaming(reply + text))
          case _ =>
            state.copy(phase = Phase.Streaming(text))

      case Right(StreamEvent.Done(response)) =>
        val reply = state.phase match
          case Phase.Streaming(buffered) => buffered
          case _                         => response.message.text
        commit(state, reply)

      // Thinking + tool events: not rendered yet.
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

      case Phase.Streaming(reply) =>
        Text(s"  ${label(Role.Auk)} $reply${Color.Green("▌").render}")

      case Phase.Idle =>
        Text("")

  /** A frameless prompt line. Steady (non-blinking) cursor so the idle view is
    * static — see [[subscriptions]] for why that matters. */
  private def prompt(state: ChatState): Element =
    val arrow = Color.Cyan("›").render
    if state.idle then Text(s"  $arrow ${state.input}${Color.Cyan("▌").render}")
    else Text(s"  $arrow ${dim("…").render}")

  /** Bold, color-coded role label, rendered to an ANSI string. */
  private def label(role: Role): String =
    role match
      case Role.You => Color.Cyan("you ▸").style(Style.Bold).render
      case Role.Auk => Color.Green("auk ▸").style(Style.Bold).render

  private def dim(s: String): Element = Text(s).style(Style.Dim)
