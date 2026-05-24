package auk.tui

import layoutz.*

/** A small, animated chat-style TUI for auk.
  *
  * For now it is a pure echo loop: whatever you submit, auk thinks for a beat
  * (spinner) and then streams the same text back, character by character. The
  * [[respond]] function is the single seam where a real agent would plug in.
  */
object ChatApp extends LayoutzApp[ChatState, Event]:

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
          phase = Phase.Thinking(Phase.ThinkingTicks)
        )
        (next, Cmd.none)

      case Event.Tick =>
        (tick(state), Cmd.none)

      case _ =>
        (state, Cmd.none)

  def subscriptions(state: ChatState): Sub[Event] =
    Sub.batch(
      Sub.time.everyMs(45, Event.Tick),
      Sub.onKeyPress {
        case Key.Char(c)   => Some(Event.KeyChar(c))
        case Key.Backspace => Some(Event.Backspace)
        case Key.Enter     => Some(Event.Submit)
        case _             => None
      }
    )

  def view(state: ChatState): Element =
    layout(
      header,
      br,
      transcript(state),
      inProgress(state),
      br,
      inputBox(state),
      footer
    )

  /* ---- State transitions ---- */

  /** Advance one animation frame and the active phase. */
  private def tick(state: ChatState): ChatState =
    val s = state.copy(frame = state.frame + 1)
    s.phase match
      case Phase.Thinking(ticksLeft) if ticksLeft <= 1 =>
        val reply = respond(s.history.lastOption.map(_.text).getOrElse(""))
        s.copy(phase = Phase.Streaming(reply, shown = 0))

      case Phase.Thinking(ticksLeft) =>
        s.copy(phase = Phase.Thinking(ticksLeft - 1))

      case Phase.Streaming(reply, shown) if shown >= reply.length =>
        s.copy(history = s.history :+ Message(Role.Auk, reply), phase = Phase.Idle)

      case Phase.Streaming(reply, shown) =>
        s.copy(phase = Phase.Streaming(reply, shown + 1))

      case Phase.Idle =>
        s

  /** The agent's reply. Echo loop for now; swap for a real call later. */
  private def respond(userText: String): String = userText

  /* ---- View helpers ---- */

  private val header: Element =
    layout(
      Color.Cyan("  auk").style(Style.Bold),
      dim("  a coding agent · echo loop")
    )

  private val footer: Element = dim("  ctrl+q to quit")

  private def transcript(state: ChatState): Element =
    if state.history.isEmpty then
      dim("  Type a message and press Enter — auk will echo it back.")
    else layout(state.history.map(renderMessage)*)

  private def renderMessage(m: Message): Element =
    Text(s"  ${label(m.role)} ${m.text}")

  private def inProgress(state: ChatState): Element =
    state.phase match
      case Phase.Thinking(_) =>
        Text("  " + spinner(label = "auk is thinking", frame = state.frame).render)

      case Phase.Streaming(reply, shown) =>
        Text(s"  ${label(Role.Auk)} ${reply.take(shown)}${Color.Green("▌").render}")

      case Phase.Idle =>
        Text("")

  private def inputBox(state: ChatState): Element =
    if state.idle then
      val cursor = if blink(state.frame) then "▌" else " "
      box()(Text(s"${Color.Cyan("›").render} ${state.input}$cursor"))
    else box()(dim("…"))

  /** Bold, color-coded role label, rendered to an ANSI string. */
  private def label(role: Role): String =
    role match
      case Role.You => Color.Cyan("you ▸").style(Style.Bold).render
      case Role.Auk => Color.Green("auk ▸").style(Style.Bold).render

  private def dim(s: String): Element = Text(s).style(Style.Dim)

  /** Slow blink for the input cursor (~toggles a couple times per second). */
  private def blink(frame: Int): Boolean = (frame / 12) % 2 == 0

@main def auk(): Unit =
  ChatApp.run(quitKey = Key.Ctrl('Q'))
