package auk.tui

/** Who authored a line in the transcript. */
enum Role:
  case You
  case Auk

/** A single, completed line in the chat transcript. */
final case class Message(role: Role, text: String)

/** What auk is doing right now — drives which animation the view shows. */
enum Phase:
  /** Waiting for the user to type and submit a line. */
  case Idle

  /** Briefly "thinking" (spinner) before the reply starts streaming. */
  case Thinking(ticksLeft: Int)

  /** Revealing the reply one character at a time. */
  case Streaming(reply: String, shown: Int)

object Phase:
  /** Animation ticks to linger on the spinner before streaming a reply. */
  val ThinkingTicks = 8

/** The full immutable state of the TUI. */
final case class ChatState(
    history: Vector[Message],
    input: String,
    phase: Phase,
    frame: Int
):
  def idle: Boolean = phase == Phase.Idle

object ChatState:
  val initial: ChatState =
    ChatState(history = Vector.empty, input = "", phase = Phase.Idle, frame = 0)

/** Messages that drive the Elm-style update loop. */
enum Event:
  case KeyChar(c: Char)
  case Backspace
  case Submit
  case Tick
