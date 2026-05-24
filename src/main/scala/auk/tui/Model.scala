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

  /** Command submitted, no reply event has arrived yet (spinner). */
  case Waiting

  /** Reply is streaming in. `reply` is the live buffer; it grows as deltas
    * arrive, and that growth is the animation. */
  case Streaming(reply: String)

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

  /** The single while-active clock: advances the spinner *and* drains the
    * engine channel. One timer only — layoutz dedupes time subscriptions by
    * interval, so a second same-interval timer would be starved. */
  case Tick
