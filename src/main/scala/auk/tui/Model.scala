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

/** The full immutable state of the TUI.
  *
  * @param inputHistory submitted user inputs, oldest first.
  * @param histNav      cursor into [[inputHistory]]; equal to its size when
  *                     editing a fresh line rather than recalling a past one.
  * @param draft        the in-progress line, stashed while recalling history.
  */
final case class ChatState(
    history: Vector[Message],
    input: String,
    phase: Phase,
    frame: Int,
    inputHistory: Vector[String] = Vector.empty,
    histNav: Int = 0,
    draft: String = ""
):
  def idle: Boolean = phase == Phase.Idle

  /** Record a submitted line: append it to the transcript and the input
    * history (collapsing an immediate repeat), clear the input, and reset
    * history navigation. Leaves [[phase]] for the caller to set. */
  def submitted(text: String): ChatState =
    val nextInputs =
      if inputHistory.lastOption.contains(text) then inputHistory
      else inputHistory :+ text
    copy(
      history = history :+ Message(Role.You, text),
      inputHistory = nextInputs,
      histNav = nextInputs.size,
      draft = "",
      input = ""
    )

  /** Recall the previous (older) input, stashing the live draft on the first
    * step back. No-op at the oldest entry or with no history. */
  def recallPrev: ChatState =
    if histNav <= 0 then this
    else
      val stash = if histNav >= inputHistory.size then input else draft
      val pos = histNav - 1
      copy(input = inputHistory(pos), histNav = pos, draft = stash)

  /** Recall the next (newer) input; stepping past the newest restores the
    * stashed draft. No-op while already editing the draft. */
  def recallNext: ChatState =
    if histNav >= inputHistory.size then this
    else
      val pos = histNav + 1
      if pos >= inputHistory.size then
        copy(input = draft, histNav = inputHistory.size)
      else copy(input = inputHistory(pos), histNav = pos)

object ChatState:
  val initial: ChatState =
    ChatState(history = Vector.empty, input = "", phase = Phase.Idle, frame = 0)

/** Messages that drive the Elm-style update loop. */
enum Event:
  case KeyChar(c: Char)
  case Backspace
  case Submit

  /** Recall older / newer submitted input (Up / Down arrows). */
  case HistoryPrev
  case HistoryNext

  /** The single while-active clock: advances the spinner *and* drains the
    * engine channel. One timer only — layoutz dedupes time subscriptions by
    * interval, so a second same-interval timer would be starved. */
  case Tick
