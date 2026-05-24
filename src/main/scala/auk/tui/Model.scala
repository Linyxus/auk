package auk.tui

/** Who authored a line in the transcript. */
enum Role:
  case You
  case Auk

/** A single, completed line in the chat transcript.
  *
  * @param thoughtMillis for an assistant line, how long it spent reasoning
  *                      before answering (rendered as "Thought for Xs").
  */
final case class Message(
    role: Role,
    text: String,
    thoughtMillis: Option[Long] = None
)

/** What auk is doing right now — drives which animation the view shows. */
enum Phase:
  /** Waiting for the user to type and submit a line. */
  case Idle

  /** Command submitted, no reply event has arrived yet (spinner). */
  case Waiting

  /** A reply is streaming in: `thinking` holds reasoning, `reply` the answer.
    * `thinkingStartedMs` marks when reasoning began; `thoughtMillis` is set once
    * the answer starts (or the turn ends), collapsing reasoning into a duration. */
  case Streaming(
      thinking: String,
      reply: String,
      thinkingStartedMs: Long,
      thoughtMillis: Option[Long]
  )

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
    draft: String = "",
    cursor: Int = 0
):
  def idle: Boolean = phase == Phase.Idle

  /* ---- Line editing. `cursor` is an index in [0, input.length]. ---- */

  /** Insert a character at the cursor. */
  def insert(c: Char): ChatState =
    copy(input = input.take(cursor) + c + input.drop(cursor), cursor = cursor + 1)

  /** Delete the character before the cursor (Backspace). */
  def backspace: ChatState =
    if cursor <= 0 then this
    else copy(input = input.take(cursor - 1) + input.drop(cursor), cursor = cursor - 1)

  /** Delete the character under the cursor (Delete / Ctrl+D). */
  def deleteForward: ChatState =
    if cursor >= input.length then this
    else copy(input = input.take(cursor) + input.drop(cursor + 1))

  def cursorLeft: ChatState = copy(cursor = math.max(0, cursor - 1))
  def cursorRight: ChatState = copy(cursor = math.min(input.length, cursor + 1))
  def cursorHome: ChatState = copy(cursor = 0)
  def cursorEnd: ChatState = copy(cursor = input.length)

  /** Delete from the cursor to the end of the line (Ctrl+K). */
  def killToEnd: ChatState = copy(input = input.take(cursor))

  /** Delete from the start of the line to the cursor (Ctrl+U). */
  def killToStart: ChatState = copy(input = input.drop(cursor), cursor = 0)

  /** Delete the word before the cursor (Ctrl+W): the run of spaces just left
    * of the cursor, then the non-space word before that. */
  def deleteWordBack: ChatState =
    if cursor <= 0 then this
    else
      var i = cursor
      while i > 0 && input(i - 1) == ' ' do i -= 1
      while i > 0 && input(i - 1) != ' ' do i -= 1
      copy(input = input.take(i) + input.drop(cursor), cursor = i)

  /* ---- History ---- */

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
      input = "",
      cursor = 0
    )

  /** Recall the previous (older) input, stashing the live draft on the first
    * step back. No-op at the oldest entry or with no history. */
  def recallPrev: ChatState =
    if histNav <= 0 then this
    else
      val stash = if histNav >= inputHistory.size then input else draft
      val pos = histNav - 1
      val recalled = inputHistory(pos)
      copy(input = recalled, cursor = recalled.length, histNav = pos, draft = stash)

  /** Recall the next (newer) input; stepping past the newest restores the
    * stashed draft. No-op while already editing the draft. */
  def recallNext: ChatState =
    if histNav >= inputHistory.size then this
    else
      val pos = histNav + 1
      val recalled = if pos >= inputHistory.size then draft else inputHistory(pos)
      val nav = math.min(pos, inputHistory.size)
      copy(input = recalled, cursor = recalled.length, histNav = nav)

  /* ---- Streaming a reply (driven by the engine's events) ---- */

  /** Append reasoning text, starting the thinking clock on the first delta. */
  def appendThinking(text: String, now: Long): ChatState =
    phase match
      case Phase.Streaming(th, r, start, thought) =>
        copy(phase = Phase.Streaming(th + text, r, start, thought))
      case _ =>
        copy(phase = Phase.Streaming(text, "", now, None))

  /** Append answer text. The first answer delta collapses reasoning into a
    * fixed duration (`now - thinkingStartedMs`). */
  def appendReply(text: String, now: Long): ChatState =
    phase match
      case Phase.Streaming(th, r, start, Some(d)) =>
        copy(phase = Phase.Streaming(th, r + text, start, Some(d)))
      case Phase.Streaming(th, r, start, None) =>
        val thought = Option.when(th.nonEmpty)(now - start)
        copy(phase = Phase.Streaming(th, r + text, start, thought))
      case _ =>
        copy(phase = Phase.Streaming("", text, 0L, None))

  /** Finish the turn: commit the answer (carrying any thinking duration) to the
    * transcript and return to idle. `fallback` is used when no answer text
    * streamed (e.g. a thinking-only turn). */
  def completeReply(fallback: String, now: Long): ChatState =
    val (thinking, reply, start, thought) = phase match
      case Phase.Streaming(th, r, s, t) => (th, r, s, t)
      case _                            => ("", "", 0L, None)
    val text = if reply.nonEmpty then reply else fallback
    val finalThought = thought.orElse(Option.when(thinking.nonEmpty)(now - start))
    copy(
      history = history :+ Message(Role.Auk, text, finalThought),
      phase = Phase.Idle
    )

  /** Abort the turn with an error line in the transcript. */
  def failed(message: String): ChatState =
    copy(history = history :+ Message(Role.Auk, message), phase = Phase.Idle)

object ChatState:
  val initial: ChatState =
    ChatState(history = Vector.empty, input = "", phase = Phase.Idle, frame = 0)

/** Messages that drive the Elm-style update loop. */
enum Event:
  case KeyChar(c: Char)
  case Backspace
  case Submit

  /** Cursor movement and line editing (arrows, Home/End, Ctrl+A/E/K/U/W, Delete). */
  case CursorLeft
  case CursorRight
  case CursorHome
  case CursorEnd
  case DeleteForward
  case KillToEnd
  case KillToStart
  case DeleteWordBack

  /** Recall older / newer submitted input (Up / Down arrows). */
  case HistoryPrev
  case HistoryNext

  /** The single while-active clock: advances the spinner *and* drains the
    * engine channel. One timer only — layoutz dedupes time subscriptions by
    * interval, so a second same-interval timer would be starved. */
  case Tick
