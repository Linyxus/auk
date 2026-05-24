package auk.tui

/** Who authored a line in the transcript. */
enum Role:
  case You
  case Auk

/** One segment of an assistant turn, in the order it streamed in.
  *
  * A turn is a sequence of these: the model may reason, call tools, and answer,
  * possibly across several tool rounds, so we keep them ordered rather than
  * folding everything into two strings.
  */
enum Block:
  /** Reasoning. While it streams, `durationMs` is None and `text` grows; once
    * the model moves on (answers, calls a tool, or finishes) it is collapsed to
    * a fixed duration and rendered as "Thought for Xs". */
  case Thinking(text: String, startedMs: Long, durationMs: Option[Long])

  /** A tool the model invoked. `rawArgs` accumulates the streamed JSON argument
    * text; the label (e.g. "Reading foo.scala") is derived from it at render. */
  case Tool(name: String, rawArgs: String)

  /** Answer text addressed to the user. */
  case Answer(text: String)

/** A committed entry in the chat transcript. */
enum Entry:
  case User(text: String)
  case Assistant(blocks: Vector[Block])
  case Error(text: String)

/** What auk is doing right now — drives which animation the view shows. */
enum Phase:
  /** Waiting for the user to type and submit a line. */
  case Idle

  /** Command submitted, no reply event has arrived yet (spinner). */
  case Waiting

  /** A reply is streaming in, accumulated as ordered [[Block]]s. The last block
    * is the one currently growing. */
  case Streaming(blocks: Vector[Block])

/** The full immutable state of the TUI.
  *
  * @param inputHistory submitted user inputs, oldest first.
  * @param histNav      cursor into [[inputHistory]]; equal to its size when
  *                     editing a fresh line rather than recalling a past one.
  * @param draft        the in-progress line, stashed while recalling history.
  */
final case class ChatState(
    history: Vector[Entry],
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
      history = history :+ Entry.User(text),
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

  /** The blocks accumulated so far this turn (empty before the first event). */
  def streamingBlocks: Vector[Block] = phase match
    case Phase.Streaming(bs) => bs
    case _                   => Vector.empty

  /** Collapse a still-open trailing thinking block into a fixed duration. */
  private def closeThinking(bs: Vector[Block], now: Long): Vector[Block] =
    bs.lastOption match
      case Some(Block.Thinking(t, start, None)) =>
        bs.init :+ Block.Thinking(t, start, Some(now - start))
      case _ => bs

  /** Append reasoning text, starting the thinking clock on the first delta. */
  def appendThinking(text: String, now: Long): ChatState =
    val bs = streamingBlocks
    val updated = bs.lastOption match
      case Some(Block.Thinking(t, start, None)) =>
        bs.init :+ Block.Thinking(t + text, start, None)
      case _ =>
        bs :+ Block.Thinking(text, now, None)
    copy(phase = Phase.Streaming(updated))

  /** Append answer text. Reasoning in progress is collapsed first (its duration
    * fixed at `now`), since the model has moved on to answering. */
  def appendReply(text: String, now: Long): ChatState =
    val bs = closeThinking(streamingBlocks, now)
    val updated = bs.lastOption match
      case Some(Block.Answer(t)) => bs.init :+ Block.Answer(t + text)
      case _                     => bs :+ Block.Answer(text)
    copy(phase = Phase.Streaming(updated))

  /** Begin a tool call. Any reasoning in progress is collapsed first. */
  def startTool(name: String, now: Long): ChatState =
    copy(phase = Phase.Streaming(closeThinking(streamingBlocks, now) :+ Block.Tool(name, "")))

  /** Append streamed JSON argument text to the most recent tool call. */
  def appendToolArgs(delta: String): ChatState =
    val bs = streamingBlocks
    val updated = bs.lastOption match
      case Some(Block.Tool(name, raw)) => bs.init :+ Block.Tool(name, raw + delta)
      case _                           => bs
    copy(phase = Phase.Streaming(updated))

  /** Finish the turn: commit the accumulated blocks to the transcript and idle.
    * `fallback` is the model's final text, used when no answer streamed as
    * deltas (e.g. an endpoint that only delivers the full reply on Done). */
  def completeReply(fallback: String, now: Long): ChatState =
    val closed = closeThinking(streamingBlocks, now)
    val hasAnswer = closed.exists { case _: Block.Answer => true; case _ => false }
    val finalBlocks =
      if !hasAnswer && fallback.nonEmpty then closed :+ Block.Answer(fallback)
      else closed
    if finalBlocks.isEmpty then copy(phase = Phase.Idle)
    else copy(history = history :+ Entry.Assistant(finalBlocks), phase = Phase.Idle)

  /** Abort the turn with an error line in the transcript. */
  def failed(message: String): ChatState =
    copy(history = history :+ Entry.Error(message), phase = Phase.Idle)

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
