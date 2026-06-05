package auk.tui

import auk.agent.AgentEvent
import auk.llm.endpoint.Content
import auk.session.{SessionEvent, SessionSnapshot, SessionSummary}

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
  /** Reasoning. While it streams, `durationMs` is None and the [[Typewriter]]
    * reveals smoothly as text arrives; once the model moves on (answers, calls a
    * tool, or finishes) it is collapsed to a fixed duration — its text fully
    * settled — and rendered as "Thought for Xs". */
  case Thinking(typed: Typewriter, startedMs: Long, durationMs: Option[Long])

  /** A tool the model invoked. `rawArgs` accumulates the streamed JSON argument
    * text; the label (e.g. "Reading foo.scala") is derived from it at render.
    *
    * Execution timing is tracked once the tool actually runs: `startedMs` is set
    * when it begins, and while `elapsedMs` is still None the call is ongoing (its
    * live duration is `clock - startedMs`). On completion `elapsedMs` freezes the
    * duration and `tokens` carries the total tokens spent, when the tool reports
    * them (e.g. a sub-agent). */
  case Tool(
      id: String,
      name: String,
      rawArgs: String,
      startedMs: Option[Long] = None,
      elapsedMs: Option[Long] = None,
      tokens: Option[Long] = None
  )

  /** Answer text addressed to the user. Held as a [[Typewriter]] so the live
    * region can reveal it smoothly, a little per animation tick, rather than in
    * the bursts the deltas arrive in. Committed and loaded answers are fully
    * shown (`Typewriter.shown`). */
  case Answer(typed: Typewriter)

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

  /** A reply is in the live region, accumulated as ordered [[Block]]s (the last
    * is the one currently growing).
    *
    * `closed` is false while the engine may still deliver deltas. It flips true
    * once the turn's final event has arrived: the blocks then stop changing, but
    * the answer's [[Typewriter]] keeps draining until it has caught up, at which
    * point the turn is committed to the transcript (see
    * [[ChatState.commitIfDrained]]). */
  case Streaming(blocks: Vector[Block], closed: Boolean = false)

/** One selectable model in the model picker, flattened across providers. */
final case class ModelChoice(
    providerName: String, // display, e.g. "OpenRouter"
    providerKey: String,  // value written to config, e.g. "openrouter"
    modelId: String,      // wire id, e.g. "z-ai/glm-5.1"
    modelLabel: String,   // display, e.g. "GLM 5.1"
    contextWindow: Int
)

/** The floating panel currently shown over the live region. */
enum Overlay:
  case None
  case KeyBindings
  case ResumeLoading(message: String)
  case SessionPicker(sessions: Vector[SessionSummary], selected: Int)
  case ModelPicker(choices: Vector[ModelChoice], selected: Int)

/** The full immutable state of the TUI.
  *
  * @param inputHistory submitted user inputs, oldest first.
  * @param histNav      cursor into [[inputHistory]]; equal to its size when
  *                     editing a fresh line rather than recalling a past one.
  * @param draft        the in-progress line, stashed while recalling history.
  * @param width        last-sampled console width in columns, fed by the
  *                     self-rearming width poller so `view` never queries it.
  */
final case class ChatState(
    history: Vector[Entry],
    input: String,
    phase: Phase,
    frame: Int,
    clockMs: Long = 0,
    inputHistory: Vector[String] = Vector.empty,
    histNav: Int = 0,
    draft: String = "",
    cursor: Int = 0,
    width: Int = 80,
    overlay: Overlay = Overlay.None,
    transcriptEpoch: Long = 0,
    modelName: String = ""
):
  def idle: Boolean = phase == Phase.Idle

  def showKeyBindings: ChatState = copy(overlay = Overlay.KeyBindings)
  def hideOverlay: ChatState = copy(overlay = Overlay.None)
  def showResumeLoading(message: String): ChatState =
    copy(overlay = Overlay.ResumeLoading(message))
  def showSessionPicker(sessions: Vector[SessionSummary]): ChatState =
    copy(overlay = Overlay.SessionPicker(sessions, selected = 0))
  def moveSessionSelection(delta: Int): ChatState =
    overlay match
      case Overlay.SessionPicker(sessions, selected) if sessions.nonEmpty =>
        val next = math.max(0, math.min(sessions.length - 1, selected + delta))
        copy(overlay = Overlay.SessionPicker(sessions, next))
      case _ => this
  def selectedSessionId: Option[String] =
    overlay match
      case Overlay.SessionPicker(sessions, selected) => sessions.lift(selected).map(_.id)
      case _                                         => None

  def showModelPicker(choices: Vector[ModelChoice]): ChatState =
    copy(overlay = Overlay.ModelPicker(choices, selected = 0))
  def moveModelSelection(delta: Int): ChatState =
    overlay match
      case Overlay.ModelPicker(choices, selected) if choices.nonEmpty =>
        val next = math.max(0, math.min(choices.length - 1, selected + delta))
        copy(overlay = Overlay.ModelPicker(choices, next))
      case _ => this
  def selectedModel: Option[ModelChoice] =
    overlay match
      case Overlay.ModelPicker(choices, selected) => choices.lift(selected)
      case _                                      => None

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
    case Phase.Streaming(bs, _) => bs
    case _                      => Vector.empty

  /** Collapse a still-open trailing thinking block into a fixed duration. Its
    * reveal is settled at once, since the collapsed form shows only the duration,
    * not the text. */
  private def closeThinking(bs: Vector[Block], now: Long): Vector[Block] =
    bs.lastOption match
      case Some(Block.Thinking(typed, start, None)) =>
        bs.init :+ Block.Thinking(typed.settle, start, Some(now - start))
      case _ => bs

  /** Append reasoning text, starting the thinking clock on the first delta. */
  def appendThinking(text: String, now: Long): ChatState =
    val bs = streamingBlocks
    val updated = bs.lastOption match
      case Some(Block.Thinking(typed, start, None)) =>
        bs.init :+ Block.Thinking(typed.append(text), start, None)
      case _ =>
        bs :+ Block.Thinking(Typewriter.empty.append(text), now, None)
    copy(phase = Phase.Streaming(updated))

  /** Append answer text. Reasoning in progress is collapsed first (its duration
    * fixed at `now`), since the model has moved on to answering. */
  def appendReply(text: String, now: Long): ChatState =
    val bs = closeThinking(streamingBlocks, now)
    val updated = bs.lastOption match
      case Some(Block.Answer(typed)) => bs.init :+ Block.Answer(typed.append(text))
      case _                         => bs :+ Block.Answer(Typewriter.empty.append(text))
    copy(phase = Phase.Streaming(updated))

  /** Begin a tool call (the model has started emitting it). Any reasoning in
    * progress is collapsed first. */
  def startTool(id: String, name: String, now: Long): ChatState =
    copy(phase = Phase.Streaming(closeThinking(streamingBlocks, now) :+ Block.Tool(id, name, "")))

  /** Append streamed JSON argument text to the most recent tool call. */
  def appendToolArgs(delta: String): ChatState =
    val bs = streamingBlocks
    val updated = bs.lastOption match
      case Some(t: Block.Tool) => bs.init :+ t.copy(rawArgs = t.rawArgs + delta)
      case _                   => bs
    copy(phase = Phase.Streaming(updated))

  /** Mark the tool call `id` as running (its execution has begun). */
  def startToolRun(id: String, now: Long): ChatState =
    mapTool(id)(_.copy(startedMs = Some(now)))

  /** Mark the tool call `id` as finished: freeze its duration and record the
    * total tokens it spent, if it reported any. */
  def endToolRun(id: String, metadata: Map[String, String], now: Long): ChatState =
    mapTool(id) { t =>
      t.copy(
        elapsedMs = t.startedMs.map(now - _).orElse(t.elapsedMs),
        tokens = ChatState.totalTokens(metadata).orElse(t.tokens)
      )
    }

  /** Apply `f` to the streaming tool block with the given id, if present. */
  private def mapTool(id: String)(f: Block.Tool => Block.Tool): ChatState =
    phase match
      case Phase.Streaming(bs, closed) =>
        copy(phase = Phase.Streaming(
          bs.map {
            case t: Block.Tool if t.id == id => f(t)
            case other                       => other
          },
          closed
        ))
      case _ => this

  /** The engine signalled the turn is over. Finalize the blocks — collapse any
    * open reasoning, and append the model's final text as an answer if none
    * streamed as deltas — and mark the live region `closed`. The blocks stop
    * changing here, but the answer's reveal may still be catching up; the turn
    * is committed to the transcript later, once [[revealSettled]], by
    * [[commitIfDrained]]. `fallback` is the model's final text, used when no
    * answer streamed as deltas (an endpoint that only delivers the full reply on
    * Done); it animates in just like streamed text would. */
  def finishReply(fallback: String, now: Long): ChatState =
    val blocks0 = closeThinking(streamingBlocks, now)
    val hasAnswer = blocks0.exists { case _: Block.Answer => true; case _ => false }
    val finalBlocks =
      if !hasAnswer && fallback.nonEmpty then blocks0 :+ Block.Answer(Typewriter.empty.append(fallback))
      else blocks0
    if finalBlocks.isEmpty then copy(phase = Phase.Idle)
    else copy(phase = Phase.Streaming(finalBlocks, closed = true))

  /** Reveal a little more of every animating answer — one [[Typewriter]] step.
    * Driven by the UI's animation tick; a no-op outside a streaming turn. */
  def advanceReveal: ChatState = phase match
    case Phase.Streaming(blocks, closed) =>
      copy(phase = Phase.Streaming(blocks.map(revealMore), closed))
    case _ => this

  private def revealMore(b: Block): Block = b match
    case Block.Answer(typed)            => Block.Answer(typed.advance)
    case Block.Thinking(typed, s, None) => Block.Thinking(typed.advance, s, None)
    case other                          => other

  /** True when a `closed` turn's reveal has fully caught up and it is ready to
    * be committed to the transcript. (Collapsed reasoning is already settled, so
    * in practice this waits only on the answer.) */
  def revealSettled: Boolean = phase match
    case Phase.Streaming(blocks, closed) => closed && blocks.forall(blockSettled)
    case _                               => false

  private def blockSettled(b: Block): Boolean = b match
    case Block.Answer(t)         => t.settled
    case Block.Thinking(t, _, _) => t.settled
    case _                       => true

  /** Commit a fully-revealed closed turn to the transcript and idle; otherwise
    * leave the state untouched (the reveal is still draining, or no turn is
    * closing). Called after every fold and every tick, so a turn with nothing
    * left to reveal commits at once and a lagging one commits as it catches up. */
  def commitIfDrained: ChatState =
    if revealSettled then commitReply else this

  private def commitReply: ChatState = phase match
    case Phase.Streaming(blocks, _) if blocks.nonEmpty =>
      copy(history = history :+ Entry.Assistant(blocks), phase = Phase.Idle)
    case _ => copy(phase = Phase.Idle)

  /** Abort the turn with an error line in the transcript. */
  def failed(message: String): ChatState =
    copy(history = history :+ Entry.Error(message), phase = Phase.Idle, overlay = Overlay.None)

  /** Replace the visible transcript and input state with a loaded session. */
  def switchedTo(snapshot: SessionSnapshot): ChatState =
    val inputs = ChatState.inputHistoryFrom(snapshot.events)
    copy(
      history = ChatState.historyFrom(snapshot.events),
      input = "",
      phase = Phase.Idle,
      inputHistory = inputs,
      histNav = inputs.size,
      draft = "",
      cursor = 0,
      overlay = Overlay.None,
      transcriptEpoch = transcriptEpoch + 1
    )

object ChatState:
  val initial: ChatState =
    ChatState(history = Vector.empty, input = "", phase = Phase.Idle, frame = 0)

  /** Sum the input/output token counts from a tool's metadata, if either is
    * present (sub-agents report them; most tools do not). */
  def totalTokens(metadata: Map[String, String]): Option[Long] =
    val in = metadata.get("inputTokens").flatMap(_.toLongOption)
    val out = metadata.get("outputTokens").flatMap(_.toLongOption)
    Option.when(in.isDefined || out.isDefined)(in.getOrElse(0L) + out.getOrElse(0L))

  def inputHistoryFrom(events: List[SessionEvent]): Vector[String] =
    events.collect { case SessionEvent.UserSubmitted(text) => text }.toVector

  def historyFrom(events: List[SessionEvent]): Vector[Entry] =
    events.flatMap:
      case SessionEvent.UserSubmitted(text) => Some(Entry.User(text))
      case SessionEvent.AssistantResponded(message) =>
        val blocks = message.content.flatMap:
          case Content.Text(text) if text.nonEmpty =>
            Some(Block.Answer(Typewriter.shown(text)))
          case Content.Thinking(text, _) if text.nonEmpty =>
            Some(Block.Thinking(Typewriter.shown(text), startedMs = 0L, durationMs = Some(0L)))
          case Content.ToolUse(id, name, input) =>
            Some(Block.Tool(id, name, input, elapsedMs = Some(0L)))
          case _ =>
            None
        Option.when(blocks.nonEmpty)(Entry.Assistant(blocks.toVector))
      case SessionEvent.ToolResultsReceived(_) => None
    .toVector

/** Messages that drive the Elm-style update loop. */
enum Event:
  case KeyChar(c: Char)
  case ShowKeyBindings
  case HideOverlay
  case RunCommand(key: String)
  case SessionPickerUp
  case SessionPickerDown
  case ResumeSelected
  case ModelPickerUp
  case ModelPickerDown
  case ModelSelected
  case Backspace
  case Newline
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

  /** One engine event, delivered by the runtime's gears-channel subscription
    * (`Sub.onChannel`). Render coalescing comes from the runtime's frame cap, so
    * events no longer need to be drained into a batch. */
  case Inbound1(event: AgentEvent)

  /** The engine channel closed; the subscription stops delivering. */
  case InboundClosed

  /** The while-active animation clock: advances the spinner frame and the live
    * render clock (`clockMs`) so a running tool's duration ticks up. Engine
    * events arrive separately via [[Inbound1]]. */
  case Tick
