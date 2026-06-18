package auk.tui

import auk.agent.AgentEvent
import auk.workflow.{Forest, OrchestrationEvent}
import auk.llm.endpoint.{Content, Usage}
import auk.tui.markdown.MarkdownDocument
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
    * duration, `tokens` carries the total tokens spent when the tool reports
    * them (e.g. a sub-agent), and `output`/`isError` record the result text for
    * tools whose result the view renders (e.g. eval_scala's REPL reply). */
  case Tool(
      id: String,
      name: String,
      rawArgs: String,
      startedMs: Option[Long] = None,
      elapsedMs: Option[Long] = None,
      tokens: Option[Long] = None,
      output: Option[String] = None,
      isError: Boolean = false,
      forest: Option[Forest] = None
  )

  /** Answer text addressed to the user. Held as a [[Typewriter]] so the live
    * region can reveal it smoothly, a little per animation tick, rather than in
    * the bursts the deltas arrive in. Committed and loaded answers are fully
    * shown (`Typewriter.shown`).
    *
    * `doc` is the answer's Markdown, parsed incrementally to track the revealed
    * text: it is fed `typed.visible` as the reveal advances, so finalised blocks
    * are parsed once and only the open tail re-parses (see [[MarkdownDocument]]). */
  case Answer(typed: Typewriter, doc: MarkdownDocument)

object Block:
  /** A streaming answer whose Markdown is parsed up to its currently-revealed
    * text. Feed the existing document forward with [[advanceAnswer]] rather than
    * rebuilding from here, so parsing stays incremental. */
  def answer(typed: Typewriter): Answer =
    Answer(typed, MarkdownDocument.empty.feedTo(typed.visible))

  /** A fully-shown answer (committed reply / resumed history): the whole text is
    * parsed at once. */
  def shownAnswer(text: String): Answer =
    Answer(Typewriter.shown(text), MarkdownDocument.parse(text))

  /** Append newly-arrived text. The reveal (and thus the parsed document) is
    * unchanged — only the backlog grows; the document catches up as the reveal
    * advances. */
  def appendAnswer(a: Answer, text: String): Answer =
    a.copy(typed = a.typed.append(text))

  /** Reveal a little more and feed the document up to the new visible text. */
  def advanceAnswer(a: Answer): Answer =
    val t = a.typed.advance
    Answer(t, a.doc.feedTo(t.visible))

  /** Reveal everything at once and finalise the document. */
  def settleAnswer(a: Answer): Answer =
    val t = a.typed.settle
    Answer(t, a.doc.feedTo(t.visible).close())

  /** Finalise an answer's Markdown when its turn commits, so the committed
    * transcript renders without re-parsing the open tail each frame. Non-answer
    * blocks pass through. */
  def closeAnswer(b: Block): Block = b match
    case a: Answer => a.copy(doc = a.doc.close())
    case other     => other

/** A committed entry in the chat transcript. */
enum Entry:
  case User(text: String)
  case Assistant(blocks: Vector[Block])
  case Error(text: String)

  /** A dim marker showing a turn was cut short by the user (`Ctrl+C k`). */
  case Interrupted

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

object ModelChoice:
  def matchesQuery(choice: ModelChoice, query: String): Boolean =
    val terms = searchTerms(query)
    if terms.isEmpty then true
    else
      val fields = Vector(
        choice.modelLabel,
        choice.modelId,
        choice.providerName,
        choice.providerKey,
        choice.contextWindow.toString
      )
      val searchable = fields.mkString(" ")
      val normalized = normalizeSearchText(searchable)
      val compacted = compactSearchText(searchable)
      val compactQuery = compactSearchText(query)

      compactQuery.nonEmpty && compacted.contains(compactQuery) ||
        terms.forall(term => normalized.contains(term) || compacted.contains(term))

  private def searchTerms(query: String): Vector[String] =
    normalizeSearchText(query).split(" ").toVector.filter(_.nonEmpty)

  private def normalizeSearchText(text: String): String =
    text.toLowerCase.map(ch => if ch.isLetterOrDigit then ch else ' ').mkString

  private def compactSearchText(text: String): String =
    text.toLowerCase.filter(_.isLetterOrDigit)

/** The floating panel currently shown over the live region. */
enum Overlay:
  case None
  case KeyBindings
  case DebugInfo
  case ResumeLoading(message: String)
  case SessionPicker(sessions: Vector[SessionSummary], selected: Int)
  case ModelPicker(choices: Vector[ModelChoice], query: String, selected: Int)

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
    turnStartMs: Long = 0,
    anchoredOutputTokens: Long = 0,
    anchorChars: Long = 0,
    inputHistory: Vector[String] = Vector.empty,
    histNav: Int = 0,
    draft: String = "",
    cursor: Int = 0,
    width: Int = 80,
    overlay: Overlay = Overlay.None,
    transcriptEpoch: Long = 0,
    modelName: String = "",
    contextWindow: Int = 0,
    contextTokens: Long = 0,
    provider: String = "",
    modelId: String = "",
    baseUrl: String = "",
    busyHint: Boolean = false,
    notices: Vector[String] = Vector.empty
):
  def idle: Boolean = phase == Phase.Idle

  /** Record the token usage of the most recently completed LLM round. The input
    * plus output tokens of the latest round approximate how full the context
    * window now is: the next request resends this whole history, so the prompt
    * the model just saw is the best estimate of current context occupancy. */
  def withContextUsage(usage: Option[Usage]): ChatState =
    usage match
      case Some(u) => copy(contextTokens = u.inputTokens + u.outputTokens)
      case None    => this

  /** Percentage of the active model's context window consumed so far, once the
    * window size is known (capped at 100). Reads 0% before any tokens are spent,
    * and is absent only while the active model's window is still unknown. */
  def contextPercentUsed: Option[Int] =
    Option.when(contextWindow > 0):
      math.min(100, math.round(contextTokens * 100.0 / contextWindow).toInt)

  def showKeyBindings: ChatState = copy(overlay = Overlay.KeyBindings)
  def showDebugInfo: ChatState = copy(overlay = Overlay.DebugInfo)
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
    val initial = choices.indexWhere(_.modelLabel == modelName).max(0)
    copy(overlay = Overlay.ModelPicker(choices, query = "", selected = initial))
  def moveModelSelection(delta: Int): ChatState =
    overlay match
      case Overlay.ModelPicker(choices, query, selected) =>
        val filtered = ChatState.filteredModelChoices(choices, query)
        if filtered.isEmpty then copy(overlay = Overlay.ModelPicker(choices, query, selected = 0))
        else
          val next = math.max(0, math.min(filtered.length - 1, selected + delta))
          copy(overlay = Overlay.ModelPicker(choices, query, next))
      case _ => this
  def appendModelSearch(c: Char): ChatState =
    overlay match
      case Overlay.ModelPicker(choices, query, _) =>
        updateModelSearch(choices, query + c)
      case _ => this
  def backspaceModelSearch: ChatState =
    overlay match
      case Overlay.ModelPicker(choices, query, _) if query.nonEmpty =>
        updateModelSearch(choices, query.dropRight(1))
      case _ => this
  def clearModelSearch: ChatState =
    overlay match
      case Overlay.ModelPicker(choices, _, _) =>
        updateModelSearch(choices, "")
      case _ => this
  def selectedModel: Option[ModelChoice] =
    overlay match
      case Overlay.ModelPicker(choices, query, selected) =>
        ChatState.filteredModelChoices(choices, query).lift(selected)
      case _ => None

  private def updateModelSearch(choices: Vector[ModelChoice], query: String): ChatState =
    copy(overlay = Overlay.ModelPicker(choices, query, selected = 0))

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

  /** Move to the start of the current logical line (Ctrl+A / Home) — just after
    * the preceding newline, or column 0 on the first line. */
  def cursorHome: ChatState = copy(cursor = input.lastIndexOf('\n', cursor - 1) + 1)

  /** Move to the end of the current logical line (Ctrl+E / End) — just before
    * the next newline, or the end of the input on the last line. */
  def cursorEnd: ChatState =
    val nextNl = input.indexOf('\n', cursor)
    copy(cursor = if nextNl < 0 then input.length else nextNl)

  /** True when the cursor sits on the first logical line of [[input]] — there is
    * no newline before it. Used to decide whether Up edits or recalls history. */
  def onFirstLine: Boolean = input.lastIndexOf('\n', cursor - 1) < 0

  /** True when the cursor sits on the last logical line — no newline at or after
    * it. Used to decide whether Down edits or recalls history. */
  def onLastLine: Boolean = input.indexOf('\n', cursor) < 0

  /** Move the cursor up one logical line, keeping its column where possible
    * (clamped to the shorter line). A no-op on the first line. */
  def cursorUp: ChatState =
    val lineStart = input.lastIndexOf('\n', cursor - 1) + 1
    if lineStart == 0 then this // already on the first line
    else
      val col = cursor - lineStart
      val prevStart = input.lastIndexOf('\n', lineStart - 2) + 1
      val prevLen = (lineStart - 1) - prevStart
      copy(cursor = prevStart + math.min(col, prevLen))

  /** Move the cursor down one logical line, keeping its column where possible
    * (clamped to the shorter line). A no-op on the last line. */
  def cursorDown: ChatState =
    val nextNl = input.indexOf('\n', cursor)
    if nextNl < 0 then this // already on the last line
    else
      val lineStart = input.lastIndexOf('\n', cursor - 1) + 1
      val col = cursor - lineStart
      val nextStart = nextNl + 1
      val nextNlAfter = input.indexOf('\n', nextStart)
      val nextEnd = if nextNlAfter < 0 then input.length else nextNlAfter
      copy(cursor = nextStart + math.min(col, nextEnd - nextStart))

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

  /** Total characters of model output streamed so far this turn — the full
    * thinking and answer text plus the tool-call argument JSON, across every
    * block. Counts received text, not yet-revealed text, so it tracks what the
    * model has actually produced. Used to estimate live token throughput on the
    * working indicator (no exact usage is available mid-turn). */
  def streamedOutputChars: Long =
    streamingBlocks.foldLeft(0L): (acc, b) =>
      b match
        case Block.Thinking(typed, _, _) => acc + typed.full.length
        case Block.Answer(typed, _)      => acc + typed.full.length
        case t: Block.Tool               => acc + t.rawArgs.length

  /** Begin a fresh assistant turn: stamp the start clock and clear the live
    * token accounting (the exact-usage anchor and the estimate baseline) so the
    * working indicator measures this turn alone. */
  def startingTurn(now: Long): ChatState =
    copy(turnStartMs = now, clockMs = now, anchoredOutputTokens = 0, anchorChars = 0)

  /** Anchor the live token tally to a completed round's exact output tokens.
    * The real figure supersedes that round's character estimate: its output
    * tokens join the running exact total, and the estimate baseline advances to
    * the current streamed length, so only the *next* round's characters are
    * estimated. Between rounds (while tools run) the baseline already equals the
    * streamed length, so the tally rests on the exact anchor. */
  def anchorRoundUsage(usage: Usage): ChatState =
    copy(anchoredOutputTokens = anchoredOutputTokens + usage.outputTokens, anchorChars = streamedOutputChars)

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
      case Some(a: Block.Answer) => bs.init :+ Block.appendAnswer(a, text)
      case _                     => bs :+ Block.answer(Typewriter.empty.append(text))
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

  /** Fold a running tool's live progress into its block: update the token total
    * when the update carries one, leaving the duration to keep ticking from
    * `startedMs`. The tool stays running (no `elapsedMs`); this only refreshes
    * what's shown mid-flight, e.g. a streaming sub-agent's climbing token count. */
  def progressToolRun(id: String, metadata: Map[String, String]): ChatState =
    mapTool(id)(t => t.copy(tokens = ChatState.totalTokens(metadata).orElse(t.tokens)))

  /** Mark the tool call `id` as finished: freeze its duration and record its
    * result — the output text, error flag, and total tokens if reported. */
  def endToolRun(
      id: String,
      isError: Boolean,
      metadata: Map[String, String],
      output: String,
      now: Long
  ): ChatState =
    mapTool(id) { t =>
      t.copy(
        elapsedMs = t.startedMs.map(now - _).orElse(t.elapsedMs),
        tokens = ChatState.totalTokens(metadata).orElse(t.tokens),
        output = Some(output).filter(_.nonEmpty),
        isError = isError
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

  /** Fold a workflow orchestration event into the forest of the in-progress
    * `eval_scala` tool block whose id is the event's run id. */
  def applyOrchestration(ev: OrchestrationEvent): ChatState =
    mapTool(ev.runId)(t => t.copy(forest = Some(t.forest.getOrElse(Forest.empty).update(ev))))

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
      if !hasAnswer && fallback.nonEmpty then blocks0 :+ Block.answer(Typewriter.empty.append(fallback))
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
    case a: Block.Answer                => Block.advanceAnswer(a)
    case Block.Thinking(typed, s, None) => Block.Thinking(typed.advance, s, None)
    case other                          => other

  /** True when a `closed` turn's reveal has fully caught up and it is ready to
    * be committed to the transcript. (Collapsed reasoning is already settled, so
    * in practice this waits only on the answer.) */
  def revealSettled: Boolean = phase match
    case Phase.Streaming(blocks, closed) => closed && blocks.forall(blockSettled)
    case _                               => false

  private def blockSettled(b: Block): Boolean = b match
    case Block.Answer(t, _)      => t.settled
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
      copy(history = history :+ Entry.Assistant(blocks.map(Block.closeAnswer)), phase = Phase.Idle)
    case _ => copy(phase = Phase.Idle)

  /** Abort the turn with an error line in the transcript. */
  def failed(message: String): ChatState =
    copy(history = history :+ Entry.Error(message), phase = Phase.Idle, overlay = Overlay.None)

  /** Record a sticky system notice (e.g. the workflow dashboard URL). It is shown
    * pinned just above the input box rather than appended to the scrolling
    * transcript, so it stays readable. Deduplicated and capped; leaves
    * `phase`/`transcriptEpoch` untouched, and never persisted to a session. */
  def notice(message: String): ChatState =
    if notices.contains(message) then this
    else copy(notices = (notices :+ message).takeRight(4))

  /** The turn was interrupted: commit whatever streamed so far (settled at once,
    * since there is no more to reveal), append a dim interruption marker, and
    * return to idle. */
  def interrupted: ChatState =
    val committed = phase match
      case Phase.Streaming(blocks, _) if blocks.nonEmpty =>
        copy(history = history :+ Entry.Assistant(blocks.map(settleBlock)))
      case _ => this
    committed.copy(history = committed.history :+ Entry.Interrupted, phase = Phase.Idle, overlay = Overlay.None)

  /** Reveal a block's text in full at once (used when committing immediately
    * rather than letting the typewriter drain). */
  private def settleBlock(b: Block): Block = b match
    case a: Block.Answer                   => Block.settleAnswer(a)
    case Block.Thinking(typed, s, None)    => Block.Thinking(typed.settle, s, Some(0L))
    case other                             => other

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
      transcriptEpoch = transcriptEpoch + 1,
      // Restore the gauge from the last reply's persisted usage; if the session
      // predates usage logging the figure stays 0 until the next turn's Done.
      contextTokens = ChatState.contextTokensFrom(snapshot.events).getOrElse(0L)
    )

object ChatState:
  val initial: ChatState =
    ChatState(history = Vector.empty, input = "", phase = Phase.Idle, frame = 0)

  def filteredModelChoices(choices: Vector[ModelChoice], query: String): Vector[ModelChoice] =
    choices.filter(ModelChoice.matchesQuery(_, query))

  /** Sum the input/output token counts from a tool's metadata, if either is
    * present (sub-agents report them; most tools do not). */
  def totalTokens(metadata: Map[String, String]): Option[Long] =
    val in = metadata.get("inputTokens").flatMap(_.toLongOption)
    val out = metadata.get("outputTokens").flatMap(_.toLongOption)
    Option.when(in.isDefined || out.isDefined)(in.getOrElse(0L) + out.getOrElse(0L))

  /** The context occupancy to show on resume: the input + output tokens of the
    * last assistant reply that carried a usage figure. That round's input is the
    * whole history resent up to that point, so it is the best estimate of how
    * full the window is — and the next turn's `Done` refreshes it exactly.
    * `None` for sessions logged before usage was persisted. */
  def contextTokensFrom(events: List[SessionEvent]): Option[Long] =
    events.reverseIterator
      .collectFirst { case SessionEvent.AssistantResponded(r) if r.usage.isDefined => r.usage.get }
      .map(u => u.inputTokens + u.outputTokens)

  def inputHistoryFrom(events: List[SessionEvent]): Vector[String] =
    events.collect { case SessionEvent.UserSubmitted(text) => text }.toVector

  def historyFrom(events: List[SessionEvent]): Vector[Entry] =
    // Tool results arrive in a later event than the calls they answer, so
    // collect them up front and attach each to its call's block by id.
    val results: Map[String, Content.ToolResult] =
      events
        .collect { case SessionEvent.ToolResultsReceived(rs) => rs }
        .flatten
        .map(r => r.toolUseId -> r)
        .toMap
    events.flatMap:
      case SessionEvent.UserSubmitted(text) => Some(Entry.User(text))
      case SessionEvent.AssistantResponded(response) =>
        val blocks = response.message.content.flatMap:
          case Content.Text(text) if text.nonEmpty =>
            Some(Block.shownAnswer(text))
          case Content.Thinking(text, _) if text.nonEmpty =>
            Some(Block.Thinking(Typewriter.shown(text), startedMs = 0L, durationMs = Some(0L)))
          case Content.Reasoning(blocks) =>
            val text = blocks.flatMap(_.displayText).mkString
            Option.when(text.nonEmpty)(Block.Thinking(Typewriter.shown(text), startedMs = 0L, durationMs = Some(0L)))
          case Content.ToolUse(id, name, input) =>
            val result = results.get(id)
            Some(Block.Tool(
              id,
              name,
              input,
              elapsedMs = Some(0L),
              output = result.map(_.content).filter(_.nonEmpty),
              isError = result.exists(_.isError)
            ))
          case _ =>
            None
        Option.when(blocks.nonEmpty)(Entry.Assistant(blocks.toVector))
      case SessionEvent.ToolResultsReceived(_) => None
      case SessionEvent.Interrupted            => Some(Entry.Interrupted)
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
  case ModelPickerSearchChar(c: Char)
  case ModelPickerSearchBackspace
  case ModelPickerSearchClear
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
