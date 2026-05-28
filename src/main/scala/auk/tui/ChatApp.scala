package auk.tui

import auk.tui.app.*
import auk.tui.render.{Color, Style}
import gears.async.{ReadableChannel, UnboundedChannel}
import auk.agent.UserCommand
import auk.llm.endpoint.{StreamEvent, LLMError}
import auk.llm.tools.Json
import auk.utils.Result

/** An animated chat-style TUI for auk, driven by the engine channels.
  *
  * A pure [[auk.tui.app.App]]: it consumes the engine's event stream (via a
  * gears-channel subscription) and pushes user commands back. The streaming
  * assistant turn, input box, and footer form the live region; finalized
  * transcript entries are committed once into the terminal's scrollback.
  *
  * @param events   LLM events from the engine (engine → UI), subscribed to as a
  *                 first-class event source.
  * @param commands user commands to the engine (UI → engine); concrete
  *                 `UnboundedChannel` so we can `sendImmediately` from a `Cmd`.
  */
final class ChatApp(
    events: ReadableChannel[Result[StreamEvent, LLMError]],
    commands: UnboundedChannel[UserCommand]
) extends App[ChatState, Event]:

  /** Spinner / live-clock animation cadence. */
  private val AnimationMs: Long = 100

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
        // sendImmediately is non-blocking and needs no Async context.
        (next, Cmd.fire(commands.sendImmediately(UserCommand.Submit(text))))

      case Event.HistoryPrev if state.idle => (state.recallPrev, Cmd.none)
      case Event.HistoryNext if state.idle => (state.recallNext, Cmd.none)

      case Event.Inbound1(result) =>
        // One engine event: fold it with a fresh clock so a running tool's
        // duration reflects arrival time.
        val now = System.currentTimeMillis()
        (applyEvent(state, result, now).copy(clockMs = now), Cmd.none)

      case Event.InboundClosed =>
        (state, Cmd.none)

      case Event.Tick =>
        // Animation only: advance the spinner and the live render clock.
        val now = System.currentTimeMillis()
        (state.copy(frame = state.frame + 1, clockMs = now), Cmd.none)

      case _ => (state, Cmd.none)

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
    // Engine events are consumed natively as a gears channel — active in every
    // phase so deltas keep folding. The spinner clock only runs while a turn is
    // live (idle stays a static frame, so the renderer never repaints it).
    val engine = Sub.onChannel(events)(Event.Inbound1.apply, Event.InboundClosed)
    if state.idle then Sub.batch(keys, engine)
    else Sub.batch(Sub.time.everyMs(AnimationMs, Event.Tick), keys, engine)

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

  def view(state: ChatState): Screen =
    val divider = hr('─', FrameBlue)
    // Committed: the header (printed once) and every finalized transcript entry,
    // each laid out and flushed to native scrollback exactly once.
    val committed: Vector[Element] = headerBlock +: state.history.map(renderEntry(_, divider))
    // Live: the still-changing turn, the input box, and the footer.
    val live: Element = layout(
      emptyHint(state),
      inProgress(state),
      br,
      divider,
      prompt(state),
      divider,
      footer
    )
    Screen(committed, live)

  /* ---- View helpers ---- */

  /** A dim, collapsed reasoning marker, e.g. "✻ Thought for 3.4s". */
  private def thoughtLabel(millis: Long): String =
    f"✻ Thought for ${millis / 1000.0}%.1fs"

  /** The left-bar glyph that marks reasoning and tool-call blocks. */
  private val Bar = "│"

  /** A soft, elegant light blue used to frame the input and user messages. */
  private val FrameBlue: Color = Color.True(135, 206, 235)

  private val header: Element =
    layout(
      Color.Cyan("  Auk").style(Style.Bold),
      dim("  a coding agent")
    )

  /** The header committed once at startup (with a trailing blank line). */
  private val headerBlock: Element = layout(header, br)

  private val footer: Element = dim("  ctrl+q to quit")

  /** The empty-transcript hint — lives in the live region so it vanishes once
    * the first message lands. */
  private def emptyHint(state: ChatState): Element =
    if state.history.isEmpty then layout(br, dim("  Type a message and press Enter."))
    else Empty

  private def renderEntry(e: Entry, divider: Element): Element = e match
    case Entry.User(text) => layout(divider, roleHeader(Role.You), textBlock(text), divider)
    case Entry.Assistant(blocks) =>
      // Committed: every tool has finished, so no live clock is needed.
      layout((roleHeader(Role.Auk) +: blocks.map(renderBlock(_, liveNow = None)))*)
    case Entry.Error(text) => Text(s"  ${Color.Red(text).render}")

  /** Render one assistant block. Reasoning and tool calls get a dim left bar;
    * answer text is plain, under the "Auk" header. `liveNow` is the render
    * clock, supplied while streaming so a running tool's duration ticks. */
  private def renderBlock(b: Block, liveNow: Option[Long]): Element = b match
    case Block.Thinking(_, _, Some(ms)) => barBlock(thoughtLabel(ms))
    case Block.Thinking(text, _, None)  => barBlock(s"thinking ▸ $text")
    case t: Block.Tool                  => barBlock(toolLabel(t, liveNow))
    case Block.Answer(text)             => textBlock(text)

  private def inProgress(state: ChatState): Element =
    state.phase match
      case Phase.Waiting =>
        layout(
          roleHeader(Role.Auk),
          Text("  " + spinner(label = "auk is thinking", frame = state.frame).render)
        )

      case Phase.Streaming(blocks) =>
        val rendered = blocks.zipWithIndex.map: (b, i) =>
          // The cursor rides the tail of the last block while it is still
          // streaming in (only meaningful for answer text).
          b match
            case Block.Answer(text) if i == blocks.length - 1 =>
              textBlock(text + Color.Green("▌").render)
            case other => renderBlock(other, liveNow = Some(state.clockMs))
        layout((roleHeader(Role.Auk) +: rendered)*)

      case Phase.Idle => Empty

  /** A frameless prompt line with a reverse-video block cursor at [[ChatState.cursor]],
    * so it can sit mid-line. Steady (non-blinking) so the idle view stays static. */
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

  /** The "You" / "Auk" header line that sits above an entry's content. */
  private def roleHeader(role: Role): Element =
    role match
      case Role.You => Text(s"  ${Color.Cyan("You").style(Style.Bold).render}")
      case Role.Auk => Text(s"  ${Color.Green("Auk").style(Style.Bold).render}")

  /** Plain, indented content; one rendered line per source line. */
  private def textBlock(text: String): Element =
    layout(splitLines(text).map(l => Text(s"  $l"))*)

  /** A dim, left-barred block; one barred line per source line. */
  private def barBlock(text: String): Element =
    layout(splitLines(text).map(l => dim(s"  $Bar $l"))*)

  private def splitLines(text: String): List[String] =
    if text.isEmpty then List("") else text.split("\n", -1).toList

  /** Fold a single LLM event into the chat state (`now` is this tick's clock). */
  private def applyEvent(
      state: ChatState,
      result: Result[StreamEvent, LLMError],
      now: Long
  ): ChatState =
    result match
      case Left(err)                            => state.failed(s"⚠ ${err.description}")
      case Right(StreamEvent.ThinkingDelta(t))  => state.appendThinking(t, now)
      case Right(StreamEvent.Delta(t))          => state.appendReply(t, now)
      case Right(StreamEvent.ToolCallStart(_, id, name)) => state.startTool(id, name, now)
      case Right(StreamEvent.ToolCallDelta(_, d))        => state.appendToolArgs(d)
      case Right(StreamEvent.ToolRunStart(id, _))        => state.startToolRun(id, now)
      case Right(StreamEvent.ToolRunEnd(id, _, md))      => state.endToolRun(id, md, now)
      case Right(StreamEvent.Done(response))    => state.completeReply(response.message.text, now)

  /** A human label for a tool call, e.g. "Reading foo.scala", followed by a
    * timing/token annotation while or after it runs (see [[toolStatus]]). */
  private def toolLabel(t: Block.Tool, liveNow: Option[Long]): String =
    toolBase(t.name, t.rawArgs) + toolStatus(t, liveNow)

  /** The descriptive part of a tool label, derived from its streamed JSON
    * arguments; until they parse, just the verb is shown. */
  private def toolBase(name: String, rawArgs: String): String =
    name match
      case "read"         => labeled("Reading", "path", rawArgs)
      case "edit"         => labeled("Editing", "path", rawArgs)
      case "write"        => labeled("Writing", "path", rawArgs)
      case "bash"         => labeled("Bash", "command", rawArgs)
      case "sub_agent"    => labeled("Sub-agent:", "description", rawArgs)
      case "write_memory" => labeled("Remembering", "key", rawArgs)
      // get_memory with a key recalls one note; without one it lists them all.
      case "get_memory" =>
        jsonField(rawArgs, "key") match
          case Some(key) => s"Recalling $key"
          case None      => "Recalling all memories"
      case other => labeled(other, "path", rawArgs)

  /** `verb arg` when the named field has streamed in, else just `verb`. */
  private def labeled(verb: String, field: String, rawArgs: String): String =
    jsonField(rawArgs, field) match
      case Some(value) => s"$verb $value"
      case None        => verb

  /** A dim " · 3.2s · 1.2k tokens" suffix describing a tool's execution. */
  private def toolStatus(t: Block.Tool, liveNow: Option[Long]): String =
    val running = t.startedMs.isDefined && t.elapsedMs.isEmpty
    val duration: Option[Long] =
      t.elapsedMs.orElse(for s <- t.startedMs; now <- liveNow yield now - s)
    val showDuration = duration.filter(ms => running || t.tokens.isDefined || ms >= 1000)
    val parts = showDuration.map(fmtDuration).toList ++ t.tokens.map(tk => s"${fmtTokens(tk)} tokens")
    if parts.isEmpty then "" else parts.mkString(" · ", " · ", "")

  private def fmtDuration(ms: Long): String = f"${ms / 1000.0}%.1fs"

  private def fmtTokens(n: Long): String =
    if n >= 1000 then f"${n / 1000.0}%.1fk" else n.toString

  /** Best-effort string-field lookup from streamed JSON arguments. */
  private def jsonField(rawArgs: String, field: String): Option[String] =
    Json.parse(rawArgs).toOption.collect { case o: Json.Obj => o }.flatMap { o =>
      o.get(field).collect { case Json.Str(s) => s }
    }

  private def dim(s: String): Element = Text(s).style(Style.Dim)
