package auk.tui

import layoutz.{Key, SttyTerminal}
import gears.async.{ReadableChannel, UnboundedChannel}
import auk.agent.UserCommand
import auk.llm.endpoint.{StreamEvent, LLMError}
import auk.utils.Result

/** A terminal UI for auk, driven entirely by two channels.
  *
  * The seam between the agent and its frontend: the UI consumes a stream of LLM
  * events and produces user commands. Any frontend that honors this contract
  * can stand in for another. [[run]] blocks the calling thread until the user
  * quits.
  */
trait Tui:
  def run(
      events: ReadableChannel[Result[StreamEvent, LLMError]],
      commands: UnboundedChannel[UserCommand]
  ): Unit

/** The default layoutz-backed TUI: a streaming chat transcript.
  *
  * A thin factory — it owns terminal setup and hands the channels to a
  * [[ChatApp]] (the actual Elm-architecture app), keeping [[Tui]] purely about
  * the channel contract.
  */
object ChatTui extends Tui:
  override def run(
      events: ReadableChannel[Result[StreamEvent, LLMError]],
      commands: UnboundedChannel[UserCommand]
  ): Unit =
    // Wrap layoutz's terminal so each frame paints atomically (see BufferedTerminal).
    val terminal = SttyTerminal.create().toOption.map(BufferedTerminal(_))
    ChatApp(events, commands).run(quitKey = Key.Ctrl('Q'), terminal = terminal)
