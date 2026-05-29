package auk.tui

import gears.async.{ReadableChannel, UnboundedChannel}
import auk.tui.app.{Key, Runtime, RuntimeConfig}
import auk.tui.render.{HeadlessTerminal, SttyTerminal, Terminal}
import auk.agent.{AgentEvent, UserCommand}

/** A terminal UI for auk, driven entirely by two channels.
  *
  * The seam between the agent and its frontend: the UI consumes a stream of
  * agent events and produces user commands. Any frontend that honors this contract
  * can stand in for another. [[run]] blocks the calling thread until the user
  * quits.
  */
trait Tui:
  def run(
      events: ReadableChannel[AgentEvent],
      commands: UnboundedChannel[UserCommand]
  ): Unit

/** The default TUI: a streaming chat transcript on auk's own rendering library.
  *
  * A thin factory — it sets up the terminal and hands the channels to a
  * [[ChatApp]] running on the gears-based [[Runtime]], keeping [[Tui]] purely
  * about the channel contract.
  */
object ChatTui extends Tui:
  override def run(
      events: ReadableChannel[AgentEvent],
      commands: UnboundedChannel[UserCommand]
  ): Unit =
    // Real terminal when we have a TTY; a headless stub otherwise (piped/CI).
    val terminal: Terminal = SttyTerminal.create() match
      case Right(t) => t
      case Left(_)  => HeadlessTerminal
    // Render at ~60fps; Ctrl+Q still provides a direct quit shortcut. run()
    // blocks until the user quits.
    Runtime.run(ChatApp(events, commands), terminal, RuntimeConfig(frameMs = 16, quitKey = Key.Ctrl('Q')))
