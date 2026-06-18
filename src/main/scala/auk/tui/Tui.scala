package auk.tui

import gears.async.{Async, ReadableChannel, UnboundedChannel}
import auk.tui.app.{Key, Runtime, RuntimeConfig}
import auk.tui.render.{HeadlessTerminal, Terminal}
import auk.platform.js.NodeTerminal
import auk.agent.{AgentEvent, UserCommand, Inbox}

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
      commands: UnboundedChannel[UserCommand],
      interrupts: UnboundedChannel[Unit],
      inbox: UnboundedChannel[Inbox],
      modelName: String = "",
      contextWindow: Int = 0,
      provider: String = "",
      modelId: String = "",
      baseUrl: String = ""
  )(using Async.Spawn): Unit

/** The default TUI: a streaming chat transcript on auk's own rendering library.
  *
  * A thin factory — it sets up the terminal and hands the channels to a
  * [[ChatApp]] running on the gears-based [[Runtime]], keeping [[Tui]] purely
  * about the channel contract.
  */
object ChatTui extends Tui:
  override def run(
      events: ReadableChannel[AgentEvent],
      commands: UnboundedChannel[UserCommand],
      interrupts: UnboundedChannel[Unit],
      inbox: UnboundedChannel[Inbox],
      modelName: String = "",
      contextWindow: Int = 0,
      provider: String = "",
      modelId: String = "",
      baseUrl: String = ""
  )(using Async.Spawn): Unit =
    // Real terminal when we have a TTY; a headless stub otherwise (piped/CI).
    val terminal: Terminal = NodeTerminal.create().getOrElse(HeadlessTerminal)
    // Render at ~60fps; Ctrl+Q still provides a direct quit shortcut. run()
    // blocks until the user quits.
    Runtime.run(
      ChatApp(
        events,
        commands,
        interrupts,
        inbox,
        modelName = modelName,
        contextWindow = contextWindow,
        provider = provider,
        modelId = modelId,
        baseUrl = baseUrl
      ),
      terminal,
      RuntimeConfig(frameMs = 16, quitKey = Key.Ctrl('Q'))
    )
