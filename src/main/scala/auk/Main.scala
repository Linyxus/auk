package auk

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.agent.{Engine, UserCommand}
import auk.llm.endpoint.{StreamEvent, LLMError}
import auk.tui.ChatTui
import auk.utils.Result

@main def main(): Unit =
  val commands = UnboundedChannel[UserCommand]() // TUI → Engine
  val events = UnboundedChannel[Result[StreamEvent, LLMError]]() // Engine → TUI
  Async.blocking:
    // Spawn the engine in the structured scope; it lives until commands closes.
    val worker = Future(Engine(commands.asReadable, events.asSendable).run())
    // Runs the layoutz loop on this thread until the user quits (ctrl+q).
    ChatTui.run(events.asReadable, commands)
    // Let the engine drain and exit; leaving the scope also cancels `worker`.
    commands.close()
