package auk

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.agent.{Engine, UserCommand}
import auk.llm.endpoint.{
  OpenRouterEndpoint,
  LLMConfig,
  ThinkingMode,
  StreamEvent,
  LLMError
}
import auk.llm.tools.RuntimeContext
import auk.runtime.{ToolRegistry, Read, Edit, Bash}
import auk.tui.ChatTui
import auk.utils.Result

@main def main(): Unit =
  val commands = UnboundedChannel[UserCommand]() // TUI → Engine
  val events = UnboundedChannel[Result[StreamEvent, LLMError]]() // Engine → TUI

  // The tools the model may call, and where they run (the process working
  // directory, auto-approving for now).
  val registry = ToolRegistry.of(Read, Edit, Bash)
  val context = RuntimeContext.cwd()

  val endpoint = OpenRouterEndpoint.createFromEnv()
  val config = LLMConfig(
    model = "deepseek/deepseek-v4-flash",
    thinking = Some(ThinkingMode.Auto),
    tools = registry.schemas
  )

  Async.blocking:
    // Spawn the engine in the structured scope; it lives until commands closes.
    val worker =
      Future(
        Engine(commands.asReadable, events.asSendable, endpoint, config, registry, context).run()
      )
    // Runs the layoutz loop on this thread until the user quits (ctrl+q).
    ChatTui.run(events.asReadable, commands)
    // Let the engine drain and exit; leaving the scope also cancels `worker`.
    commands.close()
