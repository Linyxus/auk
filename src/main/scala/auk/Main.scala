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
import auk.runtime.{ToolRegistry, Read, Edit, Write, Bash, SubAgent, GetMemory, WriteMemory}
import auk.session.SessionProvider
import auk.tui.ChatTui
import auk.utils.Result

@main def main(): Unit =
  val commands = UnboundedChannel[UserCommand]() // TUI → Engine
  val events = UnboundedChannel[Result[StreamEvent, LLMError]]() // Engine → TUI

  val endpoint = OpenRouterEndpoint.createFromEnv()
  val context = RuntimeContext.cwd()
  val session =
    SessionProvider.directory(context.resolve(SessionProvider.RelativePath)).create() match
      case Right(s) => s
      case Left(err) =>
        System.err.println(s"Session persistence error: $err")
        scala.sys.exit(1)

  // Model settings shared by the top-level agent and any sub-agent it spawns.
  // Tools are set per-registry below, so this carries no tools of its own.
  val baseConfig = LLMConfig(
    model = "deepseek/deepseek-v4-flash",
    thinking = Some(ThinkingMode.Auto)
  )

  // A sub-agent gets its own read/edit/run toolset and shares the project's
  // memory, but not the SubAgent tool itself, so it can't spawn further
  // sub-agents.
  val subAgent =
    SubAgent(endpoint, baseConfig, ToolRegistry.of(Read, Edit, Write, Bash, GetMemory, WriteMemory))

  // The tools the model may call, and where they run (the process working
  // directory, auto-approving for now).
  val registry = ToolRegistry.of(Read, Edit, Write, Bash, GetMemory, WriteMemory, subAgent)

  val config = baseConfig.copy(tools = registry.schemas)

  Async.blocking:
    // Spawn the engine in the structured scope; it lives until commands closes.
    val worker =
      Future(
        Engine(commands.asReadable, events.asSendable, endpoint, config, session, registry, context).run()
      )
    // Runs the TUI's render loop on this thread until the user quits.
    ChatTui.run(events.asReadable, commands)
    // Let the engine drain and exit; leaving the scope also cancels `worker`.
    commands.close()
    // Close the engine→UI channel; the TUI has already torn down by now.
    events.close()
