package auk

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.agent.{AgentEvent, Engine, UserCommand}
import auk.llm.endpoint.{
  LLMConfig,
  ThinkingMode
}
import auk.llm.provider.ModelSelection
import auk.llm.tools.RuntimeContext
import auk.runtime.{ToolRegistry, Read, Edit, Write, Bash, SubAgent, GetMemory, WriteMemory}
import auk.session.SessionProvider
import auk.tui.ChatTui
import auk.platform.Platform

@main def main(): Unit =
  val commands = UnboundedChannel[UserCommand]() // TUI → Engine
  val events = UnboundedChannel[AgentEvent]() // Engine → TUI

  // Resolve which provider + model to use.
  val selected =
    ModelSelection.resolve() match
      case Right(r) => r
      case Left(err) =>
        System.err.nn.println(s"Model selection error: $err")
        Platform.exit(1)
  val endpoint = selected.endpoint
  val context = RuntimeContext.cwd()
  val sessionProvider = SessionProvider.directory(context.resolve(SessionProvider.RelativePath))
  val session =
    sessionProvider.create() match
      case Right(s) => s
      case Left(err) =>
        System.err.nn.println(s"Session persistence error: $err")
        Platform.exit(1)

  // Model settings shared by the top-level agent and any sub-agent it spawns.
  // Tools are set per-registry below, so this carries no tools of its own.
  val baseConfig = LLMConfig(
    model = selected.model.id,
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

  Async.fromSync:
    // Spawn the engine in the structured scope; it lives until commands closes.
    val worker =
      Future(
        Engine(commands.asReadable, events.asSendable, endpoint, config, session, sessionProvider, registry, context).run()
      )
    // Runs the TUI's render loop on this thread until the user quits.
    ChatTui.run(events.asReadable, commands)
    // Let the engine drain and exit; leaving the scope also cancels `worker`.
    commands.close()
    // Close the engine→UI channel; the TUI has already torn down by now.
    events.close()
