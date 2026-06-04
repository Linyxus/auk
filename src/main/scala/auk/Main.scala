package auk

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.agent.{AgentEvent, Engine, UserCommand}
import auk.config.{AppConfig, ModelConfig}
import auk.llm.endpoint.{
  LLMConfig,
  ThinkingMode
}
import auk.llm.provider.{ActiveModel, ModelSelection, ModelSession}
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
  val context = RuntimeContext.cwd()
  val sessionProvider = SessionProvider.directory(context.resolve(SessionProvider.RelativePath))
  val session =
    sessionProvider.create() match
      case Right(s) => s
      case Left(err) =>
        System.err.nn.println(s"Session persistence error: $err")
        Platform.exit(1)

  // Model settings shared by the top-level agent and any sub-agent it spawns.
  // Tools are layered on per-consumer, so this base carries none of its own.
  val baseConfig = LLMConfig(
    model = selected.model.id,
    thinking = Some(ThinkingMode.Auto)
  )

  // The live, swappable model: built from the catalog, persisted to `.auk/config`
  // on every switch. The Engine and its sub-agents all read from this.
  val models = ModelSession(
    ActiveModel(selected.endpoint, baseConfig, selected.model.name),
    (providerName, modelId) =>
      ModelSelection
        .byRef(providerName, modelId)
        .map(rm => ActiveModel(rm.endpoint, baseConfig.copy(model = rm.model.id), rm.model.name))
  )

  val persistModel: (String, String) => Either[String, Unit] = (providerName, modelId) =>
    val current = AppConfig.load().getOrElse(AppConfig.empty)
    AppConfig
      .save(current.copy(model = Some(ModelConfig(Some(providerName), Some(modelId)))))
      .left
      .map(_.map(_.render).mkString("; "))

  // A sub-agent gets its own read/edit/run toolset and shares the project's
  // memory, but not the SubAgent tool itself, so it can't spawn further
  // sub-agents.
  val subAgent =
    SubAgent(models, ToolRegistry.of(Read, Edit, Write, Bash, GetMemory, WriteMemory))

  // The tools the model may call, and where they run (the process working
  // directory, auto-approving for now).
  val registry = ToolRegistry.of(Read, Edit, Write, Bash, GetMemory, WriteMemory, subAgent)

  Async.fromSync:
    // Spawn the engine in the structured scope; it lives until commands closes.
    // Closing `events` in a `finally` is the consumer-side safety net: however
    // the engine exits — clean shutdown, a crash, or scope cancellation — the
    // TUI's events subscription is released rather than left waiting forever.
    val worker =
      Future:
        try
          Engine(commands.asReadable, events.asSendable, models, session, sessionProvider, registry, context, persistModel).run()
        finally events.close()
    // Runs the TUI's render loop on this thread until the user quits.
    ChatTui.run(events.asReadable, commands, modelName = selected.model.name)
    // Closing commands ends the engine's read loop, whose `finally` closes events.
    commands.close()
