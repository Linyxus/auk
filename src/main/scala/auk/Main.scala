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
import auk.repl.ScalaRepl
import auk.runtime.{ToolRegistry, Read, Edit, Write, Bash, SubAgent, GetMemory, WriteMemory, EvalScala}
import auk.session.SessionProvider
import auk.tui.ChatTui
import auk.platform.{CrashGuard, Platform}

@main def main(): Unit =
  // Record (and survive) otherwise-fatal async failures before anything else, so
  // an intermittent crash leaves a trail in .auk/crash.log. A native engine fault
  // (JSC/Wasm under JSPI) bypasses this — its absence from the log is the tell.
  CrashGuard.install()

  val commands = UnboundedChannel[UserCommand]() // TUI → Engine
  val events = UnboundedChannel[AgentEvent]() // Engine → TUI
  // A separate out-of-band channel for interrupts: the engine reads it
  // concurrently while a turn is in flight (it is not reading `commands` then),
  // so `Ctrl+C k` can cancel mid-turn rather than queueing behind it.
  val interrupts = UnboundedChannel[Unit]() // TUI → Engine (interrupt signal)

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

  // One Scala REPL session, shared across the top-level agent's eval_scala
  // calls (its worker process is spawned lazily on first use). Sub-agents do
  // not get the tool yet, so their evals can't interleave definitions with the
  // parent's.
  val scalaRepl = ScalaRepl()

  // The tools the model may call, and where they run (the process working
  // directory, auto-approving for now).
  val registry =
    ToolRegistry.of(Read, Edit, Write, Bash, GetMemory, WriteMemory, subAgent, EvalScala(scalaRepl))

  Async.fromSync:
    // Spawn the engine in the structured scope; it lives until commands closes.
    // Closing `events` in a `finally` is the consumer-side safety net: however
    // the engine exits — clean shutdown, a crash, or scope cancellation — the
    // TUI's events subscription is released rather than left waiting forever.
    val worker =
      Future:
        try
          Engine(commands.asReadable, events.asSendable, interrupts.asReadable, models, session, sessionProvider, registry, context, persistModel).run()
        finally events.close()
    // Runs the TUI's render loop on this thread until the user quits.
    ChatTui.run(events.asReadable, commands, interrupts, modelName = selected.model.name)
    // Closing commands ends the engine's read loop, whose `finally` closes events.
    commands.close()
    // Stop the REPL worker (if one was ever spawned) so its open pipes don't
    // keep the process alive after the TUI exits.
    scalaRepl.close()
