package auk

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.agent.{AgentEvent, Engine, PromptEnv, SystemPrompt, UserCommand}
import auk.config.{AppConfig, ModelConfig}
import auk.llm.endpoint.LLMConfig
import auk.llm.provider.{ActiveModel, Model, ModelSelection, ModelSession}
import auk.llm.tools.RuntimeContext
import auk.runtime.repl.ScalaRepl
import auk.runtime.{ToolRegistry, SubAgent, GetMemory, WriteMemory, EvalScala}
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

  // Per-model base config: the model id plus its configured default reasoning
  // effort. Shared by the top-level agent and any sub-agent it spawns; tools and
  // the system prompt are layered on per-consumer, so this carries none.
  def configFor(m: Model): LLMConfig =
    LLMConfig(model = m.id, thinking = Some(m.thinking))

  // The live, swappable model: built from the catalog, persisted to `.auk/config`
  // on every switch. The Engine and its sub-agents all read from this.
  val models = ModelSession(
    ActiveModel(
      selected.endpoint,
      configFor(selected.model),
      selected.model.name,
      selected.model.contextWindow,
      selected.provider.name,
      selected.provider.baseUrl
    ),
    (providerName, modelId) =>
      ModelSelection
        .byRef(providerName, modelId)
        .map(rm =>
          ActiveModel(rm.endpoint, configFor(rm.model), rm.model.name, rm.model.contextWindow, rm.provider.name, rm.provider.baseUrl)
        )
  )

  val persistModel: (String, String) => Either[String, Unit] = (providerName, modelId) =>
    val current = AppConfig.load().getOrElse(AppConfig.empty)
    AppConfig
      .save(current.copy(model = Some(ModelConfig(Some(providerName), Some(modelId)))))
      .left
      .map(_.map(_.render).mkString("; "))

  // Two independent Scala REPL sessions, each spawning its worker lazily on
  // first use: one for the top-level agent, one shared by sub-agents. Keeping
  // them separate means a sub-agent's accumulated definitions never bleed into
  // the parent's session (and vice versa).
  val scalaRepl = ScalaRepl()
  val subAgentRepl = ScalaRepl()

  // A sub-agent shares the project's memory and gets its own eval_scala (whose
  // library carries the shell and file APIs), but not the SubAgent tool itself,
  // so it can't spawn further sub-agents.
  val subAgent =
    SubAgent(models, ToolRegistry.of(GetMemory, WriteMemory, EvalScala(subAgentRepl)))

  // The tools the model may call, and where they run (the process working
  // directory, auto-approving for now). File reads/writes/edits and shell
  // commands are not direct tools: eval_scala's runtime library (`lib.fs`,
  // `lib.shell`) covers them, so that work is done by writing Scala. Memory and
  // the sub-agent remain.
  val registry =
    ToolRegistry.of(GetMemory, WriteMemory, subAgent, EvalScala(scalaRepl))

  Async.fromSync:
    // Spawn the engine in the structured scope; it lives until commands closes.
    // Closing `events` in a `finally` is the consumer-side safety net: however
    // the engine exits — clean shutdown, a crash, or scope cancellation — the
    // TUI's events subscription is released rather than left waiting forever.
    val worker =
      Future:
        try
          // Assemble the full prompt here, where we are already under Async (git
          // status is gathered via subprocess): static instruction sections plus
          // the dynamic environment + project-instruction sections for this run.
          val systemPrompt = SystemPrompt.build(
            PromptEnv(context.workingDirectory, selected.model.name, Platform.today())
          )
          Engine(commands.asReadable, events.asSendable, interrupts.asReadable, models, session, sessionProvider, registry, context, persistModel, systemPrompt).run()
        finally events.close()
    // Runs the TUI's render loop on this thread until the user quits.
    ChatTui.run(
      events.asReadable,
      commands,
      interrupts,
      modelName = selected.model.name,
      contextWindow = selected.model.contextWindow,
      provider = selected.provider.name,
      modelId = selected.model.id,
      baseUrl = selected.provider.baseUrl
    )
    // Closing commands ends the engine's read loop, whose `finally` closes events.
    commands.close()
    // Stop the REPL workers (if either was ever spawned) so their open pipes
    // don't keep the process alive after the TUI exits.
    scalaRepl.close()
    subAgentRepl.close()
