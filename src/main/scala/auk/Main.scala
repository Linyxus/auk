package auk

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.agent.{AgentEvent, Engine, PromptEnv, SystemPrompt, UserCommand}
import auk.config.{AppConfig, ModelConfig}
import auk.llm.endpoint.LLMConfig
import auk.llm.provider.{ActiveModel, Model, ModelSelection, ModelSession}
import auk.llm.tools.RuntimeContext
import auk.runtime.repl.ScalaRepl
import auk.runtime.{ToolRegistry, SubAgent, GetMemory, WriteMemory, EvalScala, WorkflowBridge, WorkflowWebServer, ReplPool}
import auk.session.SessionProvider
import auk.workflow.WireMessage
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
  // The workflow bridge: when the model writes a `wf.start { … }` workflow in
  // eval_scala, the orchestrator worker connects here and the host runs the
  // sub-agents (it owns the model + tools), streaming a live forest to the TUI.
  // Workflow sub-agents lease their own eval_scala REPLs from the pool — without
  // the workflow socket, so they cannot recurse.
  // The live workflow dashboard: a host-side HTTP+SSE server, started lazily on
  // the first workflow event (on an OS-picked spare port), serving the prebuilt
  // webui bundle and streaming the forest + sub-agent transcripts to a browser.
  // Default-on; opt out with AUK_NO_DASHBOARD=1. The URL is surfaced in the TUI.
  val dashboard = !Platform.env.get("AUK_NO_DASHBOARD").contains("1")
  val web = WorkflowWebServer(
    onStarted = url => events.sendImmediately(AgentEvent.Notice(s"Workflow dashboard: $url")),
    onError = msg => events.sendImmediately(AgentEvent.Notice(s"Workflow dashboard unavailable: $msg"))
  )

  val workflowSocket = WorkflowBridge.defaultSocketPath()
  val workflowBridge =
    WorkflowBridge(
      socketPath = workflowSocket,
      models = models,
      pool = ReplPool(() => ScalaRepl()),
      baseTools = repl => List(GetMemory, WriteMemory, EvalScala(repl)),
      systemPrompt = SystemPrompt.workflowAgent,
      context = context,
      onEvent = ev =>
        events.sendImmediately(AgentEvent.Orchestration(ev))
        if dashboard then
          web.ensureStarted()
          web.publish(WireMessage.Event(ev)),
      maxConcurrent = 4,
      onActivity = ev => if dashboard then web.publish(WireMessage.Activity(ev))
    )

  // Two independent Scala REPL sessions, each spawning its worker lazily on
  // first use: one for the top-level agent (wired to the workflow bridge via the
  // AUK_WF_SOCK env, so its workflows reach the host), one shared by sub-agents.
  // Keeping them separate means a sub-agent's definitions never bleed into the
  // parent's session.
  val scalaRepl = ScalaRepl(extraEnv = Map("AUK_WF_SOCK" -> workflowSocket))
  val subAgentRepl = ScalaRepl()

  // A sub-agent shares the project's memory and gets its own eval_scala (whose
  // library carries the shell and file APIs), but not the SubAgent tool itself,
  // so it can't spawn further sub-agents.
  val subAgent =
    SubAgent(models, ToolRegistry.of(GetMemory, WriteMemory, EvalScala(subAgentRepl)))

  // The tools the model may call. File reads/writes/edits and shell commands are
  // not direct tools: eval_scala's runtime library (`lib.fs`, `lib.shell`) covers
  // them. The top-level eval_scala is wired to the workflow bridge.
  val registry =
    ToolRegistry.of(GetMemory, WriteMemory, subAgent, EvalScala(scalaRepl, Some(workflowBridge)))

  Async.fromSync:
    // Start the workflow bridge's socket server + dispatch loop in this scope.
    workflowBridge.start()
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
    workflowBridge.close()
    web.close()
