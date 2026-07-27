package auk

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.agent.{AgentEvent, Engine, Inbox, PromptEnv, SystemPrompt, UserCommand}
import auk.config.{AppConfig, ModelConfig}
import auk.llm.endpoint.LLMConfig
import auk.llm.provider.{ActiveModel, Model, ModelSelection, ModelSession}
import auk.llm.tools.RuntimeContext
import auk.runtime.repl.ScalaRepl
import auk.runtime.{ToolRegistry, EvalScala, WorkflowBridge, TeamBridge, WorkflowWebServer, ReplPool}
import auk.runtime.mcp.{McpHub, McpServerConfig, McpToolSource}
import auk.session.{InputHistory, SessionProvider, SessionRef}
import auk.workflow.WireMessage
import auk.tui.{ChatTui, DisplayMode}
import auk.platform.{CrashGuard, PathOps, Platform}

@main def main(): Unit =
  // `auk --version` prints the build version and exits before anything boots.
  // Also how release.sh verifies the artifact it is about to publish.
  if Platform.argv.contains("--version") then
    println(s"auk v${auk.generated.BuildInfo.version}")
    Platform.exit(0)

  // Record (and survive) otherwise-fatal async failures before anything else, so
  // an intermittent crash leaves a trail in .auk/crash.log. A native engine fault
  // (JSC/Wasm under JSPI) bypasses this — its absence from the log is the tell.
  CrashGuard.install()

  val commands = UnboundedChannel[UserCommand]() // TUI → Engine (control plane: session/model)
  val events = UnboundedChannel[AgentEvent]() // Engine → TUI
  // A separate out-of-band channel for interrupts: the engine reads it
  // concurrently while a turn is in flight (it is not reading `commands` then),
  // so `Ctrl+C k` can cancel mid-turn rather than queueing behind it.
  val interrupts = UnboundedChannel[Unit]() // TUI → Engine (interrupt signal)
  // The conversation plane: user messages and system notices. The engine drains
  // it into a running turn at round boundaries (or wakes idle on a new item).
  // Harness/automation code can push `Inbox.SystemNotice(...)` here too.
  val inbox = UnboundedChannel[Inbox]() // TUI / harness → Engine

  // The one place `.auk/config` is read at startup. Every section (model, MCP)
  // is decoded from this single load, and a malformed file is fatal here rather
  // than degrading each consumer separately: the errors are line-numbered and
  // the file is the local user's own, so refusing to start beats half-running
  // with silently-missing tools.
  val appConfig =
    AppConfig.load() match
      case Right(c) => c
      case Left(errs) =>
        System.err.nn.println(
          s"Invalid ${AppConfig.RelativePath}:\n" + errs.map("  " + _.render).mkString("\n")
        )
        Platform.exit(1)

  // Resolve which provider + model to use.
  val selected =
    ModelSelection.resolve(appConfig) match
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
  // The live session id, shared so workflow sub-agents and team members file
  // their logs under whatever session is current; the engine updates it on
  // `/new` and `/resume`.
  val sessionRef = SessionRef(session.id)
  // A durable, cross-session record of every prompt the user submits.
  val inputHistory = InputHistory(context.resolve(InputHistory.RelativePath))

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
  // Browser → host control (pause/resume), set once the bridge below exists. The
  // dashboard's POST handler calls through this indirection so `web` need not
  // forward-reference the bridge.
  var workflowControl: (String, String) => Unit = (_, _) => ()
  val web = WorkflowWebServer(
    onStarted = url => events.sendImmediately(AgentEvent.Dashboard(url)),
    onError = msg => events.sendImmediately(AgentEvent.Notice(s"Workflow dashboard unavailable: $msg")),
    onControl = (action, runId) => workflowControl(action, runId)
  )

  // MCP: the project's configured MCP servers (the `[mcp.servers.*]` sections of
  // `.auk/config`) feed a host-owned hub, consumed by the engine's native MCP
  // tool integration and closed at shutdown. Three states, three contracts: a
  // malformed config never gets here (fatal at the load above), a valid config
  // with no `[mcp]` section is the normal no-server case, and a server that
  // fails to SPAWN stays non-fatal — that is McpHub's business, not startup's.
  val mcpConfigs = McpServerConfig.fromAppConfig(appConfig)
  // MCP servers used to live in their own `.auk/mcp.json`. That file is no
  // longer read at all, so a project still carrying one would lose its servers
  // without a word — say so once at startup.
  if Platform.fs.exists(PathOps.join(Platform.cwd(), ".auk/mcp.json")) then
    events.sendImmediately(
      AgentEvent.Notice(
        ".auk/mcp.json is no longer read; declare MCP servers in .auk/config under [mcp.servers.<name>]."
      )
    )
  val mcpHub = McpHub(mcpConfigs)
  // Turns each configured server's tools (and the resource meta-tools) into
  // native model tools. Discovery runs in the background (below); the snapshot
  // grows as servers respond and is read fresh by the agents' registries.
  // Every discovery settle also pushes a status snapshot to the TUI's `/mcp`
  // panel, and one is seeded up front so the panel opens on "connecting"
  // servers rather than an empty list.
  val mcpTools = McpToolSource(
    mcpHub,
    mcpConfigs,
    onUpdate = servers => events.sendImmediately(AgentEvent.McpUpdated(servers))
  )
  if mcpConfigs.nonEmpty then events.sendImmediately(AgentEvent.McpUpdated(mcpTools.snapshot))

  val workflowSocket = WorkflowBridge.defaultSocketPath()
  val workflowBridge: WorkflowBridge =
    WorkflowBridge(
      socketPath = workflowSocket,
      models = models,
      pool = ReplPool(() => ScalaRepl()),
      // Workflow sub-agents get eval_scala plus a snapshot of the MCP tools taken
      // when the sub-agent is built; the sub-agent registry is rebuilt per task,
      // so a plain snapshot (not the dynamic supplier) is right here.
      baseTools = repl => EvalScala(repl) :: mcpTools.tools,
      systemPrompt = SystemPrompt.workflowAgent(mcpConfigs.nonEmpty),
      context = context,
      onEvent = ev =>
        events.sendImmediately(AgentEvent.Orchestration(ev))
        if dashboard then
          web.ensureStarted()
          web.publish(WireMessage.Event(ev)),
      maxConcurrent = 4,
      onActivity = ev =>
        events.sendImmediately(AgentEvent.Activity(ev))
        if dashboard then web.publish(WireMessage.Activity(ev)),
      // A background run reports its result by waking the agent with a system
      // notice carrying the full result/error (the steering inbox handles idle vs
      // mid-turn delivery). This is the non-blocking replacement for the old
      // eval_scala tool result.
      onComplete = (runId, outcome) =>
        inbox.sendImmediately(Inbox.SystemNotice(WorkflowBridge.completionNotice(runId, outcome))),
      sessionRef = Some(sessionRef),
      // Host-side lifecycle notices (e.g. a run auto-pausing after persistent
      // API failures) go to the user's notice area, not to the model — poking
      // the lead with a system notice mid-outage would just spend its own
      // retry schedule on the same dead API.
      onNotice = msg => events.sendImmediately(AgentEvent.Notice(msg))
    )
  workflowControl = (action, runId) =>
    action match
      case "pause"  => workflowBridge.pause(runId)
      case "resume" => workflowBridge.resume(runId)
      case _        => ()

  // The agent team: persistent member agents the lead creates and messages. Like
  // the workflow bridge it owns the models, tools, and member REPLs and runs each
  // member as a host fiber — but where a workflow is a one-shot typed DAG, members
  // are long-lived and exchange async messages. A member's reply, and every idle /
  // rejection notice, reaches the lead as a system notice through the steering inbox.
  val teamSocket = TeamBridge.defaultSocketPath()
  val teamBridge: TeamBridge =
    TeamBridge(
      socketPath = teamSocket,
      models = models,
      makeRepl = env => ScalaRepl(extraEnv = env),
      // Team members get eval_scala plus a snapshot of the MCP tools taken when
      // the member is created (their registry is built once per member).
      baseTools = repl => EvalScala(repl) :: mcpTools.tools,
      memberPrompt = (id, desc) => SystemPrompt.teamMember(id, desc, mcpConfigs.nonEmpty),
      context = context,
      notifyLead = msg => inbox.sendImmediately(Inbox.SystemNotice(msg)),
      // The TUI's subagent panel: roster snapshots (status + tokens) and each
      // member's live transcript, keyed ("team", <member id>) so the same
      // transcript fold and fullscreen view the workflow nodes use apply.
      onActivity = (_, ev) => events.sendImmediately(AgentEvent.Activity(ev)),
      onTeam = roster => events.sendImmediately(AgentEvent.Team(roster)),
      sessionRef = Some(sessionRef)
    )

  // The top-level agent's Scala REPL session (the lead's), spawning its worker
  // lazily on first use and wired to both the workflow bridge via AUK_WF_SOCK and
  // the team bridge via AUK_TEAM_SOCK, so its workflows and team operations reach
  // the host. Workflow-node and team-member REPLs are spawned separately by their
  // bridges; the pooled workflow-node REPLs carry no socket, so they cannot recurse.
  val scalaRepl = ScalaRepl(extraEnv = Map("AUK_WF_SOCK" -> workflowSocket, "AUK_TEAM_SOCK" -> teamSocket))

  // The tools the model may call. File reads/writes/edits and shell commands are
  // not direct tools: eval_scala's runtime library (`lib.fs`, `lib.shell`) covers
  // them. The top-level eval_scala is wired to the workflow bridge.
  // The main agent is long-lived, so its registry reads the MCP tools through a
  // dynamic supplier: tools discovered after startup appear on a later round
  // without rebuilding the registry (the engine re-reads `schemas` each round).
  val registry =
    ToolRegistry.withExtra(() => mcpTools.tools)(EvalScala(scalaRepl, Some(workflowBridge)))

  Async.fromSync:
    // Start the workflow bridge's socket server + dispatch loop in this scope.
    workflowBridge.start()
    // The team bridge's server + dispatch loop live in the same scope.
    teamBridge.start()
    // Kick off MCP tool discovery in the background so it never blocks startup:
    // the first round(s) may run before a server answers, and its tools appear on
    // a later round via the registry's dynamic supplier. The future lives in this
    // (app-lifetime) scope, so it is not cancelled while discovery is in flight.
    Future(mcpTools.discover())
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
            PromptEnv(context.workingDirectory, selected.model.name, Platform.today()),
            mcpConfigured = mcpConfigs.nonEmpty
          )
          Engine(commands.asReadable, events.asSendable, interrupts.asReadable, inbox.asReadable, models, session, sessionProvider, registry, context, persistModel, systemPrompt, history = Some(inputHistory), sessionRef = Some(sessionRef), pauseWorkflow = workflowBridge.pause, resumeWorkflow = workflowBridge.resume).run()
        finally events.close()
    // Runs the TUI's render loop on this thread until the user quits.
    // Fullscreen unless `--inline` is passed; scanned from raw argv since the
    // executable/script prefix varies across node/Bun/SEA.
    val mode = DisplayMode.fromArgv(Platform.argv)
    ChatTui.run(
      events.asReadable,
      commands,
      interrupts,
      inbox,
      modelName = selected.model.name,
      contextWindow = selected.model.contextWindow,
      provider = selected.provider.name,
      modelId = selected.model.id,
      baseUrl = selected.provider.baseUrl,
      mode = mode
    )
    // Closing either control-plane channel ends the engine's select loop, whose
    // `finally` closes events.
    commands.close()
    inbox.close()
    // Stop the lead's REPL worker (if it was ever spawned) so its open pipes
    // don't keep the process alive after the TUI exits.
    scalaRepl.close()
    workflowBridge.close()
    teamBridge.close()
    mcpHub.close()
    web.close()
