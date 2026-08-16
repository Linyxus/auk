package auk

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.agent.{AgentEvent, Engine, Inbox, PromptEnv, SystemPrompt, UserCommand}
import auk.config.{AppConfig, ModelConfig}
import auk.llm.endpoint.LLMConfig
import auk.llm.provider.{ActiveModel, Model, ModelSelection, ModelSession, Providers}
import auk.llm.tools.RuntimeContext
import auk.runtime.repl.{ScalaRepl, WorkerLog, WorkerLogs}
import auk.runtime.{ToolRegistry, EvalScala, WorkflowBridge, TeamBridge, LoopBridge, LoopStartup, LoopWirer, WorkflowWebServer, ReplPool, SkillTools}
import auk.runtime.skills.{SkillManager, SkillStore}
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
  // A missing API key is not fatal: the session opened on a stub endpoint that
  // fails each request with this same message (which names /login as the fix).
  // Say so once in the transcript. And when NO provider has a key — a fresh
  // user, not a mis-set env — the TUI opens straight onto the /login provider
  // list below.
  selected.keyMissing.foreach(missing =>
    events.sendImmediately(
      AgentEvent.TranscriptNote(s"$missing. Messages will fail until a key is added.")
    )
  )
  val onboardLogin = selected.keyMissing.isDefined && Providers.all.forall(_.apiKey.isEmpty)
  // A credentials file that exists but cannot be read deserves one loud line —
  // silently seeing "no keys" would look identical to the file being ignored.
  auk.config.Credentials.problem.foreach(msg =>
    events.sendImmediately(AgentEvent.Notice(s"credentials store unreadable — $msg"))
  )
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

  // Every REPL worker auk spawns keeps its own file under
  // `.auk/sessions/<session>/repl` — what the parent wrote to it, what it said
  // back, and every decision taken about it. The directory is resolved per
  // SPAWN, not per REPL: `/new` and `/resume` move the live session out from
  // under a long-lived pool, lead or member REPL, and a worker's record belongs
  // with the session that was actually talking to it. `label` becomes part of
  // the file name, so anything outside the safe set becomes '-'; the factory
  // adds the generation and pid.
  def workerLog(label: String): (Int, Option[Int]) => WorkerLog =
    val safe = label.replaceAll("[^A-Za-z0-9._-]", "-").nn
    (generation, pid) =>
      val sessions = context.resolve(SessionProvider.RelativePath)
      WorkerLogs(PathOps.join(PathOps.join(sessions, sessionRef.id), "repl"), safe)(generation, pid)

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
      selected.provider.effectiveBaseUrl
    ),
    (providerName, modelId) =>
      ModelSelection
        .byRef(providerName, modelId)
        .map(rm =>
          ActiveModel(rm.endpoint, configFor(rm.model), rm.model.name, rm.model.contextWindow, rm.provider.name, rm.provider.effectiveBaseUrl)
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
  // Who started a workflow run, for the runs the lead did not start. Same indirection,
  // same reason: the bridge's own completion handler is the caller.
  var workflowRunOwner: String => Option[String] = _ => None
  val web = WorkflowWebServer(
    onStarted = url => events.sendImmediately(AgentEvent.Dashboard(url)),
    onError = msg => events.sendImmediately(AgentEvent.Notice(s"Workflow dashboard unavailable: $msg")),
    onControl = (action, runId) => workflowControl(action, runId),
    // What the dashboard needs to show loops it is not being told about: the
    // project's own `.auk/loops`, the tree a generation's patch is computed in, and
    // the session directories holding transcript tees.
    loopContext = Some(context)
  )
  // The project's loops on disk, read by the session-open scan and re-read on every
  // loop tick to feed the dashboard the deep fold the TUI's snapshots do not carry.
  val loopStore = auk.loop.LoopStore.in(context)

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
      pool = ReplPool(() => ScalaRepl(makeLog = workerLog("pool"))),
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
      // Sub-agents running at once, across all of the lead's runs. Each may lease
      // a REPL worker from the pool, so this also caps the worker processes.
      maxConcurrent = 8,
      onActivity = ev =>
        events.sendImmediately(AgentEvent.Activity(ev))
        if dashboard then web.publish(WireMessage.Activity(ev)),
      // A background run reports its result by waking the agent with a system
      // notice carrying the full result/error (the steering inbox handles idle vs
      // mid-turn delivery). This is the non-blocking replacement for the old
      // eval_scala tool result.
      //
      // Only for the lead's OWN runs, though. A loop generation's worker may fan work
      // out the same way, and those runs are its business: the lead never wrote them,
      // cannot act on them, and would be told "workflow worker disconnected" every time
      // a generation ended with one still in flight. Those get a user-facing line and
      // nothing more — the loop's transcripts already show the work.
      onComplete = (runId, outcome) =>
        workflowRunOwner(runId) match
          case None =>
            inbox.sendImmediately(Inbox.SystemNotice(WorkflowBridge.completionNotice(runId, outcome)))
          case Some(owner) =>
            events.sendImmediately(
              AgentEvent.Notice(WorkflowBridge.delegatedCompletionNotice(owner, runId, outcome))),
      sessionRef = Some(sessionRef),
      // Host-side lifecycle notices (e.g. a run auto-pausing after persistent
      // API failures) go to the user's notice area, not to the model — poking
      // the lead with a system notice mid-outage would just spend its own
      // retry schedule on the same dead API.
      onNotice = msg => events.sendImmediately(AgentEvent.Notice(msg))
    )
  workflowRunOwner = workflowBridge.ownerOf
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
      // The member this REPL belongs to is in the env the bridge builds for it
      // (`AUK_TEAM_ID`, a bare literal in TeamBridge.scala — a rename there has to
      // be grepped for here), which is the only place Main can read it from.
      makeRepl = env => ScalaRepl(extraEnv = env, makeLog = workerLog("team-" + env.getOrElse("AUK_TEAM_ID", "member"))),
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

  // Which of a loop's REPLs a worker log is about, read from the env the bridge
  // builds for it — these key names are bare literals in LoopBridge.scala, so a
  // rename there has to be grepped for here. A generation's worker is named by
  // the owner tag it hires team members under (`loop:<id>:gen-<n>`) rather than
  // by AUK_LOOP_WORKER, which is a prose phrase written to read inside a
  // sentence; a gate worker carries the loop whose definition it is validating;
  // and the evaluator's REPL — which reaches nothing — carries no env at all.
  //
  // A generation worker's file therefore reads `loop-goal-gen-3-gen1-pid77.jsonl`:
  // the first number is the LOOP generation, the second the REPL's own respawn
  // counter, which starts over for every worker. Two different counters, not one
  // written twice.
  def loopLabel(env: Map[String, String]): String =
    env
      .get("AUK_TEAM_OWNER")
      .orElse(env.get("AUK_LOOP_ATTACH").map(id => s"loop-$id-gate"))
      .getOrElse("loop-evaluator")

  // Refinement loops: durable, goal-directed work the lead starts with
  // `lib.loop.start` and the host then drives. It validates a loop's definition (by
  // re-evaluating the captured eval in a private gate worker), writes the loop's
  // ledger, and then spends its budget on it: each generation is a fresh worker
  // agent improving the live tree, the definition's own Scala checker, and an
  // evaluator agent judging what passes. Its agents get the same prompt and tools
  // workflow sub-agents get — one focused task, a structured submission, no human.
  val loopSocket = LoopBridge.defaultSocketPath()
  val loopBridge: LoopBridge =
    LoopBridge(
      socketPath = loopSocket,
      models = models,
      // A loop REPL's log files under the session live at the SPAWN, not under the
      // generation's pinned genSession its transcript tee uses: the two agree in
      // practice (a generation pins the live id when it starts), and reaching
      // genSession from here would cost a signature change for a cosmetic gain.
      makeRepl = env => ScalaRepl(extraEnv = env, makeLog = workerLog(loopLabel(env))),
      baseTools = repl => EvalScala(repl, Some(workflowBridge)) :: mcpTools.tools,
      workerSystemPrompt = SystemPrompt.workflowAgent(mcpConfigs.nonEmpty),
      context = context,
      // A loop's milestones are the model's business: it wrote the definition and
      // decides what to do when one fails to validate or a loop parks.
      notifyLead = msg => inbox.sendImmediately(Inbox.SystemNotice(msg)),
      // The rare warning with no other surface (an unrestorable submodule); loop
      // progress itself goes through onLoop below, not here.
      onNotice = msg => events.sendImmediately(AgentEvent.Notice(msg)),
      // The TUI's loops window and census line: full snapshots (phase, lineage, in-flight stage) and
      // each generation agent's live transcript, keyed (<loop id>, <agent label>) so
      // the same transcript fold and fullscreen view the workflow nodes and team
      // members use apply here too.
      onLoop = views =>
        events.sendImmediately(AgentEvent.Loops(views))
        if dashboard then
          // A loop actually being worked is what justifies opening a port. Parked
          // loops on disk are standing context, not an event: starting a dashboard
          // for them would give every session with an old loop a server it never
          // asked for. They still reach a browser that connects — the server reads
          // them off disk itself.
          if views.exists(_.live) then web.ensureStarted()
          // The snapshot the TUI gets is lean by design; the browser wants every
          // attempt and every judgement, which means the ledger. A few KB per stage.
          views.foreach(view => LoopWirer.fromStore(loopStore, view).foreach(web.publishLoop)),
      onActivity = (_, ev) =>
        events.sendImmediately(AgentEvent.Activity(ev))
        if dashboard then web.publishLoopActivity(ev),
      // A generation's worker may delegate: it reaches the workflow and team bridges
      // exactly as the lead does. NOT the loop bridge — a generation that could start
      // a loop would be spending a budget nobody granted it, in a tree this loop is
      // already snapshotting — and NOT the gate or the evaluator, which judge rather
      // than delegate.
      workerEnv = Map("AUK_WF_SOCK" -> workflowSocket, "AUK_TEAM_SOCK" -> teamSocket),
      // …and the members it hires are retired with the generation that hired them.
      retireTeamOwned = owner => teamBridge.retireOwnedBy(owner),
      sessionRef = Some(sessionRef)
    )

  // What this project's `.auk/loops` already holds. A loop outlives the session that
  // started it, so a session opening on a project with unfinished ones carries them
  // in the prompt as standing context for the lead; the user's copy is the ctrl+c l
  // window, fed by the bridge's own startup scan, plus the activity line's count of
  // the ones actually running. Never an inbox item: that would fire a model turn
  // before the user has typed a word.
  val waitingLoops = LoopStartup.scan(loopStore)

  // …with one thing said out loud: a loop a dead session left RUNNING is an accident,
  // not a decision, so it gets a single line in the transcript and no further chrome.
  // Parked loops say nothing here — the window and the prompt section have them.
  LoopStartup.orphanNote(waitingLoops).foreach(msg => events.sendImmediately(AgentEvent.TranscriptNote(msg)))

  // The top-level agent's Scala REPL session (the lead's) is owned by the skill
  // manager, which loads the stored skill set into it at startup and SWAPS it
  // for a freshly validated session on every successful skill change — hence
  // every consumer reads it through `skillManager.repl`. Each session spawns its
  // worker lazily on first use and is wired to the workflow bridge via
  // AUK_WF_SOCK, the team bridge via AUK_TEAM_SOCK and the loop bridge via
  // AUK_LOOP_SOCK, so its workflows, team operations and loops reach the host.
  // Workflow-node, team-member and loop REPLs are spawned separately by their
  // bridges; the pooled workflow-node REPLs carry no socket, so they cannot recurse.
  // A loop's GENERATION worker is the one exception — it carries the workflow and
  // team sockets so a generation can delegate — while the loop's gate and evaluator
  // workers carry none.
  val skillManager = SkillManager(
    SkillStore(context.resolve(SkillStore.RelativePath)),
    () =>
      ScalaRepl(
        extraEnv =
          Map("AUK_WF_SOCK" -> workflowSocket, "AUK_TEAM_SOCK" -> teamSocket, "AUK_LOOP_SOCK" -> loopSocket),
        // A swapped session is still the lead's, so every one of them logs as "lead".
        makeLog = workerLog("lead"))
  )

  // The tools the model may call. File reads/writes/edits and shell commands are
  // not direct tools: eval_scala's runtime library (`lib.fs`, `lib.shell`) covers
  // them. The top-level eval_scala is wired to the workflow bridge and reads the
  // live REPL through the skill manager, so it follows session swaps.
  // The main agent is long-lived, so its registry reads the MCP tools through a
  // dynamic supplier: tools discovered after startup appear on a later round
  // without rebuilding the registry (the engine re-reads `schemas` each round).
  val leadTools: List[auk.llm.tools.Tool] =
    new EvalScala(() => skillManager.repl, Some(workflowBridge), Some(loopBridge)) :: SkillTools.all(skillManager)
  val registry = ToolRegistry.withExtra(() => mcpTools.tools)(leadTools*)

  Async.fromSync:
    // Start the workflow bridge's socket server + dispatch loop in this scope.
    workflowBridge.start()
    // The team bridge's server + dispatch loop live in the same scope.
    teamBridge.start()
    // …and so do the loop bridge's.
    loopBridge.start()
    // Kick off MCP tool discovery in the background so it never blocks startup:
    // the first round(s) may run before a server answers, and its tools appear on
    // a later round via the registry's dynamic supplier. The future lives in this
    // (app-lifetime) scope, so it is not cancelled while discovery is in flight.
    Future(mcpTools.discover())
    // Spawn the engine in the structured scope; it lives until commands closes.
    // Closing `events` in a `finally` is the consumer-side safety net: however
    // the engine exits — clean shutdown, a crash, or scope cancellation — the
    // TUI's events subscription is released rather than left waiting forever.
    // Skill load + prompt assembly run CONCURRENTLY with the engine loop. The
    // load must precede assembly (the Skills section reports exactly what it
    // found — loaded / broken skills; a skill-less store touches nothing,
    // keeping the lazy worker spawn), but with a stored set it compiles the
    // whole unit in a cold REPL worker — seconds — and sequencing the engine
    // after it made the first submit's InputsConsumed echo (what renders the
    // user's message in the transcript) wait that long. The engine starts at
    // once instead and only its first API request awaits the prompt.
    val promptReady =
      Future:
        skillManager.initialLoad()
        // Assemble the full prompt here, where we are already under Async (git
        // status is gathered via subprocess): static instruction sections plus
        // the skill index and the dynamic environment + project-instruction
        // sections for this run.
        SystemPrompt.build(
          PromptEnv(context.workingDirectory, selected.model.name, Platform.today()),
          mcpConfigured = mcpConfigs.nonEmpty,
          extraSections = List(SystemPrompt.Section("Skills", skillManager.promptSection)) ++
            LoopStartup.section(waitingLoops).map(SystemPrompt.Section("Loops", _))
        )
    val worker =
      Future:
        try
          Engine(commands.asReadable, events.asSendable, interrupts.asReadable, inbox.asReadable, models, session, sessionProvider, registry, context, persistModel, systemPrompt = promptReady.await, history = Some(inputHistory), sessionRef = Some(sessionRef), pauseWorkflow = workflowBridge.pause, resumeWorkflow = workflowBridge.resume).run()
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
      baseUrl = selected.provider.effectiveBaseUrl,
      mode = mode,
      // `o` in the loops window, which is the one place a dashboard is asked for
      // rather than announced: a project's loops can all be parked on disk, and then
      // nothing this session does would ever start a server. Starting one reports its
      // URL back as AgentEvent.Dashboard, which is what the TUI opens the browser on.
      // Disabled by env, the key asks for a server that never comes and opens nothing.
      requestDashboard = () => if dashboard then web.ensureStarted(),
      keyless = selected.keyMissing.isDefined,
      onboardLogin = onboardLogin
    )
    // Closing either control-plane channel ends the engine's select loop, whose
    // `finally` closes events.
    commands.close()
    inbox.close()
    // Stop the lead's REPL worker (if it was ever spawned) so its open pipes
    // don't keep the process alive after the TUI exits.
    skillManager.close()
    workflowBridge.close()
    teamBridge.close()
    loopBridge.close()
    mcpHub.close()
    web.close()
