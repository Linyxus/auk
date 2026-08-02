package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.TestFs
import auk.agent.{LoopGenerationState, LoopView}
import auk.llm.endpoint.{ChatResponse, Content, Endpoint, FinishReason, LLMConfig, LLMError, Message, Role, StreamEvent}
import auk.llm.provider.ModelSession
import auk.llm.tools.{ApprovalPolicy, Json, RuntimeContext, Schema, Tool, ToolInput, ToolResult}
import auk.loop.{LoopEvent, LoopStore}
import auk.platform.PathOps
import auk.platform.js.{Interop, ReplArtifacts}
import auk.runtime.repl.ScalaRepl
import auk.session.SessionRef
import auk.snapshot.Snapshot
import auk.utils.Result
import auk.workflow.TranscriptEvent

/** The [[LoopBridge]] generation engine end to end: a real loop, driven by the host,
  * against a throwaway git repository.
  *
  * Everything except the model is real — the ledger on disk, the snapshot refs, the
  * working tree, and the loop's own checker, which really does run as a closure inside
  * a gate REPL and answers over its stdout. The worker and evaluator agents are driven
  * by a scripted endpoint ([[ScriptedEndpoint]]) whose replies are derived from the
  * conversation rather than from a counter, so a test says what each generation does
  * and nothing depends on how many times the engine happened to ask.
  *
  * The lead's own REPL is skipped: a loop is created here by sending `hello` over the
  * raw wire and handing the definition to [[LoopBridge.announceDef]], which is exactly
  * what `eval_scala` does. That leaves ONE worker process per test — the gate — so a
  * suite that runs a dozen loops stays affordable.
  */
class LoopEngineSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 240.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight

  // -- the scripted model ------------------------------------------------------------

  /** One request to the scripted model, as the test's script sees it. `role` is
    * `worker` or `eval`, `gen` the generation being worked, `attempt` which submission
    * of that role this turn would be, and `text` every user message in the conversation
    * so far — enough for a script to key on the artifact or the feedback it is looking
    * at without the endpoint having to model the engine. */
  private final case class Ask(role: String, gen: Int, attempt: Int, text: String)

  /** What the scripted model does with one [[Ask]]. */
  private enum Reply:
    /** Call `steps` in order (one tool call per turn), then submit `args`. */
    case Submit(steps: List[(String, Json)], args: Json)
    /** Answer in prose, never submitting. */
    case Prose(text: String)
    /** Fail transiently, as a provider outage does. */
    case Outage
    /** Block until `on` completes, then behave as `then_`. */
    case Wait(on: Future[Unit], then_ : Reply)

  private object Reply:
    def submit(args: Json): Reply = Submit(Nil, args)

  /** Drives both loop agents from one script.
    *
    * The role is read off the system prompt (only the evaluator's carries its own
    * section) and the attempt number off the conversation, so a retry is answered
    * differently from a first try without the test tracking anything. A turn that
    * already holds a submission just ends, which is what keeps a script from being
    * consulted again on the closing turn of an attempt it already answered.
    */
  private class ScriptedEndpoint(script: Ask => Reply, onRequest: (String, Ask) => Unit = (_, _) => ())
      extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))

    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val role = if config.systemPrompt.exists(_.contains("What you are judging")) then "eval" else "worker"
      val submitName = if role == "eval" then "submit_verdict" else "submit_generation"
      val userTexts = messages.filter(_.role == Role.User).flatMap(_.content).collect { case Content.Text(t) => t }
      val gen = GenRegex.findFirstMatchIn(userTexts.headOption.getOrElse("")).map(_.group(1).nn.toInt).getOrElse(0)
      val lastUserText = messages.lastIndexWhere(m =>
        m.role == Role.User && m.content.exists { case _: Content.Text => true; case _ => false })
      val window = if lastUserText < 0 then messages else messages.drop(lastUserText + 1)
      val uses = (ms: List[Message]) => ms.flatMap(_.content).collect { case tu: Content.ToolUse => tu }
      val submitsSoFar = uses(messages).count(_.name == submitName)
      val inWindow = uses(window)
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        if inWindow.exists(_.name == submitName) then ch.send(Right(StreamEvent.Done(stop("ok"))))
        else
          def act(reply: Reply): Unit = reply match
            case Reply.Wait(on, next) =>
              on.await
              act(next)
            case Reply.Outage =>
              ch.send(Left(LLMError("503 service unavailable", transient = true)))
            case Reply.Prose(text) =>
              ch.send(Right(StreamEvent.Done(stop(text))))
            case Reply.Submit(steps, args) =>
              val done = inWindow.count(_.name != submitName)
              val (tool, params) = if done < steps.length then steps(done) else (submitName, args)
              ch.send(Right(StreamEvent.Done(call(tool, params))))
          val ask = Ask(role, gen, submitsSoFar + 1, userTexts.mkString("\n"))
          // Only when the script is actually consulted: the closing turn of an attempt
          // short-circuits above, and a test counting prompts should not see it. The
          // system prompt is handed over separately because that — not the user
          // messages — is where a loop's goal, rubric and budgets are written.
          onRequest(config.systemPrompt.getOrElse(""), ask)
          act(script(ask))
      ch.asReadable

    private def stop(text: String): ChatResponse =
      ChatResponse(Message(Role.Assistant, List(Content.Text(text))), FinishReason.Stop)

    private def call(tool: String, params: Json): ChatResponse =
      ChatResponse(Message(Role.Assistant, List(Content.ToolUse("c1", tool, params.render))), FinishReason.ToolUse)

  private val GenRegex = "generation (\\d+)".r

  /** Answers every turn with one fixed line — a team member with nothing to do, or a
    * workflow sub-agent with a one-word job. `gate`, when given, holds the answer back,
    * so a test can be sure the agent is still thinking when something else happens. */
  private class FixedEndpoint(reply: String, gate: Option[Future[Unit]] = None) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))

    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        gate.foreach(_.await)
        ch.send(Right(StreamEvent.Done(
          ChatResponse(Message(Role.Assistant, List(Content.Text(reply))), FinishReason.Stop))))
      ch.asReadable

  // -- tools the scripted agents act with ---------------------------------------------

  /** Writes a file in the loop's working tree, creating parents. The loop's agents get
    * this instead of `eval_scala` so that no test pays for a second REPL: what matters
    * here is that the tree really changes between generations, not how. */
  private class WriteFile(root: String) extends Tool:
    type Params = Json
    val name = "write_file"
    val description = "Write a file in the working tree. Fields: path (relative), content."
    val input: ToolInput[Json] =
      ToolInput.instance(
        Schema.obj(List("path" -> Schema.string(), "content" -> Schema.string()), List("path", "content"))
      )(j => Right(j))
    def execute(params: Json)(using RuntimeContext, Async): ToolResult =
      val path = PathOps.join(root, str(params, "path"))
      PathOps.parent(path).foreach(TestFs.mkdir)
      TestFs.write(path, str(params, "content"))
      ToolResult.ok(s"wrote $path")

  /** Removes a path (a file or a whole directory) from the loop's working tree. */
  private class DeletePath(root: String) extends Tool:
    type Params = Json
    val name = "delete_path"
    val description = "Remove a path from the working tree. Field: path (relative)."
    val input: ToolInput[Json] =
      ToolInput.instance(Schema.obj(List("path" -> Schema.string()), List("path")))(j => Right(j))
    def execute(params: Json)(using RuntimeContext, Async): ToolResult =
      val fs = js.Dynamic.global.require("node:fs")
      fs.rmSync(PathOps.join(root, str(params, "path")), js.Dynamic.literal(recursive = true, force = true))
      ToolResult.ok("removed")

  private def str(j: Json, k: String): String = j match
    case o: Json.Obj => o.get(k).collect { case Json.Str(s) => s }.getOrElse("")
    case _           => ""

  /** `eval_scala`, with everything it hands back kept for the test to read. The loop's
    * agents are scripted, so an eval's own result is otherwise invisible — and for the
    * evals that are supposed to FAIL, the result is the whole point. */
  private class RecordingEval(
      repl: ScalaRepl,
      sink: scala.collection.mutable.ListBuffer[String],
      wf: Option[WorkflowBridge]
  ) extends Tool:
    private val inner = EvalScala(repl, wf)
    type Params = EvalScalaParams
    val name: String = inner.name
    val description: String = inner.description
    val input: ToolInput[EvalScalaParams] = inner.input
    def execute(params: EvalScalaParams)(using RuntimeContext, Async): ToolResult =
      val result = inner.execute(params)
      sink += result.output
      result

  private def write(path: String, content: String): (String, Json) =
    "write_file" -> Json.Obj(List("path" -> Json.Str(path), "content" -> Json.Str(content)))

  private def remove(path: String): (String, Json) =
    "delete_path" -> Json.Obj(List("path" -> Json.Str(path)))

  private def evalStep(code: String): (String, Json) =
    "eval_scala" -> Json.Obj(List("code" -> Json.Str(code)))

  // -- submissions ---------------------------------------------------------------------

  private def generation(p99: Double, description: String, knowledge: Option[String] = None): Json =
    Json.Obj(
      List(
        "artifact" -> Json.Obj(List("p99Ms" -> Json.num(p99))),
        "description" -> Json.Str(description)
      ) ++ knowledge.map(k => "knowledge" -> Json.Str(k)).toList
    )

  private def verdict(accepted: Boolean, feedback: String, goalReached: Boolean = false): Json =
    Json.Obj(List(
      "accepted" -> Json.Bool(accepted),
      "feedback" -> Json.Str(feedback),
      "goalReached" -> Json.Bool(goalReached)
    ))

  // -- the repository, the loop, and the bridge -----------------------------------------

  private def git(dir: String, args: String*): String =
    val cp = js.Dynamic.global.require("node:child_process")
    val opts = js.Dynamic.literal(cwd = dir, encoding = "utf8")
    cp.execFileSync("git", js.Array(args.map(a => a: js.Any)*), opts).asInstanceOf[String].trim

  /** A fresh repository with one commit and a `.gitignore` — the tree a loop's baseline
    * is taken from. */
  private def tempRepo(): String =
    val dir = TestFs.tempDir("auk-loop-engine")
    git(dir, "init", "-b", "main")
    TestFs.write(PathOps.join(dir, "app.txt"), "baseline\n")
    TestFs.write(PathOps.join(dir, ".gitignore"), "*.secret\n")
    git(dir, "add", "-A")
    git(dir, "-c", "user.name=Test", "-c", "user.email=t@example.com", "commit", "-m", "first")
    dir

  private def tmpSock(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-loopengine-$name-${js.Dynamic.global.process.pid}.sock").asInstanceOf[String]

  /** The artifact type every loop here reports, as the JSON schema `LibToolInput`
    * derives for it — the same text the DSL would have sent. */
  private val PerfSchema = """{"type":"object","properties":{"p99Ms":{"type":"number"}},"required":["p99Ms"]}"""

  private val Goal = "cut p99 latency"
  private val Rubric = "accepted when p99 improves and nothing else regresses"
  private val Artifact = "case class Perf(p99Ms: Double) derives LibToolInput"

  /** A self-contained loop definition whose checker is `body`, which sees `prev` and
    * `cand` exactly as a real one does. `verb` picks which of the two definition calls
    * it makes: `start` creates the loop, `amend` redefines one that exists — the source
    * is captured and re-evaluated identically either way. */
  private def definition(
      loopId: String,
      budgets: String,
      body: String,
      verb: String = "start",
      goal: String = Goal,
      rubric: String = Rubric,
      artifact: String = Artifact
  ): String =
    s"""$artifact
       |lib.loop.$verb[Perf](
       |  id = "$loopId",
       |  goal = "$goal",
       |  rubric = "$rubric",
       |  budgets = LoopBudgets($budgets)
       |) { (prev, cand) =>
       |$body
       |}""".stripMargin

  /** The checker every test uses unless it needs a stranger one: p99 must be under the
    * loop's ceiling and better than the last accepted generation. The absolute bar is
    * what lets generation 1 fail a check at all — there is nothing to improve on yet. */
  private val ImprovesChecker =
    """  if cand.artifact.p99Ms > 100 then CheckResult.fail("p99 must be under 100, got " + cand.artifact.p99Ms)
      |  else if prev.forall(_.artifact.p99Ms > cand.artifact.p99Ms) then
      |    CheckResult.pass.withMetrics("p99Ms" -> cand.artifact.p99Ms)
      |  else CheckResult.fail("p99 did not improve: " + cand.artifact.p99Ms)""".stripMargin

  /** A raw UDS client: enough of the worker's side of the wire to create, park and
    * resume a loop without spawning the lead's REPL. */
  private class WireClient(sockPath: String):
    private val net = js.Dynamic.global.require("node:net")
    val incoming = UnboundedChannel[String]()
    private var buf = ""
    private val sock = net.createConnection(sockPath).asInstanceOf[js.Dynamic]
    sock.setEncoding("utf8")
    sock.on(
      "data",
      ((chunk: js.Any) =>
        buf += chunk.asInstanceOf[String]
        var idx = buf.indexOf("\n")
        while idx >= 0 do
          val line = buf.substring(0, idx)
          buf = buf.substring(idx + 1)
          if line.nonEmpty then incoming.sendImmediately(line)
          idx = buf.indexOf("\n")
        ()
      ): js.Function1[js.Any, Unit]
    )
    private def line(obj: js.Dynamic): Unit = { sock.write(js.JSON.stringify(obj) + "\n"); () }
    def hello(loopId: String, maxGenerations: Int, patience: Int, maxAttempts: Int): Unit =
      line(
        js.Dynamic.literal(
          t = "hello",
          loopId = loopId,
          goal = "cut p99 latency",
          rubric = "accepted when p99 improves and nothing else regresses",
          budgets = js.Dynamic.literal(
            maxGenerations = maxGenerations,
            patience = patience,
            maxAttemptsPerGeneration = maxAttempts
          ),
          artifactSchema = PerfSchema,
          checkerRegistered = true
        )
      )
    def park(loopId: String): Unit = line(js.Dynamic.literal(t = "park", loopId = loopId))
    def resume(loopId: String): Unit = line(js.Dynamic.literal(t = "resume", loopId = loopId))
    /** A data-only amendment, naming only the fields it changes — which is the whole
      * point of the message, so an unnamed field is genuinely absent from the wire. */
    def reconfigure(
        loopId: String,
        goal: Option[String] = None,
        rubric: Option[String] = None,
        budgets: Option[(Int, Int, Int)] = None
    ): Unit =
      val msg = js.Dynamic.literal(t = "reconfigure", loopId = loopId)
      goal.foreach(g => msg.updateDynamic("goal")(g))
      rubric.foreach(r => msg.updateDynamic("rubric")(r))
      budgets.foreach: (gens, patience, attempts) =>
        msg.updateDynamic("budgets")(
          js.Dynamic.literal(maxGenerations = gens, patience = patience, maxAttemptsPerGeneration = attempts))
      line(msg)
    def close(): Unit = try { sock.end(); () } catch case _: Throwable => ()

  /** Just enough of the TEAM wire to sit in the lead's seat: create a member of the
    * session's own, and watch the roster the generation's members appear in. */
  private class TeamWire(sockPath: String):
    private val net = js.Dynamic.global.require("node:net")
    val incoming = UnboundedChannel[String]()
    private var buf = ""
    private val sock = net.createConnection(sockPath).asInstanceOf[js.Dynamic]
    sock.setEncoding("utf8")
    sock.on(
      "data",
      ((chunk: js.Any) =>
        buf += chunk.asInstanceOf[String]
        var idx = buf.indexOf("\n")
        while idx >= 0 do
          val line = buf.substring(0, idx)
          buf = buf.substring(idx + 1)
          if line.nonEmpty then incoming.sendImmediately(line)
          idx = buf.indexOf("\n")
        ()
      ): js.Function1[js.Any, Unit]
    )
    private def line(obj: js.Dynamic): Unit = { sock.write(js.JSON.stringify(obj) + "\n"); () }
    def hello(): Unit = line(js.Dynamic.literal(t = "hello", me = "lead"))
    def newMember(id: String, desc: String): Unit =
      line(js.Dynamic.literal(t = "new_member", id = id, desc = desc))
    def close(): Unit = try { sock.end(); () } catch case _: Throwable => ()

  /** A `roster` line as `(id, status, owner)` triples, in roster order. */
  private def rosterOf(line: String): List[(String, String, Option[String])] =
    Json.parse(line) match
      case Right(o: Json.Obj) =>
        o.get("members") match
          case Some(Json.Arr(items)) =>
            items.collect:
              case m: Json.Obj => (str(m, "id"), str(m, "status"), m.get("owner").collect { case Json.Str(s) => s })
          case _ => Nil
      case _ => Nil

  /** A real team bridge on `repo`, for the generations that hire from it. Member REPLs
    * are never used (members get no tools here), so they cost nothing but the object. */
  private def openTeam(
      name: String,
      repo: String,
      endpoint: Endpoint,
      notices: UnboundedChannel[String] = UnboundedChannel()
  )(using Async.Spawn): TeamBridge =
    val bridge = TeamBridge(
      socketPath = tmpSock(s"team-$name"),
      models = ModelSession.of(endpoint, LLMConfig(model = "test")),
      makeRepl = _ => ScalaRepl(),
      baseTools = _ => Nil,
      memberPrompt = (_, _) => "You are a team member.",
      context = RuntimeContext(repo, ApprovalPolicy.AllowAll),
      notifyLead = msg => notices.sendImmediately(msg)
    )
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await
    bridge

  /** A real workflow bridge on `repo`, for the generations that delegate to it. Every
    * settled run lands in `outcomes`, and the first one completes `settled` — the sync
    * point for a worker that wants to poll a run it has actually finished.
    *
    * `leadInbox` and `userNotices` are the host's two destinations for a settled run,
    * wired exactly as `Main` wires them: a run the LEAD started wakes it with a
    * completion notice, and a run somebody else started (a loop generation, here) only
    * ever reaches the user's notice area. */
  private def openWorkflow(
      name: String,
      repo: String,
      endpoint: Endpoint,
      outcomes: UnboundedChannel[(String, Either[String, String])],
      settled: Future.Promise[Unit],
      leadInbox: UnboundedChannel[String] = UnboundedChannel(),
      userNotices: UnboundedChannel[String] = UnboundedChannel()
  )(using Async.Spawn): WorkflowBridge =
    // The bridge is the caller of its own completion handler, so the handler reaches it
    // through this indirection — the same one `Main` uses.
    var ownerOf: String => Option[String] = _ => None
    val complete: (String, Either[String, String]) => Unit = (runId, outcome) =>
      outcomes.sendImmediately((runId, outcome))
      ownerOf(runId) match
        case None => leadInbox.sendImmediately(WorkflowBridge.completionNotice(runId, outcome))
        case Some(who) =>
          userNotices.sendImmediately(WorkflowBridge.delegatedCompletionNotice(who, runId, outcome))
      try settled.complete(Success(()))
      catch case _: Throwable => ()
    val bridge = WorkflowBridge(
      socketPath = tmpSock(s"wf-$name"),
      models = ModelSession.of(endpoint, LLMConfig(model = "test")),
      pool = ReplPool(() => ScalaRepl()),
      baseTools = _ => Nil,
      systemPrompt = "You are a workflow sub-agent.",
      context = RuntimeContext(repo, ApprovalPolicy.AllowAll),
      onEvent = _ => (),
      maxConcurrent = 2,
      onComplete = complete,
      retryDelaysMs = Nil
    )
    ownerOf = bridge.ownerOf
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await
    bridge

  /** One test's world: a repository with a live loop in it, and everything needed to
    * watch what the driver does to it. */
  private class World(
      val repo: String,
      val bridge: LoopBridge,
      val client: WireClient,
      val notices: UnboundedChannel[String],
      gateRef: () => Option[ScalaRepl],
      envsRef: () => List[Map[String, String]],
      viewsRef: () => List[Vector[LoopView]],
      activityRef: () => List[(String, TranscriptEvent)]
  ):
    def gate: Option[ScalaRepl] = gateRef()

    /** Every snapshot the bridge pushed to the TUI, in order. */
    def views: List[Vector[LoopView]] = viewsRef()

    /** Everything `loopId` was ever seen doing, consecutive duplicates collapsed: the
      * stages the panel would have drawn, in the order it would have drawn them. */
    def stages(loopId: String): List[Option[String]] =
      views.flatMap(_.find(_.id == loopId)).map(_.activity).foldLeft(List.empty[Option[String]]): (acc, a) =>
        if acc.lastOption.contains(a) then acc else acc :+ a

    /** Every transcript event the bridge streamed to the TUI, in order. */
    def activity: List[(String, TranscriptEvent)] = activityRef()

    /** Every environment `makeRepl` was called with, in spawn order: one per loop
      * worker this session created, whatever its role. What tells the three roles
      * apart is what is IN them, which is the point. */
    def envs: List[Map[String, String]] = envsRef()
    def store: LoopStore = LoopStore(PathOps.join(repo, LoopStore.AukRelativePath))
    def events(loopId: String): Vector[LoopEvent] =
      store.readAll(loopId) match
        case Right(evs)  => evs
        case Left(error) => fail(s"could not read loop '$loopId': $error")
    def kinds(loopId: String): List[String] = events(loopId).map(_.kind).toList
    def knowledge(loopId: String): String = store.readKnowledge(loopId).getOrElse("")
    def file(name: String): String = TestFs.read(PathOps.join(repo, name))
    def resolve(id: String): Either[?, String] = Snapshot.resolve(repo, id)

  /** Build a loop and let the driver loose on it. Blocks until the loop exists (or its
    * definition is refused); the generations that follow run on the bridge's own fiber. */
  private def startLoop(
      name: String,
      loopId: String,
      endpoint: Endpoint,
      checker: String = ImprovesChecker,
      maxGenerations: Int = 10,
      patience: Int = 2,
      maxAttempts: Int = 2,
      sessionId: Option[String] = None,
      extraTools: ScalaRepl => List[Tool] = _ => Nil,
      workerEnv: Map[String, String] = Map.empty,
      retireTeamOwned: Async ?=> String => List[String] = _ => Nil
  )(using Async.Spawn): World =
    val world = openSession(name, tempRepo(), endpoint, sessionId, extraTools, workerEnv, retireTeamOwned)
    world.client.hello(loopId, maxGenerations, patience, maxAttempts)
    world.bridge.announceDef(
      loopId,
      definition(loopId, s"maxGenerations = $maxGenerations, patience = $patience, maxAttemptsPerGeneration = $maxAttempts", checker))
    world

  /** A session on `repo` with no loop of its own: a bridge, its socket, and a client.
    * The second half of every cross-session test — the ledger is already there, and
    * this is the session that has to make sense of it. */
  private def openSession(
      name: String,
      repo: String,
      endpoint: Endpoint,
      sessionId: Option[String] = None,
      extraTools: ScalaRepl => List[Tool] = _ => Nil,
      workerEnv: Map[String, String] = Map.empty,
      retireTeamOwned: Async ?=> String => List[String] = _ => Nil
  )(using Async.Spawn): World =
    val notices = UnboundedChannel[String]()
    var gate: Option[ScalaRepl] = None
    val envs = scala.collection.mutable.ListBuffer.empty[Map[String, String]]
    // The TUI's two feeds, recorded rather than rendered: what the loops window would
    // have shown, and the transcript deltas it would have streamed.
    val views = scala.collection.mutable.ListBuffer.empty[Vector[LoopView]]
    val activity = scala.collection.mutable.ListBuffer.empty[(String, TranscriptEvent)]
    val bridge = LoopBridge(
      socketPath = tmpSock(name),
      models = ModelSession.of(endpoint, LLMConfig(model = "test")),
      makeRepl = env =>
        envs += env
        val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env ++ env)))
        // The gate is the one worker identified by its environment; the attach tag is
        // what names it. Worker and evaluator REPLs are never USED unless a test gives
        // them `eval_scala`, so most tests pay for nothing but the object.
        if env.contains("AUK_LOOP_ATTACH") then gate = Some(repl)
        repl
      ,
      baseTools = repl => List(new WriteFile(repo), new DeletePath(repo)) ++ extraTools(repl),
      workerSystemPrompt = "You are a loop agent.",
      context = RuntimeContext(repo, ApprovalPolicy.AllowAll),
      notifyLead = msg => notices.sendImmediately(msg),
      onLoop = snapshot => { views += snapshot; () },
      onActivity = (loopId, ev) => { activity += ((loopId, ev)); () },
      workerEnv = workerEnv,
      retireTeamOwned = retireTeamOwned,
      sessionRef = sessionId.map(SessionRef.apply),
      retryDelaysMs = Nil
    )
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await
    World(
      repo,
      bridge,
      WireClient(bridge.socketPath),
      notices,
      () => gate,
      () => envs.toList,
      () => views.toList,
      () => activity.toList
    )

  private def shutdown(world: World)(using Async): Unit =
    world.client.close()
    Async.fromSync(world.bridge.close())

  /** Wait for the loop's phase to satisfy `pred`, which is the one sync point every
    * test needs: the driver runs on its own fiber and reports where it is through the
    * phase. */
  private def awaitPhase(world: World, loopId: String, pred: String => Boolean, timeoutMs: Int = 120_000)(using
      Async
  ): String =
    var waited = 0
    def phase = world.bridge.statusOf(loopId).getOrElse("")
    while !pred(phase) && waited < timeoutMs do
      Interop.sleep(20.0)
      waited += 20
    assert(pred(phase), s"loop '$loopId' never reached the expected phase (it is '$phase')")
    phase

  private def awaitParked(world: World, loopId: String)(using Async): String =
    awaitPhase(world, loopId, _.startsWith(LoopBridge.ParkedPrefix))

  private def notice(world: World, contains: String)(using Async): String =
    readUntil(world.notices, contains)

  /** Wait until `loopId`'s ledger satisfies `pred`. The sync point for steering: a
    * reconfigure travels over the socket on a fiber of its own, and "it has landed" is
    * the ledger saying so. */
  private def awaitLedger(world: World, loopId: String, pred: Vector[LoopEvent] => Boolean, timeoutMs: Int = 30_000)(
      using Async
  ): Unit =
    var waited = 0
    while !world.store.readAll(loopId).exists(pred) && waited < timeoutMs do
      Interop.sleep(20.0)
      waited += 20
    assert(world.store.readAll(loopId).exists(pred), s"loop '$loopId' ledger never satisfied the expectation")

  /** Hand `world`'s bridge a redefinition of `loopId`, exactly as `eval_scala` does with
    * the source of an eval that called `lib.loop.amend`. Blocks: the amendment is
    * validated in a gate worker of its own before it is accepted or refused. */
  private def amend(
      world: World,
      loopId: String,
      body: String,
      budgets: String = "maxGenerations = 2, patience = 5, maxAttemptsPerGeneration = 1",
      goal: String = Goal,
      rubric: String = Rubric,
      artifact: String = Artifact
  )(using Async): Unit =
    world.bridge.announceAmend(
      loopId,
      definition(loopId, budgets, body, verb = "amend", goal = goal, rubric = rubric, artifact = artifact))

  /** Read one item from `ch`, failing with a readable reason rather than blocking
    * forever when whatever was supposed to produce it never did. */
  private def readSoon[A](ch: UnboundedChannel[A], what: String, timeoutMs: Int = 30_000)(using Async): A =
    var waited = 0
    var got: Option[A] = None
    while got.isEmpty && waited < timeoutMs do
      ch.readSource.poll() match
        case Some(Right(item)) => got = Some(item)
        case _ =>
          Interop.sleep(20.0)
          waited += 20
    got.getOrElse(fail(s"nothing arrived on $what within ${timeoutMs}ms"))

  /** Everything waiting on `ch` right now, in order. For the claims about what the lead
    * was NOT told, where reading until a match would skip the very line under suspicion. */
  private def drain[A](ch: UnboundedChannel[A]): List[A] =
    val out = List.newBuilder[A]
    var more = true
    while more do
      ch.readSource.poll() match
        case Some(Right(item)) => out += item
        case _                 => more = false
    out.result()

  /** Read `ch` until a line contains `text`, skipping whatever comes before it. */
  private def readUntil(ch: UnboundedChannel[String], text: String)(using Async): String =
    var found: String | Null = null
    while found == null do
      ch.read() match
        case Right(line) => if line.contains(text) then found = line
        case Left(_)     => fail(s"the channel closed before a line containing '$text'")
    found

  // -- the happy path -------------------------------------------------------------------

  test("two generations run end to end: each is checked, judged, accepted, and left in the tree"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // Generation 1 halves p99 and generation 2 halves it again; the evaluator accepts
      // both and calls the goal reached on the second, which stops the loop.
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          Reply.Submit(List(write("app.txt", "gen1\n")), generation(50, "halved the hot loop", Some("cache the table")))
        case Ask("worker", 2, _, _) =>
          Reply.Submit(List(write("app.txt", "gen2\n")), generation(25, "halved it again", Some("cache the table; unroll")))
        case Ask("eval", 1, _, _) => Reply.submit(verdict(true, "clear improvement"))
        case Ask("eval", 2, _, _) => Reply.submit(verdict(true, "target met", goalReached = true))
        case ask                  => fail(s"unscripted request: $ask")
      val world = startLoop("happy", "opt", endpoint, sessionId = Some("s1"))
      try
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")

        // The ledger is the two generations, each in full, then the park.
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )
        val accepted = world.events("opt").collect { case a: LoopEvent.GenerationAccepted => a }
        assertEquals(accepted.map(_.gen).toList, List(1, 2))
        // The checker's own measurements are what the lineage records.
        assertEquals(accepted.map(_.metrics).toList, List(Map("p99Ms" -> 50.0), Map("p99Ms" -> 25.0)))
        assertEquals(accepted.last.description, "halved it again")

        // Each accepted generation's snapshot is its ATTEMPT ref, and it resolves.
        assertEquals(accepted.map(_.snapshotId).toList, List("loop/opt/gen-1-a1", "loop/opt/gen-2-a1"))
        accepted.foreach(a => assertEquals(world.resolve(a.snapshotId), Right(a.commit)))

        // The working tree is what the last accepted generation left there.
        assertEquals(world.file("app.txt"), "gen2\n")
        // …and so is the loop's knowledge, replaced wholesale by the accepted attempt.
        assertEquals(world.knowledge("opt"), "cache the table; unroll")

        // The lead hears about each acceptance, with what was measured.
        val first = notice(world, "accepted generation 1")
        assert(first.contains("p99Ms=50"), first)
        val second = notice(world, "accepted generation 2")
        assert(second.contains("goal reached"), second)
        assert(notice(world, "parked").contains("the evaluator judged the goal reached"))

        // Each agent's transcript is teed under the session that ran it, one file per
        // generation and role, in the JSONL the dashboard already reads.
        def transcript(label: String): String =
          TestFs.read(SessionRef.loopLog(PathOps.join(world.repo, ".auk/sessions"), "s1", "opt", label))
        assert(transcript("gen-1-worker").contains("\"tool\":\"submit_generation\""), transcript("gen-1-worker"))
        assert(transcript("gen-1-eval").contains("\"tool\":\"submit_verdict\""), transcript("gen-1-eval"))
        assert(transcript("gen-2-worker").contains("\"nodeId\":\"gen-2-worker\""), transcript("gen-2-worker"))
      finally shutdown(world)

  // -- retries inside a generation --------------------------------------------------------

  test("a failed check retries on the same worker conversation, carrying the reasons"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val asks = scala.collection.mutable.ListBuffer.empty[Ask]
      val endpoint = ScriptedEndpoint: ask =>
        asks += ask
        ask match
          case Ask("worker", 1, 1, _) => Reply.Submit(List(write("app.txt", "slow\n")), generation(150, "first cut"))
          case Ask("worker", 1, 2, _) => Reply.Submit(List(write("app.txt", "fast\n")), generation(80, "under the bar"))
          case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "good enough"))
          case other                  => fail(s"unscripted request: $other")
      val world = startLoop("recheck", "opt", endpoint, maxGenerations = 1, maxAttempts = 2)
      try
        assertEquals(awaitParked(world, "opt"), "parked: budget exhausted")

        // Two attempts, the first checked and rejected, the second checked and judged.
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached", "generation_started",
            "attempt_submitted", "check_completed",
            "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )
        val checks = world.events("opt").collect { case c: LoopEvent.CheckCompleted => c }
        assertEquals(checks.map(c => (c.attempt, c.pass)).toList, List((1, false), (2, true)))
        assertEquals(checks.head.reasons, List("p99 must be under 100, got 150"))

        // The retry is the SAME conversation, and it carries the checker's own words.
        val retry = asks.filter(a => a.role == "worker" && a.attempt == 2).head
        assert(retry.text.contains("p99 must be under 100, got 150"), retry.text)
        assert(retry.text.contains("Attempt 1 of generation 1 was rejected"), retry.text)
        // The first attempt's prompt is still in the conversation the retry grew from.
        assert(retry.text.contains("Work generation 1 of loop 'opt'"), retry.text)

        // The accepted attempt keeps its ref; the one that lost is gone.
        assert(world.resolve("loop/opt/gen-1-a1").isLeft, "the losing attempt's ref should be deleted")
        assert(world.resolve("loop/opt/gen-1-a2").isRight, "the accepted attempt's ref should be kept")
        assertEquals(world.file("app.txt"), "fast\n")
      finally shutdown(world)

  test("a rejected verdict retries too, and the worker is shown the evaluator's feedback"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val asks = scala.collection.mutable.ListBuffer.empty[Ask]
      val endpoint = ScriptedEndpoint: ask =>
        asks += ask
        ask match
          case Ask("worker", 1, 1, _) => Reply.submit(generation(90, "shaved a little"))
          case Ask("worker", 1, 2, _) => Reply.submit(generation(40, "shaved a lot"))
          // Both attempts clear the checker, so the evaluator is the only thing that
          // can tell them apart — it keys on the artifact it is being shown.
          case Ask("eval", 1, _, text) if text.contains("90") =>
            Reply.submit(verdict(false, "90ms is barely a change; go further"))
          case Ask("eval", 1, _, _) => Reply.submit(verdict(true, "that is a real improvement"))
          case other                => fail(s"unscripted request: $other")
      val world = startLoop("verdict", "opt", endpoint, maxGenerations = 1, maxAttempts = 2)
      try
        assertEquals(awaitParked(world, "opt"), "parked: budget exhausted")
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached", "generation_started",
            "attempt_submitted", "check_completed", "verdict_issued",
            "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )
        val verdicts = world.events("opt").collect { case v: LoopEvent.VerdictIssued => v }
        assertEquals(verdicts.map(v => (v.attempt, v.accepted)).toList, List((1, false), (2, true)))
        val retry = asks.filter(a => a.role == "worker" && a.attempt == 2).head
        assert(retry.text.contains("90ms is barely a change; go further"), retry.text)
        assertEquals(world.events("opt").collect { case a: LoopEvent.GenerationAccepted => a }.head.metrics,
          Map("p99Ms" -> 40.0))
      finally shutdown(world)

  // -- giving up ---------------------------------------------------------------------------

  test("a generation out of attempts is rescued, rolled back and abandoned; two in a row park the loop"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // Every submission is over the checker's ceiling, so nothing can be accepted.
      val endpoint = ScriptedEndpoint:
        case Ask("worker", gen, attempt, _) =>
          Reply.Submit(List(write("app.txt", s"junk-$gen-$attempt\n")), generation(500, s"try $gen/$attempt"))
        case other => fail(s"unscripted request: $other")
      val world = startLoop("giveup", "opt", endpoint, maxGenerations = 10, patience = 2, maxAttempts = 2)
      try
        assertEquals(awaitParked(world, "opt"), "parked: patience exhausted")

        // Two generations, two attempts each, neither ever judged (the checker stopped
        // them), and no acceptance in between to reset the streak.
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached",
            "generation_started", "attempt_submitted", "check_completed", "attempt_submitted", "check_completed",
            "generation_abandoned",
            "generation_started", "attempt_submitted", "check_completed", "attempt_submitted", "check_completed",
            "generation_abandoned",
            "parked"
          )
        )
        val abandoned = world.events("opt").collect { case a: LoopEvent.GenerationAbandoned => a }
        assertEquals(abandoned.map(a => (a.gen, a.attempts)).toList, List((1, 2), (2, 2)))
        assertEquals(abandoned.map(_.rescueSnapshotId).toList,
          List(Some("loop/opt/gen-1-abandoned"), Some("loop/opt/gen-2-abandoned")))

        // The rescue snapshots resolve AND hold what was discarded — nothing is lost.
        abandoned.foreach: a =>
          val commit = world.resolve(a.rescueSnapshotId.get) match
            case Right(c) => c
            case Left(e)  => fail(s"rescue ref for gen ${a.gen} did not resolve: $e")
          assertEquals(git(world.repo, "show", s"$commit:app.txt"), s"junk-${a.gen}-2")
        // The attempt refs are gone; only the rescue is kept.
        List(1, 2).foreach: gen =>
          List(1, 2).foreach(k => assert(world.resolve(s"loop/opt/gen-$gen-a$k").isLeft, s"gen-$gen-a$k should be gone"))
        // …and the working tree is back to the baseline.
        assertEquals(world.file("app.txt"), "baseline\n")

        assert(notice(world, "abandoned generation 1").contains("after 2 attempts"))
        val parked = notice(world, "parked")
        assert(parked.contains("too many generations in a row were abandoned"), parked)
      finally shutdown(world)

  test("a failed generation is carried into the next one's prompt and into the loop's knowledge"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // Generation 1 is over the checker's ceiling with one attempt to spend, so it is
      // abandoned; generation 2 starts in a context that knows nothing about it except
      // what the host wrote down.
      val prompts = scala.collection.mutable.ListBuffer.empty[(Ask, String)]
      val endpoint = ScriptedEndpoint(
        {
          case Ask("worker", 1, _, _) =>
            Reply.Submit(List(write("app.txt", "doomed\n")), generation(500, "rewrote the tokenizer in one go"))
          case Ask("worker", 2, _, _) =>
            Reply.Submit(List(write("app.txt", "gen2\n")), generation(50, "shaved the hot loop instead"))
          case Ask("eval", 2, _, _) => Reply.submit(verdict(true, "clear improvement", goalReached = true))
          case other                => fail(s"unscripted request: $other")
        },
        (system, ask) => { prompts += ((ask, system)); () }
      )
      val world = startLoop("deadend", "opt", endpoint, maxGenerations = 3, patience = 2, maxAttempts = 1)
      try
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached",
            "generation_started", "attempt_submitted", "check_completed", "generation_abandoned",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )

        val failure = "- gen 1 (1 attempt): rewrote the tokenizer in one go — rejected: p99 must be under 100, got 500"

        // (a) The next generation's worker is told what already failed, in a prompt
        // section derived from the ledger rather than from anything a worker claimed.
        val genTwo = prompts.filter((ask, _) => ask.role == "worker" && ask.gen == 2).head._2
        assert(genTwo.contains("### Approaches that failed before"), genTwo)
        assert(genTwo.contains(failure), genTwo)
        assert(genTwo.contains("unless you can say what you will do differently this time"), genTwo)

        // (b) …and the host's own record of it is on disk, under its own heading, even
        // though no generation has ever been accepted to carry a worker's knowledge.
        assertEquals(world.knowledge("opt"), s"## Dead ends\n\n$failure\n")
        // Which is what the worker reads as the loop's knowledge too, so the two agree.
        assert(genTwo.contains("### What this loop has learned so far"), genTwo)

        // The abandonment itself is unchanged: the event carries no "why", and the
        // rescue snapshot still holds what was rolled back.
        val abandoned = world.events("opt").collect { case a: LoopEvent.GenerationAbandoned => a }
        assertEquals(abandoned.map(a => (a.gen, a.attempts)).toList, List((1, 1)))
        assert(world.resolve("loop/opt/gen-1-abandoned").isRight)
        assertEquals(world.file("app.txt"), "gen2\n")
      finally shutdown(world)

  test("a checker that throws rejects the candidate; the loop retries and carries on"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val thrower =
        """  if cand.artifact.p99Ms > 100 then throw new RuntimeException("the benchmark harness blew up")
          |  else CheckResult.pass.withMetrics("p99Ms" -> cand.artifact.p99Ms)""".stripMargin
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, 1, _) => Reply.submit(generation(150, "too slow"))
        case Ask("worker", 1, 2, _) => Reply.submit(generation(60, "fixed"))
        case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "fine"))
        case other                  => fail(s"unscripted request: $other")
      val world = startLoop("throws", "opt", endpoint, checker = thrower, maxGenerations = 1, maxAttempts = 2)
      try
        assertEquals(awaitParked(world, "opt"), "parked: budget exhausted")
        val checks = world.events("opt").collect { case c: LoopEvent.CheckCompleted => c }
        assertEquals(checks.map(_.pass).toList, List(false, true))
        // The exception is the rejection reason, not a loop-ending anomaly.
        assert(checks.head.reasons.exists(_.contains("the benchmark harness blew up")), checks.head.reasons.toString)
        assertEquals(world.events("opt").collect { case a: LoopEvent.GenerationAccepted => a }.map(_.gen).toList, List(1))
      finally shutdown(world)

  // -- stopping and picking back up ----------------------------------------------------------

  test("a park mid-generation lets that generation finish and then stops the loop"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val gate = Future.Promise[Unit]()
      val endpoint = ScriptedEndpoint:
        // Generation 1's worker is held until the test has parked the loop.
        case Ask("worker", 1, _, _) => Reply.Wait(gate.asFuture, Reply.submit(generation(70, "done anyway")))
        case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "keep it"))
        case other                  => fail(s"unscripted request: $other")
      val world = startLoop("midpark", "opt", endpoint, maxGenerations = 5)
      try
        awaitPhase(world, "opt", _ == LoopBridge.runningPhase(1))
        world.client.park("opt")
        assert(notice(world, "is parked").contains("opt"))

        // The generation in flight is allowed to finish, and it does — completely.
        gate.complete(Success(()))
        assert(notice(world, "accepted generation 1").nonEmpty)
        Interop.sleep(500.0) // …and then nothing else happens.
        assertEquals(world.bridge.statusOf("opt"), Some("parked: user requested"))
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached", "generation_started", "parked",
            "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted"
          )
        )
      finally shutdown(world)

  test("a provider outage parks the loop, settling the generation it interrupted; a resume carries on"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Submit(List(write("app.txt", "gen1\n")), generation(90, "first"))
        case Ask("worker", 2, _, _) => Reply.Outage
        case Ask("worker", 3, _, _) => Reply.Submit(List(write("app.txt", "gen3\n")), generation(40, "recovered"))
        case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "fine"))
        case Ask("eval", 3, _, _)   => Reply.submit(verdict(true, "done", goalReached = true))
        case other                  => fail(s"unscripted request: $other")
      val world = startLoop("outage", "opt", endpoint, maxGenerations = 5, patience = 2)
      try
        assert(notice(world, "accepted generation 1").nonEmpty)
        val parked = awaitParked(world, "opt")
        assert(parked.startsWith("parked: api failure: "), parked)
        assert(parked.contains("503 service unavailable"), parked)

        // The driver settles the generation on its way out rather than leaving it open:
        // it is the last thing that knows the tree belongs to a worker, so it abandons
        // it here and rolls the tree back to the generation the lineage stands on.
        awaitLedger(world, "opt", _.exists { case a: LoopEvent.GenerationAbandoned => a.gen == 2; case _ => false })
        assertEquals(world.file("app.txt"), "gen1\n")
        // A dead API is the user's problem, not the model's: the outage is in the phase
        // the panel draws, and the lead hears only about the generation — never about the
        // API, since it would only spend its own retries on the same dead one.
        val heard = drain(world.notices)
        assert(heard.forall(!_.contains("503")), heard.mkString("\n"))
        assert(heard.exists(_.contains("abandoned generation 2")), heard.mkString("\n"))

        // So a resume has nothing to rescue: it simply starts the next generation.
        world.client.resume("opt")
        assert(notice(world, "is running again").nonEmpty)
        assert(notice(world, "accepted generation 3").nonEmpty)
        assertEquals(awaitPhase(world, "opt", _ == "parked: goal reached"), "parked: goal reached")
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "generation_started", "parked", "generation_abandoned", "resumed",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )
        // The abandoned generation had submitted nothing, so it is recorded with none.
        val rescued = world.events("opt").collect { case a: LoopEvent.GenerationAbandoned => a }.head
        assertEquals((rescued.gen, rescued.attempts), (2, 0))
        assertEquals(world.file("app.txt"), "gen3\n")
      finally shutdown(world)

  // -- edits the loop did not make ------------------------------------------------------------

  test("edits made while a loop is parked are adopted as its base, not absorbed into the next generation"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // Every case the two evaluators were given, so the test can read the patch each one
      // was actually asked to judge.
      val cases = scala.collection.mutable.ListBuffer.empty[(Int, String)]
      // Generation 1's evaluator announces itself and then waits, which is what pins the
      // park to one place in the ledger: it lands while that generation is in flight, so
      // the generation finishes and the driver stops without opening a second one.
      val judging = Future.Promise[Unit]()
      val parked = Future.Promise[Unit]()
      val endpoint = ScriptedEndpoint(
        {
          case Ask("worker", 1, _, _) => Reply.Submit(List(write("app.txt", "gen1\n")), generation(90, "first"))
          case Ask("eval", 1, _, _)   => Reply.Wait(parked.asFuture, Reply.submit(verdict(true, "fine")))
          case Ask("worker", 2, _, _) => Reply.Submit(List(write("app.txt", "gen2\n")), generation(40, "second"))
          case Ask("eval", 2, _, _)   => Reply.submit(verdict(true, "done", goalReached = true))
          case other                  => fail(s"unscripted request: $other")
        },
        (_, ask) =>
          if ask.role == "eval" then
            cases += ((ask.gen, ask.text))
            if ask.gen == 1 then
              try judging.complete(Success(()))
              catch case _: Throwable => ()
      )
      val world = startLoop("adopt", "opt", endpoint, maxGenerations = 5)
      try
        judging.asFuture.await
        world.client.park("opt")
        assert(notice(world, "is parked").contains("opt"))
        parked.complete(Success(()))
        assert(notice(world, "accepted generation 1").nonEmpty)

        // A human works around the parked loop, in the same tree the loop drives.
        TestFs.write(PathOps.join(world.repo, "notes.txt"), "a hand-written note nobody generated\n")

        world.client.resume("opt")
        assert(notice(world, "is running again").nonEmpty)
        val adopted = notice(world, "adopted them as its new base")
        assert(adopted.contains("opt"), adopted)
        assertEquals(awaitPhase(world, "opt", _ == "parked: goal reached"), "parked: goal reached")

        // The adoption is written down between the resume and the generation that builds
        // on it: the loop took the edits in rather than starting a generation on top of
        // them without saying so.
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached",
            // The park landed while generation 1 was being judged; that generation was
            // still allowed to finish, which is what leaves the loop stopped with a tree
            // nobody is holding.
            "generation_started", "attempt_submitted", "check_completed", "parked",
            "verdict_issued", "generation_accepted",
            "resumed", "external_edits_adopted",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )
        // The edits survive everything that came after them, and what was found is kept
        // as a snapshot of its own.
        assertEquals(world.file("notes.txt"), "a hand-written note nobody generated\n")
        assertEquals(world.file("app.txt"), "gen2\n")
        val event = world.events("opt").collect { case a: LoopEvent.ExternalEditsAdopted => a }.head
        assertEquals(event.snapshotId, "loop/opt/adopted-1")
        assertEquals(world.resolve("loop/opt/adopted-1"), Right(event.commit))
        assertEquals(git(world.repo, "show", s"${event.commit}:notes.txt"), "a hand-written note nobody generated")

        // And the point of all of it: generation 2 is judged on what IT changed. The
        // note is in the tree it started from, so it is not in the patch — where
        // generation 1's own change was in its.
        val (_, firstCase) = cases.find(_._1 == 1).getOrElse(fail("generation 1 was never evaluated"))
        val (_, secondCase) = cases.find(_._1 == 2).getOrElse(fail("generation 2 was never evaluated"))
        assert(firstCase.contains("+gen1"), firstCase)
        assert(secondCase.contains("+gen2"), secondCase)
        assert(!secondCase.contains("hand-written"), secondCase)
      finally shutdown(world)

  // -- the driver's own failure modes ----------------------------------------------------------

  test("a check that cannot reach its checker parks the loop as an anomaly"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // The worker is held until the test has taken the gate session down. The next
      // check therefore runs in a session respawned from scratch, which holds no
      // registration for this loop — the same state a decoding failure leaves the host
      // in, and the one thing a check is never allowed to report as a rejection.
      val gate = Future.Promise[Unit]()
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Wait(gate.asFuture, Reply.submit(generation(50, "fine work")))
        case other                  => fail(s"unscripted request: $other")
      val world = startLoop("nogate", "opt", endpoint, maxGenerations = 5)
      try
        awaitPhase(world, "opt", _ == LoopBridge.runningPhase(1))
        world.gate.foreach(repl => Async.fromSync(repl.close()))
        gate.complete(Success(()))

        val parked = awaitParked(world, "opt")
        assert(parked.startsWith("parked: anomaly: "), parked)
        assert(parked.contains("has no checker registered in this session"), parked)
        // The attempt was recorded — it really happened — but nothing judged it, and the
        // generation is settled on the way out rather than left open for a later session
        // to find a tree it cannot account for.
        awaitLedger(world, "opt", _.exists(_.kind == "generation_abandoned"))
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached", "generation_started", "attempt_submitted", "parked",
            "generation_abandoned"
          )
        )
      finally shutdown(world)

  test("a rollback that would destroy ignored files refuses, and the loop parks naming them"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // The generation replaces the baseline's `app.txt` FILE with a directory holding
      // an ignored file. Rolling back would have to write the file over that directory,
      // destroying content no snapshot holds, so the reset declines.
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          Reply.Submit(List(remove("app.txt"), write("app.txt/keep.secret", "irreplaceable\n")), generation(500, "oops"))
        case other => fail(s"unscripted request: $other")
      val world = startLoop("clobber", "opt", endpoint, maxGenerations = 5, maxAttempts = 1)
      try
        val parked = awaitParked(world, "opt")
        assert(parked.startsWith("parked: anomaly: "), parked)
        assert(parked.contains("app.txt/keep.secret"), parked)
        assert(parked.contains("would have destroyed ignored files"), parked)
        // Nothing was touched: the ignored file is still there, and the generation was
        // never recorded as abandoned, so a resume will try the rescue again.
        assertEquals(world.file("app.txt/keep.secret"), "irreplaceable\n")
        assert(!world.kinds("opt").contains("generation_abandoned"))
        // The rescue snapshot was taken before any of this — snapshot first, always.
        assert(world.resolve("loop/opt/gen-1-abandoned").isRight)
      finally shutdown(world)

  test("an evaluator that edits the tree while judging does not become a co-author"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Submit(List(write("app.txt", "worker's work\n")), generation(60, "did the work"))
        case Ask("eval", 1, _, _) =>
          Reply.Submit(List(write("app.txt", "evaluator meddled\n"), write("stray.txt", "left behind\n")),
            verdict(true, "looks right"))
        case other => fail(s"unscripted request: $other")
      val world = startLoop("meddle", "opt", endpoint, maxGenerations = 1)
      try
        assertEquals(awaitParked(world, "opt"), "parked: budget exhausted")
        // The accepted commit is the worker's attempt, and so is the tree afterwards.
        assertEquals(world.file("app.txt"), "worker's work\n")
        assert(!TestFs.exists(PathOps.join(world.repo, "stray.txt")), "the evaluator's stray file should be gone")
        val accepted = world.events("opt").collect { case a: LoopEvent.GenerationAccepted => a }.head
        assertEquals(git(world.repo, "show", s"${accepted.commit}:app.txt"), "worker's work")
      finally shutdown(world)

  // -- the check call, for real ------------------------------------------------------------------

  test("a candidate's text reaches the checker verbatim, however it is punctuated"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // Everything that could close a Scala string literal, a JSON string, or an
      // interpolator, in one description that has to survive both hops.
      val nasty = "he said \"hi\" \\ back\nnewline\ttab é 中文 $notAnInterpolation ${x} \"\"\"triple\"\"\""
      val echo = """  CheckResult.fail("saw:" + cand.description)"""
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.submit(generation(10, nasty))
        case other                  => fail(s"unscripted request: $other")
      val world = startLoop("escapes", "opt", endpoint, checker = echo, maxGenerations = 5, patience = 1, maxAttempts = 1)
      try
        assertEquals(awaitParked(world, "opt"), "parked: patience exhausted")
        val check = world.events("opt").collect { case c: LoopEvent.CheckCompleted => c }.head
        assertEquals(check.pass, false)
        assertEquals(check.reasons, List("saw:" + nasty))
      finally shutdown(world)

  // -- steering: retuning a loop that is already going --------------------------------------------

  test("a reconfigured goal and rubric are what the next attempt's prompts are composed from"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // Every prompt the scripted model is ever shown, in order, keyed by who was asked.
      val prompts = scala.collection.mutable.ListBuffer.empty[(String, String)]
      val release = Future.Promise[Unit]()
      val endpoint = ScriptedEndpoint(
        {
          // The first attempt is held at the model, so the reconfigure demonstrably
          // lands mid-generation. It then submits something the checker rejects, which
          // is what makes a second attempt — and a second prompt — happen at all.
          case Ask("worker", 1, 1, _) => Reply.Wait(release.asFuture, Reply.submit(generation(150, "too slow")))
          case Ask("worker", 1, 2, _) => Reply.submit(generation(50, "fast enough"))
          case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "good"))
          case other                  => fail(s"unscripted request: $other")
        },
        (system, ask) => prompts += ((s"${ask.role}-${ask.gen}", system))
      )
      val world = startLoop("retune", "opt", endpoint, maxGenerations = 1, maxAttempts = 2)
      try
        awaitPhase(world, "opt", _ == LoopBridge.runningPhase(1))
        world.client.reconfigure(
          "opt",
          goal = Some("cut p99 latency AND allocation count"),
          rubric = Some("accepted when both fall")
        )
        awaitLedger(world, "opt", _.exists(_.kind == "config_amended"))
        release.complete(Success(()))
        assertEquals(awaitParked(world, "opt"), "parked: budget exhausted")

        // The amendment names only what it changed; the budgets it said nothing about
        // are left alone rather than replaced by defaults.
        val amended = world.events("opt").collect { case a: LoopEvent.ConfigAmended => a }
        assertEquals(amended.length, 1)
        assertEquals(amended.head.goal, Some("cut p99 latency AND allocation count"))
        assertEquals(amended.head.rubric, Some("accepted when both fall"))
        assertEquals(amended.head.budgets, None)
        // …and the loop still runs on the budgets it was started with.
        assertEquals(world.events("opt").collect { case d: LoopEvent.DefAttached => d }.head.budgets.maxAttemptsPerGeneration, 2)

        val workerPrompts = prompts.filter(_._1 == "worker-1").map(_._2).toList
        assertEquals(workerPrompts.length, 2)
        // The first attempt was composed before the amendment…
        assert(workerPrompts.head.contains("Goal: cut p99 latency\n"), workerPrompts.head)
        assert(!workerPrompts.head.contains("allocation count"), workerPrompts.head)
        // …and the retry, which is the next prompt this loop composed, carries the new
        // words. That is what makes reconfigure steering rather than paperwork.
        assert(workerPrompts(1).contains("Goal: cut p99 latency AND allocation count"), workerPrompts(1))
        assert(workerPrompts(1).contains("accepted when both fall"), workerPrompts(1))

        // The evaluator of that attempt judges by the same configuration the worker was
        // given: an attempt is composed once, from one reading of the ledger, so the
        // rubric a candidate is judged against is the one its author was shown.
        val evalPrompt = prompts.filter(_._1 == "eval-1").map(_._2).toList.head
        assert(evalPrompt.contains("accepted when both fall"), evalPrompt)
      finally shutdown(world)

  test("a patience lowered mid-generation parks the loop as soon as that generation ends"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val release = Future.Promise[Unit]()
      val endpoint = ScriptedEndpoint:
        // Over the checker's ceiling, so the generation cannot be saved.
        case Ask("worker", 1, _, _) => Reply.Wait(release.asFuture, Reply.submit(generation(500, "no good")))
        case Ask("worker", 2, _, _) => fail("the loop should have parked before generation 2")
        case other                  => fail(s"unscripted request: $other")
      // Patience 3 would tolerate two more failures after this one.
      val world = startLoop("tighten", "opt", endpoint, maxGenerations = 10, patience = 3, maxAttempts = 1)
      try
        awaitPhase(world, "opt", _ == LoopBridge.runningPhase(1))
        world.client.reconfigure("opt", budgets = Some((10, 1, 1)))
        awaitLedger(world, "opt", _.exists(_.kind == "config_amended"))
        release.complete(Success(()))

        // The driver reads the budgets off the ledger every time round, so the tightened
        // patience is spent by the very generation that was in flight when it changed.
        assertEquals(awaitParked(world, "opt"), "parked: patience exhausted")
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached", "generation_started", "config_amended",
            "attempt_submitted", "check_completed", "generation_abandoned", "parked"
          )
        )
        assert(notice(world, "parked").contains("too many generations in a row were abandoned"))
      finally shutdown(world)

  // -- steering: replacing the checker ---------------------------------------------------------

  private val Under100 =
    """  if cand.artifact.p99Ms < 100 then CheckResult.pass.withMetrics("p99Ms" -> cand.artifact.p99Ms)
      |  else CheckResult.fail("v1: p99 must be under 100")""".stripMargin

  private val Under50 =
    """  if cand.artifact.p99Ms < 50 then CheckResult.pass.withMetrics("p99Ms" -> cand.artifact.p99Ms)
      |  else CheckResult.fail("v2: p99 must be under 50")""".stripMargin

  test("an amended checker judges the next generation, while the one in flight keeps the definition it started under"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val release = Future.Promise[Unit]()
      val endpoint = ScriptedEndpoint:
        // 90 passes the definition this generation started under and would fail the one
        // that replaces it mid-flight — which is exactly what makes the sequencing
        // observable rather than a matter of trust.
        case Ask("worker", 1, _, _) => Reply.Wait(release.asFuture, Reply.submit(generation(90, "first")))
        case Ask("worker", 2, _, _) => Reply.submit(generation(80, "second"))
        case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "keep it"))
        case other                  => fail(s"unscripted request: $other")
      val world = startLoop("amend", "opt", endpoint, checker = Under100, maxGenerations = 2, patience = 5, maxAttempts = 1)
      try
        awaitPhase(world, "opt", _ == LoopBridge.runningPhase(1))
        amend(world, "opt", Under50)

        // The new definition is on the ledger the moment it validates: an amendment that
        // is accepted has to survive whatever happens to the loop next.
        val attached = world.events("opt").collect { case d: LoopEvent.DefAttached => d }
        assertEquals(attached.map(_.version).toList, List(1, 2))
        assert(attached(1).source.contains("v2: p99 must be under 50"), attached(1).source)
        assert(notice(world, "new definition (version 2)").contains("opt"))

        release.complete(Success(()))
        assertEquals(awaitParked(world, "opt"), "parked: budget exhausted")

        val checks = world.events("opt").collect { case c: LoopEvent.CheckCompleted => c }
        assertEquals(checks.map(c => (c.gen, c.pass)).toList, List((1, true), (2, false)))
        // Generation 1 was checked by v1 — 90 is under 100 and over 50 — even though v2
        // was attached while it was in flight.
        assertEquals(checks.head.reasons, Nil)
        // Generation 2 was checked by v2, in its own words.
        assertEquals(checks(1).reasons, List("v2: p99 must be under 50"))
        // The lineage is untouched by the amendment: generation 1 is still accepted.
        assertEquals(world.events("opt").collect { case a: LoopEvent.GenerationAccepted => a }.map(_.gen).toList, List(1))
      finally shutdown(world)

  test("an amendment whose artifact schema changed is refused, and the loop carries on with the definition it has"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val release = Future.Promise[Unit]()
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Wait(release.asFuture, Reply.submit(generation(90, "first")))
        case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "keep it"))
        case other                  => fail(s"unscripted request: $other")
      val world = startLoop("drift", "opt", endpoint, checker = Under100, maxGenerations = 1, maxAttempts = 1)
      try
        awaitPhase(world, "opt", _ == LoopBridge.runningPhase(1))
        // The definition compiles and binds perfectly well — it is only the SHAPE of
        // what a generation reports that has moved, which is the one thing a lineage
        // cannot absorb.
        amend(world, "opt", Under50, artifact = "case class Perf(p99Ms: Double, allocs: Double) derives LibToolInput")

        val refusal = notice(world, "was NOT amended")
        assert(refusal.contains("artifact type no longer matches"), refusal)
        assert(refusal.contains("allocs"), refusal) // the schema it derives now…
        assert(refusal.contains("""{"type":"number"}"""), refusal) // …and the one on the ledger

        // Nothing was attached, and the loop is still running on version 1.
        assertEquals(world.events("opt").collect { case d: LoopEvent.DefAttached => d }.map(_.version).toList, List(1))
        assertEquals(world.bridge.statusOf("opt"), Some(LoopBridge.runningPhase(1)))

        // …and it finishes the generation it was working on, checked by the checker it
        // has always had: 90 passes v1, and v2 never got anywhere near it.
        release.complete(Success(()))
        assertEquals(awaitParked(world, "opt"), "parked: budget exhausted")
        assertEquals(world.events("opt").collect { case a: LoopEvent.GenerationAccepted => a }.map(_.gen).toList, List(1))
      finally shutdown(world)

  test("an amendment to a parked loop is attached without starting any work"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.submit(generation(90, "only generation"))
        case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "fine"))
        case other                  => fail(s"unscripted request: $other")
      val world = startLoop("parkedamend", "opt", endpoint, checker = Under100, maxGenerations = 1, maxAttempts = 1)
      try
        assertEquals(awaitParked(world, "opt"), "parked: budget exhausted")
        val before = world.kinds("opt")

        amend(world, "opt", Under50, budgets = "maxGenerations = 5, patience = 5, maxAttemptsPerGeneration = 1")
        assert(notice(world, "new definition (version 2)").contains("opt"))

        // The definition is attached and the budget it carries would allow four more
        // generations — but a parked loop is parked, and only a resume changes that.
        assertEquals(world.kinds("opt"), before :+ "def_attached")
        assertEquals(world.bridge.statusOf("opt"), Some("parked: budget exhausted"))
        Interop.sleep(500.0)
        assertEquals(world.kinds("opt"), before :+ "def_attached")
      finally shutdown(world)

  // -- picking up a loop from another session ------------------------------------------------

  test("a loop left running by a session that ended is picked up: the gap is recorded, its generation rescued, and it goes on"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // The first session's worker never answers, so when that session goes away it
      // leaves a generation in flight and a ledger that still says "running".
      val stuck = Future.Promise[Unit]()
      val first = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Wait(stuck.asFuture, Reply.submit(generation(50, "never submitted")))
        case other                  => fail(s"unscripted request in the first session: $other")
      val a = startLoop("handover-a", "opt", first, maxGenerations = 3, sessionId = Some("s1"))
      awaitPhase(a, "opt", _ == LoopBridge.runningPhase(1))
      a.client.close()
      Async.fromSync(a.bridge.close())

      val second = ScriptedEndpoint:
        case Ask("worker", 2, _, _) => Reply.Submit(List(write("app.txt", "gen2\n")), generation(40, "picked up"))
        case Ask("eval", 2, _, _)   => Reply.submit(verdict(true, "done", goalReached = true))
        case other                  => fail(s"unscripted request in the second session: $other")
      val b = openSession("handover-b", a.repo, second, sessionId = Some("s2"))
      try
        // The new session has never heard of this loop; the ledger is the whole handover.
        assertEquals(b.bridge.statusOf("opt"), None)
        // Which is to say it does not hold it: at startup the loop is a stranger read
        // off disk, and the panel is told as much.
        val opening = b.views.headOption.getOrElse(fail("the bridge told the panel nothing at startup"))
        assertEquals(opening.find(_.id == "opt").map(_.held), Some(false))
        b.client.resume("opt")
        val adopted = notice(b, "picked up from an earlier session")
        assert(adopted.contains("Nothing has been accepted on it yet"), adopted)
        assert(adopted.contains("Its previous session ended while it was still running"), adopted)

        assertEquals(awaitPhase(b, "opt", _ == "parked: goal reached"), "parked: goal reached")
        assertEquals(
          b.kinds("opt"),
          List(
            "loop_created", "def_attached", "generation_started",
            // The stretch nobody was driving is written down as what it was, before the
            // resume that ends it…
            "parked", "resumed", "generation_abandoned",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )
        val gap = b.events("opt").collect { case p: LoopEvent.Parked => p }.head
        assertEquals(gap.reason, auk.loop.ParkReason.Anomaly(LoopBridge.DeadSessionDetail))
        // …and the session that picked it up owns it from there.
        assertEquals(b.events("opt").collect { case r: LoopEvent.Resumed => r }.head.sessionId, "s2")
        // The generation the dead session left in flight is rescued, not resumed: its
        // worker's conversation died with it.
        val rescued = b.events("opt").collect { case a: LoopEvent.GenerationAbandoned => a }.head
        assertEquals((rescued.gen, rescued.attempts), (1, 0))
        assertEquals(b.file("app.txt"), "gen2\n")
        // Adopting it made it this session's loop, and parking it does not hand it back:
        // the loop it drove to a stop stays on its screen.
        val settled = b.views.last.find(_.id == "opt").getOrElse(fail("the panel lost the loop"))
        assertEquals((settled.held, settled.parked), (true, Some("goal reached")))
      finally shutdown(b)

  test("a generation left by a dead session is settled without destroying edits the loop cannot account for"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val stuck = Future.Promise[Unit]()
      val first = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Wait(stuck.asFuture, Reply.submit(generation(50, "never submitted")))
        case other                  => fail(s"unscripted request in the first session: $other")
      val a = startLoop("handover-edits-a", "opt", first, maxGenerations = 3, sessionId = Some("s1"))
      awaitPhase(a, "opt", _ == LoopBridge.runningPhase(1))
      a.client.close()
      Async.fromSync(a.bridge.close())

      // Then somebody edits the tree the dead session left behind. Nothing records
      // whether that generation's worker or a human wrote what is there now, and a
      // rescue that rolled it back would be betting the human's work on the answer.
      TestFs.write(PathOps.join(a.repo, "notes.txt"), "a hand-written note nobody generated\n")

      val second = ScriptedEndpoint:
        case Ask("worker", 2, _, _) => Reply.Submit(List(write("app.txt", "gen2\n")), generation(40, "picked up"))
        case Ask("eval", 2, _, _)   => Reply.submit(verdict(true, "done", goalReached = true))
        case other                  => fail(s"unscripted request in the second session: $other")
      val b = openSession("handover-edits-b", a.repo, second, sessionId = Some("s2"))
      try
        b.client.resume("opt")
        assertEquals(awaitPhase(b, "opt", _ == "parked: goal reached"), "parked: goal reached")
        // The generation is abandoned as it always was — and then the tree it left, which
        // this loop cannot account for, is adopted rather than rolled over.
        assertEquals(
          b.kinds("opt"),
          List(
            "loop_created", "def_attached", "generation_started",
            "parked", "resumed", "generation_abandoned", "external_edits_adopted",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )
        assertEquals(b.file("notes.txt"), "a hand-written note nobody generated\n")
        assertEquals(b.file("app.txt"), "gen2\n")
        assert(b.resolve("loop/opt/adopted-1").isRight)
        // The lead is told what the rescue did NOT do, so the edits still on disk are not
        // something it has to discover.
        val settled = notice(b, "abandoned generation 1")
        assert(settled.contains("left exactly as it was found"), settled)
      finally shutdown(b)

  test("a stored definition whose artifact type has changed is refused, and the loop is left where it was"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val stuck = Future.Promise[Unit]()
      val first = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Wait(stuck.asFuture, Reply.submit(generation(50, "never submitted")))
        case other                  => fail(s"unscripted request: $other")
      val a = startLoop("stale-a", "opt", first, maxGenerations = 3)
      awaitPhase(a, "opt", _ == LoopBridge.runningPhase(1))
      a.client.close()
      Async.fromSync(a.bridge.close())

      // Between the two sessions the code the definition names moves: the artifact case
      // class gains a field. The definition still compiles — that is what makes this
      // worth catching — but a generation reported under it would not have the shape
      // the lineage is built from.
      val ledgerPath = a.store.ledgerPath("opt")
      TestFs.write(
        ledgerPath,
        TestFs.read(ledgerPath).replace(
          "case class Perf(p99Ms: Double) derives LibToolInput",
          "case class Perf(p99Ms: Double, allocs: Double) derives LibToolInput"
        )
      )
      val before = a.kinds("opt")

      val b = openSession("stale-b", a.repo, ScriptedEndpoint(ask => fail(s"nothing should run: $ask")))
      try
        b.client.resume("opt")
        val refusal = notice(b, "could NOT be resumed")
        assert(refusal.contains("artifact type no longer matches"), refusal)
        assert(refusal.contains("allocs"), refusal)
        assert(refusal.contains("Start a new loop for the new shape"), refusal)

        // Nothing was written and nothing is driving it: the loop is exactly as the dead
        // session left it, which is the only safe place for it to be.
        assertEquals(b.kinds("opt"), before)
        assertEquals(b.bridge.statusOf("opt"), None)
      finally shutdown(b)

  test("a loop left running by a dead session can be parked where it lies, with no gate worker at all"):
    Async.fromSync:
      val repo = tempRepo()
      val world = openSession("parkdead", repo, ScriptedEndpoint(ask => fail(s"nothing should run: $ask")))
      try
        orphanLedger(world.store, "orphan")

        // A client that arrives after the fact is told the state of the world at once,
        // without waiting for something to change. Nothing here has changed — and
        // nothing ever would, since the loop's session is gone — so this snapshot is
        // the only way a new session could learn the loop is there at all.
        val arriving = WireClient(world.bridge.socketPath)
        try
          val snapshot = readUntil(arriving.incoming, "\"t\":\"status\"")
          assert(snapshot.contains("\"id\":\"orphan\""), snapshot)
          assert(snapshot.contains(s"\"phase\":\"${LoopBridge.Orphaned}\""), snapshot)
        finally arriving.close()

        world.client.park("orphan")
        assert(notice(world, "is parked").contains("orphan"))
        assertEquals(world.kinds("orphan"), List("loop_created", "def_attached", "generation_started", "parked"))
        assertEquals(
          world.events("orphan").collect { case p: LoopEvent.Parked => p }.head.reason,
          auk.loop.ParkReason.UserRequested
        )
        // Stopping a loop needs nothing compiled, so it was never adopted into this
        // session at all — it is simply a loop this project no longer has running.
        assertEquals(world.bridge.statusOf("orphan"), None)
      finally shutdown(world)

  test("a loop from an earlier session is not resumed while another one holds the active slot"):
    Async.fromSync:
      val repo = tempRepo()
      val world = openSession("crowded", repo, ScriptedEndpoint(ask => fail(s"nothing should run: $ask")))
      try
        orphanLedger(world.store, "alpha")
        orphanLedger(world.store, "beta", parked = true)
        val before = world.kinds("beta")

        // A loop this project records as running is a loop somebody may still pick up,
        // and two drivers on one working tree is the thing the single slot exists to
        // prevent — whichever session each of them belongs to.
        world.client.resume("beta")
        val refused = readUntil(world.client.incoming, "\"t\":\"error\"")
        assert(refused.contains("loop 'alpha' is recorded as running"), refused)
        assert(refused.contains("\"loopId\":\"beta\""), refused)
        assertEquals(world.kinds("beta"), before)

        // The same goes for a loop this session is in the middle of starting.
        world.client.park("alpha")
        assert(notice(world, "is parked").contains("alpha"))
        world.client.hello("pending", 5, 2, 2) // no definition follows, so it stays pending
        readUntil(world.client.incoming, "\"phase\":\"validating\"")
        world.client.resume("beta")
        val second = readUntil(world.client.incoming, "\"t\":\"error\"")
        assert(second.contains("loop 'pending' is already active"), second)
        assertEquals(world.kinds("beta"), before)
      finally shutdown(world)

  /** A ledger for a loop nobody in this session started: created, defined, one
    * generation opened — and then either parked or simply left there, which is what a
    * session that ended mid-generation leaves behind. */
  private def orphanLedger(store: LoopStore, loopId: String, parked: Boolean = false): Unit =
    val at = "2026-07-30T12:00:00Z"
    store.append(loopId, LoopEvent.LoopCreated(loopId, "base", "head", "gone", at))
    store.append(loopId, LoopEvent.DefAttached(1, "// not re-evaluated here", Goal, Rubric, auk.loop.Budgets(), Json.Null, at))
    store.append(loopId, LoopEvent.GenerationStarted(1, None, "gone", at))
    if parked then store.append(loopId, LoopEvent.Parked(auk.loop.ParkReason.UserRequested, at))
    ()

  // -- orchestrating from inside a generation ----------------------------------------------------

  test("only the generation worker is wired for orchestration; the gate and the evaluator are not"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Submit(List(write("app.txt", "gen1\n")), generation(50, "did it"))
        case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "good", goalReached = true))
        case ask                    => fail(s"unscripted request: $ask")
      val world = startLoop(
        "envs",
        "opt",
        endpoint,
        workerEnv = Map("AUK_WF_SOCK" -> "/tmp/wf.sock", "AUK_TEAM_SOCK" -> "/tmp/team.sock"))
      try
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")

        // One generation spawns exactly three workers, and each carries only what its
        // role needs. The GATE holds the checker: this bridge's socket and the loop it
        // is attached to, and nothing to delegate with.
        val gate = world.envs.filter(_.contains("AUK_LOOP_ATTACH"))
        assertEquals(gate, List(Map("AUK_LOOP_SOCK" -> world.bridge.socketPath, "AUK_LOOP_ATTACH" -> "opt")))
        // The WORKER is the one that may delegate: both orchestration sockets, the
        // owner its team members are stamped with, and — pointedly — no loop socket.
        val worker = world.envs.filter(_.contains("AUK_TEAM_OWNER"))
        assertEquals(
          worker,
          List(
            Map(
              "AUK_WF_SOCK" -> "/tmp/wf.sock",
              "AUK_TEAM_SOCK" -> "/tmp/team.sock",
              "AUK_TEAM_OWNER" -> "loop:opt:gen-1",
              "AUK_LOOP_WORKER" -> "generation 1 of loop 'opt'"
            )
          )
        )
        // The EVALUATOR judges; it delegates nothing and reaches no bridge at all.
        val evaluator = world.envs.filterNot(e => e.contains("AUK_LOOP_ATTACH") || e.contains("AUK_TEAM_OWNER"))
        assertEquals(evaluator, List(Map.empty[String, String]))
      finally shutdown(world)

  test("a generation that tries to start a loop of its own is told loops do not nest, and carries on"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val evals = scala.collection.mutable.ListBuffer.empty[String]
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          Reply.Submit(
            List(
              evalStep(
                """lib.loop.start[String](id = "nested", goal = "go faster", rubric = "any") { (prev, cand) =>
                  |  CheckResult.pass
                  |}""".stripMargin),
              write("app.txt", "gen1\n")
            ),
            generation(50, "did it the hard way"))
        case Ask("eval", 1, _, _) => Reply.submit(verdict(true, "good", goalReached = true))
        case ask                  => fail(s"unscripted request: $ask")
      val world = startLoop(
        "nested",
        "opt",
        endpoint,
        extraTools = repl => List(new RecordingEval(repl, evals, None)),
        workerEnv = Map("AUK_TEAM_SOCK" -> "/tmp/team.sock"))
      try
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")

        // The nested `lib.loop.start` failed, and said which rule it broke and where.
        val refusal = evals.head
        assert(refusal.contains("loops cannot be nested"), refusal)
        assert(refusal.contains("generation 1 of loop 'opt'"), refusal)
        assert(refusal.contains("lib.wf"), refusal)
        // Nothing was created: the ledger holds this loop and nothing else.
        assertEquals(world.store.list().sorted, List("opt"))
        // And a failed nested call does not poison the worker — it went on to submit,
        // and the generation was accepted as usual.
        assert(world.kinds("opt").contains("generation_accepted"), world.kinds("opt").toString)
      finally shutdown(world)

  test("team members a generation hires are retired when it is accepted; the lead's own are untouched"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          Reply.Submit(
            List(evalStep("""team.newMember("helper", "holds a thread of reasoning").id"""), write("app.txt", "gen1\n")),
            generation(50, "asked for help"))
        case Ask("eval", 1, _, _) => Reply.submit(verdict(true, "good", goalReached = true))
        case ask                  => fail(s"unscripted request: $ask")
      val evals = scala.collection.mutable.ListBuffer.empty[String]
      val repo = tempRepo()
      val team = openTeam("accept", repo, new FixedEndpoint("nothing to do"))
      val lead = TeamWire(team.socketPath)
      lead.hello()
      readUntil(lead.incoming, "\"t\":\"roster\"")
      lead.newMember("leadown", "the lead's own member")
      readUntil(lead.incoming, "\"id\":\"leadown\"")
      val world = openSession(
        "loop-team-accept",
        repo,
        endpoint,
        extraTools = repl => List(new RecordingEval(repl, evals, None)),
        workerEnv = Map("AUK_TEAM_SOCK" -> team.socketPath),
        retireTeamOwned = owner => team.retireOwnedBy(owner))
      try
        world.client.hello("opt", 1, 2, 1)
        world.bridge.announceDef(
          "opt",
          definition("opt", "maxGenerations = 1, patience = 2, maxAttemptsPerGeneration = 1", ImprovesChecker))
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")
        // The member really was created by the worker, from inside the generation.
        assert(evals.head.contains("helper"), evals.head)

        // The generation ended, so its member did too — and the roster is where that is
        // visible, owner tag and all. The lead's own member, which no generation hired,
        // is untouched on the same roster — one team, two fates.
        lead.hello()
        val line = readUntil(lead.incoming, "\"t\":\"roster\"")
        val roster = rosterOf(line)
        assertEquals(roster.find(_._1 == "helper"), Some(("helper", "retired", Some("loop:opt:gen-1"))), clue(line))
        assertEquals(roster.find(_._1 == "leadown"), Some(("leadown", "idle", None)), clue(line))
      finally
        lead.close()
        Async.fromSync(team.close())
        shutdown(world)

  test("team members a generation hires are retired when it is abandoned too"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // The single attempt's p99 is over the checker's ceiling, so the generation runs
      // out of attempts and is abandoned rather than accepted.
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          Reply.Submit(
            List(evalStep("""team.newMember("helper", "holds a thread of reasoning").id"""), write("app.txt", "gen1\n")),
            generation(150, "too slow"))
        case ask => fail(s"unscripted request: $ask")
      val evals = scala.collection.mutable.ListBuffer.empty[String]
      val repo = tempRepo()
      val team = openTeam("abandon", repo, new FixedEndpoint("nothing to do"))
      val lead = TeamWire(team.socketPath)
      lead.hello()
      readUntil(lead.incoming, "\"t\":\"roster\"")
      val world = openSession(
        "loop-team-abandon",
        repo,
        endpoint,
        extraTools = repl => List(new RecordingEval(repl, evals, None)),
        workerEnv = Map("AUK_TEAM_SOCK" -> team.socketPath),
        retireTeamOwned = owner => team.retireOwnedBy(owner))
      try
        world.client.hello("opt", 1, 2, 1)
        world.bridge.announceDef(
          "opt",
          definition("opt", "maxGenerations = 1, patience = 2, maxAttemptsPerGeneration = 1", ImprovesChecker))
        awaitParked(world, "opt")
        assert(evals.head.contains("helper"), evals.head)

        assert(world.kinds("opt").contains("generation_abandoned"), world.kinds("opt").toString)
        lead.hello()
        val line = readUntil(lead.incoming, "\"t\":\"roster\"")
        assertEquals(
          rosterOf(line).find(_._1 == "helper"),
          Some(("helper", "retired", Some("loop:opt:gen-1"))),
          clue(line))
      finally
        lead.close()
        Async.fromSync(team.close())
        shutdown(world)

  test("a generation can run a workflow: it settles inside the generation and its result is readable"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val evals = scala.collection.mutable.ListBuffer.empty[String]
      val outcomes = UnboundedChannel[(String, Either[String, String])]()
      val leadInbox = UnboundedChannel[String]()
      val userNotices = UnboundedChannel[String]()
      val settled = Future.Promise[Unit]()
      val repo = tempRepo()
      val wf = openWorkflow(
        "in-gen", repo, new FixedEndpoint("the sub-agent's answer"), outcomes, settled, leadInbox, userNotices)
      // Round 1 launches the run; every later round waits for it to settle first, so
      // the poll the worker makes in round 2 reads a run that is really finished (the
      // handle only advances while the worker is idle BETWEEN evals).
      var rounds = 0
      val steps = List(evalStep("""val run = wf.start { agent[String]("say hi", "n1") }"""), evalStep("run.status.toString"))
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          rounds += 1
          val reply = Reply.Submit(steps, generation(50, "delegated the reading"))
          if rounds == 1 then reply else Reply.Wait(settled.asFuture, reply)
        case Ask("eval", 1, _, _) => Reply.submit(verdict(true, "good", goalReached = true))
        case ask                  => fail(s"unscripted request: $ask")
      val world = openSession(
        "loop-wf-in-gen",
        repo,
        endpoint,
        extraTools = repl => List(new RecordingEval(repl, evals, Some(wf))),
        workerEnv = Map("AUK_WF_SOCK" -> wf.socketPath))
      try
        world.client.hello("opt", 1, 2, 1)
        world.bridge.announceDef(
          "opt",
          definition("opt", "maxGenerations = 1, patience = 2, maxAttemptsPerGeneration = 1", ImprovesChecker))
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")

        // The run really ran on the host, and it carries the sub-agent's answer.
        val (runId, outcome) = readSoon(outcomes, "the workflow's completion")
        assertEquals(outcome, Right("the sub-agent's answer"))
        // The worker saw it settle from inside the generation…
        assert(evals.last.contains("Done(true)"), evals.last)
        // …and the generation itself went through as usual.
        assert(world.kinds("opt").contains("generation_accepted"), world.kinds("opt").toString)
        assert(runId.nonEmpty, "the run is announced under its own id")

        // The lead never wrote this workflow, so it is never told about it: the run was
        // announced under the generation that started it, and the outcome went to the
        // user's notice area instead of the lead's inbox.
        val line = readSoon(userNotices, "the user's notice for the generation's run")
        assert(line.contains("generation 1 of loop 'opt'"), line)
        assert(line.contains("finished"), line)
        assert(leadInbox.readSource.poll().isEmpty, "the lead must not hear about a run it did not start")
      finally
        Async.fromSync(wf.close())
        shutdown(world)

  test("a run settling as its generation's eval returns is owned anyway: the announcement never gates it"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // The other ordering. Here the sub-agent finishes while the generation's eval is
      // STILL RUNNING, so the worker — busy until then — goes idle, drains the queued
      // result and sends `done` in the same breath as answering the eval: the host can
      // dispatch the completion before the tool's post-eval `announceCode` is made.
      //
      // Which is why this generation's eval tool is not wired to the workflow bridge at
      // all: no announcement is ever made for this run. That is what losing that race
      // amounts to, and it is the case an announcement-based ownership scheme gets
      // wrong. Announced on `hello`, the owner is already on record.
      val evals = scala.collection.mutable.ListBuffer.empty[String]
      val outcomes = UnboundedChannel[(String, Either[String, String])]()
      val leadInbox = UnboundedChannel[String]()
      val userNotices = UnboundedChannel[String]()
      val repo = tempRepo()
      val wf = openWorkflow(
        "race",
        repo,
        new FixedEndpoint("the sub-agent's answer"),
        outcomes,
        Future.Promise[Unit](),
        leadInbox,
        userNotices)
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          Reply.Submit(
            List(evalStep(
              """val run = wf.start { agent[String]("say hi", "n1") }
                |lib.shell.run("sleep", "2")""".stripMargin)),
            generation(50, "delegated the reading"))
        case Ask("eval", 1, _, _) => Reply.submit(verdict(true, "good", goalReached = true))
        case ask                  => fail(s"unscripted request: $ask")
      val world = openSession(
        "loop-wf-race",
        repo,
        endpoint,
        extraTools = repl => List(new RecordingEval(repl, evals, None)),
        workerEnv = Map("AUK_WF_SOCK" -> wf.socketPath))
      try
        world.client.hello("opt", 1, 2, 1)
        world.bridge.announceDef(
          "opt",
          definition("opt", "maxGenerations = 1, patience = 2, maxAttemptsPerGeneration = 1", ImprovesChecker))
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")

        // The run really did settle on its own (not as the generation's teardown).
        val (runId, outcome) = readSoon(outcomes, "the workflow's completion")
        assertEquals(outcome, Right("the sub-agent's answer"))
        assert(runId.nonEmpty, "the run settles under its own id")
        // …and it was attributed to the generation with no announcement to go on.
        val line = readSoon(userNotices, "the user's notice for the generation's run")
        assert(line.contains("generation 1 of loop 'opt'"), line)
        assert(line.contains("finished"), line)
        assert(leadInbox.readSource.poll().isEmpty, "the lead must not hear about a run it did not start")
      finally
        Async.fromSync(wf.close())
        shutdown(world)

  test("a workflow still running when its generation ends dies with the worker, settled as a disconnect"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val evals = scala.collection.mutable.ListBuffer.empty[String]
      val outcomes = UnboundedChannel[(String, Either[String, String])]()
      val leadInbox = UnboundedChannel[String]()
      val userNotices = UnboundedChannel[String]()
      // The sub-agent never answers, so the run is unmistakably in flight when the
      // generation settles and the worker that launched it is closed.
      val held = Future.Promise[Unit]()
      val repo = tempRepo()
      val wf = openWorkflow(
        "dropped",
        repo,
        new FixedEndpoint("never", Some(held.asFuture)),
        outcomes,
        Future.Promise[Unit](),
        leadInbox,
        userNotices)
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          Reply.Submit(
            List(evalStep("""val run = wf.start { agent[String]("say hi", "n1") }"""), write("app.txt", "gen1\n")),
            generation(50, "left something running"))
        case Ask("eval", 1, _, _) => Reply.submit(verdict(true, "good", goalReached = true))
        case ask                  => fail(s"unscripted request: $ask")
      val world = openSession(
        "loop-wf-dropped",
        repo,
        endpoint,
        extraTools = repl => List(new RecordingEval(repl, evals, Some(wf))),
        workerEnv = Map("AUK_WF_SOCK" -> wf.socketPath))
      try
        world.client.hello("opt", 1, 2, 1)
        world.bridge.announceDef(
          "opt",
          definition("opt", "maxGenerations = 1, patience = 2, maxAttemptsPerGeneration = 1", ImprovesChecker))
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")

        // The bridge settles it exactly once, as the dropped worker it was — no wedged
        // run, and nothing left active to shut down later.
        val (_, outcome) = readSoon(outcomes, "the dropped run's completion")
        assert(evals.head.contains("WorkflowRun"), evals.head)
        assertEquals(outcome, Left("workflow worker disconnected"))
        assert(outcomes.readSource.poll().isEmpty, "a dropped run settles once")

        // This is the failure the lead used to be woken by — a generation ending with a
        // run in flight, reported to it as "workflow worker disconnected" for a workflow
        // it never wrote. It is the user's line now, and the lead's inbox stays clean.
        val line = readSoon(userNotices, "the user's notice for the dropped run")
        assert(line.contains("generation 1 of loop 'opt'"), line)
        // The user's line is the only passive surface for this, so it carries the reason.
        assert(line.contains("failed: workflow worker disconnected"), line)
        assert(leadInbox.readSource.poll().isEmpty, "the lead must not hear about a run it did not start")
      finally
        held.complete(Success(()))
        Async.fromSync(wf.close())
        shutdown(world)

  // -- what the loops window is told -----------------------------------------------------------

  test("the loops window is pushed every stage of a generation, and the transcript to read at each"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) =>
          Reply.Submit(List(write("app.txt", "gen1\n")), generation(50, "halved the hot loop"))
        case Ask("eval", 1, _, _) => Reply.submit(verdict(true, "target met", goalReached = true))
        case ask                  => fail(s"unscripted request: $ask")
      val world = startLoop("panel", "opt", endpoint, sessionId = Some("s1"))
      try
        assertEquals(awaitParked(world, "opt"), "parked: goal reached")

        // The drive cycle is reported as it walks: nothing in flight while the loop is
        // being validated and the generation opened, then each of the three agents in
        // turn, then nothing again once the generation has settled.
        assertEquals(
          world.stages("opt"),
          List(
            None,
            Some("gen 1, attempt 1 — working"),
            Some("gen 1, attempt 1 — checking"),
            Some("gen 1, attempt 1 — evaluating"),
            None
          )
        )

        // Each stage names the transcript a reader should be on. A check streams
        // nothing — it runs as a closure in the gate worker — so it leaves the
        // worker's up while the work is measured.
        val labels = world.views
          .flatMap(_.find(_.id == "opt"))
          .map(_.liveLabel)
          .foldLeft(List.empty[Option[String]])((acc, l) => if acc.lastOption.contains(l) then acc else acc :+ l)
        assertEquals(labels, List(None, Some("gen-1-worker"), Some("gen-1-eval"), None))

        // The last snapshot is the loop as it came to rest: parked, with its lineage.
        val settled = world.views.last.find(_.id == "opt").getOrElse(fail("the panel lost the loop"))
        assertEquals(settled.parked, Some("goal reached"))
        assertEquals(settled.orphaned, false)
        assert(!settled.live, "a parked loop is not being driven")
        // Parking does not hand the loop back: this session drove it there, so every
        // snapshot it pushes — the parked one included — still claims it.
        assert(world.views.flatMap(_.find(_.id == "opt")).forall(_.held), "a loop this session drove is held throughout")
        assertEquals(settled.held, true)
        assertEquals(settled.goal, "cut p99 latency")
        assertEquals(settled.generations.map(_.state), Vector(LoopGenerationState.Accepted))
        assertEquals(settled.generations.head.metrics, Vector("p99Ms" -> 50.0))
        assertEquals(settled.headline.map(_.key), Some("p99Ms"))

        // The transcript feed carries the same events the JSONL keeps, keyed by loop
        // id and agent label — which is exactly where the panel's overlay looks.
        assert(world.activity.forall(_._1 == "opt"), "every event names the loop it came from")
        assert(world.activity.forall(_._2.runId == "opt"), "and carries it on the event too")
        assertEquals(world.activity.map(_._2.nodeId).distinct, List("gen-1-worker", "gen-1-eval"))
        val submitted = world.activity.collect:
          case (_, TranscriptEvent.ToolCalled(_, nodeId, _, tool, _)) => (nodeId, tool)
        assert(submitted.contains(("gen-1-worker", "submit_generation")), submitted.toString)
        assert(submitted.contains(("gen-1-eval", "submit_verdict")), submitted.toString)
      finally shutdown(world)

  test("a session opening on a project with loops on disk sees them before anything happens"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val endpoint = ScriptedEndpoint:
        case Ask("worker", 1, _, _) => Reply.Submit(List(write("app.txt", "gen1\n")), generation(50, "halved it"))
        case Ask("eval", 1, _, _)   => Reply.submit(verdict(true, "done", goalReached = true))
        case ask                    => fail(s"unscripted request: $ask")
      val first = startLoop("disk-a", "opt", endpoint, sessionId = Some("s1"))
      awaitParked(first, "opt")
      first.client.close()
      Async.fromSync(first.bridge.close())

      // A session that has never heard of this loop, on the same project.
      val second = openSession("disk-b", first.repo, endpoint, sessionId = Some("s2"))
      try
        assertEquals(second.bridge.statusOf("opt"), None)
        // The panel is told at startup, not at the first thing that changes — the
        // whole point of a ledger outliving its session is that the next one can see it.
        val opening = second.views.headOption.getOrElse(fail("the bridge told the panel nothing at startup"))
        val v = opening.find(_.id == "opt").getOrElse(fail(s"the loop on disk is missing: $opening"))
        assertEquals(v.parked, Some("goal reached"))
        assertEquals(v.generations.map(_.gen), Vector(1))
        assertEquals(v.goal, "cut p99 latency")
        // Read off disk, so there is no live agent and nothing to open a transcript on.
        assertEquals(v.activity, None)
        assertEquals(v.liveLabel, None)
        // Nor does this session hold it: it found the loop, it did not drive it, so the
        // same parked loop that stayed on the first session's activity line stays off
        // this one's until somebody picks it up.
        assertEquals(v.held, false)
      finally shutdown(second)

  // -- pure helpers ---------------------------------------------------------------------------

  test("the check call is one Scala expression whose payloads survive any punctuation"):
    // Backslash and quote are escaped; a control character becomes a \uXXXX escape, so
    // the generated source never holds a raw newline (which would not compile, and
    // would not survive the REPL's own JSON framing either).
    assertEquals(LoopBridge.scalaLiteral("plain"), "\"plain\"")
    assertEquals(LoopBridge.scalaLiteral("say \"hi\""), "\"say \\\"hi\\\"\"")
    assertEquals(LoopBridge.scalaLiteral("back\\slash"), "\"back\\\\slash\"")
    assertEquals(LoopBridge.scalaLiteral("a\nb"), "\"a\\u000ab\"")
    assertEquals(LoopBridge.scalaLiteral("a\tb\r\n"), "\"a\\u0009b\\u000d\\u000a\"")
    // Interpolator syntax is inert in a plain literal, and non-ASCII needs no help.
    assertEquals(LoopBridge.scalaLiteral("$x ${y} é中"), "\"$x ${y} é中\"")
    assert(!LoopBridge.scalaLiteral("a\nb").contains("\n"), "no raw newline may reach the generated source")

    val source = LoopBridge.checkSource("opt", Some("""{"gen":1}"""), """{"description":"a\"b"}""")
    assertEquals(
      source,
      """auk.library.LoopRegistry.runCheck("opt", "{\"gen\":1}", "{\"description\":\"a\\\"b\"}")"""
    )
    // The first generation has no predecessor, and that is a literal `null` argument.
    assert(LoopBridge.checkSource("opt", None, "{}").contains(", null, "))

  test("the checker's payloads carry the artifact, the lineage position and the commits"):
    val record = auk.loop.GenerationRecord(
      gen = 3,
      parent = Some(2),
      description = "made it faster",
      artifact = Json.Obj(List("p99Ms" -> Json.num(41.5))),
      snapshotId = "loop/opt/gen-3-a1",
      commit = "abc123",
      metrics = Map("p99Ms" -> 41.5, "accuracy" -> 0.97),
      at = "2026-07-30T00:00:00Z"
    )
    val prev = LoopBridge.prevPayload(record)
    assertEquals(
      prev,
      """{"artifact":{"p99Ms":41.5},"gen":3,"description":"made it faster","commit":"abc123","metrics":{"accuracy":0.97,"p99Ms":41.5}}"""
    )
    assertEquals(
      LoopBridge.candPayload(Json.Obj(List("p99Ms" -> Json.num(30))), "trying harder", List("def456")),
      """{"artifact":{"p99Ms":30},"description":"trying harder","commits":["def456"]}"""
    )

  test("a checker's verdict is read off its last marker line, and a broken loop is not a rejection"):
    val ok = LoopBridge.parseCheckMarker(
      "some checker chatter\nauk:loop:check:{\"passed\":true,\"reasons\":[],\"metrics\":{\"p99Ms\":41.5}}\n")
    assertEquals(ok, Right(LoopBridge.CheckReport(true, Nil, Map("p99Ms" -> 41.5))))
    assertEquals(
      LoopBridge.parseCheckMarker("auk:loop:check:{\"passed\":false,\"reasons\":[\"too slow\",\"leaks\"],\"metrics\":{}}"),
      Right(LoopBridge.CheckReport(false, List("too slow", "leaks"), Map.empty))
    )
    // A checker is free to print — including something that looks like a marker — so
    // the LAST one, written by the call itself, is the verdict.
    assertEquals(
      LoopBridge.parseCheckMarker("auk:loop:check:{\"passed\":true}\nauk:loop:check:{\"passed\":false}"),
      Right(LoopBridge.CheckReport(false, Nil, Map.empty))
    )
    // These are the loop being broken, not the candidate being bad.
    assertEquals(
      LoopBridge.parseCheckMarker("auk:loop:check:{\"error\":\"loop 'opt' has no checker registered\"}"),
      Left("loop 'opt' has no checker registered")
    )
    assert(LoopBridge.parseCheckMarker("nothing here").isLeft)
    assert(LoopBridge.parseCheckMarker("auk:loop:check:not json").isLeft)
    assert(LoopBridge.parseCheckMarker("auk:loop:check:[1,2]").isLeft)

  test("a worker's loop section carries the goal, its position, the lineage and the last rejection"):
    def record(gen: Int, description: String) =
      auk.loop.GenerationRecord(gen, None, description, Json.Null, s"s$gen", s"c$gen", Map("p99Ms" -> gen.toDouble), "t")
    val lineage = (1 to 7).map(g => record(g, s"generation $g did a thing")).toList
    val section = LoopBridge.workerSection(
      loopId = "opt",
      goal = "cut p99 latency",
      rubric = "accepted when p99 improves",
      gen = 8,
      maxGenerations = 20,
      attempt = 2,
      maxAttempts = 3,
      lineage = lineage,
      abandoned = Nil,
      adoptedEdits = false,
      knowledge = "the tokenizer is the bottleneck",
      feedback = Some("p99 did not improve")
    )
    assert(section.contains("cut p99 latency"), section)
    assert(section.contains("accepted when p99 improves"), section)
    assert(section.contains("generation 8 of at most 20, attempt 2 of 3"), section)
    // The digest names the most recent five and says how many it left out…
    assert(section.contains("the most recent 5; 2 earlier ones omitted"), section)
    assert(section.contains("- gen 7: generation 7 did a thing [p99Ms=7]"), section)
    assert(!section.contains("- gen 2:"), section)
    // …and the newest accepted generation's own words are reproduced whole, since that
    // one is a handoff written for this reader.
    assert(section.contains("### Handoff from generation 7"), section)
    assert(section.contains("the tokenizer is the bottleneck"), section)
    assert(section.contains("p99 did not improve"), section)
    // What a generation may delegate to, what that delegation outlives, and the one
    // thing it may not do.
    assert(section.contains("lib.wf.start"), section)
    assert(section.contains("team members you create are retired"), section)
    assert(section.contains("scout-gen-8"), section)
    assert(section.contains("loops do not nest"), section)

    // Nothing accepted yet reads as the baseline, with no handoff and no feedback.
    val first = LoopBridge.workerSection("opt", "goal", "rubric", 1, 20, 1, 3, Nil, Nil, false, "", None)
    assert(first.contains("you are the first generation, working from the loop's baseline"), first)
    assert(!first.contains("Handoff"), first)
    assert(!first.contains("previous attempt was rejected"), first)
    // Nothing has been abandoned either, so the worker is never told about failure.
    assert(!first.contains("Approaches that failed before"), first)
    // The tree is the lineage's own, so nothing is said about it either — a worker is
    // told about adopted edits only when there are some.
    assert(!first.contains("The tree you start from"), first)

  test("a worker whose tree holds adopted edits is told the lineage does not account for them"):
    val section = LoopBridge.workerSection("opt", "goal", "rubric", 3, 20, 1, 3, Nil, Nil, true, "", None)
    assert(section.contains("### The tree you start from"), section)
    assert(section.contains("changes made outside this loop"), section)

  test("a worker is shown the approaches that failed before, capped and framed"):
    def failed(gen: Int) =
      auk.loop.AbandonedDigest(gen, 2, Some(s"generation $gen rewrote the cache"), Some(s"p99 regressed by $gen"), "t")
    val section = LoopBridge.workerSection(
      loopId = "opt",
      goal = "cut p99 latency",
      rubric = "accepted when p99 improves",
      gen = 8,
      maxGenerations = 20,
      attempt = 1,
      maxAttempts = 3,
      lineage = List(auk.loop.GenerationRecord(1, None, "the one that worked", Json.Null, "s1", "c1", Map.empty, "t")),
      abandoned = (1 to 7).map(failed).toList,
      adoptedEdits = false,
      knowledge = "the tokenizer is the bottleneck",
      feedback = None
    )
    // Capped like the lineage is, with the same account of what was left out.
    assert(section.contains("### Approaches that failed before (the most recent 5; 2 earlier ones omitted)"), section)
    assert(section.contains("- gen 7 (2 attempts): generation 7 rewrote the cache — rejected: p99 regressed by 7"), section)
    assert(!section.contains("- gen 2 (2 attempts)"), section)
    // The framing is what makes the list actionable rather than discouraging.
    assert(section.contains("unless you can say what you will do differently this time"), section)
    // It sits between the handoff it contrasts with and the knowledge it is not.
    assert(section.indexOf("### Handoff from generation 1") < section.indexOf("### Approaches that failed"), section)
    assert(section.indexOf("### Approaches that failed") < section.indexOf("### What this loop has learned"), section)

  test("every shape of a failed generation gets an honest line rather than a blank"):
    def entry(attempts: Int, what: Option[String], why: Option[String]) =
      LoopBridge.abandonedEntry(auk.loop.AbandonedDigest(4, attempts, what, why, "t"))
    assertEquals(entry(2, Some("widened the cache"), Some("p99 regressed")),
      "gen 4 (2 attempts): widened the cache — rejected: p99 regressed")
    assertEquals(entry(1, Some("widened the cache"), None), "gen 4 (1 attempt): widened the cache")
    assertEquals(entry(1, None, Some("p99 regressed")), "gen 4 (1 attempt): (no account of what it tried) — rejected: p99 regressed")
    // The rescue case: its session died before it ever offered anything.
    assertEquals(entry(0, None, None), "gen 4 (no attempts): died with its session before submitting anything")
    // Both halves are one line each, however many the worker wrote.
    assert(entry(1, Some("a\nb\nc"), None).endsWith(": a b c"))

    // No failures at all is no section — a loop that has only succeeded is never told
    // about failure.
    assertEquals(LoopBridge.abandonedSection(Nil), None)

  test("the evaluator is given the report, the artifact, the account and a bounded patch"):
    val report = LoopBridge.CheckReport(true, Nil, Map("p99Ms" -> 41.5, "accuracy" -> 0.97))
    val huge = "+" * (LoopBridge.MaxDiffChars + 500)
    val prompt = LoopBridge.evaluatorCase(3, "p99 must improve", Nil, report,
      Json.Obj(List("p99Ms" -> Json.num(41.5))), "what I did", huge)
    assert(prompt.contains("Judge generation 3 against the rubric"), prompt.take(200))
    assert(prompt.contains("accuracy=0.97, p99Ms=41.5"), prompt.take(600))
    assert(prompt.contains("""{"p99Ms":41.5}"""))
    assert(prompt.contains("what I did"))
    assert(prompt.contains(s"[patch truncated at ${LoopBridge.MaxDiffChars} characters]"))
    assert(prompt.length < huge.length + 4000, "a huge patch must not drag the whole prompt with it")
    // An empty patch says so rather than showing an empty fence.
    assert(LoopBridge.evaluatorCase(1, "r", Nil, report, Json.Null, "d", "")
      .contains("the candidate changed nothing in the working tree"))

  test("the driver's notices name the loop, what happened, and how to pick it back up"):
    assertEquals(LoopBridge.runningPhase(4), "running (gen 4)")
    assertEquals(LoopBridge.attemptId("opt", 3, 2), "loop/opt/gen-3-a2")
    assertEquals(LoopBridge.abandonedId("opt", 3), "loop/opt/gen-3-abandoned")
    assertEquals(LoopBridge.adoptedId("opt", 2), "loop/opt/adopted-2")
    assertEquals(LoopBridge.metricsLine(Map("p99Ms" -> 41.5, "runs" -> 12.0)), "p99Ms=41.5, runs=12")

    val accepted = LoopBridge.acceptedNotice("opt", 2, Map("p99Ms" -> 41.5), goalReached = true)
    assert(accepted.contains("accepted generation 2"), accepted)
    assert(accepted.contains("p99Ms=41.5"), accepted)
    assert(accepted.contains("the goal reached"), accepted)

    val abandoned = LoopBridge.abandonedNotice("opt", 3, 2, Some("loop/opt/gen-3-abandoned"), "p99 did not improve")
    assert(abandoned.contains("abandoned generation 3 after 2 attempts"), abandoned)
    assert(abandoned.contains("back to the last accepted state"), abandoned)
    assert(abandoned.contains("loop/opt/gen-3-abandoned"), abandoned)
    assert(LoopBridge.abandonedNotice("opt", 1, 1, None, "").contains("after 1 attempt;"))
    // A rescue that could not prove the tree was the loop's own says what it did NOT do,
    // since somebody else's edits are what is still sitting there.
    val kept = LoopBridge.abandonedNotice("opt", 3, 0, None, "", rolledBack = false)
    assert(kept.contains("left exactly as it was found"), kept)

    val edits = LoopBridge.adoptedEditsNotice("opt", "0123456789abcdef0123")
    assert(edits.contains("edits in the tree that are not its own"), edits)
    assert(edits.contains("0123456789ab"), edits)

    val patience = LoopBridge.parkedForNotice("opt", auk.loop.ParkReason.PatienceExhausted)
    assert(patience.contains("too many generations in a row were abandoned"), patience)
    assert(patience.contains("""lib.loop.get("opt").resume()"""), patience)
    assert(LoopBridge.parkedForNotice("opt", auk.loop.ParkReason.ApiFailure("503")).contains("kept failing (503)"))
