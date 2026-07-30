package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.TestFs
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
  private class ScriptedEndpoint(script: Ask => Reply) extends Endpoint:
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
          act(script(Ask(role, gen, submitsSoFar + 1, userTexts.mkString("\n"))))
      ch.asReadable

    private def stop(text: String): ChatResponse =
      ChatResponse(Message(Role.Assistant, List(Content.Text(text))), FinishReason.Stop)

    private def call(tool: String, params: Json): ChatResponse =
      ChatResponse(Message(Role.Assistant, List(Content.ToolUse("c1", tool, params.render))), FinishReason.ToolUse)

  private val GenRegex = "generation (\\d+)".r

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

  private def write(path: String, content: String): (String, Json) =
    "write_file" -> Json.Obj(List("path" -> Json.Str(path), "content" -> Json.Str(content)))

  private def remove(path: String): (String, Json) =
    "delete_path" -> Json.Obj(List("path" -> Json.Str(path)))

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

  /** A self-contained loop definition whose checker is `body`, which sees `prev` and
    * `cand` exactly as a real one does. */
  private def definition(loopId: String, budgets: String, body: String): String =
    s"""case class Perf(p99Ms: Double) derives LibToolInput
       |lib.loop.start[Perf](
       |  id = "$loopId",
       |  goal = "cut p99 latency",
       |  rubric = "accepted when p99 improves and nothing else regresses",
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
    def close(): Unit = try { sock.end(); () } catch case _: Throwable => ()

  /** One test's world: a repository with a live loop in it, and everything needed to
    * watch what the driver does to it. */
  private class World(
      val repo: String,
      val bridge: LoopBridge,
      val client: WireClient,
      val notices: UnboundedChannel[String],
      val chatter: UnboundedChannel[String],
      gateRef: () => Option[ScalaRepl]
  ):
    def gate: Option[ScalaRepl] = gateRef()
    def events(loopId: String): Vector[LoopEvent] =
      LoopStore(PathOps.join(repo, LoopStore.AukRelativePath)).readAll(loopId) match
        case Right(evs)  => evs
        case Left(error) => fail(s"could not read loop '$loopId': $error")
    def kinds(loopId: String): List[String] = events(loopId).map(_.kind).toList
    def knowledge(loopId: String): String =
      LoopStore(PathOps.join(repo, LoopStore.AukRelativePath)).readKnowledge(loopId).getOrElse("")
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
      sessionId: Option[String] = None
  )(using Async.Spawn): World =
    val repo = tempRepo()
    val notices = UnboundedChannel[String]()
    val chatter = UnboundedChannel[String]()
    var gate: Option[ScalaRepl] = None
    val bridge = LoopBridge(
      socketPath = tmpSock(name),
      models = ModelSession.of(endpoint, LLMConfig(model = "test")),
      makeRepl = env =>
        val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env ++ env)))
        // Only the gate is spawned with an environment; the worker and evaluator REPLs
        // are never used here (their tools are the scripted ones), so they cost nothing.
        if env.nonEmpty then gate = Some(repl)
        repl
      ,
      baseTools = _ => List(new WriteFile(repo), new DeletePath(repo)),
      workerSystemPrompt = "You are a loop agent.",
      context = RuntimeContext(repo, ApprovalPolicy.AllowAll),
      notifyLead = msg => notices.sendImmediately(msg),
      onNotice = msg => chatter.sendImmediately(msg),
      sessionRef = sessionId.map(SessionRef.apply),
      retryDelaysMs = Nil
    )
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await
    val client = WireClient(bridge.socketPath)
    client.hello(loopId, maxGenerations, patience, maxAttempts)
    bridge.announceDef(loopId, definition(loopId, s"maxGenerations = $maxGenerations, patience = $patience, maxAttemptsPerGeneration = $maxAttempts", checker))
    World(repo, bridge, client, notices, chatter, () => gate)

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

  test("a provider outage parks the loop without telling the lead; a resume rescues the generation and carries on"):
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
        // A dead API is the user's problem, not the model's: the chatter says so and
        // the lead is told nothing (it would only spend its own retries on the outage).
        assert(readUntil(world.chatter, "parked: api failure").contains("opt"))
        assertEquals(world.notices.readSource.poll(), None)

        // Resuming settles the generation the outage left in flight, then moves on.
        world.client.resume("opt")
        assert(notice(world, "is running again").nonEmpty)
        assert(notice(world, "accepted generation 3").nonEmpty)
        assertEquals(awaitPhase(world, "opt", _ == "parked: goal reached"), "parked: goal reached")
        assertEquals(
          world.kinds("opt"),
          List(
            "loop_created", "def_attached",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "generation_started", "parked", "resumed", "generation_abandoned",
            "generation_started", "attempt_submitted", "check_completed", "verdict_issued", "generation_accepted",
            "parked"
          )
        )
        // The rescued generation had submitted nothing, so it is abandoned with none.
        val rescued = world.events("opt").collect { case a: LoopEvent.GenerationAbandoned => a }.head
        assertEquals((rescued.gen, rescued.attempts), (2, 0))
        assertEquals(world.file("app.txt"), "gen3\n")
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
        // The attempt was recorded — it really happened — but nothing judged it.
        assertEquals(
          world.kinds("opt"),
          List("loop_created", "def_attached", "generation_started", "attempt_submitted", "parked")
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

    // Nothing accepted yet reads as the baseline, with no handoff and no feedback.
    val first = LoopBridge.workerSection("opt", "goal", "rubric", 1, 20, 1, 3, Nil, "", None)
    assert(first.contains("you are the first generation, working from the loop's baseline"), first)
    assert(!first.contains("Handoff"), first)
    assert(!first.contains("previous attempt was rejected"), first)

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

    val patience = LoopBridge.parkedForNotice("opt", auk.loop.ParkReason.PatienceExhausted)
    assert(patience.contains("too many generations in a row were abandoned"), patience)
    assert(patience.contains("""lib.loop.get("opt").resume()"""), patience)
    assert(LoopBridge.parkedForNotice("opt", auk.loop.ParkReason.ApiFailure("503")).contains("kept failing (503)"))
