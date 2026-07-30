package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.TestFs
import auk.llm.endpoint.{ChatResponse, Endpoint, LLMConfig, LLMError, Message, StreamEvent}
import auk.llm.provider.ModelSession
import auk.llm.tools.{ApprovalPolicy, Json, RuntimeContext}
import auk.loop.{Budgets, LoopEvent, LoopStore, ParkReason}
import auk.platform.PathOps
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl
import auk.snapshot.Snapshot
import auk.utils.Result

/** The [[LoopBridge]] end to end, against a throwaway git repository: a real REPL
  * worker runs the actual `lib.loop` DSL, the bridge captures the eval's source,
  * validates it by re-evaluating it in a gate worker, and writes the loop's ledger.
  *
  * No LLM is involved anywhere here — this phase of the bridge only compiles a
  * definition and appends events — so nothing needs a scripted endpoint or a retry
  * schedule. The refusal path is driven over the raw wire, which needs no worker at all.
  */
class LoopBridgeSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 240.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight

  // -- fixtures ------------------------------------------------------------------

  private def git(dir: String, args: String*): String =
    val cp = js.Dynamic.global.require("node:child_process")
    val opts = js.Dynamic.literal(cwd = dir, encoding = "utf8")
    cp.execFileSync("git", js.Array(args.map(a => a: js.Any)*), opts).asInstanceOf[String].trim

  /** A fresh repository with one commit — the tree a loop's baseline is taken from. */
  private def tempRepo(): String =
    val dir = TestFs.tempDir("auk-loop-bridge")
    git(dir, "init", "-b", "main")
    TestFs.write(PathOps.join(dir, "app.txt"), "one\n")
    git(dir, "add", "-A")
    git(dir, "-c", "user.name=Test", "-c", "user.email=t@example.com", "commit", "-m", "first")
    dir

  private def tmpSock(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-loopbridge-$name-${js.Dynamic.global.process.pid}.sock").asInstanceOf[String]

  private def makeBridge(
      name: String,
      repo: String,
      notices: UnboundedChannel[String],
      chatter: UnboundedChannel[String] = UnboundedChannel[String]()
  ): LoopBridge =
    LoopBridge(
      socketPath = tmpSock(name),
      // A live loop starts driving at once, so these tests need a model — but this
      // suite is about the loop EXISTING, not about generations, so the model never
      // answers and generation 1 stays in flight for as long as the test needs it.
      // The engine itself is exercised in [[LoopEngineSuite]].
      models = ModelSession.of(new HangingEndpoint, LLMConfig(model = "test")),
      makeRepl = env => ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env ++ env))),
      baseTools = _ => Nil,
      workerSystemPrompt = "You are a loop worker.",
      context = RuntimeContext(repo, ApprovalPolicy.AllowAll),
      notifyLead = msg => notices.sendImmediately(msg),
      onNotice = msg => chatter.sendImmediately(msg),
      retryDelaysMs = Nil
    )

  /** An endpoint whose every request never answers, so the worker of generation 1
    * blocks and the generation stays in flight until the test's scope tears it down. */
  private class HangingEndpoint extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      // Nothing is ever written to it, so the round blocks until the scope is torn down.
      UnboundedChannel[Result[StreamEvent, LLMError]]().asReadable

  private def start(bridge: LoopBridge)(using Async.Spawn): Unit =
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await

  private def leadRepl(sock: String): ScalaRepl =
    ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_LOOP_SOCK" -> sock))))

  private def evalIn(repl: ScalaRepl, code: String)(using Async) =
    given RuntimeContext = RuntimeContext(auk.platform.Platform.cwd(), ApprovalPolicy.AllowAll)
    EvalScala(repl, loopBridge = None).execute(EvalScalaParams(code, Some(120_000)))

  /** An eval routed through the tool wired to `bridge` — the production path, where the
    * completed eval's source is handed to the bridge. */
  private def evalWithBridge(repl: ScalaRepl, bridge: LoopBridge, code: String)(using Async) =
    given RuntimeContext = RuntimeContext(auk.platform.Platform.cwd(), ApprovalPolicy.AllowAll)
    EvalScala(repl, loopBridge = Some(bridge)).execute(EvalScalaParams(code, Some(120_000)))

  private def awaitMatch(ch: UnboundedChannel[String], pred: String => Boolean)(using Async): String =
    var found: String | Null = null
    while found == null do
      ch.read() match
        case Right(l) => if pred(l) then found = l
        case Left(_)  => fail("channel closed")
    found

  private def storeIn(repo: String): LoopStore =
    LoopStore(PathOps.join(repo, LoopStore.AukRelativePath))

  private def events(repo: String, loopId: String): Vector[LoopEvent] =
    storeIn(repo).readAll(loopId) match
      case Right(evs)  => evs
      case Left(error) => fail(s"could not read loop '$loopId': $error")

  /** A self-contained loop definition: everything the checker needs is in the eval. */
  private def definition(loopId: String): String =
    s"""case class Perf(p99Ms: Double) derives LibToolInput
       |def better(prev: Option[LoopGen[Perf]], cand: LoopCandidate[Perf]): Boolean =
       |  prev.forall(_.artifact.p99Ms > cand.artifact.p99Ms)
       |lib.loop.start[Perf](
       |  id = "$loopId",
       |  goal = "make the tokenizer fast",
       |  rubric = "accepted when p99 improves",
       |  budgets = LoopBudgets(maxGenerations = 7, patience = 1, maxAttemptsPerGeneration = 2)
       |) { (prev, cand) =>
       |  if better(prev, cand) then CheckResult.pass.withMetrics("p99Ms" -> cand.artifact.p99Ms)
       |  else CheckResult.fail("p99 did not improve")
       |}""".stripMargin

  // -- raw wire client -------------------------------------------------------------

  /** A raw UDS client: writes JSON lines and buffers received lines into a channel. */
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
    def hello(loopId: String, goal: String = "a goal", rubric: String = "a rubric"): Unit =
      line(
        js.Dynamic.literal(
          t = "hello",
          loopId = loopId,
          goal = goal,
          rubric = rubric,
          budgets = js.Dynamic.literal(maxGenerations = 5, patience = 1, maxAttemptsPerGeneration = 2),
          artifactSchema = """{"type":"object"}""",
          checkerRegistered = true
        )
      )
    def park(loopId: String): Unit = line(js.Dynamic.literal(t = "park", loopId = loopId))
    def close(): Unit = try { sock.end(); () } catch case _: Throwable => ()

  // -- creating a loop ---------------------------------------------------------------

  test("a loop's definition is captured from the eval, validated, and persisted with its baseline"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val repo = tempRepo()
      val head = git(repo, "rev-parse", "HEAD")
      val notices = UnboundedChannel[String]()
      val chatter = UnboundedChannel[String]()
      val bridge = makeBridge("create", repo, notices, chatter)
      start(bridge)
      val repl = leadRepl(bridge.socketPath)
      try
        val source = definition("opt")
        val result = evalWithBridge(repl, bridge, source)
        assert(!result.isError, result.output)
        // The eval's value is the handle; the marker line never reaches the model.
        assert(result.output.contains("Loop(opt"), result.output)
        assert(!result.output.contains("auk:loop:start"), result.output)

        // The lead is told the loop is live, and the user gets the progress chatter.
        val notice = awaitMatch(notices, _.contains("Loop 'opt'"))
        assert(notice.contains("validated and is now running"), notice)
        assert(awaitMatch(chatter, _.contains("validating the definition")).contains("opt"))

        // A live loop starts spending its budget on its own: wait for the driver to
        // open generation 1 before reading the ledger, so what follows is not a race.
        assert(awaitMatch(chatter, _.contains("started gen 1")).contains("opt"))
        assertEquals(bridge.statusOf("opt"), Some(LoopBridge.runningPhase(1)))

        // The ledger is the two creation events, then the first generation.
        val ledger = events(repo, "opt")
        assertEquals(ledger.map(_.kind).toList, List("loop_created", "def_attached", "generation_started"))
        val created = ledger.head.asInstanceOf[LoopEvent.LoopCreated]
        assertEquals(created.loopId, "opt")
        // The baseline is a real snapshot of the working tree, and HEAD is recorded
        // separately from it.
        assertEquals(Snapshot.resolve(repo, LoopBridge.baselineId("opt")), Right(created.baselineCommit))
        assertEquals(created.headAtCreation, head)
        assertNotEquals(created.baselineCommit, head)

        val attached = ledger(1).asInstanceOf[LoopEvent.DefAttached]
        assertEquals(attached.version, 1)
        assertEquals(attached.goal, "make the tokenizer fast")
        assertEquals(attached.rubric, "accepted when p99 improves")
        assertEquals(attached.budgets, Budgets(maxGenerations = 7, patience = 1, maxAttemptsPerGeneration = 2))
        // The WHOLE eval is the definition, helper included — that is what makes it
        // re-runnable on its own.
        assertEquals(attached.source, source)
        assert(attached.source.contains("def better"), attached.source)
        // The artifact schema arrived as JSON text and is stored as JSON.
        attached.artifactSchema match
          case o: Json.Obj =>
            assertEquals(o.get("type"), Some(Json.Str("object")))
            assert(o.get("properties").exists(_.render.contains("p99Ms")), o.render)
          case other => fail(s"expected an object schema, got ${other.render}")
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  test("a definition that leans on an earlier eval does not validate, and nothing is persisted"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val repo = tempRepo()
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("invalid", repo, notices)
      start(bridge)
      val repl = leadRepl(bridge.socketPath)
      try
        // The value the checker reaches for lives in a DIFFERENT eval, so the lead's
        // session compiles it and a fresh one cannot.
        val prepared = evalIn(repl, "val ceiling = 3.0")
        assert(!prepared.isError, prepared.output)
        val source =
          """case class Perf(p99Ms: Double) derives LibToolInput
            |lib.loop.start[Perf](id = "leaky", goal = "be fast", rubric = "faster") { (prev, cand) =>
            |  if cand.artifact.p99Ms < ceiling then CheckResult.pass else CheckResult.fail("too slow")
            |}""".stripMargin
        val result = evalWithBridge(repl, bridge, source)
        // The eval itself succeeded — this is precisely the trap the gate session catches.
        assert(!result.isError, result.output)

        val notice = awaitMatch(notices, _.contains("Loop 'leaky'"))
        assert(notice.contains("was NOT created"), notice)
        assert(notice.contains("ceiling"), notice) // the compiler's own complaint is quoted
        assert(notice.contains("self-contained"), notice)

        // Nothing was written: no ledger, no baseline ref, no pending loop.
        assertEquals(bridge.statusOf("leaky"), None)
        assertEquals(storeIn(repo).list(), Nil)
        assert(Snapshot.resolve(repo, LoopBridge.baselineId("leaky")).isLeft)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  test("a second loop is refused while one is active, and the lead is told why"):
    Async.fromSync:
      val repo = tempRepo()
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("single", repo, notices)
      start(bridge)
      val client = WireClient(bridge.socketPath)
      try
        client.hello("first")
        // The first loop is accepted as pending and appears in the status snapshot.
        val status = awaitMatch(client.incoming, _.contains("\"t\":\"status\""))
        assert(status.contains("\"id\":\"first\"") && status.contains("\"phase\":\"validating\""), status)

        client.hello("second")
        val err = awaitMatch(client.incoming, _.contains("\"t\":\"error\""))
        assert(err.contains("loop 'first' is already active"), err)
        assert(err.contains("\"loopId\":\"second\""), err)
        assertEquals(
          awaitMatch(notices, _.contains("rejected")),
          "[loop] rejected starting loop 'second': loop 'first' is already active; park it before starting another"
        )
        assertEquals(bridge.statusOf("second"), None)

        // An invalid id is refused the same way, before anything else is considered.
        client.hello("../escape")
        assert(awaitMatch(client.incoming, _.contains("\"t\":\"error\"")).contains("invalid loop id"))
      finally
        client.close()
        Async.fromSync(bridge.close())

  test("park and resume append to the ledger and reach the worker's mirror"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val repo = tempRepo()
      val notices = UnboundedChannel[String]()
      val chatter = UnboundedChannel[String]()
      val bridge = makeBridge("park", repo, notices, chatter)
      start(bridge)
      val repl = leadRepl(bridge.socketPath)
      try
        val created = evalWithBridge(repl, bridge, definition("cycle"))
        assert(!created.isError, created.output)
        awaitMatch(notices, _.contains("validated and is now running"))
        // Generation 1 is under way (and stays that way — its worker never answers),
        // so the events below land in a settled order.
        awaitMatch(chatter, _.contains("started gen 1"))

        // The status snapshot reaches the worker, so the DSL reports the live phase.
        def listing(): String =
          var out = ""
          var tries = 0
          while !out.contains("running") && tries < 20 do
            out = evalIn(repl, "lib.loop.list.toString").output
            tries += 1
          out
        assert(listing().contains("(cycle,running"), "the loop should be mirrored as running")

        val parked = evalIn(repl, """lib.loop.get("cycle").park()""")
        assert(!parked.isError, parked.output)
        val parkNotice = awaitMatch(notices, _.contains("is parked"))
        assert(parkNotice.contains("cycle"), parkNotice)
        assertEquals(bridge.statusOf("cycle"), Some("parked: user requested"))
        events(repo, "cycle").last match
          case LoopEvent.Parked(reason, at) =>
            assertEquals(reason, ParkReason.UserRequested)
            assert(at.nonEmpty, "a parked event carries the time it was stamped")
          case other => fail(s"expected a parked event, got $other")

        // The park reaches the mirror too, phrased for a reader.
        var phase = ""
        var tries = 0
        while !phase.contains("parked") && tries < 20 do
          phase = evalIn(repl, """lib.loop.get("cycle").status""").output
          tries += 1
        assert(phase.contains("parked: user requested"), phase)

        val resumed = evalIn(repl, """lib.loop.get("cycle").resume()""")
        assert(!resumed.isError, resumed.output)
        awaitMatch(notices, _.contains("is running again"))
        assertEquals(bridge.statusOf("cycle"), Some(LoopBridge.Running))
        // The park landed mid-generation and the generation was left to finish, so it
        // is still in flight; the resume put the loop back to running without starting
        // a second one.
        assertEquals(
          events(repo, "cycle").map(_.kind).toList,
          List("loop_created", "def_attached", "generation_started", "parked", "resumed")
        )

        // Resuming a running loop is refused rather than appended twice.
        val again = evalIn(repl, """lib.loop.get("cycle").resume()""")
        assert(!again.isError, again.output)
        var status = ""
        tries = 0
        while !status.contains("failed:") && tries < 20 do
          status = evalIn(repl, """lib.loop.get("cycle").status""").output
          tries += 1
        assert(status.contains("is not parked"), status)
        assertEquals(events(repo, "cycle").size, 5)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  // -- pure helpers -------------------------------------------------------------------

  test("a park reason reads as a phase, and the baseline id is namespaced under the loop"):
    assertEquals(LoopBridge.phaseFor(ParkReason.UserRequested), "parked: user requested")
    assertEquals(LoopBridge.phaseFor(ParkReason.GoalReached), "parked: goal reached")
    assertEquals(LoopBridge.phaseFor(ParkReason.PatienceExhausted), "parked: patience exhausted")
    assertEquals(LoopBridge.phaseFor(ParkReason.ApiFailure("429 for 20m")), "parked: api failure: 429 for 20m")
    assert(LoopBridge.phaseFor(ParkReason.Anomaly("no attempt")).startsWith(LoopBridge.ParkedPrefix))
    assertEquals(LoopBridge.baselineId("opt"), "loop/opt/baseline")

  test("the notices name the loop and, on failure, the constraint that explains it"):
    assert(LoopBridge.startedNotice("opt", "0123456789abcdef0").contains("0123456789ab"))
    assert(!LoopBridge.startedNotice("opt", "0123456789abcdef0").contains("0123456789abcdef0"))
    val failed = LoopBridge.validationFailedNotice("opt", "not found: value ceiling")
    assert(failed.contains("was NOT created"), failed)
    assert(failed.contains("not found: value ceiling"), failed)
    assert(failed.contains("self-contained"), failed)
    assert(LoopBridge.parkedNotice("opt").contains("""lib.loop.get("opt").resume()"""))
    assertEquals(LoopBridge.rejectionNotice("opt", "no"), "[loop] rejected starting loop 'opt': no")
