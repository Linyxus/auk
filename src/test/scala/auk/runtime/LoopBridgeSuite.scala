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
        // A client is sent a snapshot the moment it connects, so the one worth waiting
        // for here is the one that names the loop this test just submitted.
        assert(awaitMatch(client.incoming, _.contains("\"t\":\"status\"")).contains("\"loops\":[]"))
        client.hello("first")
        // The first loop is accepted as pending and appears in the status snapshot.
        val status = awaitMatch(client.incoming, l => l.contains("\"t\":\"status\"") && l.contains("\"id\":\"first\""))
        assert(status.contains("\"phase\":\"validating\""), status)

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

  test("a pending loop whose defining eval never came back is replaced by the next attempt"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val repo = tempRepo()
      val notices = UnboundedChannel[String]()
      val chatter = UnboundedChannel[String]()
      val bridge = makeBridge("reclaim", repo, notices, chatter)
      start(bridge)
      val client = WireClient(bridge.socketPath)
      try
        // A `hello` reserves the single active slot, and the definition that would fill
        // it only arrives when the eval COMPLETES — so an eval that is killed leaves the
        // slot held by a loop that will never exist.
        client.hello("opt")
        assert(
          awaitMatch(client.incoming, l => l.contains("\"t\":\"status\"") && l.contains("\"id\":\"opt\""))
            .contains("\"phase\":\"validating\""))

        // The lead retrying `start` is what says the first attempt is not coming back.
        client.hello("opt")
        assert(awaitMatch(chatter, _.contains("starting it over")).contains("opt"))
        // Nothing is failed on the wire: the id belongs to the retry now, and the retry
        // is the loop the mirror should be showing.
        assertEquals(bridge.statusOf("opt"), Some(LoopBridge.Validating))

        // …and the retry really is the live one: its definition creates the loop.
        bridge.announceDef("opt", definition("opt"))
        assert(awaitMatch(notices, _.contains("Loop 'opt'")).contains("validated and is now running"))
        assertEquals(events(repo, "opt").map(_.kind).toList.take(2), List("loop_created", "def_attached"))

        // A loop that made it that far is a different matter: it exists, so a `hello`
        // for its id is refused rather than reclaimed.
        client.hello("opt")
        val refused = awaitMatch(client.incoming, _.contains("\"t\":\"error\""))
        assert(refused.contains("is already active"), refused)
        assertEquals(events(repo, "opt").count(_.kind == "loop_created"), 1)
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

  test("steering a loop from the lead's own session: reconfigure retunes it, amend redefines it"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val repo = tempRepo()
      val notices = UnboundedChannel[String]()
      val chatter = UnboundedChannel[String]()
      val bridge = makeBridge("steer", repo, notices, chatter)
      start(bridge)
      val repl = leadRepl(bridge.socketPath)
      try
        assert(!evalWithBridge(repl, bridge, definition("opt")).isError)
        awaitMatch(notices, _.contains("validated and is now running"))
        // Both operations read the mirror before submitting anything, and the mirror
        // only advances between evals — so this is the wait every steering call needs.
        var mirrored = ""
        var tries = 0
        while !mirrored.contains("running") && tries < 20 do
          mirrored = evalIn(repl, """lib.loop.get("opt").status""").output
          tries += 1
        assert(mirrored.contains("running"), mirrored)

        // A data-only amendment names only what it changes.
        val retuned = evalIn(repl, """lib.loop.reconfigure("opt", rubric = "accepted when p99 AND memory fall")""")
        assert(!retuned.isError, retuned.output)
        val retuneNotice = awaitMatch(notices, _.contains("was reconfigured"))
        assert(retuneNotice.contains("accepted when p99 AND memory fall"), retuneNotice)
        assert(!retuneNotice.contains("goal:"), retuneNotice)
        val amended = events(repo, "opt").collect { case a: LoopEvent.ConfigAmended => a }.head
        assertEquals(amended.rubric, Some("accepted when p99 AND memory fall"))
        assertEquals(amended.goal, None)
        assertEquals(amended.budgets, None)

        // A redefinition travels as its own source, exactly as a creation does — and its
        // marker is stripped from what the model sees, exactly as a creation's is.
        val amendSource =
          """case class Perf(p99Ms: Double) derives LibToolInput
            |lib.loop.amend[Perf](
            |  id = "opt",
            |  goal = "make the tokenizer fast without leaking",
            |  rubric = "accepted when p99 improves and memory does not",
            |  budgets = LoopBudgets(maxGenerations = 9, patience = 2, maxAttemptsPerGeneration = 2)
            |) { (prev, cand) =>
            |  if cand.artifact.p99Ms < 10 then CheckResult.pass else CheckResult.fail("v2 wants under 10")
            |}""".stripMargin
        val redefined = evalWithBridge(repl, bridge, amendSource)
        assert(!redefined.isError, redefined.output)
        assert(!redefined.output.contains("auk:loop:amend"), redefined.output)
        assert(!redefined.output.contains("auk:loop:start"), redefined.output)
        assert(awaitMatch(notices, _.contains("new definition")).contains("version 2"))

        val attached = events(repo, "opt").collect { case d: LoopEvent.DefAttached => d }
        assertEquals(attached.map(_.version).toList, List(1, 2))
        // The version records what the definition itself declares — the goal, rubric and
        // budgets in that source, not the ones the loop was created with.
        assertEquals(attached(1).goal, "make the tokenizer fast without leaking")
        assertEquals(attached(1).budgets, Budgets(maxGenerations = 9, patience = 2, maxAttemptsPerGeneration = 2))
        assertEquals(attached(1).source, amendSource)

        // Steering a loop that does not exist is refused where it is written, before
        // anything reaches the host — and the model is told which id it made up.
        val strayRetune = evalIn(repl, """lib.loop.reconfigure("ghost", goal = "go faster")""")
        assert(strayRetune.isError, strayRetune.output)
        assert(strayRetune.output.contains("unknown loop 'ghost'"), strayRetune.output)
        // Naming nothing to change is refused the same way: an empty amendment is a
        // mistake, not a no-op worth recording.
        val empty = evalIn(repl, """lib.loop.reconfigure("opt")""")
        assert(empty.isError, empty.output)
        assert(empty.output.contains("names nothing to change"), empty.output)

        val strayAmend = evalWithBridge(repl, bridge, amendSource.replace("\"opt\"", "\"ghost\""))
        assert(strayAmend.isError, strayAmend.output)
        // Nothing reached the host either way: no second loop, and no further events.
        assertEquals(storeIn(repo).list(), List("opt"))
        assertEquals(events(repo, "opt").count(_.kind == "config_amended"), 1)
        assertEquals(events(repo, "opt").count(_.kind == "def_attached"), 2)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  test("a session that has never seen a loop can still name it and pick it up"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val repo = tempRepo()
      // The session that starts the loop, parks it, and goes away.
      val firstNotices = UnboundedChannel[String]()
      val first = makeBridge("handover-1", repo, firstNotices)
      start(first)
      val firstRepl = leadRepl(first.socketPath)
      assert(!evalWithBridge(firstRepl, first, definition("cycle")).isError)
      awaitMatch(firstNotices, _.contains("validated and is now running"))
      var parkedYet = false
      var attempts = 0
      while !parkedYet && attempts < 20 do
        evalIn(firstRepl, """lib.loop.get("cycle").park()""")
        parkedYet = first.statusOf("cycle").exists(_.startsWith(LoopBridge.ParkedPrefix))
        attempts += 1
      assert(parkedYet, s"the loop should be parked (it is ${first.statusOf("cycle")})")
      Async.fromSync(firstRepl.close())
      Async.fromSync(first.close())

      // A new session, on the same project. It never saw the loop start, and nothing in
      // it will ever change unless somebody picks the loop up — so a lead that cannot
      // name it here can never name it at all.
      val notices = UnboundedChannel[String]()
      val chatter = UnboundedChannel[String]()
      val bridge = makeBridge("handover-2", repo, notices, chatter)
      start(bridge)
      val repl = leadRepl(bridge.socketPath)
      try
        // The VERY FIRST lib.loop call in this session: it opens the connection, so its
        // mirror is still empty however long it waits. Refusing here — the natural way
        // to catch a made-up id — would refuse the one call that cannot know better.
        val resumed = evalIn(repl, """lib.loop.get("cycle").resume()""")
        assert(!resumed.isError, resumed.output)
        assert(awaitMatch(chatter, _.contains("picking up 'cycle'")).nonEmpty)
        val adopted = awaitMatch(notices, _.contains("picked up from an earlier session"))
        assert(adopted.contains("cycle"), adopted)

        // …and it really is being driven again: the generation the first session left in
        // flight is rescued, which is this loop's whole patience, so it parks for that.
        // (The lead's own REPL is the only agent here; no model ever answers.)
        awaitMatch(chatter, _.contains("parked: patience exhausted"))
        val kinds = events(repo, "cycle").map(_.kind).toList
        assertEquals(
          kinds,
          List(
            "loop_created", "def_attached", "generation_started",
            "parked", "resumed", "generation_abandoned", "parked"
          )
        )

        // From the next eval on, the mirror knows what the project holds.
        var listing = ""
        var tries = 0
        while !listing.contains("cycle") && tries < 20 do
          listing = evalIn(repl, "lib.loop.list.toString").output
          tries += 1
        assert(listing.contains("cycle"), listing)
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
