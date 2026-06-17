package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.workflow.{OrchestrationEvent, TranscriptEvent}
import auk.llm.provider.ModelSession
import auk.llm.endpoint.{Endpoint, LLMConfig, ChatResponse, Message, Content, Role, FinishReason, StreamEvent, LLMError}
import auk.llm.tools.{RuntimeContext, ApprovalPolicy, Json, ToolResult}
import auk.platform.Platform
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl
import auk.utils.Result

/** Phase-2 end-to-end: the real worker DSL talking to the real [[WorkflowBridge]],
  * with sub-agents driven by a scripted endpoint (no LLM). Each sub-agent submits
  * its result via the injected `submit_result` tool — deterministic under
  * concurrency because the reply is derived from the messages, not a counter.
  */
class WorkflowBridgeSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 90.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight

  /** Drives a sub-agent: first turn calls `submit_result` with `result(prompt)`;
    * once the tool result is threaded back, it finishes. */
  private class SubmitEndpoint(result: String => Json) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val prompt = messages.collectFirst { case Message(Role.User, c) =>
        c.collect { case Content.Text(t) => t }.mkString
      }.getOrElse("")
      val done = messages.exists(_.content.exists { case _: Content.ToolResult => true; case _ => false })
      val resp =
        if done then ChatResponse(Message(Role.Assistant, List(Content.Text("ok"))), FinishReason.Stop)
        else
          val args = Json.Obj(List("result" -> result(prompt))).render
          ChatResponse(Message(Role.Assistant, List(Content.ToolUse("s1", "submit_result", args))), FinishReason.ToolUse)
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(resp))))
      ch.asReadable

  /** Drives a sub-agent that ignores submit_result and answers in prose on its
    * first turn (FinishReason.Stop, no tool call). */
  private class ProseEndpoint(answer: String) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val resp = ChatResponse(Message(Role.Assistant, List(Content.Text(answer))), FinishReason.Stop)
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(resp))))
      ch.asReadable

  /** Like [[SubmitEndpoint]] but each sub-agent's first turn blocks on `gate`
    * before submitting, so several pile up at once and the concurrency cap can be
    * observed (queued vs running). */
  private class GatingEndpoint(gate: Future[Unit], result: String => Json) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val prompt = messages.collectFirst { case Message(Role.User, c) =>
        c.collect { case Content.Text(t) => t }.mkString
      }.getOrElse("")
      val done = messages.exists(_.content.exists { case _: Content.ToolResult => true; case _ => false })
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        if !done then gate.await // hold the concurrency slot until released
        val resp =
          if done then ChatResponse(Message(Role.Assistant, List(Content.Text("ok"))), FinishReason.Stop)
          else
            val args = Json.Obj(List("result" -> result(prompt))).render
            ChatResponse(Message(Role.Assistant, List(Content.ToolUse("s1", "submit_result", args))), FinishReason.ToolUse)
        ch.send(Right(StreamEvent.Done(resp)))
      ch.asReadable

  /** Streams an assistant text delta, then submits via submit_result — so the
    * bridge sees real transcript activity (a `Said`) plus the (filtered) result tool. */
  private class DeltaSubmitEndpoint(text: String, result: String => Json) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val prompt = messages.collectFirst { case Message(Role.User, c) =>
        c.collect { case Content.Text(t) => t }.mkString
      }.getOrElse("")
      val done = messages.exists(_.content.exists { case _: Content.ToolResult => true; case _ => false })
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        if done then ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, List(Content.Text("ok"))), FinishReason.Stop))))
        else
          ch.send(Right(StreamEvent.Delta(text)))
          val args = Json.Obj(List("result" -> result(prompt))).render
          ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, List(Content.ToolUse("s1", "submit_result", args))), FinishReason.ToolUse))))
      ch.asReadable

  private def tmpSock(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-wfb-$name-${js.Dynamic.global.process.pid}.sock").asInstanceOf[String]

  /** Build a bridge whose sub-agents are driven by `endpoint`. */
  private def makeBridge(
      name: String,
      endpoint: Endpoint,
      onEvent: OrchestrationEvent => Unit,
      maxConcurrent: Int = 4,
      onActivity: TranscriptEvent => Unit = _ => ()
  ): WorkflowBridge =
    WorkflowBridge(
      socketPath = tmpSock(name),
      models = ModelSession.of(endpoint, LLMConfig(model = "test")),
      pool = ReplPool(() => ScalaRepl()),
      baseTools = _ => Nil, // scripted sub-agents need no tools beyond submit_result
      systemPrompt = "You are a sub-agent.",
      context = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll),
      onEvent = onEvent,
      maxConcurrent = maxConcurrent,
      onActivity = onActivity
    )

  /** Run `code` through a real worker + real bridge driven by `endpoint`. Returns
    * the tool result and the orchestration events. */
  private def runWfEndpoint(name: String, endpoint: Endpoint, code: String)(using
      Async.Spawn
  ): (ToolResult, List[OrchestrationEvent]) =
    val events = scala.collection.mutable.ListBuffer.empty[OrchestrationEvent]
    val bridge = makeBridge(name, endpoint, ev => events += ev)
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await
    val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
    try
      // Thread the run id the way the Engine does: a callId on the context, which
      // EvalScala forwards to the bridge as the run id (no manual beginRun).
      given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
      val r = EvalScala(repl, Some(bridge)).execute(EvalScalaParams(code, Some(40_000)))
      (r, events.toList)
    finally
      Async.fromSync(repl.close())
      Async.fromSync(bridge.close())

  /** Run `code` whose sub-agents submit `result(prompt)` via submit_result. */
  private def runWf(name: String, result: String => Json, code: String)(using
      Async.Spawn
  ): (ToolResult, List[OrchestrationEvent]) =
    runWfEndpoint(name, new SubmitEndpoint(result), code)

  // -- transcript emission -----------------------------------------------------

  test("transcriptOf tags each HeadlessAgent.Activity with the run + node id"):
    import HeadlessAgent.Activity
    assertEquals(WorkflowBridge.transcriptOf(Activity.Text("hi"), "r", "n"), TranscriptEvent.Said("r", "n", "hi"))
    assertEquals(WorkflowBridge.transcriptOf(Activity.Thinking("z"), "r", "n"), TranscriptEvent.Thought("r", "n", "z"))
    assertEquals(WorkflowBridge.transcriptOf(Activity.ToolStarted("c1", "grep", "p"), "r", "n"),
      TranscriptEvent.ToolCalled("r", "n", "c1", "grep", "p"))
    assertEquals(WorkflowBridge.transcriptOf(Activity.ToolEnded("c1", "out", true), "r", "n"),
      TranscriptEvent.ToolReturned("r", "n", "c1", "out", true))

  test("a sub-agent's text deltas surface as transcript Said events; submit_result is filtered"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val activity = scala.collection.mutable.ListBuffer.empty[TranscriptEvent]
      val bridge = makeBridge("activity", new DeltaSubmitEndpoint("hello ", p => Json.Str("done:" + p)), _ => (),
        onActivity = a => activity.synchronized(activity += a))
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
        val r = EvalScala(repl, Some(bridge)).execute(EvalScalaParams("""wf.start[String](agent[String]("go", id = "x"))""", Some(40_000)))
        assert(!r.isError, r.output)
        assert(activity.exists { case TranscriptEvent.Said("eval-1", "x", t) => t.contains("hello"); case _ => false },
          activity.mkString("\n"))
        assert(!activity.exists { case TranscriptEvent.ToolCalled(_, _, _, "submit_result", _) => true; case _ => false },
          s"submit_result must be filtered from the transcript: ${activity.mkString("\n")}")
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  test("a grouped workflow runs real sub-agents through the bridge (String results)"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val code =
        """wf.start[String]:
          |  val g = group("hunt", "find things")
          |  val a = inGroup(g) { agent[String]("task A", id = "a") }
          |  val b = inGroup(g) { agent[String]("task B", id = "b") }
          |  Agent.all(List(a, b)).flatMap(rs => agent[String]("summary of " + rs.mkString(","), id = "sum"))""".stripMargin
      val (r, events) = runWf("str", p => Json.Str("done:" + p), code)
      println(s"[BRIDGE str] isError=${r.isError} | ${r.output.replace("\n", " ")}")
      assert(!r.isError, r.output)
      assert(r.output.contains("done:summary of done:task A,done:task B"), r.output)
      assert(events.exists { case g: OrchestrationEvent.GroupDeclared => g.runId == "eval-1" && g.name == "hunt"; case _ => false }, events.mkString("\n"))
      assert(events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "sum" && f.ok; case _ => false }, events.mkString("\n"))
      assert(events.count { case _: OrchestrationEvent.NodeStarted => true; case _ => false } == 3, events.mkString("\n"))

  test("typed object results round-trip through submit_result and decode on the worker"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val result = Json.Obj(List("msg" -> Json.Str("hi"), "n" -> Json.num(7)))
      val code =
        """case class R(msg: String, n: Int) derives LibToolInput
          |wf.start[R](agent[R]("go", id = "x"))""".stripMargin
      val (r, _) = runWf("obj", _ => result, code)
      println(s"[BRIDGE obj] isError=${r.isError} | ${r.output.replace("\n", " ")}")
      assert(!r.isError, r.output)
      assert(r.output.contains("R(hi,7)"), r.output)

  // -- C23: a sub-agent that skips submit_result and answers in prose ----------

  test("C23: a prose answer for a typed result fails with a clear error, not a decode crash"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val code =
        """case class R(msg: String, n: Int) derives LibToolInput
          |wf.start[R](agent[R]("go", id = "x"))""".stripMargin
      val (r, events) = runWfEndpoint("prose-typed", new ProseEndpoint("the answer is 42"), code)
      println(s"[PROSE typed] isError=${r.isError} | ${r.output.replace("\n", " ")}")
      assert(r.isError, r.output)
      // Clear, actionable error — not the cryptic worker-side decode failure.
      assert(r.output.contains("did not call submit_result"), r.output)
      assert(!r.output.contains("expected object"), r.output)
      assert(
        events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "x" && !f.ok; case _ => false },
        events.mkString("\n")
      )

  test("C23: a String result is still salvaged from prose when submit_result is skipped"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val code = """wf.start[String](agent[String]("go", id = "x"))"""
      val (r, _) = runWfEndpoint("prose-string", new ProseEndpoint("hello from prose"), code)
      println(s"[PROSE string] isError=${r.isError} | ${r.output.replace("\n", " ")}")
      assert(!r.isError, r.output)
      assert(r.output.contains("hello from prose"), r.output)

  // -- C01: concurrent runs are serialised by the bridge's run lock ------------

  test("C01: beginRun serialises runs — a second run blocks until the first ends"):
    Async.fromSync:
      val bridge = makeBridge("lock", new SubmitEndpoint(s => Json.Str(s)), _ => ())
      val order = scala.collection.mutable.ListBuffer.empty[String]
      bridge.beginRun("A") // acquires the only permit
      val bStarted = Future.Promise[Unit]()
      val bDone = Future:
        bStarted.complete(Success(()))
        bridge.beginRun("B") // must block: A holds the run lock
        order += "B"
        bridge.endRun("B")
      bStarted.asFuture.await
      // A holds the lock, so B cannot have acquired it however the scheduler ran.
      assert(order.isEmpty, s"B must block while A holds the run lock, got $order")
      order += "A-end"
      bridge.endRun("A") // release; B may now proceed
      bDone.await
      assertEquals(order.toList, List("A-end", "B"))

  // -- queued vs running: the concurrency cap throttles execution --------------

  test("the cap gates running sub-agents: queued precedes started, and ≤ cap run at once"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val events = scala.collection.mutable.ListBuffer.empty[OrchestrationEvent]
      var live = 0
      var maxLive = 0
      val capReached = Future.Promise[Unit]() // fires when `live` hits the cap (2)
      val gate = Future.Promise[Unit]() // sub-agents block here until released
      def onEvent(ev: OrchestrationEvent): Unit =
        events += ev
        ev match
          case _: OrchestrationEvent.NodeStarted =>
            live += 1
            if live > maxLive then maxLive = live
            if live == 2 then try capReached.complete(Success(())) catch case _: Throwable => ()
          case _: OrchestrationEvent.NodeFinished => live -= 1
          case _                                  => ()
      val bridge = makeBridge("gate", new GatingEndpoint(gate.asFuture, p => Json.Str("done:" + p)), onEvent, maxConcurrent = 2)
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      try
        // Four leaf agents fan out under a cap of 2: two run (and block on the
        // gate), two stay queued for a slot.
        val code =
          """wf.start[List[String]]:
            |  val g = group("fan", "fan out")
            |  inGroup(g):
            |    Agent.all(List("a", "b", "c", "d").map(n => agent[String](s"task $n", id = n)))""".stripMargin
        val evalFut = Future:
          given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
          EvalScala(repl, Some(bridge)).execute(EvalScalaParams(code, Some(40_000)))

        // Two sub-agents have acquired slots and are blocked; observe the gated state.
        capReached.asFuture.await
        assertEquals(live, 2, s"exactly the cap runs at once:\n${events.mkString("\n")}")
        assertEquals(maxLive, 2, "the cap is never exceeded")
        val startedIds = events.collect { case e: OrchestrationEvent.NodeStarted => e.nodeId }.toSet
        val queuedIds = events.collect { case e: OrchestrationEvent.NodeQueued => e.nodeId }.toSet
        assertEquals(startedIds.size, 2, s"only cap=2 may be running; started=$startedIds")
        assert(startedIds.subsetOf(queuedIds), s"a node is queued before it starts; started=$startedIds queued=$queuedIds")
        assert(
          !events.exists { case _: OrchestrationEvent.NodeFinished => true; case _ => false },
          "nothing can finish while every started agent is gated"
        )

        // Release: the two finish, freeing slots for the two queued agents.
        gate.complete(Success(()))
        val r = evalFut.await
        assert(!r.isError, r.output)
        assert(r.output.contains("done:task a"), r.output)
        assertEquals(maxLive, 2, "the cap held for the whole run")

        val all = Set("a", "b", "c", "d")
        assertEquals(events.collect { case e: OrchestrationEvent.NodeQueued => e.nodeId }.toSet, all, "all queued")
        assertEquals(events.collect { case e: OrchestrationEvent.NodeStarted => e.nodeId }.toSet, all, "all started")
        assertEquals(events.collect { case e: OrchestrationEvent.NodeFinished if e.ok => e.nodeId }.toSet, all, "all finished ok")

        // Per node, the lifecycle is strictly queued → started → finished.
        val seq = events.toList
        for n <- all do
          val qi = seq.indexWhere { case e: OrchestrationEvent.NodeQueued => e.nodeId == n; case _ => false }
          val si = seq.indexWhere { case e: OrchestrationEvent.NodeStarted => e.nodeId == n; case _ => false }
          val fi = seq.indexWhere { case e: OrchestrationEvent.NodeFinished => e.nodeId == n; case _ => false }
          assert(qi >= 0 && si >= 0 && fi >= 0 && qi < si && si < fi, s"node $n out of order: q=$qi s=$si f=$fi")
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  // -- looping: a recursive worker/verifier loop until accepted ----------------

  test("a recursive writer/reviewer loop revises the prior draft until accepted"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // The loop is sequential (each round depends on the last), so a simple
      // counter is deterministic here: the reviewer rejects the first two drafts
      // and accepts the third.
      var reviews = 0
      val resultFor: String => Json = prompt =>
        if prompt.contains("Review this draft") then
          reviews += 1
          Json.Obj(List("accepted" -> Json.Bool(reviews >= 3), "feedback" -> Json.Str("needs work")))
        else
          Json.Obj(List("content" -> Json.Str("a draft")))
      // The exact pattern documented in the system prompt: fresh ids per round, the
      // previous draft + feedback carried forward, a maxRounds cap, and an
      // Agent.pure terminal on acceptance. (Running it also verifies that example.)
      val code =
        """case class Draft(content: String) derives LibToolInput
          |case class Review(accepted: Boolean, feedback: String) derives LibToolInput
          |wf.start[Draft]:
          |  val maxRounds = 5
          |  val revise = group("revise", "Draft and revise until accepted")
          |  def attempt(round: Int, prior: Option[(Draft, String)]): Agent[Draft] =
          |    val prompt = prior match
          |      case None => "Write the first draft of the report."
          |      case Some((prev, feedback)) =>
          |        s"Revise this draft:\n${prev.content}\n\nAddress this feedback: $feedback"
          |    inGroup(revise):
          |      agent[Draft](prompt, id = s"writer-$round").flatMap: draft =>
          |        agent[Review](s"Review this draft:\n${draft.content}", id = s"reviewer-$round").flatMap: review =>
          |          if review.accepted || round >= maxRounds then Agent.pure(draft)
          |          else attempt(round + 1, Some((draft, review.feedback)))
          |  attempt(1, None)""".stripMargin
      val (r, events) = runWf("loop", resultFor, code)
      println(s"[LOOP] isError=${r.isError} | ${r.output.replace("\n", " ")}")
      assert(!r.isError, r.output)
      assert(r.output.contains("a draft"), r.output)
      // The loop unrolled exactly three writer→reviewer rounds.
      val finished = events.collect { case e: OrchestrationEvent.NodeFinished if e.ok => e.nodeId }.toSet
      assertEquals(
        finished,
        Set("writer-1", "reviewer-1", "writer-2", "reviewer-2", "writer-3", "reviewer-3"),
        events.mkString("\n")
      )
      // The dependency chain threads the loop: reviewer-N ← writer-N, writer-(N+1) ← reviewer-N.
      val deps = events.collect { case e: OrchestrationEvent.NodeDeclared => e.nodeId -> e.deps }.toMap
      assertEquals(deps.get("reviewer-1"), Some(List("writer-1")), deps.toString)
      assertEquals(deps.get("writer-2"), Some(List("reviewer-1")), deps.toString)
      assertEquals(deps.get("writer-3"), Some(List("reviewer-2")), deps.toString)
