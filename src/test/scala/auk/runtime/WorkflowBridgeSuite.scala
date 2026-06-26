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
import auk.session.SessionRef
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl
import auk.utils.Result

/** Phase-2 end-to-end: the real worker DSL talking to the real [[WorkflowBridge]],
  * with sub-agents driven by a scripted endpoint (no LLM). Each sub-agent submits
  * its result via the injected `submit_result` tool — deterministic under
  * concurrency because the reply is derived from the messages, not a counter.
  */
class WorkflowBridgeSuite extends munit.FunSuite:

  // Each test spins up a real REPL worker (+ pooled sub-agent workers). Under the
  // full `sbt test` run many suites spawn workers in parallel, so worker startup
  // contends heavily; match the other worker-backed suites' generous budget
  // (WorkflowLiveSuite uses 240s) rather than the tight 90s that starved here.
  override def munitTimeout: Duration = 240.seconds

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

  /** Submits `result(prompt)` on the first turn, but a sub-agent whose prompt
    * contains `gateOn` first blocks on `gate` — so a test can pause it mid-flight.
    * `firstTurns` counts, per prompt, how many fresh (pre-tool-result) turns ran,
    * i.e. how many times that sub-agent actually executed; a cached node short-circuits
    * on the host and never reaches the endpoint, so its count does not grow on resume. */
  private class GateOneEndpoint(gateOn: String, gate: Future[Unit], result: String => Json) extends Endpoint:
    val firstTurns = scala.collection.mutable.Map.empty[String, Int]
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val prompt = messages.collectFirst { case Message(Role.User, c) => c.collect { case Content.Text(t) => t }.mkString }.getOrElse("")
      val done = messages.exists(_.content.exists { case _: Content.ToolResult => true; case _ => false })
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        if !done then
          firstTurns.synchronized { firstTurns(prompt) = firstTurns.getOrElse(prompt, 0) + 1 }
          if prompt.contains(gateOn) then gate.await // hold until the test resumes + releases
        val resp =
          if done then ChatResponse(Message(Role.Assistant, List(Content.Text("ok"))), FinishReason.Stop)
          else ChatResponse(Message(Role.Assistant, List(Content.ToolUse("s1", "submit_result", Json.Obj(List("result" -> result(prompt))).render))), FinishReason.ToolUse)
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

  /** Submits an INVALID result on its first attempt, then a VALID one once the
    * host has handed the parse error back — exercising the in-loop parse retry.
    * Deterministic: the choice is driven by how many submit_result calls are
    * already in the history, not a counter. */
  private class RetryThenSubmitEndpoint(bad: String => Json, good: String => Json) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val prompt = messages.collectFirst { case Message(Role.User, c) => c.collect { case Content.Text(t) => t }.mkString }.getOrElse("")
      val submits = messages.count(_.content.exists { case Content.ToolUse(_, "submit_result", _) => true; case _ => false })
      val resp = submits match
        case 0 => ChatResponse(Message(Role.Assistant, List(Content.ToolUse("s0", "submit_result", Json.Obj(List("result" -> bad(prompt))).render))), FinishReason.ToolUse)
        case 1 => ChatResponse(Message(Role.Assistant, List(Content.ToolUse("s1", "submit_result", Json.Obj(List("result" -> good(prompt))).render))), FinishReason.ToolUse)
        case _ => ChatResponse(Message(Role.Assistant, List(Content.Text("ok"))), FinishReason.Stop)
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(resp))))
      ch.asReadable

  /** Always submits an INVALID result — used to drive the parse-retry cap to
    * exhaustion. Counts its turns so a test can assert the cap was honored. */
  private class AlwaysBadSubmitEndpoint(bad: String => Json) extends Endpoint:
    var invocations = 0
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      invocations += 1
      val prompt = messages.collectFirst { case Message(Role.User, c) => c.collect { case Content.Text(t) => t }.mkString }.getOrElse("")
      val resp = ChatResponse(Message(Role.Assistant, List(Content.ToolUse(s"s$invocations", "submit_result", Json.Obj(List("result" -> bad(prompt))).render))), FinishReason.ToolUse)
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(resp))))
      ch.asReadable

  /** Answers in prose first (no submit_result), then submits a VALID result once a
    * nudge user-message has been appended — exercising the no-submit retry. The
    * choice is driven by the count of user *text* messages (prompt + nudges). */
  private class ProseThenSubmitEndpoint(proseText: String, good: String => Json) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      val prompt = messages.collectFirst { case Message(Role.User, c) => c.collect { case Content.Text(t) => t }.mkString }.getOrElse("")
      val submitted = messages.exists(_.content.exists { case Content.ToolUse(_, "submit_result", _) => true; case _ => false })
      val userTexts = messages.count(m => m.role == Role.User && m.content.exists { case _: Content.Text => true; case _ => false })
      val resp =
        if submitted then ChatResponse(Message(Role.Assistant, List(Content.Text("ok"))), FinishReason.Stop)
        else if userTexts >= 2 then ChatResponse(Message(Role.Assistant, List(Content.ToolUse("s1", "submit_result", Json.Obj(List("result" -> good(prompt))).render))), FinishReason.ToolUse)
        else ChatResponse(Message(Role.Assistant, List(Content.Text(proseText))), FinishReason.Stop)
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(resp))))
      ch.asReadable

  /** Always answers in prose, never submitting — used to drive the no-submit nudge
    * cap to exhaustion. Counts its turns so a test can assert how many nudges fired. */
  private class CountingProseEndpoint(answer: String) extends Endpoint:
    var invocations = 0
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
      invocations += 1
      val resp = ChatResponse(Message(Role.Assistant, List(Content.Text(answer))), FinishReason.Stop)
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(resp))))
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
      onActivity: TranscriptEvent => Unit = _ => (),
      onComplete: (String, Either[String, String]) => Unit = (_, _) => (),
      maxResultRetries: Int = WorkflowBridge.MaxResultRetries,
      sessionRef: Option[SessionRef] = None
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
      onActivity = onActivity,
      onComplete = onComplete,
      maxResultRetries = maxResultRetries,
      sessionRef = sessionRef
    )

  /** Run `code` through a real worker + real bridge driven by `endpoint`. The
    * workflow now runs in the BACKGROUND: `wf.start` returns immediately (the tool
    * result is the `WorkflowRun(...)` render), and the real outcome is delivered
    * via the bridge's `onComplete`. So we await that and return `(outcome, events,
    * evalResult)` — outcome is `Right(value)` on success / `Left(error)` on failure. */
  private def runWfEndpoint(name: String, endpoint: Endpoint, code: String, maxResultRetries: Int = WorkflowBridge.MaxResultRetries)(using
      Async.Spawn
  ): (Either[String, String], List[OrchestrationEvent], ToolResult) =
    val events = scala.collection.mutable.ListBuffer.empty[OrchestrationEvent]
    val outcome = Future.Promise[Either[String, String]]()
    val bridge = makeBridge(name, endpoint, ev => events += ev, maxResultRetries = maxResultRetries,
      onComplete = (_, oc) => try outcome.complete(Success(oc)) catch case _: Throwable => ())
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await
    val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
    try
      given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
      val r = EvalScala(repl, Some(bridge)).execute(EvalScalaParams(code, Some(40_000)))
      // The eval returned a pending WorkflowRun; wait for the background run to settle.
      val oc = outcome.asFuture.await
      (oc, events.toList, r)
    finally
      Async.fromSync(repl.close())
      Async.fromSync(bridge.close())

  /** Run `code` whose sub-agents submit `result(prompt)` via submit_result. */
  private def runWf(name: String, result: String => Json, code: String)(using
      Async.Spawn
  ): (Either[String, String], List[OrchestrationEvent], ToolResult) =
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

  test("resuming a paused run re-runs the worker's closure; finished nodes short-circuit"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      import gears.async.AsyncOperations.sleep
      // A two-node sequential workflow a → b. `a` completes immediately; `b` blocks
      // on a gate so we can pause while it runs. The terminal is `b`.
      val code = """wf.start[String](agent[String]("produce A", id = "a").flatMap(_ => agent[String]("produce B", id = "b")))"""
      val gate = Future.Promise[Unit]() // released only after we resume, to let `b` finish
      val endpoint = new GateOneEndpoint("produce B", gate.asFuture, p => Json.Str("done:" + p))
      val started = Future.Promise[String]()    // run id once `b` starts (so `a` is already cached)
      val interrupted = Future.Promise[Unit]()  // `b` interrupted by the pause
      val resumed = Future.Promise[Unit]()      // the run was resumed
      val outcome = Future.Promise[Either[String, String]]()
      def onEvent(ev: OrchestrationEvent): Unit =
        ev match
          case e: OrchestrationEvent.NodeStarted if e.nodeId == "b" => try started.complete(Success(e.runId)) catch case _: Throwable => ()
          case e: OrchestrationEvent.NodeInterrupted if e.nodeId == "b" => try interrupted.complete(Success(())) catch case _: Throwable => ()
          case _: OrchestrationEvent.WorkflowResumed => try resumed.complete(Success(())) catch case _: Throwable => ()
          case _ => ()
      val bridge = makeBridge("resume", endpoint, onEvent,
        onComplete = (_, oc) => try outcome.complete(Success(oc)) catch case _: Throwable => ())
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
        EvalScala(repl, Some(bridge)).execute(EvalScalaParams(code, Some(40_000)))
        val runId = started.asFuture.await // `a` is done and cached; `b` is running, blocked on the gate
        bridge.pause(runId)
        interrupted.asFuture.await
        sleep(200) // let the cancelled fiber unwind fully
        bridge.resume(runId)
        resumed.asFuture.await
        gate.complete(Success(())) // release `b`'s (re-run) attempt so the run can finish
        assertEquals(outcome.asFuture.await, Right("done:produce B"))
        // `a` finished before the pause; on resume its cached result short-circuits, so
        // the endpoint is asked to produce it exactly once. `b` was interrupted and re-runs.
        assertEquals(endpoint.firstTurns.get("produce A"), Some(1))
        assert(endpoint.firstTurns.getOrElse("produce B", 0) >= 2,
          s"b should have re-run on resume: ${endpoint.firstTurns}")
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  test("the model can pause and resume its own run via WorkflowRun.pause()/resume()"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      import gears.async.AsyncOperations.sleep
      // Same two-node a → (gated) b workflow, but the run is paused/resumed by the
      // MODEL calling r.pause()/r.resume() in later evals — exercising the worker→host
      // request path. The handle is bound to `r` so subsequent evals can drive it.
      val startCode = """val r = wf.start[String](agent[String]("produce A", id = "a").flatMap(_ => agent[String]("produce B", id = "b")))"""
      val gate = Future.Promise[Unit]()
      val endpoint = new GateOneEndpoint("produce B", gate.asFuture, p => Json.Str("done:" + p))
      val started = Future.Promise[String]()
      val interrupted = Future.Promise[Unit]()
      val resumed = Future.Promise[Unit]()
      val outcome = Future.Promise[Either[String, String]]()
      def onEvent(ev: OrchestrationEvent): Unit =
        ev match
          case e: OrchestrationEvent.NodeStarted if e.nodeId == "b" => try started.complete(Success(e.runId)) catch case _: Throwable => ()
          case e: OrchestrationEvent.NodeInterrupted if e.nodeId == "b" => try interrupted.complete(Success(())) catch case _: Throwable => ()
          case _: OrchestrationEvent.WorkflowResumed => try resumed.complete(Success(())) catch case _: Throwable => ()
          case _ => ()
      val bridge = makeBridge("prog-pause", endpoint, onEvent,
        onComplete = (_, oc) => try outcome.complete(Success(oc)) catch case _: Throwable => ())
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      val tool = EvalScala(repl, Some(bridge))
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval")
        tool.execute(EvalScalaParams(startCode, Some(40_000)))           // eval 1: launch, bind to `r`
        started.asFuture.await                                            // `a` cached; `b` running, gated
        tool.execute(EvalScalaParams("r.pause()", Some(40_000)))          // eval 2: programmatic pause
        interrupted.asFuture.await
        sleep(200)
        tool.execute(EvalScalaParams("r.resume()", Some(40_000)))         // eval 3: programmatic resume
        resumed.asFuture.await
        gate.complete(Success(()))
        assertEquals(outcome.asFuture.await, Right("done:produce B"))
        assertEquals(endpoint.firstTurns.get("produce A"), Some(1))       // `a` short-circuited from cache
        assert(endpoint.firstTurns.getOrElse("produce B", 0) >= 2,
          s"b should have re-run on resume: ${endpoint.firstTurns}")
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  test("a sub-agent's text deltas surface as transcript Said events; submit_result is filtered"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val activity = scala.collection.mutable.ListBuffer.empty[TranscriptEvent]
      val outcome = Future.Promise[Either[String, String]]()
      val bridge = makeBridge("activity", new DeltaSubmitEndpoint("hello ", p => Json.Str("done:" + p)), _ => (),
        onActivity = a => activity.synchronized(activity += a),
        onComplete = (_, oc) => try outcome.complete(Success(oc)) catch case _: Throwable => ())
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
        val r = EvalScala(repl, Some(bridge)).execute(EvalScalaParams("""wf.start[String](agent[String]("go", id = "x"))""", Some(40_000)))
        assert(!r.isError, r.output) // the eval itself succeeds (returns a WorkflowRun handle)
        outcome.asFuture.await // wait for the background run to finish emitting activity
        assert(activity.exists { case TranscriptEvent.Said(_, "x", t) => t.contains("hello"); case _ => false },
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
      val (oc, events, r) = runWf("str", p => Json.Str("done:" + p), code)
      println(s"[BRIDGE str] outcome=$oc | eval=${r.output.replace("\n", " ")}")
      // The eval returns immediately with a pending handle; the result arrives via onComplete.
      assert(r.output.contains("WorkflowRun(") && r.output.contains("still running"), r.output)
      assert(oc.isRight, oc.toString)
      assert(oc.toOption.get.contains("done:summary of done:task A,done:task B"), oc.toString)
      assert(events.exists { case g: OrchestrationEvent.GroupDeclared => g.name == "hunt"; case _ => false }, events.mkString("\n"))
      assert(events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "sum" && f.ok; case _ => false }, events.mkString("\n"))
      assert(events.count { case _: OrchestrationEvent.NodeStarted => true; case _ => false } == 3, events.mkString("\n"))
      assert(events.exists { case wf: OrchestrationEvent.WorkflowFinished => wf.ok; case _ => false }, events.mkString("\n"))

  test("typed object results round-trip through submit_result and decode on the worker"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val result = Json.Obj(List("msg" -> Json.Str("hi"), "n" -> Json.num(7)))
      val code =
        """case class R(msg: String, n: Int) derives LibToolInput
          |wf.start[R](agent[R]("go", id = "x"))""".stripMargin
      val (oc, _, _) = runWf("obj", _ => result, code)
      println(s"[BRIDGE obj] outcome=$oc")
      assert(oc.isRight, oc.toString)
      assert(oc.toOption.get.contains("R(hi,7)"), oc.toString)

  // -- C23: a sub-agent that skips submit_result and answers in prose ----------

  test("C23: a prose answer for a typed result fails with a clear error, not a decode crash"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val code =
        """case class R(msg: String, n: Int) derives LibToolInput
          |wf.start[R](agent[R]("go", id = "x"))""".stripMargin
      val (oc, events, _) = runWfEndpoint("prose-typed", new ProseEndpoint("the answer is 42"), code)
      println(s"[PROSE typed] outcome=$oc")
      assert(oc.isLeft, oc.toString)
      // Clear, actionable error — not the cryptic worker-side decode failure.
      assert(oc.left.toOption.get.contains("did not call submit_result"), oc.toString)
      assert(!oc.left.toOption.get.contains("expected object"), oc.toString)
      assert(
        events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "x" && !f.ok; case _ => false },
        events.mkString("\n")
      )

  test("C23: a String result is still salvaged from prose when submit_result is skipped"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val code = """wf.start[String](agent[String]("go", id = "x"))"""
      val (oc, _, _) = runWfEndpoint("prose-string", new ProseEndpoint("hello from prose"), code)
      println(s"[PROSE string] outcome=$oc")
      assert(oc.isRight, oc.toString)
      assert(oc.toOption.get.contains("hello from prose"), oc.toString)

  // -- submit_result robustness: parse retries and no-submit nudges ------------

  private val typedCode =
    """case class R(msg: String, n: Int) derives LibToolInput
      |wf.start[R](agent[R]("go", id = "x"))""".stripMargin
  private val badResult: String => Json = _ => Json.Obj(List("msg" -> Json.Str("hi")))          // missing required `n`
  private val goodResult: String => Json = _ => Json.Obj(List("msg" -> Json.Str("hi"), "n" -> Json.num(7)))

  test("an unparseable result is handed back to the agent, which retries and succeeds"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val (oc, events, _) = runWfEndpoint("retry-ok", new RetryThenSubmitEndpoint(badResult, goodResult), typedCode)
      println(s"[RETRY ok] outcome=$oc")
      // The bad first submission never reaches the worker; the corrected one decodes.
      assert(oc.isRight, oc.toString)
      assert(oc.toOption.get.contains("R(hi,7)"), oc.toString)
      assert(events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "x" && f.ok; case _ => false }, events.mkString("\n"))

  test("a result that never parses fails the node after the retry cap, with the reason"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val ep = new AlwaysBadSubmitEndpoint(badResult)
      val (oc, events, _) = runWfEndpoint("retry-cap", ep, typedCode, maxResultRetries = 3)
      println(s"[RETRY cap] outcome=$oc | invocations=${ep.invocations}")
      assert(oc.isLeft, oc.toString)
      assert(oc.left.toOption.get.contains("after 3 attempt"), oc.toString)
      assert(oc.left.toOption.get.contains("missing required field 'n'"), oc.toString)
      assertEquals(ep.invocations, 3, "the cap bounds how many invalid submissions are tried")
      assert(events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "x" && !f.ok; case _ => false }, events.mkString("\n"))

  test("an agent that ends without submitting is nudged, then submits and succeeds"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // A typed result can't be salvaged from prose, so success here proves the
      // nudge pushed the agent into calling submit_result.
      val (oc, events, _) = runWfEndpoint("nudge-ok", new ProseThenSubmitEndpoint("I think it's 7.", goodResult), typedCode)
      println(s"[NUDGE ok] outcome=$oc")
      assert(oc.isRight, oc.toString)
      assert(oc.toOption.get.contains("R(hi,7)"), oc.toString)
      assert(events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "x" && f.ok; case _ => false }, events.mkString("\n"))

  test("an agent that never submits a typed result is nudged up to the cap, then fails"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val ep = new CountingProseEndpoint("the answer is 42")
      val (oc, events, _) = runWfEndpoint("nudge-cap", ep, typedCode, maxResultRetries = 3)
      println(s"[NUDGE cap] outcome=$oc | invocations=${ep.invocations}")
      assert(oc.isLeft, oc.toString)
      assert(oc.left.toOption.get.contains("did not call submit_result"), oc.toString)
      // 3 nudged turns + 1 final tool-free turn = 4 model turns.
      assertEquals(ep.invocations, 4, "nudges are capped at maxResultRetries")
      assert(events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "x" && !f.ok; case _ => false }, events.mkString("\n"))

  test("a String result is still salvaged from prose only after the nudges are spent"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val ep = new CountingProseEndpoint("hello from prose")
      val code = """wf.start[String](agent[String]("go", id = "x"))"""
      val (oc, _, _) = runWfEndpoint("nudge-string", ep, code, maxResultRetries = 3)
      println(s"[NUDGE string] outcome=$oc | invocations=${ep.invocations}")
      assert(oc.isRight, oc.toString)
      assert(oc.toOption.get.contains("hello from prose"), oc.toString)
      assertEquals(ep.invocations, 4, "the agent is nudged before falling back to prose salvage")

  // -- concurrent runs: the bridge no longer serialises, runs are independent ---

  test("two concurrent runs each settle independently with their own onComplete"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      // One bridge, two workers (two `wf.start`s), each its own connection + run id.
      val outcomes = scala.collection.mutable.Map.empty[String, Either[String, String]]
      val finished = Future.Promise[Unit]()
      val bridge = makeBridge("concurrent", new SubmitEndpoint(p => Json.Str("done:" + p)), _ => (),
        onComplete = (runId, oc) =>
          outcomes.synchronized:
            outcomes(runId) = oc
            if outcomes.size == 2 then try finished.complete(Success(())) catch case _: Throwable => ())
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      def repl() = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      val rA = repl()
      val rB = repl()
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
        EvalScala(rA, Some(bridge)).execute(EvalScalaParams("""wf.start[String](agent[String]("A", id = "a"))""", Some(40_000)))
        EvalScala(rB, Some(bridge)).execute(EvalScalaParams("""wf.start[String](agent[String]("B", id = "b"))""", Some(40_000)))
        finished.asFuture.await
        assertEquals(outcomes.size, 2, outcomes.toString)
        // Two distinct worker-minted run ids, each with its own result.
        assertEquals(outcomes.values.toSet, Set[Either[String, String]](Right("done:A"), Right("done:B")), outcomes.toString)
      finally
        Async.fromSync(rA.close())
        Async.fromSync(rB.close())
        Async.fromSync(bridge.close())

  test("a dropped worker fails the run via onComplete (Left) exactly once"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val gate = Future.Promise[Unit]() // never released: the sub-agent runs until the worker dies
      var completions = 0
      val outcome = Future.Promise[Either[String, String]]()
      val started = Future.Promise[Unit]()
      val bridge = makeBridge("drop", new GatingEndpoint(gate.asFuture, p => Json.Str(p)),
        onEvent = {
          case _: OrchestrationEvent.NodeStarted => try started.complete(Success(())) catch case _: Throwable => ()
          case _                                 => ()
        },
        onComplete = (_, oc) =>
          completions += 1
          try outcome.complete(Success(oc)) catch case _: Throwable => ())
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
        EvalScala(repl, Some(bridge)).execute(EvalScalaParams("""wf.start[String](agent[String]("go", id = "x"))""", Some(40_000)))
        started.asFuture.await // the sub-agent is running, blocked on the gate
        repl.close()           // kill the worker mid-run → the side channel drops
        val oc = outcome.asFuture.await
        assert(oc.isLeft, oc.toString)
        // bridge.close() (in finally) must NOT fire a second completion: the run is
        // already settled by the disconnect (the activeRuns guard collapses both).
        Async.fromSync(bridge.close())
        assertEquals(completions, 1, "a run settles exactly once across disconnect + shutdown")
      finally
        Async.fromSync(repl.close())

  // -- pause: an in-flight sub-agent is interrupted, not failed -----------------

  test("pausing a run interrupts its in-flight sub-agent (Interrupted, not Failed)"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      import gears.async.AsyncOperations.sleep
      val gate = Future.Promise[Unit]() // never released: the sub-agent runs until paused
      val events = scala.collection.mutable.ListBuffer.empty[OrchestrationEvent]
      val started = Future.Promise[String]()   // resolves with the run id once the node starts
      val interrupted = Future.Promise[Unit]()  // resolves once the node is interrupted
      var completions = 0
      def onEvent(ev: OrchestrationEvent): Unit =
        events += ev
        ev match
          case e: OrchestrationEvent.NodeStarted     => try started.complete(Success(e.runId)) catch case _: Throwable => ()
          case _: OrchestrationEvent.NodeInterrupted => try interrupted.complete(Success(())) catch case _: Throwable => ()
          case _                                     => ()
      val bridge = makeBridge("pause", new GatingEndpoint(gate.asFuture, p => Json.Str(p)), onEvent,
        onComplete = (_, _) => completions += 1)
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
        EvalScala(repl, Some(bridge)).execute(EvalScalaParams("""wf.start[String](agent[String]("go", id = "x"))""", Some(40_000)))
        val runId = started.asFuture.await // the sub-agent is running, blocked on the gate
        bridge.pause(runId)
        interrupted.asFuture.await          // handlePause marked the running node interrupted
        // Let the hard-cancelled fiber fully unwind — a buggy unwind would (wrongly)
        // emit NodeFinished(ok=false) here; the CancellationException path must not.
        sleep(300)
        assert(events.exists { case OrchestrationEvent.NodeInterrupted(_, "x") => true; case _ => false },
          s"the node should be interrupted:\n${events.mkString("\n")}")
        assert(events.exists { case _: OrchestrationEvent.WorkflowPaused => true; case _ => false },
          s"the run should be paused:\n${events.mkString("\n")}")
        assert(!events.exists { case OrchestrationEvent.NodeFinished(_, "x", false, _) => true; case _ => false },
          s"a paused (interrupted) node must NOT be reported as failed:\n${events.mkString("\n")}")
        // A paused run is suppressed from the finish path — no completion notice fires.
        assertEquals(completions, 0, "pausing does not settle the run via onComplete")
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  // -- queued vs running: the concurrency cap throttles execution --------------

  test("the cap gates running sub-agents: queued precedes started, and ≤ cap run at once"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val events = scala.collection.mutable.ListBuffer.empty[OrchestrationEvent]
      var live = 0
      var maxLive = 0
      val capReached = Future.Promise[Unit]() // fires when `live` hits the cap (2)
      val gate = Future.Promise[Unit]() // sub-agents block here until released
      val runDone = Future.Promise[Either[String, String]]() // the background run's outcome
      def onEvent(ev: OrchestrationEvent): Unit =
        events += ev
        ev match
          case _: OrchestrationEvent.NodeStarted =>
            live += 1
            if live > maxLive then maxLive = live
            if live == 2 then try capReached.complete(Success(())) catch case _: Throwable => ()
          case _: OrchestrationEvent.NodeFinished => live -= 1
          case _                                  => ()
      val bridge = makeBridge("gate", new GatingEndpoint(gate.asFuture, p => Json.Str("done:" + p)), onEvent, maxConcurrent = 2,
        onComplete = (_, oc) => try runDone.complete(Success(oc)) catch case _: Throwable => ())
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
        // The eval returns immediately with a pending handle; the run proceeds in
        // the background (no Future wrapper needed).
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("eval-1")
        val r = EvalScala(repl, Some(bridge)).execute(EvalScalaParams(code, Some(40_000)))
        assert(r.output.contains("WorkflowRun("), r.output)

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
        val oc = runDone.asFuture.await
        assert(oc.isRight, oc.toString)
        assert(oc.toOption.get.contains("done:task a"), oc.toString)
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
      val (oc, events, _) = runWf("loop", resultFor, code)
      println(s"[LOOP] outcome=$oc")
      assert(oc.isRight, oc.toString)
      assert(oc.toOption.get.contains("a draft"), oc.toString)
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
