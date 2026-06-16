package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.agent.OrchestrationEvent
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

  private def tmpSock(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-wfb-$name-${js.Dynamic.global.process.pid}.sock").asInstanceOf[String]

  /** Run `code` through a real worker + real bridge whose sub-agents submit
    * `result(prompt)`. Returns the tool result and the orchestration events. */
  private def runWf(name: String, result: String => Json, code: String)(using
      Async.Spawn
  ): (ToolResult, List[OrchestrationEvent]) =
    val events = scala.collection.mutable.ListBuffer.empty[OrchestrationEvent]
    val bridge = WorkflowBridge(
      socketPath = tmpSock(name),
      models = ModelSession.of(new SubmitEndpoint(result), LLMConfig(model = "test")),
      pool = ReplPool(() => ScalaRepl()),
      baseTools = _ => Nil, // scripted sub-agents need no tools beyond submit_result
      systemPrompt = "You are a sub-agent.",
      context = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll),
      onEvent = ev => events += ev,
      maxConcurrent = 4
    )
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
