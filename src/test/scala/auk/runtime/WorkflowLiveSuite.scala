package auk.runtime

import scala.concurrent.duration.Duration
import scala.util.Success

import gears.async.{Async, Future}
import gears.async.default.given

import auk.agent.SystemPrompt
import auk.workflow.OrchestrationEvent
import auk.llm.provider.{ModelSession, ModelSelection}
import auk.llm.endpoint.LLMConfig
import auk.llm.tools.{RuntimeContext, ApprovalPolicy}
import auk.platform.Platform
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl

/** Opt-in live check: real z.ai glm-5.2 drives real sub-agents through the
  * [[WorkflowBridge]] (each calling `submit_result` for a typed result), with the
  * orchestrator graph running in the real worker. The full production path —
  * worker DSL → side channel → host executor → streaming model → typed decode →
  * awaited result — end to end.
  *
  * Runs only when `AUK_LIVE_TESTS=1` and `ZAI_API_KEY` are set:
  *   bash -c 'source .envrc && AUK_LIVE_TESTS=1 sbt "testOnly auk.runtime.WorkflowLiveSuite"'
  */
class WorkflowLiveSuite extends munit.FunSuite:

  override val munitTimeout = Duration(240, "s")

  private def enabled: Boolean =
    Platform.env.get("AUK_LIVE_TESTS").contains("1") &&
      Platform.env.get("ZAI_API_KEY").isDefined &&
      ReplArtifacts.resolve().isRight

  test("glm-5.2 drives a typed, grouped workflow end to end through the bridge"):
    assume(enabled, "set AUK_LIVE_TESTS=1 and ZAI_API_KEY to run this live test")
    Async.fromSync:
      val resolved = ModelSelection.resolve() match
        case Right(r)  => r
        case Left(err) => fail(s"model resolve failed: $err")
      val models = ModelSession.of(
        resolved.endpoint,
        LLMConfig(model = resolved.model.id, thinking = Some(resolved.model.thinking))
      )
      val events = scala.collection.mutable.ListBuffer.empty[OrchestrationEvent]
      val bridge = WorkflowBridge(
        socketPath = WorkflowBridge.defaultSocketPath() + ".live",
        models = models,
        pool = ReplPool(() => ScalaRepl()),
        baseTools = repl => List(GetMemory, WriteMemory, EvalScala(repl)),
        systemPrompt = SystemPrompt.workflowAgent,
        context = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll),
        onEvent = ev => events += ev,
        maxConcurrent = 4
      )
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_WF_SOCK" -> bridge.socketPath))))
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("live-1")
        val code =
          """case class Answer(value: String) derives LibToolInput
            |wf.start[Answer]:
            |  val g = group("compute", "simple arithmetic")
            |  val a = inGroup(g) { agent[Answer]("What is 2 + 2? Answer with only the number.", id = "two-plus-two") }
            |  val b = inGroup(g) { agent[Answer]("What is 3 + 3? Answer with only the number.", id = "three-plus-three") }
            |  Agent.all(List(a, b)).flatMap: rs =>
            |    agent[Answer]("Add these numbers and answer with only the number: " + rs.map(_.value).mkString(" and "), id = "sum")""".stripMargin
        val r = EvalScala(repl, Some(bridge)).execute(EvalScalaParams(code, Some(180_000)))
        println(s"[LIVE] isError=${r.isError}\n${r.output}\n--- ${events.size} orchestration events ---")
        events.foreach(e => println("  " + e))
        assert(!r.isError, r.output)
        assert(r.output.contains("Answer("), s"expected a typed Answer result, got:\n${r.output}")
        assert(
          events.exists { case f: OrchestrationEvent.NodeFinished => f.nodeId == "sum" && f.ok; case _ => false },
          "the summary node should finish successfully"
        )
        assert(
          events.count { case _: OrchestrationEvent.NodeFinished => true; case _ => false } == 3,
          s"expected 3 finished nodes, got events: ${events.mkString(", ")}"
        )
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())
