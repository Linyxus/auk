package auk.runtime

import scala.concurrent.duration.Duration
import scala.util.Success

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given

import auk.agent.SystemPrompt
import auk.config.AppConfig
import auk.llm.provider.{ModelSession, ModelSelection}
import auk.llm.endpoint.LLMConfig
import auk.llm.tools.{RuntimeContext, ApprovalPolicy}
import auk.platform.Platform
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl

/** Opt-in live check: real z.ai glm-5.2 runs a real team member through the
  * [[TeamBridge]]. The lead worker creates a member and sends it a task; the member
  * runs a real streamed turn and its idle notice carries a real response.
  *
  * Runs only when `AUK_LIVE_TESTS=1` and `ZAI_API_KEY` are set:
  *   bash -c 'source .envrc && AUK_LIVE_TESTS=1 ./mill test.testOnly auk.runtime.TeamLiveSuite'
  */
class TeamLiveSuite extends munit.FunSuite:

  override val munitTimeout = Duration(240, "s")

  private def enabled: Boolean =
    Platform.env.get("AUK_LIVE_TESTS").contains("1") &&
      Platform.env.get("ZAI_API_KEY").isDefined &&
      ReplArtifacts.resolve().isRight

  test("glm-5.2 runs a real team member and its idle notice carries a response"):
    assume(enabled, "set AUK_LIVE_TESTS=1 and ZAI_API_KEY to run this live test")
    Async.fromSync:
      val config = AppConfig.load() match
        case Right(c)   => c
        case Left(errs) => fail(s"invalid config: ${errs.map(_.render).mkString("; ")}")
      val resolved = ModelSelection.resolve(config) match
        case Right(r)  => r
        case Left(err) => fail(s"model resolve failed: $err")
      val models = ModelSession.of(
        resolved.endpoint,
        LLMConfig(model = resolved.model.id, thinking = Some(resolved.model.thinking))
      )
      val notices = UnboundedChannel[String]()
      val bridge = TeamBridge(
        socketPath = TeamBridge.defaultSocketPath() + ".live",
        models = models,
        makeRepl = env => ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env ++ env))),
        baseTools = repl => List(EvalScala(repl)),
        memberPrompt = (id, desc) => SystemPrompt.teamMember(id, desc),
        context = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll),
        notifyLead = msg => notices.sendImmediately(msg)
      )
      val ready = Future.Promise[Unit]()
      bridge.start(() => ready.complete(Success(())))
      ready.asFuture.await
      val repl = ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_TEAM_SOCK" -> bridge.socketPath))))
      try
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll).withCallId("live-1")
        val code =
          """team.newMember("greeter", "greets people").sendMessage("Reply with a one-word greeting.")"""
        val r = EvalScala(repl).execute(EvalScalaParams(code, Some(60_000)))
        assert(!r.isError, r.output)
        // Wait for the member's turn to finish; the notice carries its final message.
        var notice: String | Null = null
        while notice == null do
          notices.read() match
            case Right(n) if n.contains("Team member 'greeter' finished its turn") => notice = n
            case Right(_)                                                           => ()
            case Left(_)                                                            => fail("notice channel closed")
        println(s"[LIVE team] $notice")
        assert(!notice.nn.contains("(no response)"), notice)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())
