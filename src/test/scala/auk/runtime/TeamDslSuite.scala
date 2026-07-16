package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given

import auk.llm.provider.ModelSession
import auk.llm.endpoint.{Endpoint, LLMConfig, ChatResponse, Message, Content, Role, FinishReason, StreamEvent, LLMError}
import auk.llm.tools.{RuntimeContext, ApprovalPolicy}
import auk.platform.Platform
import auk.platform.js.ReplArtifacts
import auk.runtime.repl.ScalaRepl
import auk.utils.Result

/** End-to-end DSL tests: a real REPL worker runs the actual `auk.library.Team`
  * DSL over the real [[TeamBridge]] side channel, with member turns driven by a
  * scripted endpoint (no LLM). Exercises both the lead worker (`AUK_TEAM_SOCK`
  * only) and a member worker (`AUK_TEAM_ID` set), plus the between-evals staleness
  * of the roster mirror.
  */
class TeamDslSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 240.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight

  /** Finishes each member turn with a fixed assistant reply. */
  private class ReplyEndpoint(reply: String) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): gears.async.ReadableChannel[Result[StreamEvent, LLMError]] =
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, List(Content.Text(reply))), FinishReason.Stop)))))
      ch.asReadable

  private def tmpSock(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-teamdsl-$name-${js.Dynamic.global.process.pid}.sock").asInstanceOf[String]

  private def makeBridge(name: String, reply: String, notices: UnboundedChannel[String]): TeamBridge =
    TeamBridge(
      socketPath = tmpSock(name),
      models = ModelSession.of(new ReplyEndpoint(reply), LLMConfig(model = "test")),
      makeRepl = env => ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env ++ env))),
      baseTools = _ => Nil,
      memberPrompt = (_, _) => "You are a team member.",
      context = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll),
      notifyLead = msg => notices.sendImmediately(msg)
    )

  private def start(bridge: TeamBridge)(using Async.Spawn): Unit =
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await

  private def leadRepl(sock: String): ScalaRepl =
    ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_TEAM_SOCK" -> sock))))

  private def memberRepl(sock: String, id: String): ScalaRepl =
    ScalaRepl(() => ReplArtifacts.resolve().map(s => s.copy(env = s.env + ("AUK_TEAM_SOCK" -> sock) + ("AUK_TEAM_ID" -> id))))

  private def awaitMatch(ch: UnboundedChannel[String], pred: String => Boolean)(using Async): String =
    var found: String | Null = null
    while found == null do
      ch.read() match
        case Right(l) => if pred(l) then found = l
        case Left(_)  => fail("channel closed")
    found

  test("the lead worker: newMember returns a handle, listMembers includes the lead, and the fail-hard guards throw"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("lead", "ok", notices)
      start(bridge)
      val repl = leadRepl(bridge.socketPath)
      def eval(code: String) =
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        EvalScala(repl).execute(EvalScalaParams(code, Some(30_000)))
      try
        // newMember returns a handle whose toString renders id, description, status.
        val created = eval("""team.newMember("t", "tester").toString""")
        assert(!created.isError, created.output)
        assert(created.output.contains("Member(t") && created.output.contains("tester"), created.output)

        // Same-eval echo: a member created earlier in the eval is visible to listMembers,
        // which puts the lead first.
        val listed = eval("""{ team.newMember("z", "zz"); team.listMembers.map(_.id).mkString(",") }""")
        assert(!listed.isError, listed.output)
        assert(listed.output.contains("lead") && listed.output.contains("z"), listed.output)

        // Duplicate id (client-side guard, same session).
        val dup = eval("""team.newMember("t", "again")""")
        assert(dup.isError, dup.output)
        assert(dup.output.contains("duplicate member id 't'"), dup.output)

        // Reserved id.
        val reserved = eval("""team.newMember("lead", "nope")""")
        assert(reserved.isError, reserved.output)
        assert(reserved.output.contains("reserved for the main agent"), reserved.output)

        // Unknown member.
        val unknown = eval("""team.getMember("nobody")""")
        assert(unknown.isError, unknown.output)
        assert(unknown.output.contains("unknown team member 'nobody'"), unknown.output)

        // The lead cannot fetch its own lead handle.
        val leadHandle = eval("team.lead")
        assert(leadHandle.isError, leadHandle.output)
        assert(leadHandle.output.contains("you are the lead"), leadHandle.output)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  test("a member worker can reach the lead but cannot create members"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("member", "ok", notices)
      start(bridge)
      val repl = memberRepl(bridge.socketPath, "w1")
      def eval(code: String) =
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        EvalScala(repl).execute(EvalScalaParams(code, Some(30_000)))
      try
        val lead = eval("team.lead.id")
        assert(!lead.isError, lead.output)
        assert(lead.output.contains("lead"), lead.output)

        val create = eval("""team.newMember("x", "y")""")
        assert(create.isError, create.output)
        assert(create.output.contains("only the lead can create team members"), create.output)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())

  test("a member's lastResponse becomes visible in a later eval after the host broadcasts it"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("stale", "the result", notices)
      start(bridge)
      val repl = leadRepl(bridge.socketPath)
      def eval(code: String) =
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        EvalScala(repl).execute(EvalScalaParams(code, Some(30_000)))
      try
        // Create a member and message it; the send is fire-and-forget.
        val sent = eval("""team.newMember("s", "worker").sendMessage("do it")""")
        assert(!sent.isError, sent.output)
        // Wait for the host to finish the member's turn (it broadcasts the update first).
        awaitMatch(notices, _.contains("Team member 's' finished its turn"))
        // The mirror refreshes between evals, so a later eval sees the stored response.
        var out = ""
        var tries = 0
        while !out.contains("the result") && tries < 10 do
          out = eval("""team.getMember("s").lastResponse""").output
          tries += 1
        assert(out.contains("the result"), out)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())
