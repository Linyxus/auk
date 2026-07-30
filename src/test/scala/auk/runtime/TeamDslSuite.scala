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

  test("a member worker can reach the lead but cannot create or retire members"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("member", "ok", notices)
      start(bridge)
      // A lead worker alongside it, only to create the member whose identity the
      // worker below carries: retiring needs a real member in the worker's mirror.
      val leadSide = leadRepl(bridge.socketPath)
      val repl = memberRepl(bridge.socketPath, "w1")
      def evalIn(r: ScalaRepl, code: String) =
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        EvalScala(r).execute(EvalScalaParams(code, Some(30_000)))
      def eval(code: String) = evalIn(repl, code)
      try
        val created = evalIn(leadSide, """team.newMember("w1", "worker").id""")
        assert(!created.isError, created.output)

        val lead = eval("team.lead.id")
        assert(!lead.isError, lead.output)
        assert(lead.output.contains("lead"), lead.output)

        val create = eval("""team.newMember("x", "y")""")
        assert(create.isError, create.output)
        assert(create.output.contains("only the lead can create team members"), create.output)

        // Retiring is the lead's alone as well. The roster mirror only advances
        // between evals, so retry until this worker has seen the member it is
        // trying to retire (before that, the id is simply unknown to it).
        var out = ""
        var tries = 0
        while !out.contains("only the lead can retire") && tries < 10 do
          out = eval("""team.getMember("w1").retire()""").output
          tries += 1
        assert(out.contains("only the lead can retire team members"), out)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(leadSide.close())
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

  test("the lead worker: retire closes a member down, and the retired handle still reads back"):
    assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl`")
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("retire", "the answer", notices)
      start(bridge)
      val repl = leadRepl(bridge.socketPath)
      def eval(code: String) =
        given RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)
        EvalScala(repl).execute(EvalScalaParams(code, Some(30_000)))
      try
        // A member that has answered once, so there is a lastResponse to keep.
        val sent = eval("""team.newMember("r", "worker").sendMessage("do it")""")
        assert(!sent.isError, sent.output)
        awaitMatch(notices, _.contains("Team member 'r' finished its turn"))

        // The local echo makes the retirement visible in the eval that did it.
        val retired = eval("""{ val m = team.getMember("r"); m.retire(); (m.status.toString, m.toString) }""")
        assert(!retired.isError, retired.output)
        assert(retired.output.contains("Retired"), retired.output)
        assert(retired.output.contains("Member(r: worker, retired)"), retired.output)

        // Retiring twice fails, and so does messaging what has been retired.
        val again = eval("""team.getMember("r").retire()""")
        assert(again.isError, again.output)
        assert(again.output.contains("has already been retired"), again.output)

        val msg = eval("""team.getMember("r").sendMessage("one more thing")""")
        assert(msg.isError, msg.output)
        assert(msg.output.contains("can no longer be messaged"), msg.output)

        // The lead is not retirable.
        val lead = eval("""team.getMember("lead").retire()""")
        assert(lead.isError, lead.output)
        assert(lead.output.contains("the lead cannot be retired"), lead.output)

        // The record is kept, so the member's final answer outlives it.
        var out = ""
        var tries = 0
        while !out.contains("the answer") && tries < 10 do
          out = eval("""team.getMember("r").lastResponse""").output
          tries += 1
        assert(out.contains("the answer"), out)
      finally
        Async.fromSync(repl.close())
        Async.fromSync(bridge.close())
