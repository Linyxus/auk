package auk.agent

import scala.collection.mutable.ListBuffer

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.TestFs

import auk.llm.endpoint.{
  ChatResponse,
  Content,
  Endpoint,
  FinishReason,
  LLMConfig,
  LLMError,
  Message,
  Role,
  StreamEvent
}
import auk.llm.provider.{ActiveModel, ModelSession}
import auk.llm.tools.RuntimeContext
import auk.runtime.{Echo, ToolRegistry}
import auk.session.{Session, SessionEvent, SessionProvider}
import auk.utils.Result

class EngineSuite extends munit.FunSuite:

  private final class ScriptedStreamEndpoint(
      scripts: List[List[Result[StreamEvent, LLMError]]]
  ) extends Endpoint:
    private var idx = 0
    val seen: ListBuffer[List[Message]] = ListBuffer.empty

    def invoke(
        messages: List[Message],
        config: LLMConfig
    )(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("ScriptedStreamEndpoint does not invoke"))

    def stream(messages: List[Message], config: LLMConfig)(using
        Async.Spawn
    ): ReadableChannel[Result[StreamEvent, LLMError]] =
      seen += messages
      val events =
        if idx >= scripts.length then List(Left(LLMError("scripted endpoint exhausted")))
        else
          val next = scripts(idx)
          idx += 1
          next
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      // Do NOT close the channel: gears' pollRead checks isClosed before the
      // buffer, so closing would discard buffered-but-unread events when the
      // producer races ahead of the engine. Real endpoints likewise rely on a
      // terminal Done/error event rather than close. Every script ends with one.
      Future:
        events.foreach(ch.send)
      ch.asReadable

  private def tempDir(): String =
    TestFs.tempDir("auk-engine")

  private def provider(dir: String): SessionProvider =
    SessionProvider.directory(TestFs.join(dir, SessionProvider.RelativePath))

  private def session(dir: String): Session =
    provider(dir).create().toOption.get

  private def textResponse(text: String): ChatResponse =
    ChatResponse(Message.assistant(text), FinishReason.Stop)

  private def toolResponse(id: String, name: String, input: String): ChatResponse =
    ChatResponse(
      Message(Role.Assistant, List(Content.ToolUse(id, name, input))),
      FinishReason.ToolUse
    )

  private def done(response: ChatResponse): Result[StreamEvent, LLMError] =
    Right(StreamEvent.Done(response))

  private def readUntilTerminal(
      out: ReadableChannel[AgentEvent]
  )(using Async): List[AgentEvent] =
    var acc = List.empty[AgentEvent]
    var running = true
    while running do
      out.read() match
        case Left(_) =>
          running = false
        case Right(event) =>
          acc = acc :+ event
          event match
            case AgentEvent.Stream(Left(_))                    => running = false
            case AgentEvent.Stream(Right(StreamEvent.Done(_))) => running = false
            case _                                             => ()
    acc

  private def readAgentEvent(out: ReadableChannel[AgentEvent])(using Async): AgentEvent =
    out.read() match
      case Right(event) => event
      case Left(err)    => fail(s"event channel closed: $err")

  private def withEngine(
      session: Session,
      scripts: List[List[Result[StreamEvent, LLMError]]],
      registry: ToolRegistry = ToolRegistry.of(),
      sessions: SessionProvider = provider(tempDir())
  )(
      body: (
          UnboundedChannel[UserCommand],
          ReadableChannel[AgentEvent],
          ScriptedStreamEndpoint,
          Async
      ) => Unit
  )(using Async.Spawn): Unit =
    val in = UnboundedChannel[UserCommand]()
    val out = UnboundedChannel[AgentEvent]()
    val endpoint = ScriptedStreamEndpoint(scripts)
    val context = RuntimeContext(tempDir())
    val worker =
      Future:
        try
          Engine(
            in.asReadable,
            out.asSendable,
            ModelSession.of(endpoint, LLMConfig(model = "test-model")),
            session,
            sessions,
            registry,
            context
          ).run()
        catch
          case e: Throwable =>
            out.send(AgentEvent.Stream(Left(LLMError(s"engine test failure: ${e.getClass.getSimpleName}: ${e.getMessage}"))))
    try body(in, out.asReadable, endpoint, summon[Async])
    finally
      in.close()
      worker.await
      out.close()

  private def asyncTest(name: String)(body: Async.Spawn ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  asyncTest("persists user and final assistant events in order"):
    val s = session(tempDir())
    val assistant = textResponse("hello back").message

    withEngine(s, List(List(done(ChatResponse(assistant, FinishReason.Stop))))): (in, out, _, async) =>
      given Async = async
      in.sendImmediately(UserCommand.Submit("hello"))
      val events = readUntilTerminal(out)
      assertEquals(events.collect { case AgentEvent.Stream(Right(StreamEvent.Done(r))) => r.message }, List(assistant))

    assertEquals(
      s.events,
      Right(List(
        SessionEvent.UserSubmitted("hello"),
        SessionEvent.AssistantResponded(assistant)
      ))
    )

  asyncTest("persists tool loop events in order"):
    val s = session(tempDir())
    val tool = toolResponse("t1", "echo", """{"text":"pong"}""").message
    val finalMessage = Message.assistant("done")

    withEngine(
      s,
      List(
        List(done(ChatResponse(tool, FinishReason.ToolUse))),
        List(done(ChatResponse(finalMessage, FinishReason.Stop)))
      ),
      registry = ToolRegistry.of(Echo)
    ): (in, out, _, async) =>
      given Async = async
      in.sendImmediately(UserCommand.Submit("use echo"))
      val events = readUntilTerminal(out)
      assertEquals(events.exists {
        case AgentEvent.Stream(Right(StreamEvent.ToolRunStart("t1", "echo"))) => true
        case _                                                               => false
      }, true)
      assertEquals(events.collect { case AgentEvent.Stream(Right(StreamEvent.Done(r))) => r.message }, List(finalMessage))

    assertEquals(
      s.events,
      Right(List(
        SessionEvent.UserSubmitted("use echo"),
        SessionEvent.AssistantResponded(tool),
        SessionEvent.ToolResultsReceived(List(Content.ToolResult("t1", "pong"))),
        SessionEvent.AssistantResponded(finalMessage)
      ))
    )

  asyncTest("replays existing session events into the first model call"):
    val s = session(tempDir())
    val priorAssistant = Message.assistant("previous answer")
    val priorResult: Content.ToolResult = Content.ToolResult("old_tool", "old result")
    List(
      SessionEvent.UserSubmitted("previous question"),
      SessionEvent.AssistantResponded(priorAssistant),
      SessionEvent.ToolResultsReceived(List(priorResult))
    ).foreach(ev => assertEquals(s.append(ev), Right(())))

    withEngine(s, List(List(done(textResponse("next answer"))))): (in, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.Submit("next question"))
      readUntilTerminal(out)
      assertEquals(
        endpoint.seen.head,
        List(
          Message.user("previous question"),
          priorAssistant,
          Message(Role.User, List(priorResult)),
          Message.user("next question")
        )
      )

  asyncTest("lists resumable sessions with summaries"):
    val dir = tempDir()
    val p = provider(dir)
    val initial = p.create().toOption.get
    val prior = p.create().toOption.get
    prior.append(SessionEvent.UserSubmitted("pick me"))
    prior.append(SessionEvent.AssistantResponded(Message.assistant("prior answer")))

    withEngine(initial, Nil, sessions = p): (in, out, _, async) =>
      given Async = async
      in.sendImmediately(UserCommand.ListSessions)
      readAgentEvent(out) match
        case AgentEvent.SessionsListed(sessions) =>
          assert(sessions.exists(s => s.id == prior.id && s.preview == "pick me"), sessions)
        case other => fail(s"expected SessionsListed, got $other")

  asyncTest("resumes a selected session and uses its history on the next prompt"):
    val dir = tempDir()
    val p = provider(dir)
    val initial = p.create().toOption.get
    val target = p.create().toOption.get
    val priorAssistant = Message.assistant("old answer")
    target.append(SessionEvent.UserSubmitted("old question"))
    target.append(SessionEvent.AssistantResponded(priorAssistant))

    withEngine(initial, List(List(done(textResponse("new answer")))), sessions = p): (in, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.ResumeSession(target.id))
      readAgentEvent(out) match
        case AgentEvent.SessionSwitched(snapshot) =>
          assertEquals(snapshot.summary.id, target.id)
          assertEquals(snapshot.events.collect { case SessionEvent.UserSubmitted(text) => text }, List("old question"))
        case other => fail(s"expected SessionSwitched, got $other")

      in.sendImmediately(UserCommand.Submit("next question"))
      readUntilTerminal(out)
      assertEquals(
        endpoint.seen.head,
        List(
          Message.user("old question"),
          priorAssistant,
          Message.user("next question")
        )
      )

  asyncTest("new session clears model history before the next prompt"):
    val dir = tempDir()
    val p = provider(dir)
    val initial = p.create().toOption.get
    initial.append(SessionEvent.UserSubmitted("old question"))
    initial.append(SessionEvent.AssistantResponded(Message.assistant("old answer")))
    var newId = ""

    withEngine(initial, List(List(done(textResponse("fresh answer")))), sessions = p): (in, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.NewSession)
      readAgentEvent(out) match
        case AgentEvent.SessionSwitched(snapshot) =>
          newId = snapshot.summary.id
          assertEquals(snapshot.events, Nil)
          assertEquals(snapshot.summary.preview, "Empty session")
        case other => fail(s"expected SessionSwitched, got $other")

      in.sendImmediately(UserCommand.Submit("fresh prompt"))
      readUntilTerminal(out)
      assertEquals(endpoint.seen.head, List(Message.user("fresh prompt")))

    val reopened = p.open(newId).toOption.flatten.get
    assertEquals(
      reopened.events.toOption.get.collect { case SessionEvent.UserSubmitted(text) => text },
      List("fresh prompt")
    )

  asyncTest("failed resume reports an error and keeps the current session"):
    val dir = tempDir()
    val p = provider(dir)
    val initial = p.create().toOption.get
    val priorAssistant = Message.assistant("still here")
    initial.append(SessionEvent.UserSubmitted("current question"))
    initial.append(SessionEvent.AssistantResponded(priorAssistant))

    withEngine(initial, List(List(done(textResponse("answer")))), sessions = p): (in, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.ResumeSession("missing"))
      readAgentEvent(out) match
        case AgentEvent.Stream(Left(err)) =>
          assert(err.description.contains("Session persistence error"), err.description)
        case other => fail(s"expected persistence error, got $other")

      in.sendImmediately(UserCommand.Submit("next"))
      readUntilTerminal(out)
      assertEquals(
        endpoint.seen.head,
        List(Message.user("current question"), priorAssistant, Message.user("next"))
      )

  asyncTest("reports user append failure without calling the endpoint"):
    val parentFile = TestFs.join(tempDir(), "not-a-directory")
    TestFs.write(parentFile, "not a directory")
    val badSession = Session("bad", TestFs.join(parentFile, "session.jsonl"))

    withEngine(badSession, List(List(done(textResponse("unused"))))): (in, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.Submit("hello"))
      val events = readUntilTerminal(out)
      assert(events.headOption.exists {
        case AgentEvent.Stream(Left(err)) => err.description.contains("Session persistence error")
        case _                            => false
      })
      assertEquals(endpoint.seen.size, 0)

  asyncTest("reports corrupt startup session without calling the endpoint"):
    val dir = tempDir()
    val s = session(dir)
    assertEquals(s.append(SessionEvent.UserSubmitted("ok")), Right(()))
    val logFile = TestFs.join(TestFs.join(dir, SessionProvider.RelativePath), s.id + ".jsonl")
    TestFs.append(logFile, "{ not json\n")

    withEngine(s, List(List(done(textResponse("unused"))))): (_, out, endpoint, async) =>
      given Async = async
      val events = readUntilTerminal(out)
      assert(events.headOption.exists {
        case AgentEvent.Stream(Left(err)) => err.description.contains("Session persistence error")
        case _                            => false
      })
      assertEquals(endpoint.seen.size, 0)

  asyncTest("switching the model swaps the active model, emits ModelSwitched, and persists"):
    val s = session(tempDir())
    val in = UnboundedChannel[UserCommand]()
    val out = UnboundedChannel[AgentEvent]()
    val ep1 = ScriptedStreamEndpoint(Nil)
    val ep2 = ScriptedStreamEndpoint(Nil)
    val models = ModelSession(
      ActiveModel(ep1, LLMConfig(model = "m1"), "Model One"),
      (_, id) =>
        if id == "m2" then Right(ActiveModel(ep2, LLMConfig(model = "m2"), "Model Two"))
        else Left(s"no such model '$id'")
    )
    var persisted: Option[(String, String)] = None
    val persist: (String, String) => Either[String, Unit] = (pk, id) =>
      persisted = Some((pk, id)); Right(())
    val worker = Future:
      Engine(in.asReadable, out.asSendable, models, s, provider(tempDir()),
        ToolRegistry.of(), RuntimeContext(tempDir()), persist).run()
    in.sendImmediately(UserCommand.SwitchModel("openrouter", "m2"))
    assertEquals(readAgentEvent(out.asReadable), AgentEvent.ModelSwitched("Model Two"))
    assertEquals(models.active.label, "Model Two")
    assertEquals(persisted, Some(("openrouter", "m2")))
    in.close(); worker.await; out.close()

  asyncTest("an invalid model switch reports an error and keeps the current model"):
    val s = session(tempDir())
    val in = UnboundedChannel[UserCommand]()
    val out = UnboundedChannel[AgentEvent]()
    val models = ModelSession.of(ScriptedStreamEndpoint(Nil), LLMConfig(model = "m1"), "Model One")
    val worker = Future:
      Engine(in.asReadable, out.asSendable, models, s, provider(tempDir()),
        ToolRegistry.of(), RuntimeContext(tempDir())).run()
    in.sendImmediately(UserCommand.SwitchModel("x", "bad"))
    readAgentEvent(out.asReadable) match
      case AgentEvent.Stream(Left(err)) => assert(err.description.contains("Could not switch model"), err.description)
      case other                        => fail(s"expected a switch error, got $other")
    assertEquals(models.active.label, "Model One")
    in.close(); worker.await; out.close()
