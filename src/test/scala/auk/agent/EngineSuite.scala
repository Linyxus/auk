package auk.agent

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.collection.mutable.ListBuffer

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

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
    ): Result[ChatResponse, LLMError] =
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
      Future:
        events.foreach(ch.send)
        ch.close()
      ch.asReadable

  private def tempDir(): Path =
    Files.createTempDirectory("auk-engine").nn

  private def session(dir: Path): Session =
    SessionProvider.directory(dir.resolve(SessionProvider.RelativePath).nn).create().toOption.get

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
      out: ReadableChannel[Result[StreamEvent, LLMError]]
  )(using Async): List[Result[StreamEvent, LLMError]] =
    var acc = List.empty[Result[StreamEvent, LLMError]]
    var running = true
    while running do
      out.read() match
        case Left(_) =>
          running = false
        case Right(result) =>
          acc = acc :+ result
          result match
            case Left(_)                       => running = false
            case Right(StreamEvent.Done(_))    => running = false
            case Right(_)                      => ()
    acc

  private def withEngine(
      session: Session,
      scripts: List[List[Result[StreamEvent, LLMError]]],
      registry: ToolRegistry = ToolRegistry.of()
  )(
      body: (
          UnboundedChannel[UserCommand],
          ReadableChannel[Result[StreamEvent, LLMError]],
          ScriptedStreamEndpoint,
          Async
      ) => Unit
  ): Unit =
    val in = UnboundedChannel[UserCommand]()
    val out = UnboundedChannel[Result[StreamEvent, LLMError]]()
    val endpoint = ScriptedStreamEndpoint(scripts)
    val context = RuntimeContext(tempDir())
    val worker =
      new Thread(
        () =>
          Async.blocking:
            try
              Engine(
                in.asReadable,
                out.asSendable,
                endpoint,
                LLMConfig(model = "test-model"),
                session,
                registry,
                context
              ).run()
            catch
              case e: Throwable =>
                out.send(Left(LLMError(s"engine test failure: ${e.getClass.getSimpleName}: ${e.getMessage}")))
        ,
        "engine-suite-worker"
      )
    worker.setDaemon(true)
    worker.start()
    try
      Async.blocking:
        body(in, out.asReadable, endpoint, summon[Async])
    finally
      in.close()
      worker.join(1000)
      out.close()

  test("persists user and final assistant events in order"):
    val s = session(tempDir())
    val assistant = textResponse("hello back").message

    withEngine(s, List(List(done(ChatResponse(assistant, FinishReason.Stop))))): (in, out, _, async) =>
      given Async = async
      in.sendImmediately(UserCommand.Submit("hello"))
      val events = readUntilTerminal(out)
      assertEquals(events.collect { case Right(StreamEvent.Done(r)) => r.message }, List(assistant))

    assertEquals(
      s.events,
      Right(List(
        SessionEvent.UserSubmitted("hello"),
        SessionEvent.AssistantResponded(assistant)
      ))
    )

  test("persists tool loop events in order"):
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
      assertEquals(events.exists { case Right(StreamEvent.ToolRunStart("t1", "echo")) => true; case _ => false }, true)
      assertEquals(events.collect { case Right(StreamEvent.Done(r)) => r.message }, List(finalMessage))

    assertEquals(
      s.events,
      Right(List(
        SessionEvent.UserSubmitted("use echo"),
        SessionEvent.AssistantResponded(tool),
        SessionEvent.ToolResultsReceived(List(Content.ToolResult("t1", "pong"))),
        SessionEvent.AssistantResponded(finalMessage)
      ))
    )

  test("replays existing session events into the first model call"):
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

  test("reports user append failure without calling the endpoint"):
    val parentFile = tempDir().resolve("not-a-directory").nn
    Files.writeString(parentFile, "not a directory", UTF_8)
    val badSession = Session("bad", parentFile.resolve("session.jsonl").nn)

    withEngine(badSession, List(List(done(textResponse("unused"))))): (in, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.Submit("hello"))
      val events = readUntilTerminal(out)
      assert(events.headOption.exists {
        case Left(err) => err.description.contains("Session persistence error")
        case Right(_)  => false
      })
      assertEquals(endpoint.seen.size, 0)

  test("reports corrupt startup session without calling the endpoint"):
    val dir = tempDir()
    val s = session(dir)
    assertEquals(s.append(SessionEvent.UserSubmitted("ok")), Right(()))
    val logFile = dir.resolve(SessionProvider.RelativePath).nn.resolve(s.id + ".jsonl").nn
    Files.writeString(logFile, "{ not json\n", UTF_8, java.nio.file.StandardOpenOption.APPEND)

    withEngine(s, List(List(done(textResponse("unused"))))): (_, out, endpoint, async) =>
      given Async = async
      val events = readUntilTerminal(out)
      assert(events.headOption.exists {
        case Left(err) => err.description.contains("Session persistence error")
        case Right(_)  => false
      })
      assertEquals(endpoint.seen.size, 0)
