package auk.agent

import gears.async.{Async, Future, UnboundedChannel, ReadableChannel}
import gears.async.default.given
import auk.llm.endpoint.{
  Endpoint,
  LLMConfig,
  StreamEvent,
  Message,
  ChatResponse,
  FinishReason,
  LLMError
}
import auk.utils.Result
import java.util.concurrent.atomic.AtomicReference

class EngineSuite extends munit.FunSuite:

  /** An endpoint that replays a fixed script for every turn and records the
    * messages it was last asked to complete (so we can check history). */
  private class ScriptedEndpoint(script: List[Result[StreamEvent, LLMError]])
      extends Endpoint:
    val lastMessages = new AtomicReference[List[Message]](Nil)

    def invoke(
        messages: List[Message],
        config: LLMConfig
    ): Result[ChatResponse, LLMError] = Left(LLMError("not used"))

    def stream(messages: List[Message], config: LLMConfig)(using
        Async.Spawn
    ): ReadableChannel[Result[StreamEvent, LLMError]] =
      lastMessages.set(messages)
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        script.foreach(e => ch.send(e))
        ch.close()
      ch.asReadable

  private val cfg = LLMConfig(model = "test")

  private def done(text: String): Result[StreamEvent, LLMError] =
    Right(
      StreamEvent.Done(
        ChatResponse(Message.assistant(text), FinishReason.Stop, None)
      )
    )

  /** Drain the UI channel until a Done (or close), collecting the events seen. */
  private def drainTurn(
      events: UnboundedChannel[Result[StreamEvent, LLMError]]
  )(using Async): List[StreamEvent] =
    val seen = scala.collection.mutable.ListBuffer[StreamEvent]()
    var draining = true
    while draining do
      events.read() match
        case Right(Right(e @ StreamEvent.Done(_))) => seen += e; draining = false
        case Right(Right(e))  => seen += e
        case Right(Left(err)) => fail(s"unexpected error: ${err.description}")
        case Left(_)          => draining = false
    seen.toList

  test("a turn forwards thinking, text, and done events in order"):
    Async.blocking:
      val commands = UnboundedChannel[UserCommand]()
      val events = UnboundedChannel[Result[StreamEvent, LLMError]]()
      val endpoint = ScriptedEndpoint(
        List(
          Right(StreamEvent.ThinkingDelta("hmm ")),
          Right(StreamEvent.Delta("hi")),
          done("hi")
        )
      )
      Future(Engine(commands.asReadable, events.asSendable, endpoint, cfg).run())

      commands.sendImmediately(UserCommand.Submit("hello"))
      assertEquals(
        drainTurn(events),
        List(
          StreamEvent.ThinkingDelta("hmm "),
          StreamEvent.Delta("hi"),
          StreamEvent.Done(
            ChatResponse(Message.assistant("hi"), FinishReason.Stop, None)
          )
        )
      )
      commands.close()

  test("history carries across turns"):
    Async.blocking:
      val commands = UnboundedChannel[UserCommand]()
      val events = UnboundedChannel[Result[StreamEvent, LLMError]]()
      val endpoint =
        ScriptedEndpoint(List(Right(StreamEvent.Delta("ok")), done("ok")))
      Future(Engine(commands.asReadable, events.asSendable, endpoint, cfg).run())

      commands.sendImmediately(UserCommand.Submit("q1"))
      drainTurn(events)
      commands.sendImmediately(UserCommand.Submit("q2"))
      drainTurn(events)

      assertEquals(
        endpoint.lastMessages.get(),
        List(Message.user("q1"), Message.assistant("ok"), Message.user("q2"))
      )
      commands.close()
