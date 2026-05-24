package auk.agent

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given
import auk.llm.endpoint.{StreamEvent, FinishReason, LLMError}
import auk.utils.Result

class EngineSuite extends munit.FunSuite:

  test("Submit echoes the text as Deltas followed by a Done"):
    Async.blocking:
      val commands = UnboundedChannel[UserCommand]()
      val events = UnboundedChannel[Result[StreamEvent, LLMError]]()
      // chunkDelayMs = 0 so the test doesn't sleep.
      Future(Engine(commands.asReadable, events.asSendable, chunkDelayMs = 0).run())

      commands.sendImmediately(UserCommand.Submit("hello world"))

      val collected = scala.collection.mutable.ListBuffer[StreamEvent]()
      var draining = true
      while draining do
        events.read() match
          case Right(Right(e @ StreamEvent.Done(_))) => collected += e; draining = false
          case Right(Right(e))                       => collected += e
          case Right(Left(err)) => fail(s"unexpected error: ${err.description}")
          case Left(_)          => draining = false // channel closed

      val streamed = collected.collect { case StreamEvent.Delta(t) => t }.mkString
      assertEquals(streamed, "hello world")

      collected.last match
        case StreamEvent.Done(response) =>
          assertEquals(response.message.text, "hello world")
          assertEquals(response.finishReason, FinishReason.Stop)
        case other => fail(s"expected a Done event last, got $other")

      commands.close() // lets the engine's run loop exit

  test("poll-based draining (the TUI bridge) observes the engine's events"):
    // Mirrors ChatApp.drainInbound: a non-Async consumer pulls events off the
    // channel with readSource.poll(), exactly as the layoutz thread does.
    Async.blocking:
      val commands = UnboundedChannel[UserCommand]()
      val events = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(Engine(commands.asReadable, events.asSendable, chunkDelayMs = 0).run())

      commands.sendImmediately(UserCommand.Submit("hello world"))

      val src = events.readSource
      val collected = scala.collection.mutable.ListBuffer[StreamEvent]()
      var done = false
      var spins = 0
      while !done && spins < 5000 do
        src.poll() match
          case Some(Right(Right(e @ StreamEvent.Done(_)))) => collected += e; done = true
          case Some(Right(Right(e)))  => collected += e
          case Some(Right(Left(err))) => fail(s"unexpected error: ${err.description}")
          case Some(Left(_))          => done = true // channel closed
          case None => spins += 1; Thread.sleep(1) // wait for the async producer

      assert(done, "poll() never observed the engine's Done event")
      assertEquals(
        collected.collect { case StreamEvent.Delta(t) => t }.mkString,
        "hello world"
      )
      commands.close()
