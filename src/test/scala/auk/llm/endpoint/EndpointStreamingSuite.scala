package auk.llm.endpoint

import gears.async.Async
import gears.async.AsyncOperations.sleep
import gears.async.default.given

/** The contract `Engine.streamTurn` relies on to never hang: a stream channel is
  * ALWAYS closed when its producer exits, and any failure is surfaced as an
  * error event first.
  *
  * Each producer yields (`sleep`) before sending, mirroring a real endpoint that
  * `await`s the network before each event — that hand-off is what lets the
  * consumer be a registered waiting reader (so events are delivered directly
  * rather than buffered-then-discarded by the close, per gears' channel
  * semantics). */
class EndpointStreamingSuite extends munit.FunSuite:

  private def asyncTest(name: String)(body: Async.Spawn ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  asyncTest("a producer that throws surfaces a labelled error, then closes the channel"):
    val ch = Endpoint.streaming("Test API error"): _ =>
      sleep(1)
      throw new RuntimeException("kaboom")
    ch.read() match
      case Right(Left(err)) =>
        assert(err.description.contains("Test API error"), err.description)
        assert(err.description.contains("kaboom"), err.description)
      case other => fail(s"expected an error event, got $other")
    assert(ch.read().isLeft, "channel must be closed once the producer has exited")

  asyncTest("a producer that completes normally delivers its events, then closes"):
    val response = ChatResponse(Message.assistant("hi"), FinishReason.Stop)
    val ch = Endpoint.streaming("Test API error"): out =>
      sleep(1)
      out.send(Right(StreamEvent.Done(response)))
    ch.read() match
      case Right(Right(StreamEvent.Done(r))) => assertEquals(r.message, response.message)
      case other                             => fail(s"expected a Done event, got $other")
    assert(ch.read().isLeft, "channel must be closed once the producer has exited")
