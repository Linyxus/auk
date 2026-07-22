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
    val ch = Endpoint.streaming("Test API error"): (_, _) =>
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
    val ch = Endpoint.streaming("Test API error"): (out, _) =>
      sleep(1)
      out.send(Right(StreamEvent.Done(response)))
    ch.read() match
      case Right(Right(StreamEvent.Done(r))) => assertEquals(r.message, response.message)
      case other                             => fail(s"expected a Done event, got $other")
    assert(ch.read().isLeft, "channel must be closed once the producer has exited")

  // ---- transient-vs-permanent classification (the retry gate) ----------------

  private def jsError(status: Option[Int]): Throwable =
    import scala.scalajs.js
    val err = js.Dynamic.literal(message = "boom")
    status.foreach(s => err.status = s)
    js.JavaScriptException(err)

  test("SDK errors classify by HTTP status: 408/429/5xx retry, 4xx fail fast"):
    assert(Endpoint.isTransient(jsError(Some(429))))
    assert(Endpoint.isTransient(jsError(Some(408))))
    assert(Endpoint.isTransient(jsError(Some(500))))
    assert(Endpoint.isTransient(jsError(Some(529))))
    assert(!Endpoint.isTransient(jsError(Some(400))))
    assert(!Endpoint.isTransient(jsError(Some(401))))
    assert(!Endpoint.isTransient(jsError(Some(403))))
    assert(!Endpoint.isTransient(jsError(Some(404))))

  test("a failure with no HTTP status (dropped connection, stall timeout) is transient"):
    assert(Endpoint.isTransient(jsError(None)))
    assert(Endpoint.isTransient(new RuntimeException("socket hang up")))

  asyncTest("a throwing producer's error event carries its transience"):
    val ch = Endpoint.streaming("Test API error"): (_, _) =>
      sleep(1)
      throw jsError(Some(401))
    ch.read() match
      case Right(Left(err)) => assert(!err.transient, "a 401 must not be retried")
      case other            => fail(s"expected an error event, got $other")
    val ch2 = Endpoint.streaming("Test API error"): (_, _) =>
      sleep(1)
      throw jsError(Some(503))
    ch2.read() match
      case Right(Left(err)) => assert(err.transient, "a 503 must be retried")
      case other            => fail(s"expected an error event, got $other")
