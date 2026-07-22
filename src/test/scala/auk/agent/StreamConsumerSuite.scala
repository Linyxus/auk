package auk.agent

import scala.collection.mutable.ListBuffer

import gears.async.{Async, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.llm.endpoint.{ChatResponse, FinishReason, LLMError, Message, StreamEvent}
import auk.utils.Result

/** The shared retry loop around a streamed model turn ([[StreamConsumer.collectRetrying]]):
  * transient failures ride the backoff schedule, permanent ones surface at once,
  * and the terminal error reaches `onError` exactly once. */
class StreamConsumerSuite extends munit.FunSuite:

  private def asyncTest(name: String)(body: Async.Spawn ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  private def doneResp(text: String): ChatResponse =
    ChatResponse(Message.assistant(text), FinishReason.Stop)

  /** One scripted attempt: a channel pre-loaded with `events`. Left open — real
    * endpoints end on a terminal event; the close-without-terminal case builds
    * its channel by hand. */
  private def attempt(events: List[Result[StreamEvent, LLMError]]): ReadableChannel[Result[StreamEvent, LLMError]] =
    val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
    events.foreach(ch.sendImmediately)
    ch.asReadable

  asyncTest("a transient failure retries on the schedule until an attempt succeeds"):
    var opens = 0
    val retries = ListBuffer.empty[(Int, Int, Long, String)]
    val deltas = ListBuffer.empty[String]
    var errors = List.empty[LLMError]
    val r = StreamConsumer.collectRetrying(
      open = () =>
        opens += 1
        if opens < 3 then attempt(List(Right(StreamEvent.Delta(s"try$opens")), Left(LLMError(s"blip$opens", transient = true))))
        else attempt(List(Right(StreamEvent.Delta("ok")), Right(StreamEvent.Done(doneResp("ok"))))),
      onEvent = { case StreamEvent.Delta(t) => deltas += t; case _ => () },
      onError = e => errors ::= e,
      onRetry = (a, m, d, e) => retries += ((a, m, d, e.description)),
      delaysMs = List(1L, 2L, 4L)
    )
    assertEquals(r.map(_.message.text), Some("ok"))
    assertEquals(opens, 3)
    assertEquals(errors, Nil)
    // Each wait reports the attempt that died, the schedule size, and its delay.
    assertEquals(retries.toList, List((1, 4, 1L, "blip1"), (2, 4, 2L, "blip2")))
    // Every attempt's events were forwarded — rewinding the dead partial is the
    // caller's job, signalled by onRetry.
    assertEquals(deltas.toList, List("try1", "try2", "ok"))

  asyncTest("a permanent failure surfaces at once, no retry"):
    var opens = 0
    var retried = false
    var errors = List.empty[LLMError]
    val r = StreamConsumer.collectRetrying(
      open = () =>
        opens += 1
        attempt(List(Left(LLMError("401 invalid api key")))),
      onEvent = _ => (),
      onError = e => errors ::= e,
      onRetry = (_, _, _, _) => retried = true,
      delaysMs = List(1L, 1L)
    )
    assertEquals(r, None)
    assertEquals(opens, 1)
    assert(!retried)
    assertEquals(errors.map(_.description), List("401 invalid api key"))

  asyncTest("an exhausted schedule surfaces the last error exactly once"):
    var opens = 0
    val retries = ListBuffer.empty[Int]
    var errors = List.empty[LLMError]
    val r = StreamConsumer.collectRetrying(
      open = () =>
        opens += 1
        attempt(List(Left(LLMError(s"outage $opens", transient = true)))),
      onEvent = _ => (),
      onError = e => errors ::= e,
      onRetry = (a, _, _, _) => retries += a,
      delaysMs = List(0L, 0L)
    )
    assertEquals(r, None)
    assertEquals(opens, 3) // initial attempt + one per schedule entry
    assertEquals(retries.toList, List(1, 2))
    assertEquals(errors.map(_.description), List("outage 3"))

  asyncTest("a stream that closes without a terminal event is transient and retried"):
    var opens = 0
    val retries = ListBuffer.empty[String]
    val r = StreamConsumer.collectRetrying(
      open = () =>
        opens += 1
        if opens == 1 then
          val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
          ch.close() // dropped connection: nothing terminal ever arrives
          ch.asReadable
        else attempt(List(Right(StreamEvent.Done(doneResp("recovered"))))),
      onEvent = _ => (),
      onError = _ => fail("the retried turn should not error"),
      onRetry = (_, _, _, e) => retries += e.description,
      delaysMs = List(0L)
    )
    assertEquals(r.map(_.message.text), Some("recovered"))
    assertEquals(opens, 2)
    assertEquals(retries.toList, List("the model stream ended unexpectedly"))
