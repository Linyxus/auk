package auk.agent

import gears.async.{Async, ReadableChannel}

import auk.llm.endpoint.{ChatResponse, Endpoint, LLMError, StreamEvent}
import auk.platform.js.Interop
import auk.utils.Result

/** Drains a model's streaming response down to its terminal [[ChatResponse]].
  *
  * An endpoint stream yields a run of incremental events (deltas, reasoning,
  * tool-call fragments) ending in exactly one `Done` that carries the assembled
  * response — or a terminal error, or, on an abnormally dropped connection, a
  * bare channel close with nothing terminal at all. The interactive [[Engine]]
  * and the headless [[auk.runtime.HeadlessAgent]] consume a stream the same way and
  * differ only in what they do with each event (forward it to a UI vs. ignore
  * it) and how they surface a failure. That shared draining lives here so a turn
  * is read identically on both paths, with the per-path behaviour injected as
  * callbacks — including the retry-on-transient-failure loop, so every consumer
  * rides out rate limits and provider blips the same way.
  */
object StreamConsumer:
  /** Read `upstream` to completion. Every non-terminal event is handed to
    * `onEvent`; a terminal error — or an unexpected close that delivered no
    * `Done` — is handed to `onError`. Returns the final response, or `None` if
    * the stream errored or closed without one. The `Done` is captured, not
    * forwarded to `onEvent`, so the caller decides what to do with it (run the
    * tools it requested, or surface it as the turn's end). */
  def collect(
      upstream: ReadableChannel[Result[StreamEvent, LLMError]],
      onEvent: StreamEvent => Unit,
      onError: LLMError => Unit
  )(using Async): Option[ChatResponse] =
    var response: Option[ChatResponse] = None
    var streaming = true
    while streaming do
      upstream.read() match
        case Left(_) =>
          // The channel closed without ever delivering a Done or an error event
          // (the endpoint's `finally` close backstop, or an abnormally dropped
          // stream). A terminal event would have stopped the loop above, so a
          // close here means nothing terminal arrived: surface it rather than
          // returning silently, which would leave a UI waiting forever.
          // Connection-level, so worth retrying.
          onError(LLMError("the model stream ended unexpectedly", transient = true))
          streaming = false
        case Right(Right(StreamEvent.Done(r))) =>
          response = Some(r)
          streaming = false
        case Right(Left(err)) =>
          onError(err)
          streaming = false
        case Right(Right(event)) =>
          onEvent(event)
    response
  end collect

  /** [[collect]] wrapped in the shared retry loop: `open` issues a fresh request
    * per attempt, and a transient failure ([[LLMError.transient]]) is retried
    * after the next `delaysMs` backoff step instead of surfacing. Only once the
    * schedule is exhausted — or on a permanent failure — is the error handed to
    * `onError`, exactly once.
    *
    * `onRetry(attempt, maxAttempts, delayMs, error)` fires before each backoff
    * wait, so a caller can rewind whatever partial output the failed attempt
    * already pushed through `onEvent` (a retried round re-streams from the
    * start) and surface the wait to its UI. The wait itself suspends the fiber
    * cancellably: an interrupt mid-backoff unwinds like an interrupt mid-stream. */
  def collectRetrying(
      open: () => ReadableChannel[Result[StreamEvent, LLMError]],
      onEvent: StreamEvent => Unit,
      onError: LLMError => Unit,
      onRetry: (Int, Int, Long, LLMError) => Unit = (_, _, _, _) => (),
      delaysMs: List[Long] = Endpoint.RetryDelaysMs
  )(using Async): Option[ChatResponse] =
    val maxAttempts = delaysMs.length + 1
    var remaining = delaysMs
    var attempt = 1
    var response: Option[ChatResponse] = None
    var trying = true
    while trying do
      var failure: Option[LLMError] = None
      response = collect(open(), onEvent, err => failure = Some(err))
      failure match
        case Some(err) if err.transient && remaining.nonEmpty =>
          val delay = remaining.head
          remaining = remaining.tail
          onRetry(attempt, maxAttempts, delay, err)
          Interop.sleep(delay.toDouble)
          attempt += 1
        case Some(err) =>
          onError(err)
          trying = false
        case None =>
          trying = false
    response
