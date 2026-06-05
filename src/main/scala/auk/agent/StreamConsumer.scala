package auk.agent

import gears.async.{Async, ReadableChannel}

import auk.llm.endpoint.{ChatResponse, LLMError, StreamEvent}
import auk.utils.Result

/** Drains a model's streaming response down to its terminal [[ChatResponse]].
  *
  * An endpoint stream yields a run of incremental events (deltas, reasoning,
  * tool-call fragments) ending in exactly one `Done` that carries the assembled
  * response — or a terminal error, or, on an abnormally dropped connection, a
  * bare channel close with nothing terminal at all. The interactive [[Engine]]
  * and the headless [[auk.runtime.SubAgent]] consume a stream the same way and
  * differ only in what they do with each event (forward it to a UI vs. ignore
  * it) and how they surface a failure. That shared draining lives here so a turn
  * is read identically on both paths, with the per-path behaviour injected as
  * callbacks.
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
          onError(LLMError("the model stream ended unexpectedly"))
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
