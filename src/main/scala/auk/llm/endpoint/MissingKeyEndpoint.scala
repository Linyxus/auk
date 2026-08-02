package auk.llm.endpoint

import gears.async.{Async, ReadableChannel, UnboundedChannel}
import auk.utils.Result

/** The endpoint a session runs on when its provider's API key is unset.
  *
  * A missing key used to abort startup. Instead the session opens on this stub
  * and every request fails at once with the same human-readable message the
  * fatal path used to print. The error is non-transient (the [[LLMError]]
  * default): retrying cannot conjure a key, so the turn fails immediately
  * rather than riding the backoff schedule.
  */
final class MissingKeyEndpoint(message: String) extends Endpoint:
  private def failure: LLMError = LLMError(message)

  def invoke(
      messages: List[Message],
      config: LLMConfig
  )(using Async): Result[ChatResponse, LLMError] =
    Left(failure)

  def stream(messages: List[Message], config: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
    // A terminal error event, and no close: gears' pollRead checks isClosed
    // before the buffer, so closing here could discard the one event this
    // stream exists to deliver.
    ch.sendImmediately(Left(failure))
    ch.asReadable
