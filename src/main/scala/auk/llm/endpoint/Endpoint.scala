package auk.llm.endpoint

import scala.scalajs.js
import gears.async.{Async, Future, ReadableChannel, SendableChannel, UnboundedChannel}
import auk.platform.js.AbortController
import auk.utils.Result

case class EndpointConfig(
    baseUrl: String,
    apiKey: String
):
  override def toString: String = s"EndpointConfig($baseUrl, ***)"

enum EffortLevel:
  case Low, Medium, High, XHigh, Max

enum ThinkingMode:
  case Disabled
  case Auto
  case Budget(tokens: Int)
  case Effort(level: EffortLevel)

/** Configuration for LLM invocation. */
case class LLMConfig(
    model: String,
    systemPrompt: Option[String] = None,
    temperature: Option[Double] = None,
    maxTokens: Option[Int] = None,
    stopSequences: List[String] = List.empty,
    topP: Option[Double] = None,
    tools: List[ToolSchema] = List.empty,
    thinking: Option[ThinkingMode] = None
)

/** A failed LLM request. `transient` marks failures worth retrying — rate
  * limits, provider hiccups, dropped connections — as opposed to permanent ones
  * (invalid request, bad credentials) that would fail identically on every
  * attempt. Defaults to false so the many non-API uses (persistence errors,
  * config errors) never look retryable. */
class LLMError(val description: String, val transient: Boolean = false):
  override def toString: String = s"Error when invoking LLM: $description"

/** Interface for LLM endpoints. */
trait Endpoint:
  def invoke(
      messages: List[Message],
      config: LLMConfig
  )(using Async): Result[ChatResponse, LLMError]
  def stream(messages: List[Message], config: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]]

object Endpoint:
  /** Idle timeout for a streaming response: if no chunk arrives within this
    * window the connection is treated as dead and the stream fails, rather than
    * the consumer blocking forever on a stalled/half-open socket. Generous (10
    * min) because a single round can stall for a long time between chunks — e.g.
    * extended thinking that buffers before emitting visible output — and cutting
    * a slow-but-alive round short is worse than waiting. */
  val StreamIdleTimeoutMs: Double = 600_000

  /** Overall timeout for a non-streaming request and for establishing a
    * streaming connection. */
  val RequestTimeoutMs: Double = 300_000

  /** The backoff schedule for retrying a transient API failure: one entry per
    * retry, so a request is attempted at most `RetryDelaysMs.length + 1` times
    * (~30s of riding out a rate limit or provider blip before giving up). */
  val RetryDelaysMs: List[Long] = List(1_000L, 2_000L, 4_000L, 8_000L, 16_000L)

  /** Run `produce` on a fresh fiber, feeding stream events into a channel that
    * is ALWAYS closed when the producer exits — on clean completion, on a sent
    * error, or on ANY escaping throwable. This is the contract the consumer
    * (`Engine.streamTurn`) relies on: every producer exit yields either a
    * terminal event or a channel close, so a dead or stalled producer can never
    * leave the consumer's `read()` blocked forever (the historical hang). Any
    * throwable that escapes `produce` is first surfaced as a final
    * `Left(LLMError)` labelled with `label`, then the channel is closed. */
  def streaming(label: String)(
      produce: (SendableChannel[Result[StreamEvent, LLMError]], js.Object) => Async ?=> Unit
  )(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
    val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
    Future:
      // An AbortController whose `signal` the producer threads into the SDK's
      // `create(body, opts)`. Aborting it in the `finally` tears down the actual
      // request — so when this fiber is cancelled mid-stream (a user interrupt),
      // the `CancellationException` thrown into the suspended `forEachAsync`
      // await unwinds here and stops the model generating, rather than leaving
      // the request streaming (and billing) in the background. A normal
      // completion aborts an already-finished request, which is a no-op.
      val controller = new AbortController()
      val opts = js.Dynamic.literal(signal = controller.signal).asInstanceOf[js.Object]
      try produce(ch, opts)
      catch
        case e: Throwable =>
          // Best-effort: surface the failure to the UI. If even this send fails
          // (e.g. the channel is being torn down), the `finally` close still
          // unblocks the consumer with a clean Left(Closed).
          try ch.send(Left(LLMError(s"$label: ${errMsg(e)}", transient = isTransient(e))))
          catch case _: Throwable => ()
      finally
        controller.abort()
        ch.close()
    ch.asReadable

  /** A human-readable message for a JS or JVM throwable. */
  def errMsg(e: Throwable): String = e match
    case js.JavaScriptException(err) => err.toString
    case other                       => Option(other.getMessage).getOrElse(other.toString).nn

  /** Whether a request failure is worth retrying.
    *
    * The provider SDKs (Anthropic/OpenAI) throw error objects carrying the HTTP
    * `status` of a failed request: 408/429 and 5xx are transient by definition;
    * any other status (400 invalid request, 401/403 bad credentials, 404…)
    * would fail identically on a retry, so it is surfaced at once. A failure
    * with no status at all — a dropped connection, an abort, one of our own
    * stall/timeout errors — is connection-level and treated as transient. */
  def isTransient(e: Throwable): Boolean = e match
    case js.JavaScriptException(err) if err != null =>
      Dyn.num(err.asInstanceOf[js.Dynamic].status) match
        case Some(status) =>
          val s = status.toInt
          s == 408 || s == 429 || s >= 500
        case None => true
    case _ => true

/** Endpoint provider interface. */
trait EndpointProvider:
  type EndpointType <: Endpoint

  /** Create an endpoint instance from a configuration. */
  def create(config: EndpointConfig): EndpointType

  /** Create an endpoint instance from environment variables. */
  def createFromEnv(): EndpointType
