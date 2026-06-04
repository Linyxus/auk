package auk.llm.endpoint

import scala.scalajs.js
import gears.async.{Async, Future, ReadableChannel, SendableChannel, UnboundedChannel}
import auk.utils.Result

case class EndpointConfig(
    baseUrl: String,
    apiKey: String
):
  override def toString: String = s"EndpointConfig($baseUrl, ***)"

enum EffortLevel:
  case Low, Medium, High, XHigh

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

class LLMError(val description: String):
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
    * the consumer blocking forever on a stalled/half-open socket. */
  val StreamIdleTimeoutMs: Double = 120_000

  /** Overall timeout for a non-streaming request and for establishing a
    * streaming connection. */
  val RequestTimeoutMs: Double = 300_000

  /** Run `produce` on a fresh fiber, feeding stream events into a channel that
    * is ALWAYS closed when the producer exits — on clean completion, on a sent
    * error, or on ANY escaping throwable. This is the contract the consumer
    * (`Engine.streamTurn`) relies on: every producer exit yields either a
    * terminal event or a channel close, so a dead or stalled producer can never
    * leave the consumer's `read()` blocked forever (the historical hang). Any
    * throwable that escapes `produce` is first surfaced as a final
    * `Left(LLMError)` labelled with `label`, then the channel is closed. */
  def streaming(label: String)(
      produce: SendableChannel[Result[StreamEvent, LLMError]] => Async ?=> Unit
  )(using Async.Spawn): ReadableChannel[Result[StreamEvent, LLMError]] =
    val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
    Future:
      try produce(ch)
      catch
        case e: Throwable =>
          // Best-effort: surface the failure to the UI. If even this send fails
          // (e.g. the channel is being torn down), the `finally` close still
          // unblocks the consumer with a clean Left(Closed).
          try ch.send(Left(LLMError(s"$label: ${errMsg(e)}")))
          catch case _: Throwable => ()
      finally ch.close()
    ch.asReadable

  /** A human-readable message for a JS or JVM throwable. */
  def errMsg(e: Throwable): String = e match
    case js.JavaScriptException(err) => err.toString
    case other                       => Option(other.getMessage).getOrElse(other.toString).nn

/** Endpoint provider interface. */
trait EndpointProvider:
  type EndpointType <: Endpoint

  /** Create an endpoint instance from a configuration. */
  def create(config: EndpointConfig): EndpointType

  /** Create an endpoint instance from environment variables. */
  def createFromEnv(): EndpointType
