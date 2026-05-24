package auk.agent

import gears.async.{Async, ReadableChannel, SendableChannel}
import auk.llm.endpoint.{Endpoint, LLMConfig, StreamEvent, Message, LLMError}
import auk.utils.Result

/** A simple, single-threaded agent loop.
  *
  * Reads [[UserCommand]]s and, for each submitted line, streams one assistant
  * turn from the endpoint — forwarding every event (thinking, text, done,
  * errors) to the UI verbatim and growing the conversation history so the model
  * keeps context across turns. No tools yet.
  *
  * The Engine is a relay: `endpoint.stream` already yields
  * `Result[StreamEvent, LLMError]`, the exact element type the UI consumes, so a
  * turn is just "read the upstream channel, forward each event to [[out]]".
  */
final class Engine(
    in: ReadableChannel[UserCommand],
    out: SendableChannel[Result[StreamEvent, LLMError]],
    endpoint: Endpoint,
    config: LLMConfig
):

  def run()(using Async.Spawn): Unit =
    var history = List.empty[Message]
    var running = true
    while running do
      in.read() match
        case Left(_) => running = false // command channel closed
        case Right(UserCommand.Submit(text)) =>
          history = history :+ Message.user(text)
          streamTurn(history).foreach(reply => history = history :+ reply)
        case Right(UserCommand.Interrupt) => () // not handled yet

  /** Stream one assistant turn, forwarding every event to the UI. Returns the
    * assembled assistant message to append to history, or None if the turn
    * ended without a terminal Done (an error, or a closed channel). */
  private def streamTurn(
      messages: List[Message]
  )(using Async.Spawn): Option[Message] =
    val upstream = endpoint.stream(messages, config)
    var assistant: Option[Message] = None
    var streaming = true
    while streaming do
      upstream.read() match
        case Left(_) => streaming = false // upstream closed without a Done
        case Right(result) =>
          out.send(result) // forward verbatim: thinking, text, done, error
          result match
            case Right(StreamEvent.Done(response)) =>
              assistant = Some(response.message)
              streaming = false
            case Left(_)  => streaming = false // error already forwarded
            case Right(_) => () // delta / thinking — keep going
    assistant
