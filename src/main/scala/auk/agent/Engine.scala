package auk.agent

import gears.async.{Async, Future, ReadableChannel, SendableChannel}
import auk.llm.endpoint.{Endpoint, LLMConfig, StreamEvent, Message, Content, Role, ChatResponse, LLMError}
import auk.llm.tools.RuntimeContext
import auk.runtime.ToolRegistry
import auk.utils.Result

/** A single-threaded agent loop with tool use.
  *
  * Reads [[UserCommand]]s and, for each submitted line, drives one turn to
  * completion: it streams an assistant reply, and while the model asks for
  * tools, it runs them through the [[registry]], feeds the results back, and
  * streams again — until the model answers without a tool call (or the round
  * cap is hit). Conversation history grows across turns so the model keeps
  * context.
  *
  * Streaming events flow to the UI verbatim, with one exception: the terminal
  * `Done` of an *intermediate* (tool-requesting) round is held back, so the UI
  * sees a single continuous turn ending in one `Done` rather than blinking back
  * to idle between tool rounds.
  */
final class Engine(
    in: ReadableChannel[UserCommand],
    out: SendableChannel[Result[StreamEvent, LLMError]],
    endpoint: Endpoint,
    config: LLMConfig,
    registry: ToolRegistry = ToolRegistry.of(),
    context: RuntimeContext = RuntimeContext.cwd(),
    maxToolRounds: Int = 8
):
  private given RuntimeContext = context

  def run()(using Async.Spawn): Unit =
    var history = List.empty[Message]
    var running = true
    while running do
      in.read() match
        case Left(_) => running = false // command channel closed
        case Right(UserCommand.Submit(text)) =>
          history = converse(history :+ Message.user(text))
        case Right(UserCommand.Interrupt) => () // not handled yet

  /** Drive a user turn to completion: stream the reply, and while the model
    * requests tools, run them and stream again. Returns the conversation grown
    * with every message exchanged (assistant replies and tool results). */
  private def converse(initial: List[Message])(using Async.Spawn): List[Message] =
    var messages = initial
    var round = 0
    var turning = true
    while turning do
      streamTurn(messages) match
        case None => turning = false // error or closed; already forwarded
        case Some(response) =>
          val assistant = response.message
          messages = messages :+ assistant
          round += 1
          val toolUses = assistant.content.collect { case t: Content.ToolUse => t }
          if toolUses.nonEmpty && round < maxToolRounds then
            // Intermediate round: keep the turn alive (the Done was held back),
            // run the tools, and feed their results back as the next message.
            val results = runTools(toolUses)
            messages = messages :+ Message(Role.User, results)
          else
            // Final round: surface the completed turn to the UI.
            out.send(Right(StreamEvent.Done(response)))
            turning = false
    messages

  /** Run each requested tool concurrently, bracketing every call with
    * `ToolRunStart`/`ToolRunEnd` events so the UI can show progress and the
    * tool's metadata (e.g. a sub-agent's token totals). Results are collected in
    * the original order to line up with the model's calls. */
  private def runTools(
      toolUses: List[Content.ToolUse]
  )(using Async.Spawn): List[Content.ToolResult] =
    toolUses.foreach(tu => out.send(Right(StreamEvent.ToolRunStart(tu.id, tu.name))))
    toolUses
      .map: tu =>
        Future[Content.ToolResult]:
          val result = registry.run(tu)
          out.send(Right(StreamEvent.ToolRunEnd(tu.id, result.isError, result.metadata)))
          Content.ToolResult(tu.id, result.output, isError = result.isError)
      .map(_.await)

  /** Stream one assistant turn, forwarding every event to the UI except the
    * terminal `Done`, which is captured and returned so [[converse]] can decide
    * whether to continue (run tools) or surface it as the turn's end. Returns
    * the full `ChatResponse`, or None if the turn ended without a Done (an
    * error — already forwarded — or a closed channel). */
  private def streamTurn(
      messages: List[Message]
  )(using Async.Spawn): Option[ChatResponse] =
    val upstream = endpoint.stream(messages, config)
    var response: Option[ChatResponse] = None
    var streaming = true
    while streaming do
      upstream.read() match
        case Left(_) => streaming = false // upstream closed without a Done
        case Right(result) =>
          result match
            case Right(StreamEvent.Done(r)) =>
              response = Some(r) // held back; converse forwards the final one
              streaming = false
            case Left(_) =>
              out.send(result) // forward the error and stop
              streaming = false
            case Right(_) =>
              out.send(result) // delta / thinking / tool-call — forward verbatim
    response
