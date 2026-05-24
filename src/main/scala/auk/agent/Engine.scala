package auk.agent

import gears.async.{Async, AsyncOperations, ReadableChannel, SendableChannel}
import auk.llm.endpoint.{
  StreamEvent,
  ChatResponse,
  Message,
  FinishReason,
  LLMError
}
import auk.utils.Result

/** The agent engine: reads [[UserCommand]]s and emits a stream of LLM events.
  *
  * This is the seam the TUI talks to. For now it is a pure echo: on
  * [[UserCommand.Submit]] it streams the same text back as a sequence of
  * [[StreamEvent.Delta]]s (chunked, with a small delay so it reads like typing)
  * followed by a terminal [[StreamEvent.Done]]. A real, model-backed loop will
  * later replace [[respond]] without changing the channel contract.
  *
  * Output is wrapped in [[Result]] to match `Endpoint.stream`, so a future
  * engine can forward an endpoint's channel near-verbatim.
  *
  * @param chunkDelayMs delay between streamed chunks; pass `0` in tests.
  */
final class Engine(
    in: ReadableChannel[UserCommand],
    out: SendableChannel[Result[StreamEvent, LLMError]],
    chunkDelayMs: Long = 40
):

  /** Drive the engine until the command channel closes. */
  def run()(using Async, AsyncOperations): Unit =
    var running = true
    while running do
      in.read() match
        case Left(_)                         => running = false // channel closed
        case Right(UserCommand.Submit(text)) => respond(text)
        case Right(UserCommand.Interrupt)    => () // ignored for now

  /** Echo `text` back as a streamed assistant turn. */
  private def respond(text: String)(using Async, AsyncOperations): Unit =
    chunk(text).foreach: piece =>
      out.send(Right(StreamEvent.Delta(piece)))
      if chunkDelayMs > 0 then summon[AsyncOperations].sleep(chunkDelayMs)
    val response =
      ChatResponse(Message.assistant(text), FinishReason.Stop, usage = None)
    out.send(Right(StreamEvent.Done(response)))

  /** Split into word-ish chunks: each is a run of characters up to and
    * including the next space, so concatenating the chunks reproduces `text`
    * exactly. (Avoids `String.split`, which is nullable under -Yexplicit-nulls.)
    */
  private def chunk(text: String): List[String] =
    val pieces = scala.collection.mutable.ListBuffer[String]()
    val sb = new StringBuilder
    var i = 0
    while i < text.length do
      val c = text.charAt(i)
      sb.append(c)
      if c == ' ' then
        pieces += sb.toString
        sb.setLength(0)
      i += 1
    if sb.nonEmpty then pieces += sb.toString
    pieces.toList
