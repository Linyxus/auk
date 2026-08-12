package auk.llm.endpoint

import gears.async.{Async, ReadableChannel}
import gears.async.default.given

import auk.platform.Platform
import auk.utils.Result

/** End-to-end check against the real Anthropic API that auk's own
  * [[AnthropicEndpoint]] captures a thinking block's signature while streaming
  * and replays it correctly on the next tool-use turn — the full
  * capture→persist-shape→replay loop the unit tests can only cover in pieces.
  *
  * Opt-in: runs only when `AUK_LIVE_TESTS=1` and `ANTHROPIC_API_KEY` are set, so
  * a normal test run (and CI) never makes a network call. Run it with:
  *   bash -c 'source .envrc && AUK_LIVE_TESTS=1 ./mill test.testOnly auk.llm.endpoint.AnthropicLiveSuite'
  */
class AnthropicLiveSuite extends munit.FunSuite:

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private def enabled: Boolean =
    Platform.env.get("AUK_LIVE_TESTS").contains("1") && Platform.env.get("ANTHROPIC_API_KEY").isDefined

  private def endpoint: AnthropicEndpoint =
    val key = Platform.env.get("ANTHROPIC_API_KEY").get
    val baseUrl = Platform.env.get("ANTHROPIC_BASE_URL").getOrElse("https://api.anthropic.com")
    AnthropicEndpoint(EndpointConfig(baseUrl = baseUrl, apiKey = key))

  private val weatherTool = ToolSchema(
    name = "get_weather",
    description = "Get the current weather for a city.",
    parameters = ToolSchema.Parameters(
      properties = Map("city" -> ToolSchema.Property("string", "City name")),
      required = List("city")
    )
  )

  private val config = LLMConfig(
    model = "claude-sonnet-4-6",
    thinking = Some(ThinkingMode.Auto), // -> {"type":"adaptive"}, confirmed valid live
    tools = List(weatherTool)
  )

  /** Drain a stream to its terminal event: the final `ChatResponse`, or a `Left`
    * if the endpoint surfaced an error / closed without a Done. */
  private def drain(
      ch: ReadableChannel[Result[StreamEvent, LLMError]]
  )(using Async): Either[String, ChatResponse] =
    var result: Option[Either[String, ChatResponse]] = None
    while result.isEmpty do
      ch.read() match
        case Left(_)                                  => result = Some(Left("stream closed without a Done"))
        case Right(Left(err))                         => result = Some(Left(err.description))
        case Right(Right(StreamEvent.Done(response))) => result = Some(Right(response))
        case Right(Right(_))                          => () // delta / thinking / tool-call
    result.get

  test("streamed thinking signatures replay cleanly on the next tool-use turn"):
    assume(enabled, "set AUK_LIVE_TESTS=1 and ANTHROPIC_API_KEY to run this live test")
    Async.fromSync:
      val ep = endpoint
      val prompt = "What's the weather in Paris? Think it through, then call the get_weather tool."

      // Turn 1: the model reasons (signed thinking) and calls the tool.
      val first = drain(ep.stream(List(Message.user(prompt)), config)) match
        case Right(r)  => r
        case Left(err) => fail(s"first turn failed: $err")

      val thinking = first.message.content.collect { case t: Content.Thinking => t }
      val toolUses = first.message.content.collect { case t: Content.ToolUse => t }
      assert(thinking.nonEmpty, s"expected a thinking block; got ${first.message.content}")
      assert(thinking.forall(_.signature.isDefined), "thinking block must carry a captured signature")
      assert(toolUses.nonEmpty, s"expected a tool call; got ${first.message.content}")

      // Turn 2: replay the assistant turn (thinking + tool_use) + a tool result.
      // If the signature were dropped or mis-serialized, the API would 400 here.
      val toolResult = Message(Role.User, toolUses.map(tu => Content.ToolResult(tu.id, "18°C, sunny", isError = false)))
      val second = drain(ep.stream(List(Message.user(prompt), first.message, toolResult), config))
      second match
        case Right(r)  => assert(r.message.text.nonEmpty, "expected a final answer after the tool result")
        case Left(err) => fail(s"replay of the signed thinking turn was rejected: $err")
