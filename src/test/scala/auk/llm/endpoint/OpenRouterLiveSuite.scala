package auk.llm.endpoint

import gears.async.{Async, ReadableChannel}
import gears.async.default.given

import auk.platform.Platform
import auk.utils.Result

/** End-to-end check against the real OpenRouter API that auk's
  * [[OpenAICompletionEndpoint]] captures a model's `reasoning_details` (with
  * signatures) while streaming and replays them correctly on the next tool-use
  * turn — the full capture→persist-shape→replay loop the unit tests cover only in
  * pieces. A signed Anthropic-via-OpenRouter model is used because a dropped or
  * mangled signature is a hard API error on replay, making turn 2 a true assertion.
  *
  * Opt-in: runs only when `AUK_LIVE_TESTS=1` and `OPENROUTER_API_KEY` are set, so a
  * normal test run (and CI) never makes a network call. The model defaults to a
  * signed Anthropic model and is overridable via `OPENROUTER_MODEL`. Run with:
  *   bash -c 'source .envrc && AUK_LIVE_TESTS=1 ./mill test.testOnly auk.llm.endpoint.OpenRouterLiveSuite'
  */
class OpenRouterLiveSuite extends munit.FunSuite:

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private def enabled: Boolean =
    Platform.env.get("AUK_LIVE_TESTS").contains("1") && Platform.env.get("OPENROUTER_API_KEY").isDefined

  private def endpoint: OpenAICompletionEndpoint =
    val key = Platform.env.get("OPENROUTER_API_KEY").get
    val baseUrl = Platform.env.get("OPENROUTER_BASE_URL").getOrElse("https://openrouter.ai/api/v1")
    OpenAICompletionEndpoint(EndpointConfig(baseUrl = baseUrl, apiKey = key))

  private val weatherTool = ToolSchema(
    name = "get_weather",
    description = "Get the current weather for a city.",
    parameters = ToolSchema.Parameters(
      properties = Map("city" -> ToolSchema.Property("string", "City name")),
      required = List("city")
    )
  )

  // A signed Anthropic model via OpenRouter; override the exact slug with OPENROUTER_MODEL.
  private val config = LLMConfig(
    model = Platform.env.get("OPENROUTER_MODEL").getOrElse("anthropic/claude-sonnet-4.5"),
    thinking = Some(ThinkingMode.Effort(EffortLevel.High)),
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

  test("streamed reasoning_details replay cleanly on the next tool-use turn"):
    assume(enabled, "set AUK_LIVE_TESTS=1 and OPENROUTER_API_KEY to run this live test")
    Async.fromSync:
      val ep = endpoint
      val prompt = "What's the weather in Paris? Think it through, then call the get_weather tool."

      // Turn 1: the model reasons (signed reasoning_details) and calls the tool.
      val first = drain(ep.stream(List(Message.user(prompt)), config)) match
        case Right(r)  => r
        case Left(err) => fail(s"first turn failed: $err")

      val reasoning = first.message.content.collect { case r: Content.Reasoning => r }.flatMap(_.blocks)
      val toolUses = first.message.content.collect { case t: Content.ToolUse => t }
      assert(reasoning.nonEmpty, s"expected reasoning_details; got ${first.message.content}")
      assert(reasoning.exists(_.signature.isDefined), s"expected a signed reasoning.text block; got $reasoning")
      assert(toolUses.nonEmpty, s"expected a tool call; got ${first.message.content}")

      // Turn 2: replay the assistant turn (reasoning + tool_use) + a tool result.
      // If reasoning_details were dropped or mutated, OpenRouter rejects this.
      val toolResult = Message(Role.User, toolUses.map(tu => Content.ToolResult(tu.id, "18°C, sunny", isError = false)))
      val second = drain(ep.stream(List(Message.user(prompt), first.message, toolResult), config))
      second match
        case Right(r)  => assert(r.message.text.nonEmpty, "expected a final answer after the tool result")
        case Left(err) => fail(s"replay of the reasoning turn was rejected: $err")
