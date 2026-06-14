package auk.llm.endpoint

import scala.scalajs.js
import gears.async.{Async, ReadableChannel}
import auk.utils.Result
import auk.platform.js.{Anthropic, Interop}

/** Anthropic API endpoint, via the npm `@anthropic-ai/sdk`. */
class AnthropicEndpoint(config: EndpointConfig) extends Endpoint:

  private val DefaultMaxTokens = 65536

  private lazy val client: Anthropic =
    Anthropic(js.Dynamic.literal(apiKey = config.apiKey, baseURL = config.baseUrl).asInstanceOf[js.Object])

  private[endpoint] def buildParams(
      messages: List[Message],
      llmConfig: LLMConfig,
      stream: Boolean
  ): js.Object =
    val msgs = js.Array[js.Object]()
    def push(o: js.Dictionary[Any]): Unit = msgs.push(o.asInstanceOf[js.Object])
    def block(d: js.Dictionary[Any]): js.Object = d.asInstanceOf[js.Object]

    messages
      .filterNot(_.role == Role.System)
      .foreach: msg =>
        msg.role match
          case Role.User =>
            if msg.content.exists(_.isInstanceOf[Content.ToolResult]) then
              val blocks = js.Array[js.Object]()
              msg.content.foreach:
                case Content.ToolResult(toolUseId, content, isError) =>
                  blocks.push(block(js.Dictionary("type" -> "tool_result", "tool_use_id" -> toolUseId, "content" -> content, "is_error" -> isError)))
                case Content.Text(text) =>
                  blocks.push(block(js.Dictionary("type" -> "text", "text" -> text)))
                case other =>
                  blocks.push(block(js.Dictionary("type" -> "text", "text" -> other.toString)))
              push(js.Dictionary("role" -> "user", "content" -> blocks))
            else push(js.Dictionary("role" -> "user", "content" -> msg.text))
          case Role.Assistant =>
            if msg.content.exists(_.isInstanceOf[Content.ToolUse]) then
              // A tool-use turn must replay its reasoning: Anthropic validates a
              // thinking block's signature against its text, and expects the
              // (redacted_)thinking block(s) to precede the tool_use. We emit
              // them in their original order. A thinking block with no signature
              // (e.g. carried over from another provider, or a pre-signature
              // session) cannot be sent — an unsigned thinking block is a 400 —
              // so it is dropped, which the API accepts.
              val blocks = js.Array[js.Object]()
              msg.content.foreach:
                case Content.Thinking(text, Some(signature)) =>
                  blocks.push(block(js.Dictionary("type" -> "thinking", "thinking" -> text, "signature" -> signature)))
                case Content.Thinking(_, None) =>
                  () // unsigned reasoning: drop rather than send an invalid block
                case Content.RedactedThinking(data) =>
                  blocks.push(block(js.Dictionary("type" -> "redacted_thinking", "data" -> data)))
                case Content.ToolUse(id, name, input) =>
                  blocks.push(block(js.Dictionary("type" -> "tool_use", "id" -> id, "name" -> name, "input" -> parseJson(input))))
                case Content.Text(text) =>
                  blocks.push(block(js.Dictionary("type" -> "text", "text" -> text)))
                case other =>
                  blocks.push(block(js.Dictionary("type" -> "text", "text" -> other.toString)))
              push(js.Dictionary("role" -> "assistant", "content" -> blocks))
            else push(js.Dictionary("role" -> "assistant", "content" -> msg.text))
          case Role.System => ()

    val params = js.Dictionary[Any](
      "model" -> llmConfig.model,
      "max_tokens" -> llmConfig.maxTokens.getOrElse(DefaultMaxTokens),
      "messages" -> msgs
    )
    llmConfig.systemPrompt.foreach(p => params("system") = p)
    llmConfig.temperature.foreach(t => params("temperature") = t)
    llmConfig.topP.foreach(p => params("top_p") = p)
    if llmConfig.stopSequences.nonEmpty then params("stop_sequences") = js.Array(llmConfig.stopSequences*)

    if llmConfig.tools.nonEmpty then
      val tools = js.Array[js.Object]()
      llmConfig.tools.foreach: tool =>
        tools.push(
          js.Dictionary[Any](
            "name" -> tool.name,
            "description" -> tool.description,
            "input_schema" -> convertInputSchema(tool.parameters)
          ).asInstanceOf[js.Object]
        )
      params("tools") = tools

    llmConfig.thinking.foreach:
      case ThinkingMode.Disabled  => params("thinking") = js.Dictionary[Any]("type" -> "disabled")
      case ThinkingMode.Auto      => params("thinking") = js.Dictionary[Any]("type" -> "adaptive")
      case ThinkingMode.Budget(n) => params("thinking") = js.Dictionary[Any]("type" -> "enabled", "budget_tokens" -> n)
      case ThinkingMode.Effort(level) =>
        // Effort tunes reasoning depth via `output_config.effort` (a GA control
        // on Anthropic; z.ai's Anthropic endpoint accepts the same field). Pair
        // it with adaptive thinking so the model still returns *signed* thinking
        // blocks — effort on its own yields no thinking block, leaving nothing
        // to replay across tool calls.
        params("thinking") = js.Dictionary[Any]("type" -> "adaptive")
        params("output_config") = js.Dictionary[Any]("effort" -> effortString(level))

    if stream then params("stream") = true
    params.asInstanceOf[js.Object]

  /** Anthropic's `output_config.effort` levels. */
  private def effortString(level: EffortLevel): String = level match
    case EffortLevel.Low    => "low"
    case EffortLevel.Medium => "medium"
    case EffortLevel.High   => "high"
    case EffortLevel.XHigh  => "xhigh"
    case EffortLevel.Max    => "max"

  override def invoke(
      messages: List[Message],
      llmConfig: LLMConfig
  )(using Async): Result[ChatResponse, LLMError] =
    try
      Right(
        convertResponse(
          Interop.withAbort: opts =>
            Interop.awaitWithin(
              client.messages.create(buildParams(messages, llmConfig, stream = false), opts),
              Endpoint.RequestTimeoutMs,
              "Anthropic request timed out"
            )
        )
      )
    catch case e: Exception => Left(LLMError(s"Anthropic API error: ${Endpoint.errMsg(e)}"))

  override def stream(messages: List[Message], llmConfig: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    Endpoint.streaming("Anthropic API error"): (ch, opts) =>
      val streamObj = Interop.awaitWithin(
        client.messages.create(buildParams(messages, llmConfig, stream = true), opts),
        Endpoint.RequestTimeoutMs,
        "Anthropic request timed out"
      )

      // Accumulate one builder per content-block index, preserving order. Block
      // structure must survive streaming: each thinking block's signature is
      // validated against its own text on replay, so thinking/text/tool_use
      // cannot be flattened into single buffers.
      val blocks = scala.collection.mutable.SortedMap.empty[Int, AnthropicEndpoint.BlockAccum]
      def accum(idx: Int): AnthropicEndpoint.BlockAccum =
        blocks.getOrElseUpdate(idx, new AnthropicEndpoint.BlockAccum)
      var lastFinishReason: FinishReason = FinishReason.Stop
      var lastUsage: Option[Usage] = None

      Interop.forEachAsync(streamObj, Endpoint.StreamIdleTimeoutMs): event =>
        Dyn.str(event.`type`).getOrElse("") match
          case "content_block_start" =>
            val idx = Dyn.num(event.index).map(_.toInt).getOrElse(0)
            val cb = event.content_block
            val acc = accum(idx)
            acc.kind = Dyn.str(cb.`type`).getOrElse("")
            acc.kind match
              case "tool_use" =>
                acc.id = Dyn.str(cb.id).getOrElse("")
                acc.name = Dyn.str(cb.name).getOrElse("")
                ch.send(Right(StreamEvent.ToolCallStart(idx, acc.id, acc.name)))
              case "redacted_thinking" =>
                // Redacted reasoning arrives whole, with no deltas to follow.
                acc.data = Dyn.str(cb.data).getOrElse("")
              case _ => ()
          case "content_block_delta" =>
            val idx = Dyn.num(event.index).map(_.toInt).getOrElse(0)
            val acc = accum(idx)
            val delta = event.delta
            Dyn.str(delta.`type`).getOrElse("") match
              case "thinking_delta" =>
                val t = Dyn.str(delta.thinking).getOrElse("")
                acc.text.append(t); ch.send(Right(StreamEvent.ThinkingDelta(t)))
              case "text_delta" =>
                val t = Dyn.str(delta.text).getOrElse("")
                acc.text.append(t); ch.send(Right(StreamEvent.Delta(t)))
              case "signature_delta" =>
                Dyn.str(delta.signature).foreach(s => acc.signature = Some(s))
              case "input_json_delta" =>
                val j = Dyn.str(delta.partial_json).getOrElse("")
                acc.text.append(j); ch.send(Right(StreamEvent.ToolCallDelta(idx, j)))
              case _ => ()
          case "message_delta" =>
            Dyn.str(event.delta.stop_reason).foreach(r => lastFinishReason = stopReason(r))
            if Dyn.defined(event.usage) then
              lastUsage = Some(
                Usage(
                  inputTokens = Dyn.num(event.usage.input_tokens).map(_.toLong).getOrElse(0L),
                  outputTokens = Dyn.num(event.usage.output_tokens).map(_.toLong).getOrElse(0L)
                )
              )
          case _ => ()

      val contents = scala.collection.mutable.ListBuffer[Content]()
      blocks.valuesIterator.foreach: acc =>
        acc.kind match
          case "thinking" =>
            if acc.text.nonEmpty then contents += Content.Thinking(acc.text.toString, acc.signature)
          case "redacted_thinking" =>
            if acc.data.nonEmpty then contents += Content.RedactedThinking(acc.data)
          case "text" =>
            if acc.text.nonEmpty then contents += Content.Text(acc.text.toString)
          case "tool_use" =>
            contents += Content.ToolUse(acc.id, acc.name, acc.text.toString)
          case _ => ()
      ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, contents.toList), lastFinishReason, lastUsage))))

  private def convertResponse(response: js.Dynamic): ChatResponse =
    val contents = scala.collection.mutable.ListBuffer[Content]()
    Dyn.arr(response.content).foreach: block =>
      Dyn.str(block.`type`).getOrElse("") match
        case "thinking" =>
          Dyn.str(block.thinking).filter(_.nonEmpty).foreach(t => contents += Content.Thinking(t, Dyn.str(block.signature)))
        case "redacted_thinking" =>
          Dyn.str(block.data).filter(_.nonEmpty).foreach(d => contents += Content.RedactedThinking(d))
        case "text"     => contents += Content.Text(Dyn.str(block.text).getOrElse(""))
        case "tool_use" =>
          contents += Content.ToolUse(
            id = Dyn.str(block.id).getOrElse(""),
            name = Dyn.str(block.name).getOrElse(""),
            input = js.JSON.stringify(block.input)
          )
        case _ => ()

    val usage = Usage(
      inputTokens = Dyn.num(response.usage.input_tokens).map(_.toLong).getOrElse(0L),
      outputTokens = Dyn.num(response.usage.output_tokens).map(_.toLong).getOrElse(0L)
    )
    ChatResponse(
      Message(Role.Assistant, contents.toList),
      stopReason(Dyn.str(response.stop_reason).getOrElse("end_turn")),
      Some(usage)
    )

  private def stopReason(s: String): FinishReason = s match
    case "end_turn"   => FinishReason.Stop
    case "max_tokens" => FinishReason.MaxTokens
    case "tool_use"   => FinishReason.ToolUse
    case other        => FinishReason.Other(other)

  private def parseJson(s: String): js.Any =
    try js.JSON.parse(s)
    catch case _: Throwable => js.Dictionary[Any]().asInstanceOf[js.Any]

  private def convertInputSchema(params: ToolSchema.Parameters): js.Object =
    val props = js.Dictionary[Any]()
    params.properties.foreach: (name, prop) =>
      val p = js.Dictionary[Any]("type" -> prop.`type`)
      if prop.description.nonEmpty then p("description") = prop.description
      if prop.enumValues.nonEmpty then p("enum") = js.Array(prop.enumValues*)
      prop.items.foreach(it => p("items") = js.Dictionary[Any]("type" -> it.`type`))
      props(name) = p
    js.Dictionary[Any](
      "type" -> "object",
      "properties" -> props,
      "required" -> js.Array(params.required*)
    ).asInstanceOf[js.Object]

object AnthropicEndpoint extends EndpointProvider:
  type EndpointType = AnthropicEndpoint

  /** Mutable accumulator for one streamed content block. `text` collects a
    * thinking/text block's characters or a tool_use's partial JSON; `signature`
    * captures a thinking block's `signature_delta`; `data` holds a whole
    * `redacted_thinking` payload. */
  private final class BlockAccum:
    var kind: String = ""
    val text: StringBuilder = new StringBuilder
    var signature: Option[String] = None
    var id: String = ""
    var name: String = ""
    var data: String = ""

  override def create(config: EndpointConfig): AnthropicEndpoint =
    AnthropicEndpoint(config)

  override def createFromEnv(): AnthropicEndpoint =
    val apiKey = auk.platform.Platform.env
      .get("ANTHROPIC_API_KEY")
      .getOrElse(throw RuntimeException("ANTHROPIC_API_KEY environment variable is not set"))
    val baseUrl = auk.platform.Platform.env.get("ANTHROPIC_BASE_URL").getOrElse("https://api.anthropic.com")
    create(EndpointConfig(baseUrl = baseUrl, apiKey = apiKey))
