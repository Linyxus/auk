package auk.llm.endpoint

import scala.scalajs.js
import gears.async.{Async, Future, UnboundedChannel, ReadableChannel}
import auk.utils.Result
import auk.platform.js.{Anthropic, Interop}

/** Anthropic API endpoint, via the npm `@anthropic-ai/sdk`. */
class AnthropicEndpoint(config: EndpointConfig) extends Endpoint:

  private val DefaultMaxTokens = 4096

  private lazy val client: Anthropic =
    Anthropic(js.Dynamic.literal(apiKey = config.apiKey, baseURL = config.baseUrl).asInstanceOf[js.Object])

  private def buildParams(
      messages: List[Message],
      llmConfig: LLMConfig,
      stream: Boolean
  ): js.Object =
    val msgs = js.Array[js.Object]()
    def push(o: js.Dictionary[Any]): Unit = msgs.push(o.asInstanceOf[js.Object]); ()
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
              val blocks = js.Array[js.Object]()
              msg.content.foreach:
                case Content.ToolUse(id, name, input) =>
                  blocks.push(block(js.Dictionary("type" -> "tool_use", "id" -> id, "name" -> name, "input" -> parseJson(input))))
                case Content.Text(text) =>
                  blocks.push(block(js.Dictionary("type" -> "text", "text" -> text)))
                case Content.Thinking(text) =>
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
      case ThinkingMode.Effort(_) =>
        throw IllegalArgumentException(
          "Effort levels are not valid for Anthropic. Use ThinkingMode.Budget or ThinkingMode.Auto."
        )

    if stream then params("stream") = true
    params.asInstanceOf[js.Object]

  override def invoke(
      messages: List[Message],
      llmConfig: LLMConfig
  )(using Async): Result[ChatResponse, LLMError] =
    try Right(convertResponse(Interop.await(client.messages.create(buildParams(messages, llmConfig, stream = false)))))
    catch case e: Exception => Left(LLMError(s"Anthropic API error: ${errMsg(e)}"))

  override def stream(messages: List[Message], llmConfig: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
    Future:
      try
        val streamObj = Interop.await(client.messages.create(buildParams(messages, llmConfig, stream = true)))

        val thinkingBuf = new StringBuilder
        val textBuf = new StringBuilder
        val toolCalls = scala.collection.mutable.Map[Int, (String, String, StringBuilder)]()
        var lastFinishReason: FinishReason = FinishReason.Stop
        var lastUsage: Option[Usage] = None

        Interop.forEachAsync(streamObj): event =>
          Dyn.str(event.`type`).getOrElse("") match
            case "content_block_start" =>
              val idx = Dyn.num(event.index).map(_.toInt).getOrElse(0)
              val cb = event.content_block
              if Dyn.str(cb.`type`).contains("tool_use") then
                val id = Dyn.str(cb.id).getOrElse("")
                val name = Dyn.str(cb.name).getOrElse("")
                toolCalls(idx) = (id, name, new StringBuilder)
                ch.send(Right(StreamEvent.ToolCallStart(idx, id, name)))
            case "content_block_delta" =>
              val idx = Dyn.num(event.index).map(_.toInt).getOrElse(0)
              val delta = event.delta
              Dyn.str(delta.`type`).getOrElse("") match
                case "thinking_delta" =>
                  val t = Dyn.str(delta.thinking).getOrElse("")
                  thinkingBuf.append(t); ch.send(Right(StreamEvent.ThinkingDelta(t)))
                case "text_delta" =>
                  val t = Dyn.str(delta.text).getOrElse("")
                  textBuf.append(t); ch.send(Right(StreamEvent.Delta(t)))
                case "input_json_delta" =>
                  val j = Dyn.str(delta.partial_json).getOrElse("")
                  toolCalls.get(idx).foreach((_, _, buf) => buf.append(j))
                  ch.send(Right(StreamEvent.ToolCallDelta(idx, j)))
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
        if thinkingBuf.nonEmpty then contents += Content.Thinking(thinkingBuf.toString)
        if textBuf.nonEmpty then contents += Content.Text(textBuf.toString)
        toolCalls.toList.sortBy(_._1).foreach((_, t) => contents += Content.ToolUse(t._1, t._2, t._3.toString))
        ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, contents.toList), lastFinishReason, lastUsage))))
      catch
        case e: Exception => ch.send(Left(LLMError(s"Anthropic API error: ${errMsg(e)}")))
    ch.asReadable

  private def convertResponse(response: js.Dynamic): ChatResponse =
    val contents = scala.collection.mutable.ListBuffer[Content]()
    Dyn.arr(response.content).foreach: block =>
      Dyn.str(block.`type`).getOrElse("") match
        case "thinking" => Dyn.str(block.thinking).filter(_.nonEmpty).foreach(t => contents += Content.Thinking(t))
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

  private def errMsg(e: Throwable): String = e match
    case js.JavaScriptException(err) => err.toString
    case other                       => Option(other.getMessage).getOrElse(other.toString).nn

object AnthropicEndpoint extends EndpointProvider:
  type EndpointType = AnthropicEndpoint

  override def create(config: EndpointConfig): AnthropicEndpoint =
    AnthropicEndpoint(config)

  override def createFromEnv(): AnthropicEndpoint =
    val apiKey = auk.platform.Platform.env
      .get("ANTHROPIC_API_KEY")
      .getOrElse(throw RuntimeException("ANTHROPIC_API_KEY environment variable is not set"))
    val baseUrl = auk.platform.Platform.env.get("ANTHROPIC_BASE_URL").getOrElse("https://api.anthropic.com")
    create(EndpointConfig(baseUrl = baseUrl, apiKey = apiKey))
