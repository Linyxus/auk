package auk.llm.endpoint

import scala.scalajs.js
import gears.async.{Async, ReadableChannel}
import auk.utils.Result
import auk.platform.js.{OpenAI, Interop}

/** OpenAI endpoint using the Chat Completions API (`/v1/chat/completions`),
  * via the npm `openai` SDK.
  *
  * This is the legacy endpoint. It does not support combining tools with
  * reasoning. For the newer Responses API, use [[OpenAIEndpoint]] instead.
  */
class OpenAICompletionEndpoint(config: EndpointConfig) extends Endpoint:

  private lazy val client: OpenAI =
    OpenAI(js.Dynamic.literal(apiKey = config.apiKey, baseURL = config.baseUrl).asInstanceOf[js.Object])

  /** Build the request body (the Chat Completions wire JSON) as a JS object. */
  private def buildParams(
      messages: List[Message],
      llmConfig: LLMConfig,
      stream: Boolean
  ): js.Object =
    val msgs = js.Array[js.Object]()

    def push(o: js.Dictionary[Any]): Unit = msgs.push(o.asInstanceOf[js.Object]); ()

    llmConfig.systemPrompt.foreach(p => push(js.Dictionary("role" -> "system", "content" -> p)))

    messages.foreach: msg =>
      msg.role match
        case Role.System =>
          push(js.Dictionary("role" -> "system", "content" -> msg.text))
        case Role.User =>
          val toolResults = msg.content.collect { case tr: Content.ToolResult => tr }
          if toolResults.nonEmpty then
            toolResults.foreach: tr =>
              push(js.Dictionary("role" -> "tool", "tool_call_id" -> tr.toolUseId, "content" -> tr.content))
          else push(js.Dictionary("role" -> "user", "content" -> msg.text))
        case Role.Assistant =>
          val toolUses = msg.content.collect { case tu: Content.ToolUse => tu }
          if toolUses.nonEmpty then
            val calls = js.Array[js.Object]()
            toolUses.foreach: tu =>
              calls.push(
                js.Dictionary[Any](
                  "id" -> tu.id,
                  "type" -> "function",
                  "function" -> js.Dictionary[Any]("name" -> tu.name, "arguments" -> tu.input)
                ).asInstanceOf[js.Object]
              )
            val o = js.Dictionary[Any]("role" -> "assistant", "tool_calls" -> calls)
            if msg.text.nonEmpty then o("content") = msg.text
            push(o)
          else push(js.Dictionary("role" -> "assistant", "content" -> msg.text))

    val params = js.Dictionary[Any]("model" -> llmConfig.model, "messages" -> msgs)
    llmConfig.temperature.foreach(t => params("temperature") = t)
    llmConfig.maxTokens.foreach(n => params("max_completion_tokens") = n)
    llmConfig.topP.foreach(p => params("top_p") = p)
    if llmConfig.stopSequences.nonEmpty then
      params("stop") = js.Array(llmConfig.stopSequences*)
    if llmConfig.tools.nonEmpty then
      val tools = js.Array[js.Object]()
      llmConfig.tools.foreach: tool =>
        tools.push(
          js.Dictionary[Any](
            "type" -> "function",
            "function" -> js.Dictionary[Any](
              "name" -> tool.name,
              "description" -> tool.description,
              "parameters" -> convertParameters(tool.parameters)
            )
          ).asInstanceOf[js.Object]
        )
      params("tools") = tools

    reasoningEffort(llmConfig.thinking).foreach(e => params("reasoning_effort") = e)

    if stream then
      params("stream") = true
      params("stream_options") = js.Dictionary[Any]("include_usage" -> true)

    params.asInstanceOf[js.Object]

  /** Map our thinking config to the `reasoning_effort` string, or `None` to omit. */
  private def reasoningEffort(thinking: Option[ThinkingMode]): Option[String] =
    thinking.flatMap:
      case ThinkingMode.Disabled                 => Some("minimal")
      case ThinkingMode.Auto                     => Some("medium")
      case ThinkingMode.Effort(EffortLevel.Low)  => Some("low")
      case ThinkingMode.Effort(EffortLevel.Medium) => Some("medium")
      case ThinkingMode.Effort(EffortLevel.High) => Some("high")
      case ThinkingMode.Effort(EffortLevel.XHigh) => Some("high")
      case ThinkingMode.Budget(n) =>
        throw IllegalArgumentException(
          s"Budget($n) is not valid for OpenAI. Use ThinkingMode.Effort instead."
        )

  override def invoke(
      messages: List[Message],
      llmConfig: LLMConfig
  )(using Async): Result[ChatResponse, LLMError] =
    try
      val resp = Interop.awaitWithin(
        client.chat.completions.create(buildParams(messages, llmConfig, stream = false)),
        Endpoint.RequestTimeoutMs,
        "OpenAI request timed out"
      )
      Right(convertResponse(resp))
    catch
      case e: Exception => Left(LLMError(s"OpenAI API error: ${Endpoint.errMsg(e)}"))

  override def stream(messages: List[Message], llmConfig: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    Endpoint.streaming("OpenAI API error"): ch =>
      val streamObj = Interop.awaitWithin(
        client.chat.completions.create(buildParams(messages, llmConfig, stream = true)),
        Endpoint.RequestTimeoutMs,
        "OpenAI request timed out"
      )

      val textBuf = new StringBuilder
      val thinkingBuf = new StringBuilder
      val toolCalls = scala.collection.mutable.Map[Int, (String, String, StringBuilder)]()
      var lastFinishReason: FinishReason = FinishReason.Stop
      var lastUsage: Option[Usage] = None

      Interop.forEachAsync(streamObj, Endpoint.StreamIdleTimeoutMs): chunk =>
        val choices = Dyn.arr(chunk.choices)
        if choices.nonEmpty then
          val choice = choices.head
          val delta = choice.delta

          // Reasoning delta — Chat Completions has no typed reasoning field, so
          // providers (e.g. OpenRouter) surface it as a `reasoning` string.
          Dyn.str(delta.reasoning).filter(_.nonEmpty).foreach: rtext =>
            thinkingBuf.append(rtext)
            ch.send(Right(StreamEvent.ThinkingDelta(rtext)))

          Dyn.str(delta.content).filter(_.nonEmpty).foreach: text =>
            textBuf.append(text)
            ch.send(Right(StreamEvent.Delta(text)))

          Dyn.arr(delta.tool_calls).foreach: tc =>
            val idx = Dyn.num(tc.index).map(_.toInt).getOrElse(0)
            Dyn.str(tc.id).foreach: id =>
              val name = Dyn.str(tc.function.name).getOrElse("")
              toolCalls(idx) = (id, name, new StringBuilder)
              ch.send(Right(StreamEvent.ToolCallStart(idx, id, name)))
            Dyn.str(tc.function.arguments).foreach: args =>
              toolCalls.get(idx).foreach((_, _, buf) => buf.append(args))
              ch.send(Right(StreamEvent.ToolCallDelta(idx, args)))

          Dyn.str(choice.finish_reason).foreach(r => lastFinishReason = finishReason(r))

        Dyn.defined(chunk.usage) match
          case true =>
            lastUsage = Some(
              Usage(
                inputTokens = Dyn.num(chunk.usage.prompt_tokens).map(_.toLong).getOrElse(0L),
                outputTokens = Dyn.num(chunk.usage.completion_tokens).map(_.toLong).getOrElse(0L)
              )
            )
          case false => ()

      val contents = scala.collection.mutable.ListBuffer[Content]()
      if thinkingBuf.nonEmpty then contents += Content.Thinking(thinkingBuf.toString)
      if textBuf.nonEmpty then contents += Content.Text(textBuf.toString)
      toolCalls.toList
        .sortBy(_._1)
        .foreach((_, t) => contents += Content.ToolUse(t._1, t._2, t._3.toString))
      val response = ChatResponse(
        message = Message(Role.Assistant, contents.toList),
        finishReason = lastFinishReason,
        usage = lastUsage
      )
      ch.send(Right(StreamEvent.Done(response)))

  private def convertResponse(resp: js.Dynamic): ChatResponse =
    val choice = Dyn.arr(resp.choices).headOption.getOrElse(js.Dynamic.literal())
    val message = choice.message
    val contents = scala.collection.mutable.ListBuffer[Content]()

    Dyn.str(message.content).filter(_.nonEmpty).foreach(t => contents += Content.Text(t))

    Dyn.arr(message.tool_calls).foreach: tc =>
      contents += Content.ToolUse(
        id = Dyn.str(tc.id).getOrElse(""),
        name = Dyn.str(tc.function.name).getOrElse(""),
        input = Dyn.str(tc.function.arguments).getOrElse("")
      )

    val usage =
      if Dyn.defined(resp.usage) then
        Some(
          Usage(
            inputTokens = Dyn.num(resp.usage.prompt_tokens).map(_.toLong).getOrElse(0L),
            outputTokens = Dyn.num(resp.usage.completion_tokens).map(_.toLong).getOrElse(0L)
          )
        )
      else None

    ChatResponse(
      message = Message(Role.Assistant, contents.toList),
      finishReason = finishReason(Dyn.str(choice.finish_reason).getOrElse("stop")),
      usage = usage
    )

  private def finishReason(s: String): FinishReason = s match
    case "stop"       => FinishReason.Stop
    case "length"     => FinishReason.MaxTokens
    case "tool_calls" => FinishReason.ToolUse
    case other        => FinishReason.Other(other)

  private def convertParameters(params: ToolSchema.Parameters): js.Object =
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

object OpenAICompletionEndpoint extends EndpointProvider:
  type EndpointType = OpenAICompletionEndpoint

  override def create(config: EndpointConfig): OpenAICompletionEndpoint =
    OpenAICompletionEndpoint(config)

  override def createFromEnv(): OpenAICompletionEndpoint =
    val apiKey = auk.platform.Platform.env
      .get("OPENAI_API_KEY")
      .getOrElse(throw RuntimeException("OPENAI_API_KEY environment variable is not set"))
    val baseUrl = auk.platform.Platform.env.get("OPENAI_BASE_URL").getOrElse("https://api.openai.com/v1")
    create(EndpointConfig(baseUrl = baseUrl, apiKey = apiKey))
