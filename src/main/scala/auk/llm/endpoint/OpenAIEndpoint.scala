package auk.llm.endpoint

import scala.scalajs.js
import gears.async.{Async, ReadableChannel}
import auk.utils.Result
import auk.platform.js.{OpenAI, Interop}

/** OpenAI endpoint using the Responses API (`/v1/responses`), via the npm
  * `openai` SDK.
  *
  * Supports tools and reasoning/thinking simultaneously, which the older Chat
  * Completions API does not. For the legacy API, use
  * [[OpenAICompletionEndpoint]].
  */
class OpenAIEndpoint(config: EndpointConfig) extends Endpoint:

  private lazy val client: OpenAI =
    OpenAI(js.Dynamic.literal(apiKey = config.apiKey, baseURL = config.baseUrl).asInstanceOf[js.Object])

  private[endpoint] def buildParams(
      messages: List[Message],
      llmConfig: LLMConfig,
      stream: Boolean
  ): js.Object =
    val input = js.Array[js.Object]()
    def push(o: js.Dictionary[Any]): Unit = input.push(o.asInstanceOf[js.Object])

    messages.foreach: msg =>
      msg.role match
        case Role.System => () // carried via `instructions`
        case Role.User =>
          val toolResults = msg.content.collect { case tr: Content.ToolResult => tr }
          if toolResults.nonEmpty then
            toolResults.foreach: tr =>
              push(js.Dictionary("type" -> "function_call_output", "call_id" -> tr.toolUseId, "output" -> tr.content))
            // Text alongside tool results (e.g. a steering message coalesced after
            // them) must still be sent — as its own user turn after the outputs.
            val text = msg.content.collect { case Content.Text(t) => t }.mkString("\n")
            if text.nonEmpty then push(js.Dictionary("role" -> "user", "content" -> text))
          else push(js.Dictionary("role" -> "user", "content" -> msg.text))
        case Role.Assistant =>
          if msg.text.nonEmpty then push(js.Dictionary("role" -> "assistant", "content" -> msg.text))
          msg.content
            .collect { case tu: Content.ToolUse => tu }
            .foreach: tu =>
              push(js.Dictionary("type" -> "function_call", "call_id" -> tu.id, "name" -> tu.name, "arguments" -> tu.input))

    val params = js.Dictionary[Any]("model" -> llmConfig.model, "input" -> input)
    llmConfig.maxTokens.foreach(n => params("max_output_tokens") = n)
    llmConfig.systemPrompt.foreach(p => params("instructions") = p)
    llmConfig.temperature.foreach(t => params("temperature") = t)
    llmConfig.topP.foreach(p => params("top_p") = p)

    if llmConfig.tools.nonEmpty then
      val tools = js.Array[js.Object]()
      llmConfig.tools.foreach: tool =>
        tools.push(
          js.Dictionary[Any](
            "type" -> "function",
            "name" -> tool.name,
            "description" -> tool.description,
            "parameters" -> convertParameters(tool.parameters),
            "strict" -> false
          ).asInstanceOf[js.Object]
        )
      params("tools") = tools

    reasoning(llmConfig.thinking).foreach(r => params("reasoning") = r)

    if stream then params("stream") = true
    params.asInstanceOf[js.Object]

  private def reasoning(thinking: Option[ThinkingMode]): Option[js.Object] =
    def of(effort: String): js.Object =
      js.Dictionary[Any]("effort" -> effort, "summary" -> "auto").asInstanceOf[js.Object]
    thinking.flatMap:
      case ThinkingMode.Disabled                   => None
      case ThinkingMode.Auto                       => Some(of("medium"))
      case ThinkingMode.Effort(EffortLevel.Low)    => Some(of("low"))
      case ThinkingMode.Effort(EffortLevel.Medium) => Some(of("medium"))
      case ThinkingMode.Effort(EffortLevel.High)   => Some(of("high"))
      case ThinkingMode.Effort(EffortLevel.XHigh)  => Some(of("xhigh"))
      case ThinkingMode.Effort(EffortLevel.Max)   => Some(of("xhigh"))
      case ThinkingMode.Budget(n) =>
        throw IllegalArgumentException(s"Budget($n) is not valid for OpenAI. Use ThinkingMode.Effort instead.")

  override def invoke(
      messages: List[Message],
      llmConfig: LLMConfig
  )(using Async): Result[ChatResponse, LLMError] =
    try
      Right(
        convertResponse(
          Interop.withAbort: opts =>
            Interop.awaitWithin(
              client.responses.create(buildParams(messages, llmConfig, stream = false), opts),
              Endpoint.RequestTimeoutMs,
              "OpenAI request timed out"
            )
        )
      )
    catch case e: Exception => Left(LLMError(s"OpenAI API error: ${Endpoint.errMsg(e)}"))

  override def stream(messages: List[Message], llmConfig: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    Endpoint.streaming("OpenAI API error"): (ch, opts) =>
      val streamObj = Interop.awaitWithin(
        client.responses.create(buildParams(messages, llmConfig, stream = true), opts),
        Endpoint.RequestTimeoutMs,
        "OpenAI request timed out"
      )

      val textBuf = new StringBuilder
      val thinkingBuf = new StringBuilder
      val toolCalls = scala.collection.mutable.Map[String, (Int, String, StringBuilder)]()
      var lastResponse: js.Dynamic | Null = null

      Interop.forEachAsync(streamObj, Endpoint.StreamIdleTimeoutMs): event =>
        Dyn.str(event.`type`).getOrElse("") match
          case "response.output_text.delta" =>
            val text = Dyn.str(event.delta).getOrElse("")
            textBuf.append(text)
            ch.send(Right(StreamEvent.Delta(text)))
          case "response.reasoning_text.delta" | "response.reasoning_summary_text.delta" =>
            val text = Dyn.str(event.delta).getOrElse("")
            thinkingBuf.append(text)
            ch.send(Right(StreamEvent.ThinkingDelta(text)))
          case "response.output_item.added" =>
            val item = event.item
            val idx = Dyn.num(event.output_index).map(_.toInt).getOrElse(0)
            if Dyn.str(item.`type`).contains("function_call") then
              val callId = Dyn.str(item.call_id).getOrElse("")
              val name = Dyn.str(item.name).getOrElse("")
              toolCalls(callId) = (idx, name, new StringBuilder)
              ch.send(Right(StreamEvent.ToolCallStart(idx, callId, name)))
          case "response.function_call_arguments.delta" =>
            val itemId = Dyn.str(event.item_id).getOrElse("")
            val argsDelta = Dyn.str(event.delta).getOrElse("")
            toolCalls.get(itemId).foreach((_, _, buf) => buf.append(argsDelta))
            ch.send(Right(StreamEvent.ToolCallDelta(Dyn.num(event.output_index).map(_.toInt).getOrElse(0), argsDelta)))
          case "response.completed" =>
            lastResponse = event.response
          case _ => ()

      val response =
        if lastResponse != null then convertResponse(lastResponse.nn)
        else
          val contents = scala.collection.mutable.ListBuffer[Content]()
          if thinkingBuf.nonEmpty then contents += Content.Thinking(thinkingBuf.toString)
          if textBuf.nonEmpty then contents += Content.Text(textBuf.toString)
          toolCalls.toList.sortBy(_._2._1).foreach((id, t) => contents += Content.ToolUse(id, t._2, t._3.toString))
          ChatResponse(
            message = Message(Role.Assistant, contents.toList),
            finishReason = if toolCalls.nonEmpty then FinishReason.ToolUse else FinishReason.Stop,
            usage = None
          )
      ch.send(Right(StreamEvent.Done(response)))

  private def convertResponse(response: js.Dynamic): ChatResponse =
    val contents = scala.collection.mutable.ListBuffer[Content]()
    val output = Dyn.arr(response.output)

    output.foreach: item =>
      Dyn.str(item.`type`).getOrElse("") match
        case "reasoning" =>
          val summary = Dyn.arr(item.summary).flatMap(s => Dyn.str(s.text)).mkString
          if summary.nonEmpty then contents += Content.Thinking(summary)
        case "message" =>
          Dyn.arr(item.content).foreach: c =>
            if Dyn.str(c.`type`).contains("output_text") then
              Dyn.str(c.text).filter(_.nonEmpty).foreach(t => contents += Content.Text(t))
        case "function_call" =>
          contents += Content.ToolUse(
            id = Dyn.str(item.call_id).getOrElse(""),
            name = Dyn.str(item.name).getOrElse(""),
            input = Dyn.str(item.arguments).getOrElse("")
          )
        case _ => ()

    val hasFunctionCalls = output.exists(i => Dyn.str(i.`type`).contains("function_call"))
    val finishReason =
      if hasFunctionCalls then FinishReason.ToolUse
      else
        Dyn.str(response.status).getOrElse("completed") match
          case "completed"  => FinishReason.Stop
          case "incomplete" => FinishReason.MaxTokens
          case other        => FinishReason.Other(other)

    val usage =
      if Dyn.defined(response.usage) then
        Some(
          Usage(
            inputTokens = Dyn.num(response.usage.input_tokens).map(_.toLong).getOrElse(0L),
            outputTokens = Dyn.num(response.usage.output_tokens).map(_.toLong).getOrElse(0L)
          )
        )
      else None

    ChatResponse(Message(Role.Assistant, contents.toList), finishReason, usage)

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

object OpenAIEndpoint extends EndpointProvider:
  type EndpointType = OpenAIEndpoint

  override def create(config: EndpointConfig): OpenAIEndpoint =
    OpenAIEndpoint(config)

  override def createFromEnv(): OpenAIEndpoint =
    val apiKey = auk.platform.Platform.env
      .get("OPENAI_API_KEY")
      .getOrElse(throw RuntimeException("OPENAI_API_KEY environment variable is not set"))
    val baseUrl = auk.platform.Platform.env.get("OPENAI_BASE_URL").getOrElse("https://api.openai.com/v1")
    create(EndpointConfig(baseUrl = baseUrl, apiKey = apiKey))
