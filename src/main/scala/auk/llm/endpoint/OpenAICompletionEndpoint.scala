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
  private[endpoint] def buildParams(
      messages: List[Message],
      llmConfig: LLMConfig,
      stream: Boolean
  ): js.Object =
    val msgs = js.Array[js.Object]()

    def push(o: js.Dictionary[Any]): Unit = msgs.push(o.asInstanceOf[js.Object])

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
          // Replay the model's own reasoning so the chain survives across tool calls
          // (OpenRouter echoes these back unmodified; signatures ride along).
          val reasoning = msg.content.collect { case r: Content.Reasoning => r }.flatMap(_.blocks)
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
            if reasoning.nonEmpty then o("reasoning_details") = OpenAICompletionEndpoint.serializeReasoning(reasoning)
            push(o)
          else
            val o = js.Dictionary[Any]("role" -> "assistant", "content" -> msg.text)
            if reasoning.nonEmpty then o("reasoning_details") = OpenAICompletionEndpoint.serializeReasoning(reasoning)
            push(o)

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
      case ThinkingMode.Effort(EffortLevel.XHigh) => Some("xhigh")
      case ThinkingMode.Effort(EffortLevel.Max)  => Some("xhigh")
      case ThinkingMode.Budget(n) =>
        throw IllegalArgumentException(
          s"Budget($n) is not valid for OpenAI. Use ThinkingMode.Effort instead."
        )

  override def invoke(
      messages: List[Message],
      llmConfig: LLMConfig
  )(using Async): Result[ChatResponse, LLMError] =
    try
      val resp = Interop.withAbort: opts =>
        Interop.awaitWithin(
          client.chat.completions.create(buildParams(messages, llmConfig, stream = false), opts),
          Endpoint.RequestTimeoutMs,
          "OpenAI request timed out"
        )
      Right(convertResponse(resp))
    catch
      case e: Exception => Left(LLMError(s"OpenAI API error: ${Endpoint.errMsg(e)}"))

  override def stream(messages: List[Message], llmConfig: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    Endpoint.streaming("OpenAI API error"): (ch, opts) =>
      val streamObj = Interop.awaitWithin(
        client.chat.completions.create(buildParams(messages, llmConfig, stream = true), opts),
        Endpoint.RequestTimeoutMs,
        "OpenAI request timed out"
      )

      val textBuf = new StringBuilder
      val thinkingBuf = new StringBuilder
      val toolCalls = scala.collection.mutable.Map[Int, (String, String, StringBuilder)]()
      val reasoningAcc = scala.collection.mutable.Map.empty[Int, OpenAICompletionEndpoint.ReasoningAccum]
      var lastFinishReason: FinishReason = FinishReason.Stop
      var lastUsage: Option[Usage] = None

      Interop.forEachAsync(streamObj, Endpoint.StreamIdleTimeoutMs): chunk =>
        val choices = Dyn.arr(chunk.choices)
        if choices.nonEmpty then
          val choice = choices.head
          val delta = choice.delta

          // Reasoning delta — Chat Completions has no typed reasoning field, so
          // providers surface it under different names: OpenRouter uses
          // `reasoning`, while z.ai/DeepSeek-style APIs use `reasoning_content`.
          OpenAICompletionEndpoint.reasoningText(delta).foreach: rtext =>
            thinkingBuf.append(rtext)
            ch.send(Right(StreamEvent.ThinkingDelta(rtext)))

          // Structured reasoning_details (OpenRouter): accumulate by index for
          // verbatim replay. The flat `reasoning` deltas above still drive the
          // live UI, so display is unchanged.
          Dyn.arr(delta.reasoning_details).foreach(rd => OpenAICompletionEndpoint.mergeReasoningDelta(reasoningAcc, rd))

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
      // Prefer the structured reasoning_details (replayable, possibly signed); fall
      // back to the flat reasoning string (z.ai/DeepSeek, which send no details).
      val reasoning = OpenAICompletionEndpoint.finalizeReasoning(reasoningAcc)
      if reasoning.nonEmpty then contents += Content.Reasoning(reasoning)
      else if thinkingBuf.nonEmpty then contents += Content.Thinking(thinkingBuf.toString)
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

    val reasoning = OpenAICompletionEndpoint.reasoningBlocks(message)
    if reasoning.nonEmpty then contents += Content.Reasoning(reasoning)
    else OpenAICompletionEndpoint.reasoningText(message).foreach(t => contents += Content.Thinking(t))
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

  /** Read a reasoning/thinking string off a Chat Completions delta or message.
    * The API has no standard reasoning field, so providers diverge: OpenRouter
    * uses `reasoning`, while z.ai and DeepSeek-style APIs use `reasoning_content`
    * (verified live against z.ai's GLM). Returns `None` when neither is present
    * or the value is empty. */
  private[endpoint] def reasoningText(obj: js.Dynamic): Option[String] =
    Dyn.str(obj.reasoning).orElse(Dyn.str(obj.reasoning_content)).filter(_.nonEmpty)

  /** Mutable accumulator for one `reasoning_details` block. Scalar fields are
    * last-wins (they arrive whole, possibly in a later streamed delta than the
    * text — e.g. a signature); `text`/`summary` accumulate across deltas. */
  private[endpoint] final class ReasoningAccum:
    var `type`: Option[String] = None
    var signature: Option[String] = None
    var data: Option[String] = None
    var id: Option[String] = None
    var format: Option[String] = None
    var index: Option[Int] = None
    private val text = new StringBuilder
    private val summary = new StringBuilder

    def merge(rd: js.Dynamic): Unit =
      Dyn.str(rd.`type`).foreach(t => `type` = Some(t))
      Dyn.str(rd.signature).foreach(s => signature = Some(s))
      Dyn.str(rd.data).foreach(d => data = Some(d))
      Dyn.str(rd.id).foreach(i => id = Some(i))
      Dyn.str(rd.format).foreach(f => format = Some(f))
      Dyn.num(rd.index).foreach(n => index = Some(n.toInt))
      Dyn.str(rd.text).foreach(text.append)
      Dyn.str(rd.summary).foreach(summary.append)

    /** A block is worth keeping only if it carries replayable content; a bare
      * signature/id with no text/summary/data is dropped. */
    def nonEmpty: Boolean = text.nonEmpty || summary.nonEmpty || data.isDefined

    def toBlock: ReasoningBlock = ReasoningBlock(
      `type` = `type`.getOrElse(""),
      text = Option.when(text.nonEmpty)(text.toString),
      summary = Option.when(summary.nonEmpty)(summary.toString),
      data = data,
      signature = signature,
      id = id,
      format = format,
      index = index
    )

  /** Fold one streamed `reasoning_details` entry into the per-`index` accumulators
    * (deltas for the same block share an `index`). SDK-free, so unit-tested. */
  private[endpoint] def mergeReasoningDelta(
      acc: scala.collection.mutable.Map[Int, ReasoningAccum],
      rd: js.Dynamic
  ): Unit =
    val idx = Dyn.num(rd.index).map(_.toInt).getOrElse(0)
    acc.getOrElseUpdate(idx, new ReasoningAccum).merge(rd)

  /** Finalize streamed accumulators into blocks, sorted by index, empties dropped. */
  private[endpoint] def finalizeReasoning(acc: scala.collection.mutable.Map[Int, ReasoningAccum]): List[ReasoningBlock] =
    acc.toList.sortBy(_._1).map(_._2).filter(_.nonEmpty).map(_.toBlock)

  /** Parse a non-streamed message's complete `reasoning_details` array into blocks,
    * one per array entry (each is a whole, distinct block), in array order. */
  private[endpoint] def reasoningBlocks(message: js.Dynamic): List[ReasoningBlock] =
    Dyn.arr(message.reasoning_details).flatMap: rd =>
      val a = new ReasoningAccum
      a.merge(rd)
      Option.when(a.nonEmpty)(a.toBlock)

  /** Serialize blocks back to the `reasoning_details` wire array, each carrying only
    * the fields it has, so the consecutive sequence replays unmodified. */
  private[endpoint] def serializeReasoning(blocks: List[ReasoningBlock]): js.Array[js.Object] =
    val arr = js.Array[js.Object]()
    blocks.foreach: b =>
      val d = js.Dictionary[Any]("type" -> b.`type`)
      b.text.foreach(t => d("text") = t)
      b.summary.foreach(s => d("summary") = s)
      b.data.foreach(x => d("data") = x)
      b.signature.foreach(s => d("signature") = s)
      b.id.foreach(i => d("id") = i)
      b.format.foreach(f => d("format") = f)
      b.index.foreach(i => d("index") = i)
      arr.push(d.asInstanceOf[js.Object])
    arr

  override def create(config: EndpointConfig): OpenAICompletionEndpoint =
    OpenAICompletionEndpoint(config)

  override def createFromEnv(): OpenAICompletionEndpoint =
    val apiKey = auk.platform.Platform.env
      .get("OPENAI_API_KEY")
      .getOrElse(throw RuntimeException("OPENAI_API_KEY environment variable is not set"))
    val baseUrl = auk.platform.Platform.env.get("OPENAI_BASE_URL").getOrElse("https://api.openai.com/v1")
    create(EndpointConfig(baseUrl = baseUrl, apiKey = apiKey))
