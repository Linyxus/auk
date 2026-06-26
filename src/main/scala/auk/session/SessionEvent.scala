package auk.session

import auk.llm.endpoint.{ChatResponse, Content, FinishReason, Message, ReasoningBlock, Role, Usage}
import auk.llm.tools.Json
import auk.llm.tools.Json.get

/** Which model produced an assistant reply. The live event stream carries this
  * (via `AgentEvent.ModelSwitched`), but it used to be lost on persistence, so a
  * resumed session could not tell which model generated which turn. Captured
  * per-reply now so the log is self-describing. */
final case class ModelInfo(provider: String, modelId: String, label: String, contextWindow: Int)

/** A single, durable step in a session's history.
  *
  * Events are the *source of truth* for a conversation: the model-facing
  * `List[Message]` the agent loop carries is a fold over them. Recording at
  * this granularity — one event per message appended to the conversation —
  * makes a session resumable a step at a time: a crash loses at most the
  * in-flight step, and the rest replays exactly.
  *
  * The three cases mirror the three points where the loop grows its history: the
  * user submits a line, the model replies, and a batch of tool results comes
  * back. Streaming deltas are deliberately *not* events — they are a rendering
  * concern that is reconstructed from the completed [[AssistantResponded]] reply.
  */
enum SessionEvent:
  /** The user submitted a line of input, starting (or continuing) a turn. */
  case UserSubmitted(text: String)

  /** The model produced a complete assistant reply (which may itself request
    * tools via [[Content.ToolUse]] blocks). The whole [[ChatResponse]] is kept —
    * not just its message — so a resumed session can restore the finish reason
    * and, notably, the token usage that drives the context-window gauge. `model`
    * records which model produced it (absent in old logs and any reply persisted
    * before the model was known). */
  case AssistantResponded(response: ChatResponse, model: Option[ModelInfo] = None)

  /** A batch of tool results, fed back to the model as the next user message.
    * `metadata` is a side-channel of per-tool execution stats (keyed by
    * `toolUseId`) — e.g. a sub-agent's or eval's token totals and round count —
    * that the live stream carries on `ToolRunEnd` but the model never sees. It is
    * recorded for inspection and ignored on replay. */
  case ToolResultsReceived(results: List[Content.ToolResult], metadata: Map[String, Map[String, String]] = Map.empty)

  /** The user interrupted the in-flight turn (`Ctrl+C k`). Replays as a
    * user-role note so the model sees, on the next turn, that the previous turn
    * was cut short. */
  case Interrupted

  /** An out-of-band system notification folded into the conversation (idle: it
    * woke the agent; mid-turn: it was drained at a round boundary). Replays as a
    * user-role message wrapped in `<system-reminder>` tags — see
    * [[auk.llm.endpoint.Message.systemNotice]] — so resume reconstructs the same
    * context the live turn saw. */
  case SystemNotice(text: String)

  /** A checkpoint replacing all earlier events in the model-facing context.
    *
    * The log remains append-only: prior events stay in the transcript/audit trail,
    * while replay to the model starts at the latest compaction event and carries
    * this summary as the authoritative context for everything before it. */
  case ContextCompacted(summary: String, model: Option[ModelInfo] = None)

object SessionEvent:
  /** Encode an event as one compact JSON line for an append-only log. */
  def encode(event: SessionEvent): String = toJson(event).render

  /** Decode one log line back into an event, or describe why it could not be. */
  def decode(line: String): Either[String, SessionEvent] =
    Json.parse(line).flatMap(fromJson)

  // --- JSON encoding ---------------------------------------------------------

  def toJson(event: SessionEvent): Json = event match
    case UserSubmitted(text) =>
      Json.Obj(List("type" -> Json.Str("user_submitted"), "text" -> Json.Str(text)))
    case AssistantResponded(response, model) =>
      Json.Obj(List(
        "type"         -> Json.Str("assistant_responded"),
        "message"      -> encodeMessage(response.message),
        "finishReason" -> Json.Str(encodeFinishReason(response.finishReason))
      ) ++ response.usage.map(u => "usage" -> encodeUsage(u))
        ++ model.map(m => "model" -> encodeModel(m)))
    case ToolResultsReceived(results, metadata) =>
      Json.Obj(List(
        "type"    -> Json.Str("tool_results_received"),
        "results" -> Json.Arr(results.map(encodeContent))
      ) ++ Option.when(metadata.nonEmpty)("metadata" -> encodeToolMetadata(metadata)))
    case Interrupted =>
      Json.Obj(List("type" -> Json.Str("interrupted")))
    case SystemNotice(text) =>
      Json.Obj(List("type" -> Json.Str("system_notice"), "text" -> Json.Str(text)))
    case ContextCompacted(summary, model) =>
      Json.Obj(List(
        "type"    -> Json.Str("context_compacted"),
        "summary" -> Json.Str(summary)
      ) ++ model.map(m => "model" -> encodeModel(m)))

  private def encodeMessage(m: Message): Json =
    Json.Obj(List(
      "role"    -> Json.Str(encodeRole(m.role)),
      "content" -> Json.Arr(m.content.map(encodeContent))
    ))

  private def encodeRole(r: Role): String = r match
    case Role.System    => "system"
    case Role.User      => "user"
    case Role.Assistant => "assistant"

  private def encodeFinishReason(r: FinishReason): String = r match
    case FinishReason.Stop       => "stop"
    case FinishReason.MaxTokens  => "max_tokens"
    case FinishReason.ToolUse    => "tool_use"
    case FinishReason.Other(v)   => v

  private def encodeUsage(u: Usage): Json =
    Json.Obj(List(
      "inputTokens"  -> Json.num(u.inputTokens),
      "outputTokens" -> Json.num(u.outputTokens)
    ))

  private def encodeModel(m: ModelInfo): Json =
    Json.Obj(List(
      "provider"      -> Json.Str(m.provider),
      "id"            -> Json.Str(m.modelId),
      "label"         -> Json.Str(m.label),
      "contextWindow" -> Json.num(m.contextWindow)
    ))

  /** Encode the per-tool execution stats as `{ toolUseId: { key: value } }`. */
  private def encodeToolMetadata(metadata: Map[String, Map[String, String]]): Json =
    Json.Obj(metadata.toList.map { (id, kv) =>
      id -> Json.Obj(kv.toList.map((k, v) => k -> Json.Str(v)))
    })

  private def encodeContent(c: Content): Json = c match
    case Content.Text(text) =>
      Json.Obj(List("kind" -> Json.Str("text"), "text" -> Json.Str(text)))
    case Content.Thinking(text, signature) =>
      val base = List("kind" -> Json.Str("thinking"), "text" -> Json.Str(text))
      Json.Obj(signature.fold(base)(s => base :+ ("signature" -> Json.Str(s))))
    case Content.RedactedThinking(data) =>
      Json.Obj(List("kind" -> Json.Str("redacted_thinking"), "data" -> Json.Str(data)))
    case Content.Reasoning(blocks) =>
      Json.Obj(List("kind" -> Json.Str("reasoning"), "blocks" -> Json.Arr(blocks.map(encodeReasoningBlock))))
    case Content.ToolUse(id, name, input) =>
      Json.Obj(List(
        "kind"  -> Json.Str("tool_use"),
        "id"    -> Json.Str(id),
        "name"  -> Json.Str(name),
        "input" -> Json.Str(input)
      ))
    case Content.ToolResult(toolUseId, content, isError) =>
      Json.Obj(List(
        "kind"      -> Json.Str("tool_result"),
        "toolUseId" -> Json.Str(toolUseId),
        "content"   -> Json.Str(content),
        "isError"   -> Json.Bool(isError)
      ))

  /** Encode a reasoning block with only the fields it carries (mirrors the wire
    * shape, so the block round-trips and replays unmodified). */
  private def encodeReasoningBlock(b: ReasoningBlock): Json =
    val fields = List.newBuilder[(String, Json)]
    fields += ("type" -> Json.Str(b.`type`))
    b.text.foreach(t => fields += ("text" -> Json.Str(t)))
    b.summary.foreach(s => fields += ("summary" -> Json.Str(s)))
    b.data.foreach(d => fields += ("data" -> Json.Str(d)))
    b.signature.foreach(s => fields += ("signature" -> Json.Str(s)))
    b.id.foreach(i => fields += ("id" -> Json.Str(i)))
    b.format.foreach(f => fields += ("format" -> Json.Str(f)))
    b.index.foreach(i => fields += ("index" -> Json.num(i)))
    Json.Obj(fields.result())

  // --- JSON decoding ---------------------------------------------------------

  def fromJson(json: Json): Either[String, SessionEvent] = json match
    case obj: Json.Obj =>
      obj.get("type") match
        case Some(Json.Str("user_submitted")) =>
          str(obj, "text").map(UserSubmitted(_))
        case Some(Json.Str("assistant_responded")) =>
          field(obj, "message").flatMap(decodeMessage).map: message =>
            AssistantResponded(
              ChatResponse(
                message,
                finishReason = decodeFinishReason(strOpt(obj, "finishReason")),
                usage = decodeUsage(obj)
              ),
              model = decodeModel(obj)
            )
        case Some(Json.Str("tool_results_received")) =>
          for
            items    <- arr(obj, "results")
            contents <- traverse(items)(decodeContent)
            results  <- traverse(contents)(asToolResult)
          yield ToolResultsReceived(results, decodeToolMetadata(obj))
        case Some(Json.Str("interrupted")) => Right(Interrupted)
        case Some(Json.Str("system_notice")) =>
          str(obj, "text").map(SystemNotice(_))
        case Some(Json.Str("context_compacted")) =>
          str(obj, "summary").map(ContextCompacted(_, decodeModel(obj)))
        case Some(Json.Str(other)) => Left(s"unknown session event type '$other'")
        case Some(other)           => Left(s"'type' should be a string but was ${other.typeName}")
        case None                  => Left("session event is missing a 'type'")
    case other => Left(s"session event should be an object but was ${other.typeName}")

  /** The suffix of `events` that should be replayed into model context.
    *
    * A compaction event is a checkpoint: include the latest checkpoint itself
    * (its summary is the replacement context), then every event after it. If no
    * checkpoint exists, the full log is replayed. */
  def modelContextEvents(events: List[SessionEvent]): List[SessionEvent] =
    events.lastIndexWhere:
      case ContextCompacted(_, _) => true
      case _                      => false
    match
      case -1  => events
      case idx => events.drop(idx)

  private def decodeMessage(json: Json): Either[String, Message] = json match
    case obj: Json.Obj =>
      for
        roleStr     <- str(obj, "role")
        role        <- decodeRole(roleStr)
        contentJson <- arr(obj, "content")
        content     <- traverse(contentJson)(decodeContent)
      yield Message(role, content)
    case other => Left(s"message should be an object but was ${other.typeName}")

  private def decodeRole(s: String): Either[String, Role] = s match
    case "system"    => Right(Role.System)
    case "user"      => Right(Role.User)
    case "assistant" => Right(Role.Assistant)
    case other       => Left(s"unknown role '$other'")

  /** Decode a finish reason, tolerating old logs (and any unmapped string) by
    * falling back to [[FinishReason.Stop]] / [[FinishReason.Other]]. */
  private def decodeFinishReason(s: Option[String]): FinishReason = s match
    case Some("stop")       => FinishReason.Stop
    case Some("max_tokens") => FinishReason.MaxTokens
    case Some("tool_use")   => FinishReason.ToolUse
    case Some(other)        => FinishReason.Other(other)
    case None               => FinishReason.Stop

  /** Decode token usage, absent in old logs (and then `None`). */
  private def decodeUsage(obj: Json.Obj): Option[Usage] =
    obj.get("usage") match
      case Some(u: Json.Obj) =>
        Some(Usage(inputTokens = longOr(u, "inputTokens", 0L), outputTokens = longOr(u, "outputTokens", 0L)))
      case _ => None

  /** Decode the model that produced a reply, absent in old logs (then `None`). */
  private def decodeModel(obj: Json.Obj): Option[ModelInfo] =
    obj.get("model") match
      case Some(m: Json.Obj) =>
        Some(ModelInfo(
          provider = strOpt(m, "provider").getOrElse(""),
          modelId = strOpt(m, "id").getOrElse(""),
          label = strOpt(m, "label").getOrElse(""),
          contextWindow = intOpt(m, "contextWindow").getOrElse(0)
        ))
      case _ => None

  /** Decode the per-tool execution stats, absent in old logs (then empty). */
  private def decodeToolMetadata(obj: Json.Obj): Map[String, Map[String, String]] =
    obj.get("metadata") match
      case Some(m: Json.Obj) =>
        m.fields.collect {
          case (id, kv: Json.Obj) =>
            id -> kv.fields.collect { case (k, Json.Str(v)) => k -> v }.toMap
        }.toMap
      case _ => Map.empty

  private def decodeContent(json: Json): Either[String, Content] = json match
    case obj: Json.Obj =>
      obj.get("kind") match
        case Some(Json.Str("text"))     => str(obj, "text").map(Content.Text(_))
        case Some(Json.Str("thinking")) => str(obj, "text").map(Content.Thinking(_, strOpt(obj, "signature")))
        case Some(Json.Str("redacted_thinking")) => str(obj, "data").map(Content.RedactedThinking(_))
        case Some(Json.Str("reasoning")) =>
          arr(obj, "blocks").flatMap(traverse(_)(decodeReasoningBlock)).map(Content.Reasoning(_))
        case Some(Json.Str("tool_use")) =>
          for
            id    <- str(obj, "id")
            name  <- str(obj, "name")
            input <- str(obj, "input")
          yield Content.ToolUse(id, name, input)
        case Some(Json.Str("tool_result")) =>
          for
            toolUseId <- str(obj, "toolUseId")
            content   <- str(obj, "content")
          yield Content.ToolResult(toolUseId, content, boolOr(obj, "isError", false))
        case Some(Json.Str(other)) => Left(s"unknown content kind '$other'")
        case Some(other)           => Left(s"'kind' should be a string but was ${other.typeName}")
        case None                  => Left("content is missing a 'kind'")
    case other => Left(s"content should be an object but was ${other.typeName}")

  private def asToolResult(c: Content): Either[String, Content.ToolResult] = c match
    case tr: Content.ToolResult => Right(tr)
    case other                  => Left("a tool_results event may only contain tool_result blocks")

  // --- small decode helpers --------------------------------------------------

  private def field(obj: Json.Obj, key: String): Either[String, Json] =
    obj.get(key).toRight(s"missing field '$key'")

  private def str(obj: Json.Obj, key: String): Either[String, String] =
    field(obj, key).flatMap:
      case Json.Str(s) => Right(s)
      case other       => Left(s"field '$key' should be a string but was ${other.typeName}")

  /** An optional string field: `None` if absent or not a string. Used for
    * back-compatible additions like a thinking block's `signature`. */
  private def strOpt(obj: Json.Obj, key: String): Option[String] =
    obj.get(key) match
      case Some(Json.Str(s)) => Some(s)
      case _                 => None

  /** An optional int field: `None` if absent or not a number. */
  private def intOpt(obj: Json.Obj, key: String): Option[Int] =
    obj.get(key) match
      case Some(Json.Num(n)) => Some(n.toInt)
      case _                 => None

  private def decodeReasoningBlock(json: Json): Either[String, ReasoningBlock] = json match
    case obj: Json.Obj =>
      str(obj, "type").map: tpe =>
        ReasoningBlock(
          `type` = tpe,
          text = strOpt(obj, "text"),
          summary = strOpt(obj, "summary"),
          data = strOpt(obj, "data"),
          signature = strOpt(obj, "signature"),
          id = strOpt(obj, "id"),
          format = strOpt(obj, "format"),
          index = intOpt(obj, "index")
        )
    case other => Left(s"reasoning block should be an object but was ${other.typeName}")

  private def arr(obj: Json.Obj, key: String): Either[String, List[Json]] =
    field(obj, key).flatMap:
      case Json.Arr(es) => Right(es)
      case other        => Left(s"field '$key' should be an array but was ${other.typeName}")

  private def boolOr(obj: Json.Obj, key: String, default: Boolean): Boolean =
    obj.get(key) match
      case Some(Json.Bool(b)) => b
      case _                  => default

  private def longOr(obj: Json.Obj, key: String, default: Long): Long =
    obj.get(key) match
      case Some(Json.Num(n)) => n.toLong
      case _                 => default

  private def traverse[A, B](xs: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    xs.foldRight[Either[String, List[B]]](Right(Nil)): (x, acc) =>
      for h <- f(x); t <- acc yield h :: t
