package auk.session

import auk.llm.endpoint.{Content, Message, Role}
import auk.llm.tools.Json
import auk.llm.tools.Json.get

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
    * tools via [[Content.ToolUse]] blocks). */
  case AssistantResponded(message: Message)

  /** A batch of tool results, fed back to the model as the next user message. */
  case ToolResultsReceived(results: List[Content.ToolResult])

  /** The user interrupted the in-flight turn (`Ctrl+C k`). Replays as a
    * user-role note so the model sees, on the next turn, that the previous turn
    * was cut short. */
  case Interrupted

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
    case AssistantResponded(message) =>
      Json.Obj(List("type" -> Json.Str("assistant_responded"), "message" -> encodeMessage(message)))
    case ToolResultsReceived(results) =>
      Json.Obj(List("type" -> Json.Str("tool_results_received"), "results" -> Json.Arr(results.map(encodeContent))))
    case Interrupted =>
      Json.Obj(List("type" -> Json.Str("interrupted")))

  private def encodeMessage(m: Message): Json =
    Json.Obj(List(
      "role"    -> Json.Str(encodeRole(m.role)),
      "content" -> Json.Arr(m.content.map(encodeContent))
    ))

  private def encodeRole(r: Role): String = r match
    case Role.System    => "system"
    case Role.User      => "user"
    case Role.Assistant => "assistant"

  private def encodeContent(c: Content): Json = c match
    case Content.Text(text) =>
      Json.Obj(List("kind" -> Json.Str("text"), "text" -> Json.Str(text)))
    case Content.Thinking(text, signature) =>
      val base = List("kind" -> Json.Str("thinking"), "text" -> Json.Str(text))
      Json.Obj(signature.fold(base)(s => base :+ ("signature" -> Json.Str(s))))
    case Content.RedactedThinking(data) =>
      Json.Obj(List("kind" -> Json.Str("redacted_thinking"), "data" -> Json.Str(data)))
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

  // --- JSON decoding ---------------------------------------------------------

  def fromJson(json: Json): Either[String, SessionEvent] = json match
    case obj: Json.Obj =>
      obj.get("type") match
        case Some(Json.Str("user_submitted")) =>
          str(obj, "text").map(UserSubmitted(_))
        case Some(Json.Str("assistant_responded")) =>
          field(obj, "message").flatMap(decodeMessage).map(AssistantResponded(_))
        case Some(Json.Str("tool_results_received")) =>
          for
            items    <- arr(obj, "results")
            contents <- traverse(items)(decodeContent)
            results  <- traverse(contents)(asToolResult)
          yield ToolResultsReceived(results)
        case Some(Json.Str("interrupted")) => Right(Interrupted)
        case Some(Json.Str(other)) => Left(s"unknown session event type '$other'")
        case Some(other)           => Left(s"'type' should be a string but was ${other.typeName}")
        case None                  => Left("session event is missing a 'type'")
    case other => Left(s"session event should be an object but was ${other.typeName}")

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

  private def decodeContent(json: Json): Either[String, Content] = json match
    case obj: Json.Obj =>
      obj.get("kind") match
        case Some(Json.Str("text"))     => str(obj, "text").map(Content.Text(_))
        case Some(Json.Str("thinking")) => str(obj, "text").map(Content.Thinking(_, strOpt(obj, "signature")))
        case Some(Json.Str("redacted_thinking")) => str(obj, "data").map(Content.RedactedThinking(_))
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

  private def arr(obj: Json.Obj, key: String): Either[String, List[Json]] =
    field(obj, key).flatMap:
      case Json.Arr(es) => Right(es)
      case other        => Left(s"field '$key' should be an array but was ${other.typeName}")

  private def boolOr(obj: Json.Obj, key: String, default: Boolean): Boolean =
    obj.get(key) match
      case Some(Json.Bool(b)) => b
      case _                  => default

  private def traverse[A, B](xs: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    xs.foldRight[Either[String, List[B]]](Right(Nil)): (x, acc) =>
      for h <- f(x); t <- acc yield h :: t
