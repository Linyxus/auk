package auk.llm.endpoint

enum Role:
  case System, User, Assistant

enum Content:
  case Text(text: String)

  /** A reasoning block. `signature` is the provider's cryptographic attestation
    * of the reasoning (Anthropic extended thinking): when present it MUST be
    * replayed verbatim alongside the text on subsequent tool-use turns, or the
    * API rejects the request. Providers without signed reasoning (e.g. OpenAI)
    * leave it `None`, in which case the block is dropped on replay rather than
    * sent unsigned. */
  case Thinking(text: String, signature: Option[String] = None)

  /** An opaque, encrypted reasoning block Anthropic returns when its own text is
    * withheld (`redacted_thinking`). It carries no human-readable text — only the
    * `data` blob, which must be replayed verbatim to preserve the reasoning
    * chain across tool use. */
  case RedactedThinking(data: String)

  case ToolUse(id: String, name: String, input: String)
  case ToolResult(toolUseId: String, content: String, isError: Boolean = false)

case class Message(role: Role, content: List[Content]):
  def text: String =
    val texts = content.collect:
      case Content.Text(t) => t
    texts.mkString

  def thinking: String =
    val thoughts = content.collect:
      case Content.Thinking(t, _) => t
    thoughts.mkString

object Message:
  def user(text: String): Message =
    Message(Role.User, List(Content.Text(text)))

  def assistant(text: String): Message =
    Message(Role.Assistant, List(Content.Text(text)))

  def system(text: String): Message =
    Message(Role.System, List(Content.Text(text)))

  def toolResult(
      toolUseId: String,
      content: String,
      isError: Boolean = false
  ): Message =
    Message(Role.User, List(Content.ToolResult(toolUseId, content, isError)))

case class ChatResponse(
    message: Message,
    finishReason: FinishReason,
    usage: Option[Usage] = None
)

enum FinishReason:
  case Stop, MaxTokens, ToolUse
  case Other(value: String)

case class Usage(
    inputTokens: Long,
    outputTokens: Long
)

enum StreamEvent:
  case Delta(text: String)
  case ThinkingDelta(text: String)
  case ToolCallStart(index: Int, id: String, name: String)
  case ToolCallDelta(index: Int, argumentDelta: String)
  case Done(response: ChatResponse)

  /** A tool the model requested has begun executing locally. Emitted by the
    * agent loop (not the endpoint) so the UI can show a running indicator. */
  case ToolRunStart(id: String, name: String)

  /** A tool finished executing. `metadata` carries the tool's structured
    * side-channel (e.g. a sub-agent's token totals) for the UI to display. */
  case ToolRunEnd(id: String, isError: Boolean, metadata: Map[String, String])
