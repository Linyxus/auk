package auk.llm.endpoint

enum Role:
  case System, User, Assistant

/** One block of OpenRouter "reasoning_details" reasoning, carried on an assistant
  * message so it can be replayed verbatim on the next tool-calling request — the
  * platform requires the consecutive blocks, in `index` order, passed back
  * unmodified to keep the reasoning chain alive across tool use.
  *
  * `type` is the block kind (`reasoning.text` | `reasoning.encrypted` |
  * `reasoning.summary`), kept as a raw `String` so an unrecognized future kind
  * still round-trips. The remaining fields are whichever the block carried
  * (`text`(+`signature`) for text, `data` for encrypted, `summary` for summary,
  * plus `id`/`format`/`index`); absent fields stay `None` and are omitted on
  * replay. We deliberately keep no catch-all bag for unknown scalar fields — the
  * documented schema is stable and a typed shape round-trips cleanly through
  * session persistence. */
final case class ReasoningBlock(
    `type`: String,
    text: Option[String] = None,
    summary: Option[String] = None,
    data: Option[String] = None,
    signature: Option[String] = None,
    id: Option[String] = None,
    format: Option[String] = None,
    index: Option[Int] = None
):
  /** The human-readable reasoning this block carries, if any (an encrypted block
    * has none). Used for display on session resume. */
  def displayText: Option[String] = text.orElse(summary).filter(_.nonEmpty)

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

  /** OpenRouter "reasoning_details": the model's reasoning for one assistant turn,
    * kept as the exact blocks returned so they can be replayed unmodified across
    * tool calls (see [[ReasoningBlock]]). Distinct from [[Thinking]] — that is the
    * Anthropic-native / flat-string form; this carries OpenRouter's structured
    * (and possibly signed or encrypted) blocks. Endpoints that don't speak this
    * shape drop it on replay rather than mis-send it. */
  case Reasoning(blocks: List[ReasoningBlock])

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

  /** An out-of-band system notification carried as a user-role message, wrapped
    * in `<system-reminder>` tags so the model reads it as ambient context rather
    * than the user speaking. The single source of truth for the wrapper, so the
    * live steering path and session replay produce identical text. */
  def systemNotice(text: String): Message =
    Message(Role.User, List(Content.Text(s"<system-reminder>\n$text\n</system-reminder>")))

  /** Merge adjacent same-role messages by concatenating their content, so the
    * wire payload never carries two consecutive same-role turns (which several
    * providers reject). Real-time steering appends user-role messages right after
    * a tool-results user message; coalescing collapses them into one user turn
    * (e.g. tool_result blocks then a text block). Pure, total, and idempotent —
    * a no-op on an already-alternating history. Apply only at the wire boundary,
    * never to the carried history, which must stay 1:1 with session events. */
  def coalesce(messages: List[Message]): List[Message] =
    // Accumulate reversed (newest at the head) so each merge/append is O(1);
    // reverse once at the end. O(n) overall — this runs on every model round.
    messages
      .foldLeft(List.empty[Message]): (acc, m) =>
        acc match
          case last :: rest if last.role == m.role => last.copy(content = last.content ++ m.content) :: rest
          case _                                   => m :: acc
      .reverse

case class ChatResponse(
    message: Message,
    finishReason: FinishReason,
    usage: Option[Usage] = None
)

enum FinishReason:
  case Stop, MaxTokens, ToolUse
  case Other(value: String)

/** Token usage for one LLM round. `inputTokens` is the *total* prompt size —
  * freshly-processed input plus any cache-read and cache-creation tokens — not
  * merely the uncached remainder the wire reports under `input_tokens`. Folding
  * the cache counts in keeps context occupancy honest when prompt caching is
  * active: on a cache hit the bulk of the prompt is billed as cache reads and
  * the raw `input_tokens` collapses, which would otherwise read as ~0% context.
  * See [[auk.llm.endpoint.AnthropicEndpoint.mergeUsage]]. */
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

  /** An LLM round finished with exact token usage. Where [[Done]] is the turn's
    * single terminal event (the UI commits on it), this fires after *every*
    * round — including ones that requested tools — so a UI can anchor a live
    * token tally to real usage as the turn unfolds, instead of estimating the
    * whole turn. Emitted by the agent loop (not the endpoint). */
  case RoundComplete(usage: Usage)

  /** A tool the model requested has begun executing locally. Emitted by the
    * agent loop (not the endpoint) so the UI can show a running indicator. */
  case ToolRunStart(id: String, name: String)

  /** A running tool reported live progress before finishing. `metadata` uses the
    * same vocabulary as [[ToolRunEnd]] (e.g. a sub-agent's running token totals)
    * so the UI can update the same fields it would on completion. Emitted by the
    * agent loop (not the endpoint), driven by a tool's [[ProgressSink]]. */
  case ToolRunProgress(id: String, metadata: Map[String, String])

  /** A tool finished executing. `metadata` carries the tool's structured
    * side-channel (e.g. a sub-agent's token totals) and `output` the text
    * handed back to the model, for UIs that render a call's result (e.g. a
    * REPL transcript). */
  case ToolRunEnd(id: String, isError: Boolean, metadata: Map[String, String], output: String)
