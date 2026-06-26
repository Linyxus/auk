package auk.agent

import gears.async.Async

import auk.llm.endpoint.{Content, Message, Role}
import auk.llm.provider.ModelSession
import auk.llm.tools.{Json, RuntimeContext, Schema, Tool, ToolInput, ToolResult}
import auk.runtime.{HeadlessAgent, ToolRegistry}

/** Builds a compact replacement for the current model-facing conversation.
  *
  * The compactor is intentionally a normal tool-using model run: it gets one
  * mandatory result tool, and the accepted tool argument is the only value the
  * engine persists. Any prose the model emits while reaching that value is
  * discarded, so the session log remains a clean append-only sequence of the
  * user's conversation events plus the final checkpoint. */
object ContextCompactor:
  val MaxSubmitRetries: Int = 5

  def compact(messages: List[Message], models: ModelSession)(using RuntimeContext, Async): Either[String, String] =
    val submit = SubmitCompaction(MaxSubmitRetries)
    val registry = ToolRegistry.of(submit)
    var nudges = 0
    val outcome = HeadlessAgent.run(
      buildPrompt(messages),
      models,
      registry,
      systemPrompt,
      finishGuard = () =>
        if submit.captured.isDefined || submit.rejections >= MaxSubmitRetries || nudges >= MaxSubmitRetries then None
        else
          nudges += 1
          Some(NudgeMessage),
      haltAfterTools = () => submit.captured.isDefined || submit.rejections >= MaxSubmitRetries
    )
    outcome.llmError match
      case Some(err) => Left(s"context compaction failed: $err")
      case None =>
        submit.captured match
          case Some(summary) => Right(summary)
          case None if submit.rejections >= MaxSubmitRetries =>
            val tail = submit.lastError.fold("")(e => s"; last error: $e")
            Left(s"context compaction did not submit a valid summary after ${submit.rejections} attempt(s)$tail")
          case None =>
            val prose = outcome.finalText.trim
            val shown =
              if prose.isEmpty then "(no message)"
              else if prose.length > 300 then prose.take(300) + "..."
              else prose
            Left(s"context compaction ended without calling submit_compaction; model said: $shown")

  private val NudgeMessage: String =
    "You ended your turn without calling the `submit_compaction` tool. You must call " +
      "`submit_compaction` exactly once with your final compacted summary in its `summary` field. " +
      "Do not answer in prose; call `submit_compaction` now."

  private def systemPrompt: String =
    """You are Auk's context compaction worker. Your output replaces all earlier
      |model context for an interactive coding-agent session.
      |
      |Read the supplied conversation transcript and produce a compact Markdown
      |summary that preserves the information needed to continue the work.
      |
      |Your summary must use exactly these headings, in this order:
      |
      |## Current Goal
      |## Durable Instructions
      |## Project State
      |## Decisions
      |## Files Commands Results
      |## Open Work
      |
      |Preserve exact file paths, commands, error messages, tool/API names,
      |constraints, user preferences, decisions, and unresolved assumptions. Record
      |what was changed or learned and the evidence for it. Omit chit-chat,
      |superseded dead ends, repeated exploration, and hidden reasoning. Do not copy
      |private chain-of-thought; summarize only conclusions and user-visible facts.
      |
      |When the summary is ready, call `submit_compaction` exactly once. Do not answer
      |in prose. The host reads only the tool submission.""".stripMargin

  private def buildPrompt(messages: List[Message]): String =
    val rendered =
      messages.zipWithIndex.map((message, idx) => renderMessage(idx + 1, message)).mkString("\n\n")
    s"""Compact this session transcript. It is already the model-facing context
       |that should be replaced by your summary.
       |
       |<transcript>
       |$rendered
       |</transcript>""".stripMargin

  private def renderMessage(index: Int, message: Message): String =
    val role = message.role match
      case Role.System    => "system"
      case Role.User      => "user"
      case Role.Assistant => "assistant"
    val content = message.content.map(renderContent).mkString("\n")
    s"<message index=\"$index\" role=\"$role\">\n$content\n</message>"

  private def renderContent(content: Content): String = content match
    case Content.Text(text) =>
      s"<text>\n$text\n</text>"
    case Content.ToolUse(id, name, input) =>
      s"<tool_call id=\"$id\" name=\"$name\">\n$input\n</tool_call>"
    case Content.ToolResult(toolUseId, value, isError) =>
      val status = if isError then "error" else "ok"
      s"<tool_result id=\"$toolUseId\" status=\"$status\">\n$value\n</tool_result>"
    case Content.Thinking(_, _) | Content.RedactedThinking(_) | Content.Reasoning(_) =>
      "<reasoning omitted=\"true\" />"

  final class SubmitCompaction(maxRetries: Int) extends Tool:
    type Params = Json

    var captured: Option[String] = None
    var rejections: Int = 0
    var lastError: Option[String] = None

    val name = "submit_compaction"
    val description =
      "Submit the final compacted context summary. Call this exactly once when done, " +
        "with a non-empty Markdown `summary` field."
    val input: ToolInput[Json] =
      ToolInput.instance(
        Schema.obj(
          List("summary" -> Schema.string(Some("The compacted Markdown summary."))),
          List("summary")
        )
      )(j => Right(j))

    def execute(params: Json)(using RuntimeContext, Async): ToolResult =
      if captured.isDefined then ToolResult.ok("compaction summary already recorded")
      else
        extract(params) match
          case Right(summary) =>
            captured = Some(summary)
            lastError = None
            ToolResult.ok("compaction summary recorded")
          case Left(err) =>
            rejections += 1
            lastError = Some(err)
            ToolResult.error(rejectionMessage(err))

    private def extract(params: Json): Either[String, String] =
      params match
        case obj: Json.Obj =>
          obj.get("summary") match
            case Some(Json.Str(summary)) if summary.trim.nonEmpty => Right(summary.trim)
            case Some(Json.Str(_))                                => Left("summary must be non-empty")
            case Some(other)                                      => Left(s"summary should be a string but was ${other.typeName}")
            case None                                             => Left("missing required field 'summary'")
        case other => Left(s"expected an object with a 'summary' field, got ${other.typeName}")

    private def rejectionMessage(err: String): String =
      if rejections >= maxRetries then
        s"Your compaction summary is invalid: $err. You have used all $maxRetries attempts; " +
          "no further submissions will be accepted."
      else
        s"Your compaction summary is invalid: $err. Correct it and call `submit_compaction` again " +
          s"(attempt $rejections of $maxRetries)."
