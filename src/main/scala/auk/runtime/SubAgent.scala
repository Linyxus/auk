package auk.runtime

import gears.async.Async

import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, desc}
import auk.llm.endpoint.{Message, Content, Role}
import auk.llm.provider.ModelSession

case class SubAgentParams(
    @desc("A short (3-5 word) description of the task, e.g. \"find the auth bug\".")
    description: String,
    @desc(
      "The full, self-contained task for the sub-agent. It runs autonomously " +
        "and cannot ask follow-up questions, so include every piece of context " +
        "it needs and state exactly what it should report back."
    )
    prompt: String
) derives ToolInput

/** A tool that spawns a nested, headless agent to carry out a focused task.
  *
  * Unlike the interactive [[auk.agent.Engine]], which streams to a UI and reads
  * user turns from a channel, a sub-agent runs to completion on a single prompt:
  * it calls the model, runs whatever tools the model asks for through its own
  * [[ToolRegistry]], feeds the results back, and repeats until the model answers
  * without a tool call (or the [[maxRounds]] cap is hit). The model's final text
  * becomes the [[ToolResult]] handed back to the *caller's* turn.
  *
  * The sub-agent shares the caller's [[RuntimeContext]] — same working directory
  * and approval policy — so a sub-agent's mutating tools are gated exactly as the
  * parent's are. It runs under its own [[systemPrompt]] and advertises only the
  * tools in its [[registry]], which lets a parent hand off a large, tool-heavy
  * exploration without flooding the main conversation with the intermediate
  * steps; only the distilled report comes back.
  *
  * Because the sub-agent only needs a request/response exchange (no streaming to
  * a UI), it drives the model through [[Endpoint.invoke]] and dispatches tool
  * calls sequentially via [[ToolRegistry.dispatch]], keeping the loop free of any
  * `Async.Spawn` requirement.
  *
  * `name` and `description` are constructor parameters so a parent can register
  * several specialised sub-agents (e.g. a read-only "explore" agent alongside a
  * general one) without subclassing.
  */
final class SubAgent(
    models: ModelSession,
    registry: ToolRegistry = ToolRegistry.of(),
    systemPrompt: String = SubAgent.DefaultSystemPrompt,
    maxRounds: Int = 16,
    override val name: String = "sub_agent",
    override val description: String = SubAgent.DefaultDescription
) extends Tool:
  type Params = SubAgentParams

  val input: ToolInput[SubAgentParams] = ToolInput[SubAgentParams]

  def execute(params: SubAgentParams)(using RuntimeContext, Async): ToolResult =
    if params.prompt.trim.isEmpty then ToolResult.error("empty prompt")
    else run(params.prompt)

  /** Drive the model to completion on `prompt`, running tools each round, and
    * return its final answer. Never throws: an endpoint error or a hit round cap
    * becomes an error [[ToolResult]].
    */
  private def run(prompt: String)(using RuntimeContext, Async): ToolResult =
    // Snapshot the active model for this run, layering on the sub-agent's own
    // tools and system prompt — so a sub-agent uses whatever model is current.
    val active = models.active
    val subConfig = active.config.copy(tools = registry.schemas, systemPrompt = Some(systemPrompt))
    var messages = List(Message.user(prompt))
    var round = 0
    var inputTokens = 0L
    var outputTokens = 0L
    var result: Option[ToolResult] = None

    while result.isEmpty do
      active.endpoint.invoke(messages, subConfig) match
        case Left(err) =>
          result = Some(ToolResult.error(s"sub-agent LLM error: ${err.description}"))
        case Right(response) =>
          response.usage.foreach: u =>
            inputTokens += u.inputTokens
            outputTokens += u.outputTokens
          val assistant = response.message
          messages = messages :+ assistant
          round += 1
          val toolUses = assistant.content.collect { case t: Content.ToolUse => t }
          if toolUses.nonEmpty && round < maxRounds then
            // Run the requested tools and feed their results back as the next turn.
            val results = toolUses.map(tu => registry.dispatch(tu))
            messages = messages :+ Message(Role.User, results)
          else
            result = Some(
              finish(assistant.text, round, toolUses.nonEmpty, inputTokens, outputTokens)
            )
    result.get

  private def finish(
      text: String,
      rounds: Int,
      cappedWithPendingTools: Boolean,
      inputTokens: Long,
      outputTokens: Long
  ): ToolResult =
    val metadata = Map(
      "rounds" -> rounds.toString,
      "inputTokens" -> inputTokens.toString,
      "outputTokens" -> outputTokens.toString
    )
    if cappedWithPendingTools then
      val note =
        s"[sub-agent stopped after the $maxRounds-round cap with tool calls still pending]"
      val body = if text.trim.isEmpty then note else s"$text\n\n$note"
      ToolResult(body, isError = true, metadata = metadata)
    else if text.trim.isEmpty then
      ToolResult.ok("(sub-agent finished without a final message)", metadata)
    else ToolResult.ok(text, metadata)

object SubAgent:
  val DefaultDescription: String =
    "Launch a sub-agent to autonomously handle a focused, multi-step task and " +
      "report back. The sub-agent has its own tools and conversation, runs to " +
      "completion, and returns a single final summary. Give it a complete, " +
      "self-contained prompt — it cannot ask follow-up questions. Use it to keep " +
      "a large exploration out of the main conversation."

  val DefaultSystemPrompt: String =
    "You are a sub-agent launched to complete a single, focused task on your " +
      "own. Use the tools available to you to carry it out end to end. You " +
      "cannot ask follow-up questions, so make reasonable assumptions when " +
      "details are missing. When you are done, reply with a concise report of " +
      "what you found or did — that reply is all the caller will see."
