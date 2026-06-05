package auk.runtime

import gears.async.Async

import auk.agent.ToolLoop
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, desc}
import auk.llm.endpoint.{ChatResponse, Message, Content}
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
  * user turns from a channel, a sub-agent runs to completion on a single prompt.
  * Both, however, drive the same tool-calling cycle — call the model, run the
  * tools it asks for, feed the results back, repeat until it answers without a
  * tool call — so that cycle lives once in [[ToolLoop]]; the sub-agent only
  * supplies the headless seams (a blocking [[Endpoint.invoke]] turn and
  * sequential tool dispatch) and turns the loop's outcome into a [[ToolResult]].
  * The model's final text becomes the result handed back to the *caller's* turn.
  *
  * The sub-agent shares the caller's [[RuntimeContext]] — same working directory
  * and approval policy — so a sub-agent's mutating tools are gated exactly as the
  * parent's are. It runs under its own [[systemPrompt]] and advertises only the
  * tools in its [[registry]], which lets a parent hand off a large, tool-heavy
  * exploration without flooding the main conversation with the intermediate
  * steps; only the distilled report comes back.
  *
  * Driving through [[Endpoint.invoke]] (a request/response, no streaming to a UI)
  * and dispatching tools sequentially via [[ToolRegistry.dispatch]] keeps the
  * run free of any `Async.Spawn` requirement.
  *
  * `name` and `description` are constructor parameters so a parent can register
  * several specialised sub-agents (e.g. a read-only "explore" agent alongside a
  * general one) without subclassing.
  */
final class SubAgent(
    models: ModelSession,
    registry: ToolRegistry = ToolRegistry.of(),
    systemPrompt: String = SubAgent.DefaultSystemPrompt,
    override val name: String = "sub_agent",
    override val description: String = SubAgent.DefaultDescription
) extends Tool:
  type Params = SubAgentParams

  val input: ToolInput[SubAgentParams] = ToolInput[SubAgentParams]

  def execute(params: SubAgentParams)(using RuntimeContext, Async): ToolResult =
    if params.prompt.trim.isEmpty then ToolResult.error("empty prompt")
    else run(params.prompt)

  /** Drive the model to completion on `prompt` via the shared [[ToolLoop]] and
    * return its final answer. Never throws: an endpoint error becomes an error
    * [[ToolResult]].
    */
  private def run(prompt: String)(using RuntimeContext, Async): ToolResult =
    // Snapshot the active model for this run, layering on the sub-agent's own
    // tools and system prompt — so a sub-agent uses whatever model is current.
    val active = models.active
    val subConfig = active.config.copy(tools = registry.schemas, systemPrompt = Some(systemPrompt))
    var llmError: Option[String] = None
    val driver = new ToolLoop.Driver:
      def turn(messages: List[Message]): Option[ChatResponse] =
        active.endpoint.invoke(messages, subConfig) match
          case Right(response) => Some(response)
          case Left(err) =>
            llmError = Some(err.description)
            None
      def runTools(toolUses: List[Content.ToolUse]): List[Content.ToolResult] =
        toolUses.map(registry.dispatch)
    finish(ToolLoop.run(List(Message.user(prompt)), driver), llmError)

  private def finish(outcome: ToolLoop.Outcome, llmError: Option[String]): ToolResult =
    val metadata = Map(
      "rounds" -> outcome.rounds.toString,
      "inputTokens" -> outcome.usage.inputTokens.toString,
      "outputTokens" -> outcome.usage.outputTokens.toString
    )
    llmError match
      case Some(err) =>
        ToolResult.error(s"sub-agent LLM error: $err")
      case None =>
        val text = outcome.finalResponse.map(_.message.text).getOrElse("")
        if text.trim.isEmpty then ToolResult.ok("(sub-agent finished without a final message)", metadata)
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
