package auk.runtime

import gears.async.Async

import auk.agent.{ToolLoop, StreamConsumer}
import auk.llm.tools.RuntimeContext
import auk.llm.endpoint.{ChatResponse, Message, Content, StreamEvent}
import auk.llm.provider.ModelSession

/** Drives a model to completion on a single prompt, headlessly — the shared
  * engine behind both the [[SubAgent]] tool and the workflow [[WorkflowBridge]].
  *
  * Each round is streamed (so a long-but-healthy turn is never cut off by a
  * single overall timeout), tools dispatch sequentially, and the loop runs as
  * many rounds as the model asks for. Progress hooks let a caller surface live
  * token totals and the currently-running tool without coupling to a UI.
  */
object HeadlessAgent:

  /** The distilled result of a headless run. `llmError` set means the model
    * stream failed; otherwise `finalText` is the model's last message. */
  final case class Outcome(
      finalText: String,
      llmError: Option[String],
      inputTokens: Long,
      outputTokens: Long,
      rounds: Int
  ):
    def ok: Boolean = llmError.isEmpty

  /** A live fragment of a headless run's transcript: assistant prose/thinking
    * deltas and tool input/output. Surfaced via `onActivity` so a caller can
    * stream a sub-agent's full transcript without this runner depending on any
    * UI or wire type. */
  enum Activity:
    case Text(text: String)
    case Thinking(text: String)
    case ToolStarted(id: String, name: String, input: String)
    case ToolEnded(id: String, output: String, isError: Boolean)

  /** Run `prompt` to completion using the session's active model, advertising
    * `registry`'s tools under `systemPrompt`. `onRound` receives cumulative token
    * totals after each streamed round; `onTools` receives the names of the tools
    * about to run in a round. Never throws: an endpoint error becomes
    * `Outcome.llmError`.
    */
  def run(
      prompt: String,
      models: ModelSession,
      registry: ToolRegistry,
      systemPrompt: String,
      onRound: (Long, Long) => Unit = (_, _) => (),
      onTools: List[String] => Unit = _ => (),
      onActivity: Activity => Unit = _ => ()
  )(using ctx: RuntimeContext, async: Async): Outcome =
    val active = models.active
    val subConfig = active.config.copy(tools = registry.schemas, systemPrompt = Some(systemPrompt))
    var llmError: Option[String] = None
    var inputTokens = 0L
    var outputTokens = 0L
    val driver = new ToolLoop.Driver:
      def turn(messages: List[Message]): Option[ChatResponse] =
        val response = Async.group:
          val upstream = active.endpoint.stream(messages, subConfig)
          StreamConsumer.collect(
            upstream,
            onEvent = {
              case StreamEvent.Delta(t)         => onActivity(Activity.Text(t))
              case StreamEvent.ThinkingDelta(t) => onActivity(Activity.Thinking(t))
              case _                            => ()
            },
            onError = err => llmError = Some(err.description)
          )
        response.foreach: r =>
          r.usage.foreach: u =>
            inputTokens += u.inputTokens
            outputTokens += u.outputTokens
          onRound(inputTokens, outputTokens)
        response
      def runTools(toolUses: List[Content.ToolUse]): List[Content.ToolResult] =
        onTools(toolUses.map(_.name))
        toolUses.map: tu =>
          // submit_result is the protocol's result channel, not real agent work —
          // its outcome already surfaces as the node summary, so keep it out of the
          // transcript (but still dispatch it).
          val show = tu.name != "submit_result"
          if show then onActivity(Activity.ToolStarted(tu.id, tu.name, tu.input))
          val r = registry.dispatch(tu)
          if show then onActivity(Activity.ToolEnded(r.toolUseId, r.content, r.isError))
          r
    val outcome = ToolLoop.run(List(Message.user(prompt)), driver)
    Outcome(
      finalText = outcome.finalResponse.map(_.message.text).getOrElse(""),
      llmError = llmError,
      inputTokens = outcome.usage.inputTokens,
      outputTokens = outcome.usage.outputTokens,
      rounds = outcome.rounds
    )
