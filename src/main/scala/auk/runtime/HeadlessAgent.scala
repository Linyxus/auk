package auk.runtime

import gears.async.Async

import auk.agent.{ToolLoop, StreamConsumer}
import auk.llm.tools.RuntimeContext
import auk.llm.endpoint.{ChatResponse, Endpoint, LLMError, Message, Content, StreamEvent}
import auk.llm.provider.ModelSession

/** Drives a model to completion on a single prompt, headlessly — the shared
  * engine behind the workflow [[WorkflowBridge]] and the team [[TeamBridge]].
  *
  * Each round is streamed (so a long-but-healthy turn is never cut off by a
  * single overall timeout), tools dispatch sequentially, and the loop runs as
  * many rounds as the model asks for. Progress hooks let a caller surface live
  * token totals and the currently-running tool without coupling to a UI.
  */
object HeadlessAgent:

  /** The distilled result of a headless run. `llmError` set means the model
    * stream failed for good (the shared retry schedule is exhausted, or the
    * failure was permanent — its `transient` flag says which); otherwise
    * `finalText` is the model's last message. `messages` is the full
    * conversation the loop grew (seed + every assistant reply and tool-result
    * message), so a caller that keeps a member alive across turns can feed it
    * back as the next turn's prior history — including after an error, so a
    * failed turn's completed steps survive. */
  final case class Outcome(
      finalText: String,
      llmError: Option[LLMError],
      inputTokens: Long,
      outputTokens: Long,
      rounds: Int,
      messages: List[Message]
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

    /** The round's request failed transiently; the shared retry loop re-issues
      * it after `delayMs`. Surfaced so a sub-agent transcript records the stall
      * (and why) instead of silently going quiet through the backoff. */
    case Retrying(attempt: Int, maxAttempts: Int, delayMs: Long, reason: String)

  /** Run `prompt` to completion using the session's active model, advertising
    * `registry`'s tools under `systemPrompt`. A thin wrapper over
    * [[runConversation]] that seeds the loop with a single user message. */
  def run(
      prompt: String,
      models: ModelSession,
      registry: ToolRegistry,
      systemPrompt: String,
      onRound: (Long, Long) => Unit = (_, _) => (),
      onTools: List[String] => Unit = _ => (),
      onActivity: Activity => Unit = _ => (),
      finishGuard: () => Option[String] = () => None,
      haltAfterTools: () => Boolean = () => false,
      retryDelaysMs: List[Long] = Endpoint.RetryDelaysMs
  )(using RuntimeContext, Async): Outcome =
    runConversation(List(Message.user(prompt)), models, registry, systemPrompt, onRound, onTools, onActivity, finishGuard, haltAfterTools, retryDelaysMs)

  /** Run to completion starting from an arbitrary prior conversation `initial`,
    * using the session's active model and advertising `registry`'s tools under
    * `systemPrompt`. `onRound` receives cumulative token totals after each
    * streamed round; `onTools` receives the names of the tools about to run in a
    * round. Never throws: an endpoint error becomes `Outcome.llmError`.
    *
    * Seeding with prior history (rather than a single prompt) is what lets a
    * long-lived agent — a team member — resume where it left off: pass its stored
    * `Outcome.messages` plus the new user messages, and take the returned
    * `messages` as its next stored history.
    *
    * `finishGuard` and `haltAfterTools` let a caller keep a model on the rails
    * when a tool result is mandatory (the workflow `submit_result` channel):
    *   - `finishGuard` is consulted whenever the model would end its turn without
    *     a tool call; returning `Some(text)` appends that as a user message and
    *     keeps the model going (e.g. to nudge it into calling a tool it skipped),
    *     `None` lets the run finish. The caller is responsible for capping it.
    *   - `haltAfterTools` is consulted after each tool batch; `true` stops the
    *     loop immediately (e.g. once retries are exhausted), leaving `finalText`
    *     empty so the caller decides the outcome from its own state.
    */
  def runConversation(
      initial: List[Message],
      models: ModelSession,
      registry: ToolRegistry,
      systemPrompt: String,
      onRound: (Long, Long) => Unit = (_, _) => (),
      onTools: List[String] => Unit = _ => (),
      onActivity: Activity => Unit = _ => (),
      finishGuard: () => Option[String] = () => None,
      haltAfterTools: () => Boolean = () => false,
      retryDelaysMs: List[Long] = Endpoint.RetryDelaysMs
  )(using ctx: RuntimeContext, async: Async): Outcome =
    val active = models.active
    val subConfig = active.config.copy(tools = registry.schemas, systemPrompt = Some(systemPrompt))
    var llmError: Option[LLMError] = None
    var inputTokens = 0L
    var outputTokens = 0L
    val driver = new ToolLoop.Driver:
      def turn(messages: List[Message]): Option[ChatResponse] =
        val response = Async.group:
          StreamConsumer.collectRetrying(
            open = () => active.endpoint.stream(messages, subConfig),
            onEvent = {
              case StreamEvent.Delta(t)         => onActivity(Activity.Text(t))
              case StreamEvent.ThinkingDelta(t) => onActivity(Activity.Thinking(t))
              case _                            => ()
            },
            onError = err => llmError = Some(err),
            onRetry = (attempt, maxAttempts, delayMs, err) =>
              onActivity(Activity.Retrying(attempt, maxAttempts, delayMs, err.description)),
            delaysMs = retryDelaysMs
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
          // All tool calls — including submit_result — are surfaced as transcript
          // activities so the sub-agent log records the full call/response history
          // (arguments, rejections, acceptance), not just the eval_scala calls.
          onActivity(Activity.ToolStarted(tu.id, tu.name, tu.input))
          val r = registry.dispatch(tu)
          onActivity(Activity.ToolEnded(r.toolUseId, r.content, r.isError))
          r
      // Stop the loop the moment the caller says retries are spent, so a model
      // that keeps re-calling a tool can never spin unbounded.
      override def onToolResults(results: List[Content.ToolResult]): Boolean =
        !haltAfterTools()
      // Give the caller a chance to keep the model going (e.g. nudge it to call a
      // mandatory tool) instead of ending on a tool-free turn.
      override def onWouldFinish(response: ChatResponse): List[Message] =
        finishGuard() match
          case Some(text) => List(Message.user(text))
          case None       => Nil
    val outcome = ToolLoop.run(initial, driver)
    Outcome(
      finalText = outcome.finalResponse.map(_.message.text).getOrElse(""),
      llmError = llmError,
      inputTokens = outcome.usage.inputTokens,
      outputTokens = outcome.usage.outputTokens,
      rounds = outcome.rounds,
      messages = outcome.messages
    )
