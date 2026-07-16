package auk.agent

import auk.llm.endpoint.{ChatResponse, Content, Message, Role, Usage}

/** The provider-agnostic tool-calling loop shared by the interactive [[Engine]]
  * and the headless [[auk.runtime.HeadlessAgent]].
  *
  * Both drive the same cycle — ask the model for a turn, and while it requests
  * tools, run them and feed the results back — differing only in how a turn is
  * produced (streamed to a UI vs a single blocking request) and what happens
  * around each step (session persistence, event forwarding). Those variations
  * are injected through the [[Driver]]; the loop owns the cycle itself, including
  * accumulating token usage and growing the message list.
  *
  * The loop runs until the model answers without a tool call (or a driver hook
  * asks it to stop). There is deliberately no round cap: a turn is bounded by the
  * model deciding it is done, not by an arbitrary ceiling.
  */
object ToolLoop:

  /** What the loop produced: the conversation grown with every message exchanged
    * (assistant replies and tool results), the model's final tool-free response
    * if it reached one, the token usage summed across all rounds, and the number
    * of model turns taken. `stopped` is true when a [[Driver]] hook aborted the
    * loop before a clean final answer (an endpoint error, a closed stream, or a
    * persistence failure). */
  final case class Outcome(
      messages: List[Message],
      finalResponse: Option[ChatResponse],
      usage: Usage,
      rounds: Int,
      stopped: Boolean
  )

  /** The provider-specific seams the loop is parameterized over. Each concrete
    * driver closes over its own `Async` context, so the loop itself needs none.
    */
  trait Driver:
    /** Produce one assistant turn for `messages`, or `None` to stop the loop
      * (e.g. an endpoint error or a closed stream — already surfaced by the
      * driver). */
    def turn(messages: List[Message]): Option[ChatResponse]

    /** Run the model's requested tool calls, returning the results in the same
      * order as the calls. */
    def runTools(toolUses: List[Content.ToolUse]): List[Content.ToolResult]

    /** React to a fresh assistant response before any tools run (persist it,
      * forward it). Carries the whole [[ChatResponse]] — message, finish reason,
      * and token usage — so a driver can durably record all three. Return
      * `false` to abort the loop. */
    def onAssistant(response: ChatResponse): Boolean = true

    /** React to a batch of tool results before they are fed back to the model
      * (persist them). Return `false` to abort the loop. */
    def onToolResults(results: List[Content.ToolResult]): Boolean = true

    /** React to the model's final, tool-free turn — the turn that ends the loop
      * (e.g. forward its `Done` to the UI). */
    def onFinal(response: ChatResponse): Unit = ()

    /** Consulted when the model produces a tool-free turn that would otherwise end
      * the loop. Return a non-empty list of messages to append and keep looping
      * instead of finishing — e.g. to nudge the model toward a required tool it
      * skipped. The default ends the loop. A driver that continues is responsible
      * for its own cap; this loop imposes none. */
    def onWouldFinish(response: ChatResponse): List[Message] = Nil

    /** Real-time steering: user messages and system notices queued since the last
      * round, as messages to append before the next model turn. Consulted at each
      * tool-calling round boundary and when the model would finish (where it takes
      * priority — pending steering re-opens the loop so the model answers it in
      * the same turn). The default is none, so a headless driver is unaffected. */
    def drainSteering(): List[Message] = Nil

  /** Drive `driver` from `initial` to completion. */
  def run(initial: List[Message], driver: Driver): Outcome =
    var messages = initial
    var inputTokens = 0L
    var outputTokens = 0L
    var rounds = 0
    var finalResponse: Option[ChatResponse] = None
    var stopped = false
    var looping = true
    while looping do
      driver.turn(messages) match
        case None =>
          stopped = true
          looping = false
        case Some(response) =>
          rounds += 1
          response.usage.foreach: u =>
            inputTokens += u.inputTokens
            outputTokens += u.outputTokens
          val assistant = response.message
          if !driver.onAssistant(response) then
            stopped = true
            looping = false
          else
            messages = messages :+ assistant
            val toolUses = assistant.content.collect { case t: Content.ToolUse => t }
            if toolUses.isEmpty then
              // Steering queued during the final round takes priority over a
              // would-finish nudge: append it and keep looping so the model
              // answers it in this same turn, re-evaluating "finish" next time.
              val steering = driver.drainSteering()
              if steering.nonEmpty then messages = messages ++ steering
              else
                val injected = driver.onWouldFinish(response)
                if injected.isEmpty then
                  driver.onFinal(response)
                  finalResponse = Some(response)
                  looping = false
                else messages = messages ++ injected
            else
              val results = driver.runTools(toolUses)
              if !driver.onToolResults(results) then
                stopped = true
                looping = false
              else
                // Fold in any steering queued while the tools ran, before the
                // next model turn (the requested round-boundary drain point).
                messages = (messages :+ Message(Role.User, results)) ++ driver.drainSteering()
    Outcome(messages, finalResponse, Usage(inputTokens, outputTokens), rounds, stopped)
