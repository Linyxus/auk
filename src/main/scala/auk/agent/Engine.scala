package auk.agent

import gears.async.{Async, Future, ReadableChannel, SendableChannel}
import java.util.concurrent.CancellationException
import scala.util.{Success, Failure}
import auk.llm.endpoint.{Endpoint, StreamEvent, Message, Content, Role, ChatResponse, FinishReason, LLMError}
import auk.llm.provider.ModelSession
import auk.llm.tools.{RuntimeContext, ProgressSink}
import auk.runtime.ToolRegistry
import auk.session.{Session, SessionEvent, SessionProvider, SessionSnapshot, SessionSummary}
import auk.utils.Result

/** A single-threaded agent loop with tool use.
  *
  * Reads [[UserCommand]]s and, for each submitted line, drives one turn to
  * completion: it streams an assistant reply, and while the model asks for
  * tools, it runs them through the [[registry]], feeds the results back, and
  * streams again — until the model answers without a tool call. The cycle itself
  * lives in [[ToolLoop]], shared with the headless [[auk.runtime.SubAgent]];
  * this class supplies the streaming-and-persistence behaviour around it.
  * Conversation history grows across turns so the model keeps context.
  *
  * Streaming events flow to the UI verbatim, with one exception: the terminal
  * `Done` of an *intermediate* (tool-requesting) round is held back, so the UI
  * sees a single continuous turn ending in one `Done` rather than blinking back
  * to idle between tool rounds.
  */
final class Engine(
    in: ReadableChannel[UserCommand],
    out: SendableChannel[AgentEvent],
    interrupts: ReadableChannel[Unit],
    models: ModelSession,
    initialSession: Session,
    sessions: SessionProvider,
    registry: ToolRegistry = ToolRegistry.of(),
    context: RuntimeContext = RuntimeContext.cwd(),
    persistModel: (String, String) => Either[String, Unit] = (_, _) => Right(()),
    systemPrompt: String = SystemPrompt.default
):
  private given RuntimeContext = context

  private var currentSession: Session = initialSession

  // ---- Per-turn interruption bookkeeping (single-threaded ⇒ no locks) ----
  // Captured as the turn progresses so that, if it is cancelled mid-flight, the
  // history can be reconciled into a valid state (no dangling tool_use). All are
  // reset at the start of each turn.
  //
  // `partialAssistantText` accumulates the streamed answer text of the current
  // round; it is cleared once the round's full assistant message is persisted, so
  // it is non-empty only while a reply is mid-stream (Phase A). `inToolExecution`
  // and `pendingTool*` track an in-flight `runTools` batch (Phase B): the
  // assistant's tool_use turn is already persisted, but its results are not, so a
  // result must be synthesized for every tool_use to keep history valid.
  private val partialAssistantText = new StringBuilder
  private var inToolExecution = false
  private var pendingToolUses: List[Content.ToolUse] = Nil
  private val pendingToolResults = scala.collection.mutable.Map.empty[String, Content.ToolResult]

  private def resetTurnState(): Unit =
    partialAssistantText.clear()
    inToolExecution = false
    pendingToolUses = Nil
    pendingToolResults.clear()

  def run()(using Async.Spawn): Unit =
    loadHistory(currentSession) match
      case Left(err) =>
        reportPersistence(err)
      case Right(initialHistory) =>
        var history = initialHistory
        var running = true
        while running do
          in.read() match
            case Left(_) => running = false // command channel closed
            case Right(UserCommand.Submit(text)) =>
              appendEvent(SessionEvent.UserSubmitted(text)) match
                case Left(_) => ()
                case Right(()) =>
                  history = runTurn(history :+ Message.user(text))
            case Right(UserCommand.ListSessions) =>
              sessions.summaries() match
                case Right(summaries) => out.send(AgentEvent.SessionsListed(summaries))
                case Left(err)        => reportPersistence(err)
            case Right(UserCommand.ResumeSession(id)) =>
              resumeSession(id) match
                case Right((snapshot, nextHistory)) =>
                  history = nextHistory
                  out.send(AgentEvent.SessionSwitched(snapshot))
                case Left(err) =>
                  reportPersistence(err)
            case Right(UserCommand.NewSession) =>
              newSession() match
                case Right((snapshot, nextHistory)) =>
                  history = nextHistory
                  out.send(AgentEvent.SessionSwitched(snapshot))
                case Left(err) =>
                  reportPersistence(err)
            case Right(UserCommand.SwitchModel(providerName, modelId)) =>
              switchModel(providerName, modelId)

  /** Drive a user turn to completion via the shared [[ToolLoop]]: stream the
    * reply, and while the model requests tools, run them and stream again.
    * Returns the conversation grown with every message exchanged (assistant
    * replies and tool results).
    *
    * The driver supplies the engine's streaming-and-persistence behaviour: each
    * assistant message and each batch of tool results is persisted to the
    * session (a failed write aborts the turn), and the final tool-free turn's
    * held-back `Done` is forwarded to the UI. */
  private def converse(initial: List[Message])(using Async.Spawn): List[Message] =
    val driver = new ToolLoop.Driver:
      def turn(messages: List[Message]): Option[ChatResponse] = streamTurn(messages)
      def runTools(toolUses: List[Content.ToolUse]): List[Content.ToolResult] =
        Engine.this.runTools(toolUses)
      override def onAssistant(response: ChatResponse): Boolean =
        val ok = appendEvent(SessionEvent.AssistantResponded(response)).isRight
        // The full reply is now durable; the streamed partial is redundant, so a
        // later interrupt (mid-tools) must not re-persist it as a stray message.
        if ok then partialAssistantText.clear()
        ok
      override def onToolResults(results: List[Content.ToolResult]): Boolean =
        val ok = appendEvent(SessionEvent.ToolResultsReceived(results)).isRight
        // This round's tool_use is now answered; leave Phase B so a later
        // interrupt does not synthesize a duplicate results batch.
        if ok then
          inToolExecution = false
          pendingToolUses = Nil
        ok
      override def onFinal(response: ChatResponse): Unit =
        out.send(AgentEvent.Stream(Right(StreamEvent.Done(response))))
    ToolLoop.run(initial, driver).messages

  /** Drive one user turn as a cancellable child fiber, watching the out-of-band
    * [[interrupts]] channel concurrently so `Ctrl+C k` can stop it mid-flight.
    *
    * The turn (and every fiber it spawns — the stream producer and each tool)
    * runs under one `Future`; cancelling it propagates through the structured
    * scope, and each descendant's cleanup runs on the way out: the LLM request is
    * aborted (`Endpoint.streaming`'s `finally`) and a running subprocess is killed
    * (`NodeProcess`'s `finally`). `awaitResult` returns only once the turn has
    * fully unwound, so by the time we reconcile, all captured state is final. */
  private def runTurn(messages: List[Message])(using Async.Spawn): List[Message] =
    resetTurnState()
    // Discard any interrupt that arrived between turns (e.g. a late keypress)
    // so it cannot instantly cancel this fresh turn.
    while interrupts.readSource.poll().isDefined do ()
    Async.group:
      val turn = Future(converse(messages))
      // Wakes on the first interrupt signal and cancels the turn; if the turn
      // finishes first, this watcher is cancelled out of its blocking read.
      val watcher = Future:
        interrupts.read()
        turn.cancel()
      turn.awaitResult match
        case Success(grown) =>
          watcher.cancel()
          grown
        case Failure(_: CancellationException) =>
          reconcileInterrupted(messages)
        case Failure(e) =>
          watcher.cancel()
          out.send(AgentEvent.Stream(Left(LLMError(s"turn failed: ${Endpoint.errMsg(e)}"))))
          loadHistory(currentSession).getOrElse(messages)

  /** Bring history back to a valid state after an interrupt, persisting the
    * minimum needed so the next turn is well-formed, then return the reloaded
    * history. Two cases:
    *
    *   - Phase B (interrupted while tools ran): the assistant's tool_use turn is
    *     already persisted but its results are not. Synthesize a result for every
    *     requested tool — the real one if it finished, else an "interrupted"
    *     error — so no tool_use is left dangling (which the API rejects).
    *   - Phase A (interrupted mid-stream): the assistant message was never
    *     persisted. Keep the partial answer *text* (incomplete tool_use / unsigned
    *     thinking are dropped) as the assistant's reply, if any streamed.
    *
    * Either way a final [[SessionEvent.Interrupted]] marker records the cut-off,
    * and the UI is told to finalize its live turn. */
  private def reconcileInterrupted(prev: List[Message])(using Async): List[Message] =
    if inToolExecution then
      val results: List[Content.ToolResult] = pendingToolUses.map: tu =>
        pendingToolResults.getOrElse(tu.id, Content.ToolResult(tu.id, "Interrupted by user", isError = true))
      appendEvent(SessionEvent.ToolResultsReceived(results))
    else if partialAssistantText.nonEmpty then
      // A cut-off partial stream: no usage was reported, so record the text alone.
      appendEvent(SessionEvent.AssistantResponded(
        ChatResponse(Message(Role.Assistant, List(Content.Text(partialAssistantText.toString))), FinishReason.Stop)
      ))
    appendEvent(SessionEvent.Interrupted)
    out.send(AgentEvent.Interrupted)
    loadHistory(currentSession).getOrElse(prev)

  /** Rebuild model-facing history from this session's durable event log. */
  private def loadHistory(session: Session): Either[String, List[Message]] =
    session.events.map(replayMessages)

  private def resumeSession(id: String): Either[String, (SessionSnapshot, List[Message])] =
    for
      maybeSession <- sessions.open(id)
      session <- maybeSession.toRight(s"session '$id' does not exist")
      events <- session.events
    yield
      currentSession = session
      val snapshot = SessionSnapshot(SessionSummary.from(session.id, None, events), events)
      (snapshot, replayMessages(events))

  private def newSession(): Either[String, (SessionSnapshot, List[Message])] =
    for
      session <- sessions.create()
      events <- session.events
    yield
      currentSession = session
      val snapshot = SessionSnapshot(SessionSummary.from(session.id, None, events), events)
      (snapshot, replayMessages(events))

  /** Swap the live model for the rest of this instance, then persist the choice.
    * A resolve failure (unknown model, missing API key) leaves the current model
    * in place; a persist failure is reported but does not undo the live switch. */
  private def switchModel(providerName: String, modelId: String)(using Async): Unit =
    models.switch(providerName, modelId) match
      case Right(active) =>
        out.send(AgentEvent.ModelSwitched(active.label, active.contextWindow))
        persistModel(providerName, modelId).left.foreach: err =>
          out.send(AgentEvent.Stream(Left(LLMError(s"Model switched, but saving config failed: $err"))))
      case Left(err) =>
        out.send(AgentEvent.Stream(Left(LLMError(s"Could not switch model: $err"))))

  private def replayMessages(events: List[SessionEvent]): List[Message] =
    events.map:
      case SessionEvent.UserSubmitted(text)         => Message.user(text)
      case SessionEvent.AssistantResponded(response) => response.message
      case SessionEvent.ToolResultsReceived(results) => Message(Role.User, results)
      case SessionEvent.Interrupted                  => Message.user("[Request interrupted by user]")

  private def appendEvent(event: SessionEvent)(using Async): Either[String, Unit] =
    currentSession.append(event).left.map: err =>
      reportPersistence(err)
      err

  private def reportPersistence(err: String)(using Async): Unit =
    out.send(AgentEvent.Stream(Left(LLMError(s"Session persistence error: $err"))))

  /** Run each requested tool concurrently, bracketing every call with
    * `ToolRunStart`/`ToolRunEnd` events so the UI can show progress and the
    * tool's metadata (e.g. a sub-agent's token totals). Each call also gets a
    * [[RuntimeContext]] whose progress sink is bound to its id, so a tool that
    * reports live updates (e.g. a streaming sub-agent's running token totals)
    * surfaces them as `ToolRunProgress` between the start and end brackets.
    * Results are collected in the original order to line up with the model's
    * calls. */
  private def runTools(
      toolUses: List[Content.ToolUse]
  )(using Async.Spawn): List[Content.ToolResult] =
    // Enter Phase B: record what is being run so an interrupt mid-batch can
    // synthesize results for whatever did not finish (each tool records its own
    // result below as it completes).
    inToolExecution = true
    pendingToolUses = toolUses
    pendingToolResults.clear()
    toolUses.foreach(tu => out.send(AgentEvent.Stream(Right(StreamEvent.ToolRunStart(tu.id, tu.name)))))
    toolUses
      .map: tu =>
        Future[Content.ToolResult]:
          val callContext = context.withProgress(ProgressSink: update =>
            out.send(AgentEvent.Stream(Right(StreamEvent.ToolRunProgress(tu.id, update)))))
          val result = registry.run(tu)(using callContext)
          out.send(AgentEvent.Stream(Right(StreamEvent.ToolRunEnd(tu.id, result.isError, result.metadata, result.output))))
          val toolResult: Content.ToolResult = Content.ToolResult(tu.id, result.output, isError = result.isError)
          pendingToolResults(tu.id) = toolResult // a finished tool's real result, for reconciliation
          toolResult
      .map(_.await)

  /** Stream one assistant turn via the shared [[StreamConsumer]], forwarding
    * every event to the UI except the terminal `Done`, which the consumer
    * captures and returns so [[converse]] can decide whether to continue (run
    * tools) or surface it as the turn's end. Returns the full `ChatResponse`, or
    * None if the turn ended without a Done (an error — already forwarded — or a
    * closed channel). */
  private def streamTurn(
      messages: List[Message]
  )(using Async.Spawn): Option[ChatResponse] =
    // Snapshot the active model for the whole turn (switches only land between
    // turns, since the command loop is single-threaded). Tools and the system
    // prompt are advertised from this engine, layered onto the model's base
    // config.
    val active = models.active
    val upstream = active.endpoint.stream(
      messages,
      active.config.copy(tools = registry.schemas, systemPrompt = Some(systemPrompt))
    )
    // A fresh round begins: leave Phase B, and start capturing this reply's
    // answer text so an interrupt mid-stream (Phase A) can keep the partial.
    inToolExecution = false
    pendingToolUses = Nil
    partialAssistantText.clear()
    StreamConsumer.collect(
      upstream,
      onEvent = event =>
        event match
          case StreamEvent.Delta(t) => partialAssistantText.append(t)
          case _                    => ()
        out.send(AgentEvent.Stream(Right(event))),
      onError = err => out.send(AgentEvent.Stream(Left(err)))
    )
