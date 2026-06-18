package auk.agent

import gears.async.{Async, Future, ReadableChannel, SendableChannel}
import java.util.concurrent.CancellationException
import scala.util.{Success, Failure}
import scala.collection.mutable
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
    inbox: ReadableChannel[Inbox],
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

  // Conversation inputs (user messages, system notices) observed — and echoed as
  // AgentEvent.InputQueued — by the run loop's inner select while a turn was in
  // flight, awaiting the next drain point. Single-threaded by the engine's
  // contract (see the class comment): the inner select enqueues, the turn fiber's
  // drainSteering drains, and the two never run simultaneously (no `await`
  // between consume and produce), so no lock is needed.
  private val pendingInbox = mutable.Queue.empty[Inbox]

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

  /** Take everything buffered in [[pendingInbox]] (FIFO), emptying it. */
  private def drainPending(): List[Inbox] =
    val xs = pendingInbox.toList
    pendingInbox.clear()
    xs

  /** Non-blocking drain of the external [[inbox]] channel (FIFO); stops on empty
    * or a closed channel. */
  private def drainInbox()(using Async): List[Inbox] =
    val acc = List.newBuilder[Inbox]
    var more = true
    while more do
      inbox.readSource.poll() match
        case Some(Right(item)) => acc += item
        case _                 => more = false
    acc.result()

  /** Fold queued inputs into the conversation: persist each (a failed write is
    * surfaced but does not drop the item from this live turn), tell the UI they
    * were consumed, and return them as model-facing messages. Shared by a turn's
    * seed and the mid-turn steering drain so both persist, echo, and wrap
    * identically. A system notice becomes a `<system-reminder>`-wrapped user
    * message ([[Message.systemNotice]]); resume reproduces it via [[replayMessages]]. */
  private def foldItems(items: List[Inbox])(using Async): List[Message] =
    // Persist each input; an item whose write fails is surfaced (via appendEvent
    // → reportPersistence) and dropped from this turn, so the model never sees
    // input we couldn't durably record — mirroring the old Submit path, which
    // skipped the turn entirely on a failed append.
    val kept = items.filter: item =>
      val event = item match
        case Inbox.UserMessage(text)  => SessionEvent.UserSubmitted(text)
        case Inbox.SystemNotice(text) => SessionEvent.SystemNotice(text)
      appendEvent(event).isRight
    // Echo *all* drained items as consumed (not just `kept`): the UI panel drops a
    // FIFO prefix by count, so under-reporting on a persist failure would orphan
    // the dropped item in the panel forever. The model still gets only `kept`.
    if items.nonEmpty then out.send(AgentEvent.InputsConsumed(items))
    kept.map(toMessage)

  private def toMessage(item: Inbox): Message = item match
    case Inbox.UserMessage(text)  => Message.user(text)
    case Inbox.SystemNotice(text) => Message.systemNotice(text)

  def run()(using Async.Spawn): Unit =
    loadHistory(currentSession) match
      case Left(err) =>
        reportPersistence(err)
      case Right(initialHistory) =>
        var history = initialHistory
        var running = true
        while running do
          // `pendingInbox` holds inputs observed during the previous turn (older);
          // the channel holds anything that arrived since (newer). Drain pending
          // first so global order stays FIFO, then run a turn if anything waits —
          // this wakes an idle agent on new input and flushes the queue into a
          // fresh turn after an interrupt. Otherwise block for control or input;
          // a single inbox reader at a time (here, or the turn's inner select).
          val leftover = drainPending() ++ drainInbox()
          if leftover.nonEmpty then history = runTurn(history, leftover)
          else
            Async.select(
              in.readSource.handle {
                case Right(command) => history = handleControl(command, history)
                case Left(_)        => running = false // control channel closed → shut down
              },
              inbox.readSource.handle {
                case Right(item) => history = runTurn(history, item :: drainInbox()) // drain them all
                case Left(_)     => running = false // inbox closed → shut down
              }
            )

  /** Handle a between-turn control command, returning the (possibly switched)
    * history. Session/model operations only — conversation input arrives on the
    * inbox, never here. */
  private def handleControl(command: UserCommand, history: List[Message])(using Async): List[Message] =
    command match
      case UserCommand.ListSessions =>
        sessions.summaries() match
          case Right(summaries) => out.send(AgentEvent.SessionsListed(summaries))
          case Left(err)        => reportPersistence(err)
        history
      case UserCommand.ResumeSession(id) =>
        resumeSession(id) match
          case Right((snapshot, nextHistory)) =>
            out.send(AgentEvent.SessionSwitched(snapshot))
            nextHistory
          case Left(err) =>
            reportPersistence(err)
            history
      case UserCommand.NewSession =>
        newSession() match
          case Right((snapshot, nextHistory)) =>
            out.send(AgentEvent.SessionSwitched(snapshot))
            nextHistory
          case Left(err) =>
            reportPersistence(err)
            history
      case UserCommand.SwitchModel(providerName, modelId) =>
        switchModel(providerName, modelId)
        history

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
        if ok then
          partialAssistantText.clear()
          // Surface this round's exact usage so the UI can anchor its live token
          // tally to real figures rather than an estimate of the whole turn.
          response.usage.foreach(u => out.send(AgentEvent.Stream(Right(StreamEvent.RoundComplete(u)))))
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
      // Round-boundary steering: fold in everything the run loop's inner select
      // moved to `pendingInbox` while this round ran (persisting + echoing
      // InputsConsumed). Empty ⇒ Nil, so a quiet turn is unchanged.
      override def drainSteering(): List[Message] = foldItems(drainPending())
    ToolLoop.run(initial, driver).messages

  /** Seed a turn with `items` (persisted, echoed, wrapped via [[foldItems]]) and
    * drive it to completion as a cancellable child fiber.
    *
    * One `Async.select` race drives the whole turn — completion, interrupt, and
    * observing fresh inbox arrivals (buffered in [[pendingInbox]] for the next
    * round-boundary drain). `turn.handle` is the sole classifier of the outcome,
    * so there is no separate interrupt-watcher fiber. The turn (and every fiber
    * it spawns — the stream producer and each tool) runs under one `Future`;
    * cancelling it propagates through the structured scope, and each descendant's
    * cleanup runs on the way out: the LLM request is aborted
    * (`Endpoint.streaming`'s `finally`) and a running subprocess is killed
    * (`NodeProcess`'s `finally`). The Future resolves only once the turn has
    * fully unwound, so by the time we reconcile, all captured state is final.
    *
    * Items that arrived but were not drained before the turn ended stay in
    * `pendingInbox`; the run loop's leftover-check picks them up next — which is
    * how an interrupt flushes the whole queue into a fresh turn. */
  private def runTurn(history: List[Message], items: List[Inbox])(using Async.Spawn): List[Message] =
    resetTurnState()
    // Discard any interrupt that arrived between turns (e.g. a late keypress)
    // so it cannot instantly cancel this fresh turn.
    while interrupts.readSource.poll().isDefined do ()
    val seeded = foldItems(items)
    if seeded.isEmpty then history // nothing could be persisted → no turn to run
    else
      val messages = history ++ seeded
      var grown: Option[List[Message]] = None
      Async.group:
        val turn = Future(converse(messages))
        while grown.isEmpty do
          Async.select(
            turn.handle {
              case Success(g)                        => grown = Some(g)
              case Failure(_: CancellationException) => grown = Some(reconcileInterrupted(messages))
              case Failure(e) =>
                out.send(AgentEvent.Stream(Left(LLMError(s"turn failed: ${Endpoint.errMsg(e)}"))))
                grown = Some(loadHistory(currentSession).getOrElse(messages))
            },
            interrupts.readSource.handle { _ => turn.cancel() },
            inbox.readSource.handle {
              // Observe + echo immediately; hold for the next drain point. The turn
              // fiber drains `pendingInbox` via drainSteering, never this channel,
              // so the run loop / inner select is its only reader.
              case Right(item) => pendingInbox.enqueue(item); out.send(AgentEvent.InputQueued(item))
              case Left(_)     => turn.cancel() // inbox closed → stop the turn; the loop will see the close
            }
          )
      grown.get

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
        out.send(AgentEvent.ModelSwitched(active.label, active.contextWindow, active.provider, active.config.model, active.baseUrl))
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
      case SessionEvent.SystemNotice(text)           => Message.systemNotice(text)

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
            out.send(AgentEvent.Stream(Right(StreamEvent.ToolRunProgress(tu.id, update))))).withCallId(tu.id)
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
    // Coalesce only the wire snapshot — steering appends user-role messages after
    // a tool-results user message, and most providers reject consecutive same-role
    // turns. The carried history stays granular (1:1 with session events).
    val upstream = active.endpoint.stream(
      Message.coalesce(messages),
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
