package auk.agent

import gears.async.{Async, Future, ReadableChannel, SendableChannel}
import auk.llm.endpoint.{Endpoint, LLMConfig, StreamEvent, Message, Content, Role, ChatResponse, LLMError}
import auk.llm.tools.RuntimeContext
import auk.runtime.ToolRegistry
import auk.session.{Session, SessionEvent, SessionProvider, SessionSnapshot, SessionSummary}
import auk.utils.Result

/** A single-threaded agent loop with tool use.
  *
  * Reads [[UserCommand]]s and, for each submitted line, drives one turn to
  * completion: it streams an assistant reply, and while the model asks for
  * tools, it runs them through the [[registry]], feeds the results back, and
  * streams again — until the model answers without a tool call (or the round
  * cap is hit). Conversation history grows across turns so the model keeps
  * context.
  *
  * Streaming events flow to the UI verbatim, with one exception: the terminal
  * `Done` of an *intermediate* (tool-requesting) round is held back, so the UI
  * sees a single continuous turn ending in one `Done` rather than blinking back
  * to idle between tool rounds.
  */
final class Engine(
    in: ReadableChannel[UserCommand],
    out: SendableChannel[AgentEvent],
    endpoint: Endpoint,
    config: LLMConfig,
    initialSession: Session,
    sessions: SessionProvider,
    registry: ToolRegistry = ToolRegistry.of(),
    context: RuntimeContext = RuntimeContext.cwd(),
    maxToolRounds: Int = 128
):
  private given RuntimeContext = context

  private var currentSession: Session = initialSession

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
                  history = converse(history :+ Message.user(text))
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
            case Right(UserCommand.Interrupt) => () // not handled yet

  /** Drive a user turn to completion: stream the reply, and while the model
    * requests tools, run them and stream again. Returns the conversation grown
    * with every message exchanged (assistant replies and tool results). */
  private def converse(initial: List[Message])(using Async.Spawn): List[Message] =
    var messages = initial
    var round = 0
    var turning = true
    while turning do
      streamTurn(messages) match
        case None => turning = false // error or closed; already forwarded
        case Some(response) =>
          val assistant = response.message
          if appendEvent(SessionEvent.AssistantResponded(assistant)).isLeft then
            turning = false
          else
            messages = messages :+ assistant
            round += 1
            val toolUses = assistant.content.collect { case t: Content.ToolUse => t }
            if toolUses.nonEmpty && round < maxToolRounds then
              // Intermediate round: keep the turn alive (the Done was held back),
              // run the tools, and feed their results back as the next message.
              val results = runTools(toolUses)
              if appendEvent(SessionEvent.ToolResultsReceived(results)).isLeft then
                turning = false
              else
                messages = messages :+ Message(Role.User, results)
            else
              // Final round: surface the completed turn to the UI.
              out.send(AgentEvent.Stream(Right(StreamEvent.Done(response))))
              turning = false
    messages

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

  private def replayMessages(events: List[SessionEvent]): List[Message] =
    events.map:
      case SessionEvent.UserSubmitted(text)         => Message.user(text)
      case SessionEvent.AssistantResponded(message) => message
      case SessionEvent.ToolResultsReceived(results) => Message(Role.User, results)

  private def appendEvent(event: SessionEvent)(using Async): Either[String, Unit] =
    currentSession.append(event).left.map: err =>
      reportPersistence(err)
      err

  private def reportPersistence(err: String)(using Async): Unit =
    out.send(AgentEvent.Stream(Left(LLMError(s"Session persistence error: $err"))))

  /** Run each requested tool concurrently, bracketing every call with
    * `ToolRunStart`/`ToolRunEnd` events so the UI can show progress and the
    * tool's metadata (e.g. a sub-agent's token totals). Results are collected in
    * the original order to line up with the model's calls. */
  private def runTools(
      toolUses: List[Content.ToolUse]
  )(using Async.Spawn): List[Content.ToolResult] =
    toolUses.foreach(tu => out.send(AgentEvent.Stream(Right(StreamEvent.ToolRunStart(tu.id, tu.name)))))
    toolUses
      .map: tu =>
        Future[Content.ToolResult]:
          val result = registry.run(tu)
          out.send(AgentEvent.Stream(Right(StreamEvent.ToolRunEnd(tu.id, result.isError, result.metadata))))
          Content.ToolResult(tu.id, result.output, isError = result.isError)
      .map(_.await)

  /** Stream one assistant turn, forwarding every event to the UI except the
    * terminal `Done`, which is captured and returned so [[converse]] can decide
    * whether to continue (run tools) or surface it as the turn's end. Returns
    * the full `ChatResponse`, or None if the turn ended without a Done (an
    * error — already forwarded — or a closed channel). */
  private def streamTurn(
      messages: List[Message]
  )(using Async.Spawn): Option[ChatResponse] =
    val upstream = endpoint.stream(messages, config)
    var response: Option[ChatResponse] = None
    var streaming = true
    while streaming do
      upstream.read() match
        case Left(_) => streaming = false // upstream closed without a Done
        case Right(result) =>
          result match
            case Right(StreamEvent.Done(r)) =>
              response = Some(r) // held back; converse forwards the final one
              streaming = false
            case Left(_) =>
              out.send(AgentEvent.Stream(result)) // forward the error and stop
              streaming = false
            case Right(_) =>
              out.send(AgentEvent.Stream(result)) // delta / thinking / tool-call — forward verbatim
    response
