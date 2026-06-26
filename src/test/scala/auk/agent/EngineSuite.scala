package auk.agent

import scala.collection.mutable.ListBuffer

import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.TestFs

import auk.llm.endpoint.{
  ChatResponse,
  Content,
  Endpoint,
  FinishReason,
  LLMConfig,
  LLMError,
  Message,
  Role,
  StreamEvent
}
import auk.llm.provider.{ActiveModel, ModelSession}
import auk.llm.tools.{Json, RuntimeContext, Tool, ToolInput, ToolResult, desc}
import auk.runtime.{Echo, Surrogate, ToolRegistry}
import auk.session.{ModelInfo, Session, SessionEvent, SessionProvider}
import auk.utils.Result

/** Parameters for the test-only [[EngineSuite]] blocking tool. */
case class BlockParams(@desc("ignored") ignored: Option[String] = None) derives ToolInput

class EngineSuite extends munit.FunSuite:

  private final class ScriptedStreamEndpoint(
      scripts: List[List[Result[StreamEvent, LLMError]]]
  ) extends Endpoint:
    private var idx = 0
    val seen: ListBuffer[List[Message]] = ListBuffer.empty

    def invoke(
        messages: List[Message],
        config: LLMConfig
    )(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("ScriptedStreamEndpoint does not invoke"))

    def stream(messages: List[Message], config: LLMConfig)(using
        Async.Spawn
    ): ReadableChannel[Result[StreamEvent, LLMError]] =
      seen += messages
      val events =
        if idx >= scripts.length then List(Left(LLMError("scripted endpoint exhausted")))
        else
          val next = scripts(idx)
          idx += 1
          next
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      // Do NOT close the channel: gears' pollRead checks isClosed before the
      // buffer, so closing would discard buffered-but-unread events when the
      // producer races ahead of the engine. Real endpoints likewise rely on a
      // terminal Done/error event rather than close. Every script ends with one.
      Future:
        events.foreach(ch.send)
      ch.asReadable

  /** A stream whose channel closes without ever delivering a Done or error —
    * the failure mode the endpoint `finally` close backstop guards against. */
  private final class SilentlyClosingEndpoint extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("SilentlyClosingEndpoint does not invoke"))
    def stream(messages: List[Message], config: LLMConfig)(using
        Async.Spawn
    ): ReadableChannel[Result[StreamEvent, LLMError]] =
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.close())
      ch.asReadable

  // A tool that suspends until its fiber is cancelled — used to park a turn in
  // tool execution so an interrupt can be exercised. The gate is never sent to,
  // so `read()` blocks (and is interrupted by cancellation).
  private val blockGate = UnboundedChannel[Unit]()
  private object BlockingTool extends Tool:
    type Params = BlockParams
    val name = "block"
    val description = "Blocks until its fiber is cancelled."
    val input: ToolInput[BlockParams] = ToolInput[BlockParams]
    def execute(p: BlockParams)(using RuntimeContext, Async): ToolResult =
      blockGate.read()
      ToolResult.ok("unreached")

  /** A tool that blocks until its own `gate` is sent — lets a test park a turn
    * in tool execution, queue steering while it runs, then release it to reach a
    * round boundary deterministically. Each test uses a fresh gate. */
  private final class GatedTool(gate: UnboundedChannel[Unit]) extends Tool:
    type Params = BlockParams
    val name = "block"
    val description = "Blocks until its gate is sent."
    val input: ToolInput[BlockParams] = ToolInput[BlockParams]
    def execute(p: BlockParams)(using RuntimeContext, Async): ToolResult =
      gate.read()
      ToolResult.ok("ok")

  private def tempDir(): String =
    TestFs.tempDir("auk-engine")

  private def provider(dir: String): SessionProvider =
    SessionProvider.directory(TestFs.join(dir, SessionProvider.RelativePath))

  private def session(dir: String): Session =
    provider(dir).create().toOption.get

  private def textResponse(text: String): ChatResponse =
    ChatResponse(Message.assistant(text), FinishReason.Stop)

  private def toolResponse(id: String, name: String, input: String): ChatResponse =
    ChatResponse(
      Message(Role.Assistant, List(Content.ToolUse(id, name, input))),
      FinishReason.ToolUse
    )

  private def compactionResponse(summary: String): ChatResponse =
    toolResponse("compact_1", "submit_compaction", Json.Obj(List("summary" -> Json.Str(summary))).render)

  private def done(response: ChatResponse): Result[StreamEvent, LLMError] =
    Right(StreamEvent.Done(response))

  /** A persisted assistant event for setup/replay tests that only read back the
    * message (finish reason / usage are immaterial there). */
  private def responded(message: Message): SessionEvent =
    SessionEvent.AssistantResponded(ChatResponse(message, FinishReason.Stop))

  /** The model the live engine stamps on every reply it persists: the test's
    * `ModelSession.of(..., "test-model")` leaves provider/label empty and the
    * window at 0, naming only the model id. */
  private val testModel = ModelInfo("", "test-model", "", 0)

  /** An engine-produced assistant event (carrying [[testModel]]), for the
    * expected `s.events` lists that compare against what the live engine wrote. */
  private def respondedWith(response: ChatResponse): SessionEvent =
    SessionEvent.AssistantResponded(response, Some(testModel))

  private def readUntilTerminal(
      out: ReadableChannel[AgentEvent]
  )(using Async): List[AgentEvent] =
    var acc = List.empty[AgentEvent]
    var running = true
    while running do
      out.read() match
        case Left(_) =>
          running = false
        case Right(event) =>
          acc = acc :+ event
          event match
            case AgentEvent.Stream(Left(_))                    => running = false
            case AgentEvent.Stream(Right(StreamEvent.Done(_))) => running = false
            case _                                             => ()
    acc

  private def readAgentEvent(out: ReadableChannel[AgentEvent])(using Async): AgentEvent =
    out.read() match
      case Right(event) => event
      case Left(err)    => fail(s"event channel closed: $err")

  /** Read (and discard) events until one matches `pred`, returning that event —
    * used to synchronize on a mid-turn signal (a tool starting, an InputQueued
    * echo) before driving the next step. */
  private def readUntil(out: ReadableChannel[AgentEvent])(pred: AgentEvent => Boolean)(using Async): AgentEvent =
    var found: Option[AgentEvent] = None
    while found.isEmpty do
      out.read() match
        case Right(event) => if pred(event) then found = Some(event)
        case Left(err)    => fail(s"event channel closed before match: $err")
    found.get

  private def withEngine(
      session: Session,
      scripts: List[List[Result[StreamEvent, LLMError]]],
      registry: ToolRegistry = ToolRegistry.of(),
      sessions: SessionProvider = provider(tempDir())
  )(
      body: (
          UnboundedChannel[UserCommand],
          UnboundedChannel[Inbox],
          ReadableChannel[AgentEvent],
          ScriptedStreamEndpoint,
          Async
      ) => Unit
  )(using Async.Spawn): Unit =
    val in = UnboundedChannel[UserCommand]()
    val inbox = UnboundedChannel[Inbox]()
    val out = UnboundedChannel[AgentEvent]()
    val endpoint = ScriptedStreamEndpoint(scripts)
    val context = RuntimeContext(tempDir())
    val worker =
      Future:
        try
          Engine(
            in.asReadable,
            out.asSendable,
            UnboundedChannel[Unit]().asReadable,
            inbox.asReadable,
            ModelSession.of(endpoint, LLMConfig(model = "test-model")),
            session,
            sessions,
            registry,
            context
          ).run()
        catch
          case e: Throwable =>
            out.send(AgentEvent.Stream(Left(LLMError(s"engine test failure: ${e.getClass.getSimpleName}: ${e.getMessage}"))))
    try body(in, inbox, out.asReadable, endpoint, summon[Async])
    finally
      in.close()
      inbox.close()
      worker.await
      out.close()

  private def asyncTest(name: String)(body: Async.Spawn ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  asyncTest("a stream that closes without a Done is surfaced as an error, not a hang"):
    val s = session(tempDir())
    val in = UnboundedChannel[UserCommand]()
    val inbox = UnboundedChannel[Inbox]()
    val out = UnboundedChannel[AgentEvent]()
    val worker =
      Future:
        Engine(
          in.asReadable,
          out.asSendable,
          UnboundedChannel[Unit]().asReadable,
          inbox.asReadable,
          ModelSession.of(SilentlyClosingEndpoint(), LLMConfig(model = "test-model")),
          s,
          provider(tempDir())
        ).run()
    try
      inbox.sendImmediately(Inbox.UserMessage("hi"))
      val events = readUntilTerminal(out.asReadable)
      assert(
        events.exists {
          case AgentEvent.Stream(Left(err)) => err.description.contains("ended unexpectedly")
          case _                            => false
        },
        events.toString
      )
    finally
      in.close()
      inbox.close()
      worker.await
      out.close()

  asyncTest("persists user and final assistant events in order"):
    val s = session(tempDir())
    val assistant = textResponse("hello back").message
    val assistantResp = ChatResponse(assistant, FinishReason.Stop)

    withEngine(s, List(List(done(assistantResp)))): (in, inbox, out, _, async) =>
      given Async = async
      inbox.sendImmediately(Inbox.UserMessage("hello"))
      val events = readUntilTerminal(out)
      assertEquals(events.collect { case AgentEvent.Stream(Right(StreamEvent.Done(r))) => r.message }, List(assistant))

    assertEquals(
      s.events,
      Right(List(
        SessionEvent.UserSubmitted("hello"),
        respondedWith(assistantResp)
      ))
    )

  asyncTest("scrubs a lone surrogate from user input before persisting and sending"):
    val s = session(tempDir())
    val assistantResp = ChatResponse(textResponse("ok").message, FinishReason.Stop)

    withEngine(s, List(List(done(assistantResp)))): (in, inbox, out, _, async) =>
      given Async = async
      inbox.sendImmediately(Inbox.UserMessage("hi\uD800there")) // lone high surrogate (e.g. pasted)
      readUntilTerminal(out)

    // The persisted event — replayed verbatim into the next request — is clean, so
    // the lone surrogate never reaches the wire and can't wedge the conversation.
    assertEquals(
      s.events,
      Right(List(
        SessionEvent.UserSubmitted("hi�there"),
        respondedWith(assistantResp)
      ))
    )

  asyncTest("persists tool loop events in order"):
    val s = session(tempDir())
    val tool = toolResponse("t1", "echo", """{"text":"pong"}""").message
    val finalMessage = Message.assistant("done")
    val toolResp = ChatResponse(tool, FinishReason.ToolUse)
    val finalResp = ChatResponse(finalMessage, FinishReason.Stop)

    withEngine(
      s,
      List(
        List(done(toolResp)),
        List(done(finalResp))
      ),
      registry = ToolRegistry.of(Echo)
    ): (in, inbox, out, _, async) =>
      given Async = async
      inbox.sendImmediately(Inbox.UserMessage("use echo"))
      val events = readUntilTerminal(out)
      assertEquals(events.exists {
        case AgentEvent.Stream(Right(StreamEvent.ToolRunStart("t1", "echo"))) => true
        case _                                                               => false
      }, true)
      assertEquals(events.collect { case AgentEvent.Stream(Right(StreamEvent.Done(r))) => r.message }, List(finalMessage))

    assertEquals(
      s.events,
      Right(List(
        SessionEvent.UserSubmitted("use echo"),
        respondedWith(toolResp),
        SessionEvent.ToolResultsReceived(List(Content.ToolResult("t1", "pong"))),
        respondedWith(finalResp)
      ))
    )

  asyncTest("scrubs a lone surrogate in tool output before persisting and sending"):
    // Exercises the main loop's own runTools path (distinct from sub-agent dispatch):
    // `eval_scala` printing a lone surrogate must not survive into the history, or
    // the next request 400s and the conversation wedges.
    val s = session(tempDir())
    val toolResp = ChatResponse(toolResponse("t1", "surrogate", """{"text":"x"}""").message, FinishReason.ToolUse)
    val finalResp = ChatResponse(Message.assistant("done"), FinishReason.Stop)

    withEngine(
      s,
      List(List(done(toolResp)), List(done(finalResp))),
      registry = ToolRegistry.of(Surrogate)
    ): (in, inbox, out, _, async) =>
      given Async = async
      inbox.sendImmediately(Inbox.UserMessage("print a surrogate"))
      readUntilTerminal(out)

    assertEquals(
      s.events,
      Right(List(
        SessionEvent.UserSubmitted("print a surrogate"),
        respondedWith(toolResp),
        SessionEvent.ToolResultsReceived(List(Content.ToolResult("t1", "before�after"))),
        respondedWith(finalResp)
      ))
    )

  asyncTest("an idle system notice wakes the agent and the model sees it xml-wrapped"):
    val s = session(tempDir())
    val ackResp = ChatResponse(Message.assistant("ack"), FinishReason.Stop)

    withEngine(s, List(List(done(ackResp)))): (in, inbox, out, endpoint, async) =>
      given Async = async
      inbox.sendImmediately(Inbox.SystemNotice("disk is full"))
      readUntilTerminal(out)
      // A turn ran on its own; the model saw the notice wrapped in <system-reminder>.
      val firstCall = endpoint.seen.head
      assertEquals(firstCall.size, 1)
      assertEquals(firstCall.head.role, Role.User)
      assert(
        firstCall.head.text.contains("<system-reminder>") && firstCall.head.text.contains("disk is full"),
        firstCall.head.text
      )

    assertEquals(
      s.events,
      Right(List(
        SessionEvent.SystemNotice("disk is full"),
        respondedWith(ackResp)
      ))
    )

  asyncTest("an input that fails to persist is still echoed as consumed so the UI panel drains"):
    // A persist failure surfaces an error and runs no turn, but the input must
    // still be echoed as InputsConsumed — the UI panel drops by FIFO count, so
    // under-reporting would orphan the queued item in the panel forever.
    val parentFile = TestFs.join(tempDir(), "not-a-directory")
    TestFs.write(parentFile, "not a directory")
    val badSession = Session("bad", TestFs.join(parentFile, "session.jsonl"))

    withEngine(badSession, List(List(done(textResponse("unused"))))): (in, inbox, out, endpoint, async) =>
      given Async = async
      inbox.sendImmediately(Inbox.UserMessage("hello"))
      val seen = List(readAgentEvent(out), readAgentEvent(out))
      assert(seen.exists { case AgentEvent.Stream(Left(err)) => err.description.contains("persistence"); case _ => false }, seen.toString)
      assert(seen.contains(AgentEvent.InputsConsumed(List(Inbox.UserMessage("hello")))), seen.toString)
      assertEquals(endpoint.seen.size, 0) // the un-persistable input never reaches the model

  asyncTest("inputs queued during a tool round drain at the boundary, coalesced and in order"):
    val s = session(tempDir())
    val gate = UnboundedChannel[Unit]()
    val toolResp = ChatResponse(toolResponse("t1", "block", "{}").message, FinishReason.ToolUse)
    val finalResp = ChatResponse(Message.assistant("done"), FinishReason.Stop)
    val in = UnboundedChannel[UserCommand]()
    val inbox = UnboundedChannel[Inbox]()
    val out = UnboundedChannel[AgentEvent]()
    val endpoint = ScriptedStreamEndpoint(List(List(done(toolResp)), List(done(finalResp))))
    val worker = Future:
      Engine(in.asReadable, out.asSendable, UnboundedChannel[Unit]().asReadable, inbox.asReadable,
        ModelSession.of(endpoint, LLMConfig(model = "test-model")), s, provider(tempDir()),
        ToolRegistry.of(GatedTool(gate))).run()
    try
      inbox.sendImmediately(Inbox.UserMessage("start"))
      // The blocking tool is now running, so the turn's inner select is observing the inbox.
      readUntil(out.asReadable) {
        case AgentEvent.Stream(Right(StreamEvent.ToolRunStart("t1", "block"))) => true
        case _                                                                 => false
      }
      // Queue a user steer and a system notice while the tool runs.
      inbox.sendImmediately(Inbox.UserMessage("steer me"))
      inbox.sendImmediately(Inbox.SystemNotice("heads up"))
      // Confirm both were observed (queued) before releasing the tool to the boundary.
      var queued = 0
      while queued < 2 do
        out.asReadable.read() match
          case Right(AgentEvent.InputQueued(_)) => queued += 1
          case _                                => ()
      gate.sendImmediately(()) // release the tool → boundary drain → round 2
      readUntilTerminal(out.asReadable)

      // Round 2's wire input: coalesced (no consecutive same-role), and the final
      // user turn carries the tool result, the steer text, and the wrapped notice.
      val round2 = endpoint.seen(1)
      assert(round2.sliding(2).forall { case Seq(a, b) => a.role != b.role; case _ => true }, round2.toString)
      val lastUser = round2.last
      assertEquals(lastUser.role, Role.User)
      assert(lastUser.content.exists { case _: Content.ToolResult => true; case _ => false }, lastUser.toString)
      val texts = lastUser.content.collect { case Content.Text(t) => t }
      assert(texts.exists(_.contains("steer me")), texts.toString)
      assert(texts.exists(t => t.contains("<system-reminder>") && t.contains("heads up")), texts.toString)

      // Persisted in causal order: drained inputs land between the tool results
      // and the next assistant reply, user before system (FIFO).
      assertEquals(
        s.events,
        Right(List(
          SessionEvent.UserSubmitted("start"),
          respondedWith(toolResp),
          SessionEvent.ToolResultsReceived(List(Content.ToolResult("t1", "ok"))),
          SessionEvent.UserSubmitted("steer me"),
          SessionEvent.SystemNotice("heads up"),
          respondedWith(finalResp)
        ))
      )
    finally
      in.close(); inbox.close(); worker.await; out.close()

  asyncTest("queued inputs are flushed into a new turn when the current turn is interrupted"):
    val s = session(tempDir())
    val gate = UnboundedChannel[Unit]()
    val toolResp = ChatResponse(toolResponse("t1", "block", "{}").message, FinishReason.ToolUse)
    val flushResp = ChatResponse(Message.assistant("handled the steer"), FinishReason.Stop)
    val in = UnboundedChannel[UserCommand]()
    val inbox = UnboundedChannel[Inbox]()
    val out = UnboundedChannel[AgentEvent]()
    val interrupts = UnboundedChannel[Unit]()
    val endpoint = ScriptedStreamEndpoint(List(List(done(toolResp)), List(done(flushResp))))
    val worker = Future:
      Engine(in.asReadable, out.asSendable, interrupts.asReadable, inbox.asReadable,
        ModelSession.of(endpoint, LLMConfig(model = "test-model")), s, provider(tempDir()),
        ToolRegistry.of(GatedTool(gate))).run()
    try
      inbox.sendImmediately(Inbox.UserMessage("start"))
      readUntil(out.asReadable) {
        case AgentEvent.Stream(Right(StreamEvent.ToolRunStart("t1", "block"))) => true
        case _                                                                 => false
      }
      // Queue a steer, confirm it is observed, then interrupt the running turn.
      inbox.sendImmediately(Inbox.UserMessage("actually do X"))
      readUntil(out.asReadable) { case AgentEvent.InputQueued(_) => true; case _ => false }
      interrupts.sendImmediately(())
      readUntilTerminal(out.asReadable)

      // The flush turn ran with the queued steer.
      assert(endpoint.seen.exists(call => call.exists(_.text.contains("actually do X"))), endpoint.seen.toString)
      val events = s.events.toOption.get
      assert(events.contains(SessionEvent.Interrupted), events.toString)
      assert(events.contains(SessionEvent.UserSubmitted("actually do X")), events.toString)
      assertEquals(events.last, respondedWith(flushResp))
    finally
      in.close(); inbox.close(); worker.await; out.close()

  asyncTest("an interrupt mid-stream keeps the partial answer and records the interruption"):
    val s = session(tempDir())
    val in = UnboundedChannel[UserCommand]()
    val inbox = UnboundedChannel[Inbox]()
    val out = UnboundedChannel[AgentEvent]()
    val interrupts = UnboundedChannel[Unit]()
    // Two answer deltas, then no Done: the turn parks awaiting more when we interrupt.
    val endpoint = ScriptedStreamEndpoint(
      List(List(Right(StreamEvent.Delta("partial ")), Right(StreamEvent.Delta("answer"))))
    )
    val worker = Future:
      Engine(in.asReadable, out.asSendable, interrupts.asReadable, inbox.asReadable,
        ModelSession.of(endpoint, LLMConfig(model = "test-model")), s, provider(tempDir())).run()
    try
      inbox.sendImmediately(Inbox.UserMessage("hi"))
      def nextDelta(): String =
        out.asReadable.read() match
          case Right(AgentEvent.Stream(Right(StreamEvent.Delta(t)))) => t
          case Right(_)                                              => nextDelta()
          case other                                                => fail(s"expected a delta, got $other")
      // Draining both deltas means their text is captured and the engine is parked.
      assertEquals(nextDelta(), "partial ")
      assertEquals(nextDelta(), "answer")
      interrupts.sendImmediately(())
      var ev = out.asReadable.read()
      while ev.exists(_ != AgentEvent.Interrupted) do ev = out.asReadable.read()
      assertEquals(ev, Right(AgentEvent.Interrupted))
      assertEquals(
        s.events,
        Right(List(
          SessionEvent.UserSubmitted("hi"),
          respondedWith(ChatResponse(Message(Role.Assistant, List(Content.Text("partial answer"))), FinishReason.Stop)),
          SessionEvent.Interrupted
        ))
      )
    finally
      in.close(); inbox.close(); worker.await; out.close()

  asyncTest("an interrupt during tool execution synthesizes results so history stays valid"):
    val s = session(tempDir())
    val toolMsg = toolResponse("t1", "block", "{}").message
    val toolResp = ChatResponse(toolMsg, FinishReason.ToolUse)
    val in = UnboundedChannel[UserCommand]()
    val inbox = UnboundedChannel[Inbox]()
    val out = UnboundedChannel[AgentEvent]()
    val interrupts = UnboundedChannel[Unit]()
    val endpoint = ScriptedStreamEndpoint(List(List(done(toolResp))))
    val worker = Future:
      Engine(in.asReadable, out.asSendable, interrupts.asReadable, inbox.asReadable,
        ModelSession.of(endpoint, LLMConfig(model = "test-model")), s, provider(tempDir()),
        ToolRegistry.of(BlockingTool)).run()
    try
      inbox.sendImmediately(Inbox.UserMessage("go"))
      // Wait until the (blocking) tool has been announced as running, then interrupt.
      var ev = out.asReadable.read()
      while ev.toOption.collect { case AgentEvent.Stream(Right(StreamEvent.ToolRunStart("t1", "block"))) => () }.isEmpty do
        ev = out.asReadable.read()
      interrupts.sendImmediately(())
      var ack = out.asReadable.read()
      while ack.exists(_ != AgentEvent.Interrupted) do ack = out.asReadable.read()
      assertEquals(ack, Right(AgentEvent.Interrupted))
      // The unfinished tool_use gets a synthesized "interrupted" result — no dangling call.
      assertEquals(
        s.events,
        Right(List(
          SessionEvent.UserSubmitted("go"),
          respondedWith(toolResp),
          SessionEvent.ToolResultsReceived(List(Content.ToolResult("t1", "Interrupted by user", isError = true))),
          SessionEvent.Interrupted
        ))
      )
    finally
      in.close(); inbox.close(); worker.await; out.close()

  asyncTest("replays existing session events into the first model call"):
    val s = session(tempDir())
    val priorAssistant = Message.assistant("previous answer")
    val priorResult: Content.ToolResult = Content.ToolResult("old_tool", "old result")
    List(
      SessionEvent.UserSubmitted("previous question"),
      responded(priorAssistant),
      SessionEvent.ToolResultsReceived(List(priorResult))
    ).foreach(ev => assertEquals(s.append(ev), Right(())))

    withEngine(s, List(List(done(textResponse("next answer"))))): (in, inbox, out, endpoint, async) =>
      given Async = async
      inbox.sendImmediately(Inbox.UserMessage("next question"))
      readUntilTerminal(out)
      assertEquals(
        endpoint.seen.head,
        List(
          Message.user("previous question"),
          priorAssistant,
          // The trailing tool-results user message and the fresh user turn coalesce
          // into one user message — the wire never carries two consecutive same-role
          // turns (this session ended on tool results, with no final assistant turn).
          Message(Role.User, List(priorResult, Content.Text("next question")))
        )
      )

  asyncTest("compact command appends a checkpoint and future turns replay from it"):
    val s = session(tempDir())
    s.append(SessionEvent.UserSubmitted("old question"))
    s.append(responded(Message.assistant("old answer")))
    val summary = "## Current Goal\nContinue after compaction."
    val nextAnswer = ChatResponse(Message.assistant("new answer"), FinishReason.Stop)

    withEngine(
      s,
      List(
        List(done(compactionResponse(summary))),
        List(done(nextAnswer))
      )
    ): (in, inbox, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.CompactContext(1000))
      assertEquals(readAgentEvent(out), AgentEvent.ContextCompactionStarted)
      assertEquals(readAgentEvent(out), AgentEvent.ContextCompacted(summary))

      assertEquals(
        s.events.toOption.get.last,
        SessionEvent.ContextCompacted(summary, Some(testModel))
      )

      inbox.sendImmediately(Inbox.UserMessage("next question"))
      readUntilTerminal(out)
      val replayed = endpoint.seen(1)
      assertEquals(replayed.length, 1) // checkpoint + fresh prompt coalesce into one user turn
      assertEquals(replayed.head.role, Role.User)
      val text = replayed.head.text
      assert(text.contains("<context-compaction>"), text)
      assert(text.contains(summary), text)
      assert(text.contains("next question"), text)
      assert(!text.contains("old question"), text)
      assert(!text.contains("old answer"), text)

  asyncTest("a compaction failure leaves the session log unchanged"):
    val s = session(tempDir())
    s.append(SessionEvent.UserSubmitted("old question"))
    val invalid = Json.Obj(List("summary" -> Json.Str(""))).render
    val scripts = List.fill(ContextCompactor.MaxSubmitRetries)(List(done(toolResponse("bad", "submit_compaction", invalid))))

    withEngine(s, scripts): (in, inbox, out, endpoint, async) =>
      given Async = async
      val before = s.events.toOption.get
      in.sendImmediately(UserCommand.CompactContext(1000))
      assertEquals(readAgentEvent(out), AgentEvent.ContextCompactionStarted)
      readAgentEvent(out) match
        case AgentEvent.Stream(Left(err)) =>
          assert(err.description.contains("did not submit a valid summary"), err.description)
        case other => fail(s"expected compaction error, got $other")
      assertEquals(s.events.toOption.get, before)

  asyncTest("compaction requests queued during a compaction are ignored"):
    val s = session(tempDir())
    s.append(SessionEvent.UserSubmitted("old question"))
    val summary = "## Current Goal\nOne compaction."

    withEngine(
      s,
      List(
        List(done(compactionResponse(summary))),
        List(done(compactionResponse("should not run")))
      )
    ): (in, inbox, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.CompactContext(1000))
      in.sendImmediately(UserCommand.CompactContext(1001))
      assertEquals(readAgentEvent(out), AgentEvent.ContextCompactionStarted)
      assertEquals(readAgentEvent(out), AgentEvent.ContextCompacted(summary))

      in.sendImmediately(UserCommand.ListSessions)
      readUntil(out) {
        case AgentEvent.SessionsListed(_) => true
        case _                            => false
      }
      assertEquals(endpoint.seen.size, 1)
      assertEquals(
        s.events.toOption.get.collect { case ev @ SessionEvent.ContextCompacted(_, _) => ev },
        List(SessionEvent.ContextCompacted(summary, Some(testModel)))
      )

  asyncTest("lists resumable sessions with summaries"):
    val dir = tempDir()
    val p = provider(dir)
    val initial = p.create().toOption.get
    val prior = p.create().toOption.get
    prior.append(SessionEvent.UserSubmitted("pick me"))
    prior.append(responded(Message.assistant("prior answer")))

    withEngine(initial, Nil, sessions = p): (in, inbox, out, _, async) =>
      given Async = async
      in.sendImmediately(UserCommand.ListSessions)
      readAgentEvent(out) match
        case AgentEvent.SessionsListed(sessions) =>
          assert(sessions.exists(s => s.id == prior.id && s.preview == "pick me"), sessions)
        case other => fail(s"expected SessionsListed, got $other")

  asyncTest("resumes a selected session and uses its history on the next prompt"):
    val dir = tempDir()
    val p = provider(dir)
    val initial = p.create().toOption.get
    val target = p.create().toOption.get
    val priorAssistant = Message.assistant("old answer")
    target.append(SessionEvent.UserSubmitted("old question"))
    target.append(responded(priorAssistant))

    withEngine(initial, List(List(done(textResponse("new answer")))), sessions = p): (in, inbox, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.ResumeSession(target.id))
      readAgentEvent(out) match
        case AgentEvent.SessionSwitched(snapshot) =>
          assertEquals(snapshot.summary.id, target.id)
          assertEquals(snapshot.events.collect { case SessionEvent.UserSubmitted(text) => text }, List("old question"))
        case other => fail(s"expected SessionSwitched, got $other")

      inbox.sendImmediately(Inbox.UserMessage("next question"))
      readUntilTerminal(out)
      assertEquals(
        endpoint.seen.head,
        List(
          Message.user("old question"),
          priorAssistant,
          Message.user("next question")
        )
      )

  asyncTest("new session clears model history before the next prompt"):
    val dir = tempDir()
    val p = provider(dir)
    val initial = p.create().toOption.get
    initial.append(SessionEvent.UserSubmitted("old question"))
    initial.append(responded(Message.assistant("old answer")))
    var newId = ""

    withEngine(initial, List(List(done(textResponse("fresh answer")))), sessions = p): (in, inbox, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.NewSession)
      readAgentEvent(out) match
        case AgentEvent.SessionSwitched(snapshot) =>
          newId = snapshot.summary.id
          assertEquals(snapshot.events, Nil)
          assertEquals(snapshot.summary.preview, "Empty session")
        case other => fail(s"expected SessionSwitched, got $other")

      inbox.sendImmediately(Inbox.UserMessage("fresh prompt"))
      readUntilTerminal(out)
      assertEquals(endpoint.seen.head, List(Message.user("fresh prompt")))

    val reopened = p.open(newId).toOption.flatten.get
    assertEquals(
      reopened.events.toOption.get.collect { case SessionEvent.UserSubmitted(text) => text },
      List("fresh prompt")
    )

  asyncTest("failed resume reports an error and keeps the current session"):
    val dir = tempDir()
    val p = provider(dir)
    val initial = p.create().toOption.get
    val priorAssistant = Message.assistant("still here")
    initial.append(SessionEvent.UserSubmitted("current question"))
    initial.append(responded(priorAssistant))

    withEngine(initial, List(List(done(textResponse("answer")))), sessions = p): (in, inbox, out, endpoint, async) =>
      given Async = async
      in.sendImmediately(UserCommand.ResumeSession("missing"))
      readAgentEvent(out) match
        case AgentEvent.Stream(Left(err)) =>
          assert(err.description.contains("Session persistence error"), err.description)
        case other => fail(s"expected persistence error, got $other")

      inbox.sendImmediately(Inbox.UserMessage("next"))
      readUntilTerminal(out)
      assertEquals(
        endpoint.seen.head,
        List(Message.user("current question"), priorAssistant, Message.user("next"))
      )

  asyncTest("reports user append failure without calling the endpoint"):
    val parentFile = TestFs.join(tempDir(), "not-a-directory")
    TestFs.write(parentFile, "not a directory")
    val badSession = Session("bad", TestFs.join(parentFile, "session.jsonl"))

    withEngine(badSession, List(List(done(textResponse("unused"))))): (in, inbox, out, endpoint, async) =>
      given Async = async
      inbox.sendImmediately(Inbox.UserMessage("hello"))
      val events = readUntilTerminal(out)
      assert(events.headOption.exists {
        case AgentEvent.Stream(Left(err)) => err.description.contains("Session persistence error")
        case _                            => false
      })
      assertEquals(endpoint.seen.size, 0)

  asyncTest("reports corrupt startup session without calling the endpoint"):
    val dir = tempDir()
    val s = session(dir)
    assertEquals(s.append(SessionEvent.UserSubmitted("ok")), Right(()))
    val logFile = TestFs.join(TestFs.join(dir, SessionProvider.RelativePath), s.id + ".jsonl")
    TestFs.append(logFile, "{ not json\n")

    withEngine(s, List(List(done(textResponse("unused"))))): (_, _, out, endpoint, async) =>
      given Async = async
      val events = readUntilTerminal(out)
      assert(events.headOption.exists {
        case AgentEvent.Stream(Left(err)) => err.description.contains("Session persistence error")
        case _                            => false
      })
      assertEquals(endpoint.seen.size, 0)

  asyncTest("switching the model swaps the active model, emits ModelSwitched, and persists"):
    val s = session(tempDir())
    val in = UnboundedChannel[UserCommand]()
    val inbox = UnboundedChannel[Inbox]()
    val out = UnboundedChannel[AgentEvent]()
    val ep1 = ScriptedStreamEndpoint(Nil)
    val ep2 = ScriptedStreamEndpoint(Nil)
    val models = ModelSession(
      ActiveModel(ep1, LLMConfig(model = "m1"), "Model One"),
      (_, id) =>
        if id == "m2" then Right(ActiveModel(ep2, LLMConfig(model = "m2"), "Model Two", contextWindow = 128_000))
        else Left(s"no such model '$id'")
    )
    var persisted: Option[(String, String)] = None
    val persist: (String, String) => Either[String, Unit] = (pk, id) =>
      persisted = Some((pk, id)); Right(())
    val worker = Future:
      Engine(in.asReadable, out.asSendable, UnboundedChannel[Unit]().asReadable, inbox.asReadable, models, s, provider(tempDir()),
        ToolRegistry.of(), RuntimeContext(tempDir()), persist).run()
    in.sendImmediately(UserCommand.SwitchModel("openrouter", "m2"))
    assertEquals(readAgentEvent(out.asReadable), AgentEvent.ModelSwitched("Model Two", 128_000, "", "m2", ""))
    assertEquals(models.active.label, "Model Two")
    assertEquals(persisted, Some(("openrouter", "m2")))
    in.close(); inbox.close(); worker.await; out.close()

  asyncTest("an invalid model switch reports an error and keeps the current model"):
    val s = session(tempDir())
    val in = UnboundedChannel[UserCommand]()
    val inbox = UnboundedChannel[Inbox]()
    val out = UnboundedChannel[AgentEvent]()
    val models = ModelSession.of(ScriptedStreamEndpoint(Nil), LLMConfig(model = "m1"), "Model One")
    val worker = Future:
      Engine(in.asReadable, out.asSendable, UnboundedChannel[Unit]().asReadable, inbox.asReadable, models, s, provider(tempDir()),
        ToolRegistry.of(), RuntimeContext(tempDir())).run()
    in.sendImmediately(UserCommand.SwitchModel("x", "bad"))
    readAgentEvent(out.asReadable) match
      case AgentEvent.Stream(Left(err)) => assert(err.description.contains("Could not switch model"), err.description)
      case other                        => fail(s"expected a switch error, got $other")
    assertEquals(models.active.label, "Model One")
    in.close(); inbox.close(); worker.await; out.close()
