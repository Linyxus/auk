package auk.runtime

import scala.collection.mutable.ListBuffer
import gears.async.{Async, Future, ReadableChannel, UnboundedChannel}
import gears.async.default.given

import auk.llm.provider.ModelSession
import auk.llm.tools.{RuntimeContext, ProgressSink}
import auk.llm.endpoint.{
  Endpoint,
  LLMConfig,
  ChatResponse,
  Message,
  Content,
  Role,
  FinishReason,
  StreamEvent,
  LLMError
}
import auk.utils.Result

/** An endpoint that replays a fixed script of results, one per streamed turn,
  * and records the messages it was asked with so tests can assert how the
  * sub-agent threads tool results back into the conversation. Each scripted
  * result is delivered as a single-event stream — a `Done` carrying the response,
  * or the error — mirroring how a real endpoint surfaces a turn to the shared
  * [[auk.agent.StreamConsumer]]. `invoke` is no longer on the sub-agent's path.
  */
class ScriptedEndpoint(script: List[Result[ChatResponse, LLMError]]) extends Endpoint:
  private var idx = 0
  val seen: ListBuffer[List[Message]] = ListBuffer.empty

  def invoke(
      messages: List[Message],
      config: LLMConfig
  )(using Async): Result[ChatResponse, LLMError] =
    Left(LLMError("ScriptedEndpoint streams; invoke is unused"))

  def stream(messages: List[Message], config: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    seen += messages
    val event: Result[StreamEvent, LLMError] =
      if idx >= script.length then Left(LLMError("scripted endpoint exhausted"))
      else
        val r = script(idx)
        idx += 1
        r match
          case Right(response) => Right(StreamEvent.Done(response))
          case Left(err)       => Left(err)
    val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
    // Match real endpoints: deliver the terminal event on a fiber and leave the
    // channel open (closing would race away buffered-but-unread events).
    Future(ch.send(event))
    ch.asReadable

class SubAgentSuite extends munit.FunSuite:

  private given RuntimeContext = RuntimeContext.cwd()

  private def text(s: String): ChatResponse =
    ChatResponse(Message(Role.Assistant, List(Content.Text(s))), FinishReason.Stop)

  private def toolCall(id: String, name: String, input: String): ChatResponse =
    ChatResponse(
      Message(Role.Assistant, List(Content.ToolUse(id, name, input))),
      FinishReason.ToolUse
    )

  private def subAgent(
      script: List[Result[ChatResponse, LLMError]],
      registry: ToolRegistry = ToolRegistry.of()
  ): (SubAgent, ScriptedEndpoint) =
    val endpoint = ScriptedEndpoint(script)
    val agent = SubAgent(
      models = ModelSession.of(endpoint, LLMConfig(model = "test-model")),
      registry = registry
    )
    (agent, endpoint)

  test("returns the model's final text when no tools are requested"):
    Async.fromSync:
      val (agent, _) = subAgent(List(Right(text("all done"))))
      val r = agent.execute(SubAgentParams("a task", "do the thing"))
      assertEquals(r.isError, false)
      assertEquals(r.output, "all done")
      assertEquals(r.metadata("rounds"), "1")

  test("seeds the conversation with the prompt"):
    Async.fromSync:
      val (agent, endpoint) = subAgent(List(Right(text("ok"))))
      agent.execute(SubAgentParams("greet", "say hello"))
      assertEquals(endpoint.seen.head.map(_.text), List("say hello"))

  test("runs a requested tool and feeds the result back before finishing"):
    Async.fromSync:
      val (agent, endpoint) = subAgent(
        script = List(
          Right(toolCall("t1", "echo", """{"text":"pong"}""")),
          Right(text("the tool said pong"))
        ),
        registry = ToolRegistry.of(Echo)
      )
      val r = agent.execute(SubAgentParams("echo task", "use the echo tool"))
      assertEquals(r.isError, false)
      assertEquals(r.output, "the tool said pong")
      assertEquals(r.metadata("rounds"), "2")

      // The second invocation must have seen the tool result threaded in as a
      // user turn: prompt, assistant tool-use, tool result.
      val secondCall = endpoint.seen(1)
      val toolResults = secondCall.flatMap(_.content).collect {
        case tr: Content.ToolResult => tr
      }
      assertEquals(toolResults.map(_.content), List("pong"))
      assertEquals(toolResults.map(_.toolUseId), List("t1"))

  test("aggregates token usage across rounds"):
    import auk.llm.endpoint.Usage
    Async.fromSync:
      val withUsage =
        ChatResponse(
          Message(Role.Assistant, List(Content.ToolUse("t1", "echo", """{"text":"x"}"""))),
          FinishReason.ToolUse,
          usage = Some(Usage(10, 5))
        )
      val finalWithUsage =
        ChatResponse(
          Message(Role.Assistant, List(Content.Text("done"))),
          FinishReason.Stop,
          usage = Some(Usage(20, 7))
        )
      val (agent, _) =
        subAgent(List(Right(withUsage), Right(finalWithUsage)), ToolRegistry.of(Echo))
      val r = agent.execute(SubAgentParams("t", "go"))
      assertEquals(r.metadata("inputTokens"), "30")
      assertEquals(r.metadata("outputTokens"), "12")

  test("surfaces an endpoint error as an error result"):
    Async.fromSync:
      val (agent, _) = subAgent(List(Left(LLMError("boom"))))
      val r = agent.execute(SubAgentParams("t", "go"))
      assert(r.isError)
      assert(r.output.contains("boom"))

  test("runs as many tool rounds as the model asks for, with no cap"):
    Async.fromSync:
      // Five tool rounds in a row, then a final answer — more than the old cap.
      val rounds = List.tabulate(5)(i => Right(toolCall(s"t$i", "echo", s"""{"text":"r$i"}""")))
      val (agent, endpoint) = subAgent(
        script = rounds :+ Right(text("done after five")),
        registry = ToolRegistry.of(Echo)
      )
      val r = agent.execute(SubAgentParams("t", "loop a while"))
      assertEquals(r.isError, false)
      assertEquals(r.output, "done after five")
      assertEquals(r.metadata("rounds"), "6")
      assertEquals(endpoint.seen.size, 6)

  test("rejects an empty prompt without calling the model"):
    Async.fromSync:
      val (agent, endpoint) = subAgent(List(Right(text("unused"))))
      val r = agent.execute(SubAgentParams("t", "   "))
      assert(r.isError)
      assertEquals(endpoint.seen.size, 0)

  test("writes a sub-agent transcript log under the session when a ref and call id are in scope"):
    Async.fromSync:
      val dir = auk.TestFs.tempDir("auk-subagent")
      val endpoint = ScriptedEndpoint(List(
        Right(toolCall("t1", "echo", """{"text":"pong"}""")),
        Right(text("done"))
      ))
      val agent = SubAgent(
        models = ModelSession.of(endpoint, LLMConfig(model = "test-model")),
        registry = ToolRegistry.of(Echo),
        sessionRef = Some(auk.session.SessionRef("sess1"))
      )
      val ctx = RuntimeContext(workingDirectory = dir, callId = Some("call_x"))
      agent.execute(SubAgentParams("echo task", "use echo"))(using ctx, summon[Async])

      val path = auk.session.SessionRef.subagentLog(
        ctx.resolve(auk.session.SessionProvider.RelativePath), "sess1", "call_x")
      val lines = auk.session.JsonlLog.readLines(path).toOption.get
      // Lifecycle (started … finished) brackets the tool transcript.
      assert(lines.size >= 2, lines.toString)
      assert(lines.forall(l => auk.workflow.WireCodec.decode(l).isRight), lines.toString)
      assert(lines.head.contains("use echo"), lines.head) // synthesized NodeStarted carries the prompt
      assert(lines.last.contains("done"), lines.last)      // synthesized NodeFinished carries the result

  test("writes no sub-agent log when no session ref is configured"):
    Async.fromSync:
      val dir = auk.TestFs.tempDir("auk-subagent")
      val (agent, _) = subAgent(List(Right(text("done"))))
      val ctx = RuntimeContext(workingDirectory = dir, callId = Some("call_y"))
      agent.execute(SubAgentParams("t", "go"))(using ctx, summon[Async])
      val path = auk.session.SessionRef.subagentLog(
        ctx.resolve(auk.session.SessionProvider.RelativePath), "sess1", "call_y")
      assertEquals(auk.session.JsonlLog.readLines(path), Right(Nil))

  test("reports cumulative token totals through the progress sink after each round"):
    import auk.llm.endpoint.Usage
    Async.fromSync:
      val round1 =
        ChatResponse(
          Message(Role.Assistant, List(Content.ToolUse("t1", "echo", """{"text":"x"}"""))),
          FinishReason.ToolUse,
          usage = Some(Usage(10, 5))
        )
      val round2 =
        ChatResponse(
          Message(Role.Assistant, List(Content.Text("done"))),
          FinishReason.Stop,
          usage = Some(Usage(20, 7))
        )
      val updates = ListBuffer.empty[Map[String, String]]
      // A context whose progress sink records every update, shadowing the
      // suite's no-op default for this test.
      given RuntimeContext =
        RuntimeContext.cwd().withProgress(ProgressSink(update => updates += update))
      val (agent, _) = subAgent(List(Right(round1), Right(round2)), ToolRegistry.of(Echo))
      agent.execute(SubAgentParams("t", "go"))

      // The totals accumulate: round one's usage, then the sum after round two.
      assertEquals(
        updates.toList,
        List(
          Map("inputTokens" -> "10", "outputTokens" -> "5"),
          Map("inputTokens" -> "30", "outputTokens" -> "12")
        )
      )
