package auk.runtime

import scala.collection.mutable.ListBuffer
import gears.async.{Async, ReadableChannel}
import gears.async.default.given

import auk.llm.tools.RuntimeContext
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

/** An endpoint that replays a fixed script of results, one per `invoke`, and
  * records the messages it was asked with so tests can assert how the sub-agent
  * threads tool results back into the conversation. `stream` is never exercised
  * by the sub-agent and is left unimplemented.
  */
class ScriptedEndpoint(script: List[Result[ChatResponse, LLMError]]) extends Endpoint:
  private var idx = 0
  val seen: ListBuffer[List[Message]] = ListBuffer.empty

  def invoke(
      messages: List[Message],
      config: LLMConfig
  ): Result[ChatResponse, LLMError] =
    seen += messages
    if idx >= script.length then Left(LLMError("scripted endpoint exhausted"))
    else
      val r = script(idx)
      idx += 1
      r

  def stream(messages: List[Message], config: LLMConfig)(using
      Async.Spawn
  ): ReadableChannel[Result[StreamEvent, LLMError]] =
    throw UnsupportedOperationException("ScriptedEndpoint does not stream")

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
      registry: ToolRegistry = ToolRegistry.of(),
      maxRounds: Int = 16
  ): (SubAgent, ScriptedEndpoint) =
    val endpoint = ScriptedEndpoint(script)
    val agent = SubAgent(
      endpoint = endpoint,
      config = LLMConfig(model = "test-model"),
      registry = registry,
      maxRounds = maxRounds
    )
    (agent, endpoint)

  test("returns the model's final text when no tools are requested"):
    Async.blocking:
      val (agent, _) = subAgent(List(Right(text("all done"))))
      val r = agent.execute(SubAgentParams("a task", "do the thing"))
      assertEquals(r.isError, false)
      assertEquals(r.output, "all done")
      assertEquals(r.metadata("rounds"), "1")

  test("seeds the conversation with the prompt"):
    Async.blocking:
      val (agent, endpoint) = subAgent(List(Right(text("ok"))))
      agent.execute(SubAgentParams("greet", "say hello"))
      assertEquals(endpoint.seen.head.map(_.text), List("say hello"))

  test("runs a requested tool and feeds the result back before finishing"):
    Async.blocking:
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
    Async.blocking:
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
    Async.blocking:
      val (agent, _) = subAgent(List(Left(LLMError("boom"))))
      val r = agent.execute(SubAgentParams("t", "go"))
      assert(r.isError)
      assert(r.output.contains("boom"))

  test("stops with an error when the round cap is reached mid-tool-loop"):
    Async.blocking:
      val (agent, _) = subAgent(
        script = List(Right(toolCall("t1", "echo", """{"text":"again"}"""))),
        registry = ToolRegistry.of(Echo),
        maxRounds = 1
      )
      val r = agent.execute(SubAgentParams("t", "loop forever"))
      assert(r.isError)
      assert(r.output.contains("cap"))
      assertEquals(r.metadata("rounds"), "1")

  test("rejects an empty prompt without calling the model"):
    Async.blocking:
      val (agent, endpoint) = subAgent(List(Right(text("unused"))))
      val r = agent.execute(SubAgentParams("t", "   "))
      assert(r.isError)
      assertEquals(endpoint.seen.size, 0)
