package auk.runtime

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, desc}
import auk.llm.endpoint.Content

// A trivial tool used to exercise the runtime. Echoes its `text` back, and
// fails when asked to, so error wiring can be tested.
case class EchoParams(
    @desc("Text to echo back") text: String,
    @desc("Fail instead of echoing") fail: Option[Boolean] = None
) derives ToolInput

object Echo extends Tool:
  type Params = EchoParams
  val name = "echo"
  val description = "Echo the given text back."
  val input: ToolInput[EchoParams] = ToolInput[EchoParams]
  def execute(p: EchoParams)(using RuntimeContext, Async): ToolResult =
    if p.fail.contains(true) then ToolResult.error(s"asked to fail: ${p.text}")
    else ToolResult.ok(p.text)

// A tool that throws, to test the dispatcher's defensive catch.
object Boom extends Tool:
  type Params = EchoParams
  val name = "boom"
  val description = "Always throws."
  val input: ToolInput[EchoParams] = ToolInput[EchoParams]
  def execute(p: EchoParams)(using RuntimeContext, Async): ToolResult =
    throw RuntimeException("kaboom")

class ToolRegistrySuite extends munit.FunSuite:

  private val registry = ToolRegistry.of(Echo, Boom)
  private given RuntimeContext = RuntimeContext.cwd()

  test("schemas bridges name, description, properties and required"):
    val schema = registry.schemas.find(_.name == "echo").get
    assertEquals(schema.description, "Echo the given text back.")
    assertEquals(schema.parameters.properties("text").`type`, "string")
    assertEquals(schema.parameters.properties("text").description, "Text to echo back")
    // `text` is required; the Option field `fail` is not.
    assertEquals(schema.parameters.required, List("text"))

  test("dispatch runs the tool and preserves the tool-use id"):
    Async.blocking:
      val out = registry.dispatch(Content.ToolUse("u1", "echo", """{"text":"hi"}"""))
      assertEquals(out.toolUseId, "u1")
      assertEquals(out.content, "hi")
      assertEquals(out.isError, false)

  test("an error result sets isError"):
    Async.blocking:
      val out =
        registry.dispatch(Content.ToolUse("u2", "echo", """{"text":"x","fail":true}"""))
      assert(out.isError)
      assert(out.content.contains("asked to fail"))

  test("unknown tool becomes an error result, not an exception"):
    Async.blocking:
      val out = registry.dispatch(Content.ToolUse("u3", "nope", "{}"))
      assert(out.isError)
      assert(out.content.contains("unknown tool"))

  test("bad arguments become an error result"):
    Async.blocking:
      val out = registry.dispatch(Content.ToolUse("u4", "echo", """{"wrong":1}"""))
      assert(out.isError)
      assert(out.content.contains("invalid arguments"))

  test("a thrown exception is caught and reported"):
    Async.blocking:
      val out = registry.dispatch(Content.ToolUse("u5", "boom", """{"text":"x"}"""))
      assert(out.isError)
      assert(out.content.contains("failed"))

  test("runToolCalls fans out and preserves order"):
    Async.blocking:
      val calls = List[Content.ToolUse](
        Content.ToolUse("a", "echo", """{"text":"1"}"""),
        Content.ToolUse("b", "echo", """{"text":"2"}"""),
        Content.ToolUse("c", "echo", """{"text":"3"}""")
      )
      val results = registry.runToolCalls(calls)
      assertEquals(results.map(_.toolUseId), List("a", "b", "c"))
      assertEquals(results.map(_.content), List("1", "2", "3"))
