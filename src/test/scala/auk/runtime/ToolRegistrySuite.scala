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

// A tool whose output carries a lone UTF-16 surrogate (as `eval_scala` printing
// one would), to test that dispatch scrubs it before it enters the history.
object Surrogate extends Tool:
  type Params = EchoParams
  val name = "surrogate"
  val description = "Emits a lone surrogate."
  val input: ToolInput[EchoParams] = ToolInput[EchoParams]
  def execute(p: EchoParams)(using RuntimeContext, Async): ToolResult =
    ToolResult.ok("before\uD800after") // lone high surrogate

// A supplier-only tool, added after the registry is built, to exercise the
// dynamic supplier.
object LateTool extends Tool:
  type Params = EchoParams
  val name = "late"
  val description = "A tool added after the registry was built."
  val input: ToolInput[EchoParams] = ToolInput[EchoParams]
  def execute(p: EchoParams)(using RuntimeContext, Async): ToolResult = ToolResult.ok(s"late:${p.text}")

// A supplier tool whose name shadows a static one, to prove static tools win.
object ShadowEcho extends Tool:
  type Params = EchoParams
  val name = "echo"
  val description = "Shadow echo (should never be reachable — static wins)."
  val input: ToolInput[EchoParams] = ToolInput[EchoParams]
  def execute(p: EchoParams)(using RuntimeContext, Async): ToolResult = ToolResult.ok(s"SHADOW:${p.text}")

class ToolRegistrySuite extends munit.FunSuite:

  private val registry = ToolRegistry.of(Echo, Boom, Surrogate)
  private given RuntimeContext = RuntimeContext.cwd()

  test("schemas bridges name, description, properties and required"):
    val schema = registry.schemas.find(_.name == "echo").get
    assertEquals(schema.description, "Echo the given text back.")
    assertEquals(schema.parameters.properties("text").`type`, "string")
    assertEquals(schema.parameters.properties("text").description, "Text to echo back")
    // `text` is required; the Option field `fail` is not.
    assertEquals(schema.parameters.required, List("text"))

  test("dispatch runs the tool and preserves the tool-use id"):
    Async.fromSync:
      val out = registry.dispatch(Content.ToolUse("u1", "echo", """{"text":"hi"}"""))
      assertEquals(out.toolUseId, "u1")
      assertEquals(out.content, "hi")
      assertEquals(out.isError, false)

  test("an error result sets isError"):
    Async.fromSync:
      val out =
        registry.dispatch(Content.ToolUse("u2", "echo", """{"text":"x","fail":true}"""))
      assert(out.isError)
      assert(out.content.contains("asked to fail"))

  test("unknown tool becomes an error result, not an exception"):
    Async.fromSync:
      val out = registry.dispatch(Content.ToolUse("u3", "nope", "{}"))
      assert(out.isError)
      assert(out.content.contains("unknown tool"))

  test("bad arguments become an error result"):
    Async.fromSync:
      val out = registry.dispatch(Content.ToolUse("u4", "echo", """{"wrong":1}"""))
      assert(out.isError)
      assert(out.content.contains("invalid arguments"))

  test("a thrown exception is caught and reported"):
    Async.fromSync:
      val out = registry.dispatch(Content.ToolUse("u5", "boom", """{"text":"x"}"""))
      assert(out.isError)
      assert(out.content.contains("failed"))

  test("dispatch scrubs a lone surrogate from tool output (would otherwise wedge the API)"):
    Async.fromSync:
      val out = registry.dispatch(Content.ToolUse("u6", "surrogate", """{"text":"x"}"""))
      assertEquals(out.content, "before�after")
      assert(!out.content.exists(c => c >= '\uD800' && c <= '\uDFFF'), "a lone surrogate code unit survived")

  test("runToolCalls fans out and preserves order"):
    Async.fromSync:
      val calls = List[Content.ToolUse](
        Content.ToolUse("a", "echo", """{"text":"1"}"""),
        Content.ToolUse("b", "echo", """{"text":"2"}"""),
        Content.ToolUse("c", "echo", """{"text":"3"}""")
      )
      val results = registry.runToolCalls(calls)
      assertEquals(results.map(_.toolUseId), List("a", "b", "c"))
      assertEquals(results.map(_.content), List("1", "2", "3"))

  test("a dynamic supplier's tools are advertised and dispatchable, and appear without rebuilding"):
    var extra = List.empty[Tool]
    val reg = ToolRegistry.withExtra(() => extra)(Echo)
    // Before the supplier has anything, only the static tool is present.
    assertEquals(reg.schemas.map(_.name), List("echo"))
    assertEquals(reg.get("late"), None)
    // A late-added tool is picked up on the next read — no registry rebuild.
    extra = List(LateTool)
    assert(reg.schemas.map(_.name).contains("late"))
    assert(reg.get("late").isDefined)
    Async.fromSync:
      val out = reg.dispatch(Content.ToolUse("x", "late", """{"text":"hi"}"""))
      assertEquals(out.content, "late:hi")

  test("a static tool wins over a dynamic supplier tool with the same name"):
    val reg = ToolRegistry.withExtra(() => List(ShadowEcho))(Echo)
    assert(reg.get("echo").exists(_ eq Echo), "static Echo must win the name lookup")
    Async.fromSync:
      val out = reg.dispatch(Content.ToolUse("x", "echo", """{"text":"hi"}"""))
      assertEquals(out.content, "hi") // Echo, not SHADOW
