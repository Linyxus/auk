package auk.runtime

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.{ApprovalPolicy, Json, RuntimeContext, Schema}
import auk.platform.Platform

/** Unit tests for the injected `submit_result` tool's validate-and-retry
  * behavior: a matching value is captured (unwrapped); a mismatching or malformed
  * one is rejected with a precise, retry-encouraging error and counted; the cap
  * changes the message; and once captured, further calls are idempotent. */
class SubmitResultSuite extends munit.FunSuite:

  private def ctx: RuntimeContext = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll)

  private def submitTool(resultSchema: Json, maxRetries: Int = 5): WorkflowBridge.SubmitResult =
    new WorkflowBridge.SubmitResult(
      Schema.obj(List("result" -> WorkflowBridge.jsonToSchema(resultSchema)), List("result")),
      resultSchema,
      maxRetries
    )

  /** The model's args to `submit_result`: `{ "result": <value> }`. */
  private def wrap(value: Json): Json = Json.Obj(List("result" -> value))

  private val intSchema: Json = Json.Obj(List("type" -> Json.Str("integer")))
  private val objSchema: Json = Json.Obj(List(
    "type" -> Json.Str("object"),
    "properties" -> Json.Obj(List("msg" -> Json.Obj(List("type" -> Json.Str("string"))), "n" -> intSchema)),
    "required" -> Json.Arr(List(Json.Str("msg"), Json.Str("n")))
  ))

  test("a matching value is captured (unwrapped) and reported ok"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(intSchema)
      val r = s.execute(wrap(Json.num(7)))
      assert(!r.isError, r.output)
      assertEquals(s.captured, Some(Json.num(7)))
      assertEquals(s.rejections, 0)
      assertEquals(s.lastError, None)

  test("a typed object result is captured as the bare result value"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(objSchema)
      val value = Json.Obj(List("msg" -> Json.Str("hi"), "n" -> Json.num(3)))
      val r = s.execute(wrap(value))
      assert(!r.isError, r.output)
      assertEquals(s.captured, Some(value))

  test("a mismatching value is rejected with the parse error and counted, nothing captured"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(intSchema)
      val r = s.execute(wrap(Json.Str("not a number")))
      assert(r.isError, r.output)
      assert(r.output.contains("expected integer, got string"), r.output)
      assert(r.output.contains("submit_result"), r.output)
      assertEquals(s.captured, None)
      assertEquals(s.rejections, 1)
      assertEquals(s.lastError, Some("expected integer, got string"))

  test("a wrong nested field is rejected with its field path"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(objSchema)
      val r = s.execute(wrap(Json.Obj(List("msg" -> Json.Str("hi")))))
      assert(r.isError, r.output)
      assert(r.output.contains("missing required field 'n'"), r.output)
      assertEquals(s.rejections, 1)

  test("submit_result without a `result` field is rejected"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(intSchema)
      val r = s.execute(Json.Obj(List("answer" -> Json.num(1))))
      assert(r.isError, r.output)
      assert(r.output.contains("missing required field 'result'"), r.output)
      assertEquals(s.rejections, 1)

  test("non-object args are rejected"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(intSchema)
      val r = s.execute(Json.Str("oops"))
      assert(r.isError, r.output)
      assert(r.output.contains("expected an object with a 'result' field"), r.output)
      assertEquals(s.rejections, 1)

  test("rejections count up and the message turns to 'used all N attempts' at the cap"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(intSchema, maxRetries = 3)
      val r1 = s.execute(wrap(Json.Str("x")))
      assertEquals(s.rejections, 1)
      assert(r1.output.contains("attempt 1 of 3"), r1.output)
      val r2 = s.execute(wrap(Json.Str("x")))
      assertEquals(s.rejections, 2)
      assert(r2.output.contains("attempt 2 of 3"), r2.output)
      val r3 = s.execute(wrap(Json.Str("x")))
      assertEquals(s.rejections, 3)
      assert(r3.output.contains("used all 3 attempts"), r3.output)
      assertEquals(s.captured, None)

  test("after a valid capture further calls are idempotent and never overwrite it"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(intSchema)
      assert(!s.execute(wrap(Json.num(7))).isError)
      assertEquals(s.captured, Some(Json.num(7)))
      // A later (even invalid) call must not change the captured result or count.
      val again = s.execute(wrap(Json.Str("nope")))
      assert(!again.isError, again.output)
      assert(again.output.contains("already recorded"), again.output)
      assertEquals(s.captured, Some(Json.num(7)))
      assertEquals(s.rejections, 0)

  test("a recovered submission (invalid then valid) ends captured with no lingering error"):
    Async.fromSync:
      given RuntimeContext = ctx
      val s = submitTool(intSchema)
      assert(s.execute(wrap(Json.Str("x"))).isError)
      assertEquals(s.rejections, 1)
      assert(!s.execute(wrap(Json.num(42))).isError)
      assertEquals(s.captured, Some(Json.num(42)))
      assertEquals(s.lastError, None)
