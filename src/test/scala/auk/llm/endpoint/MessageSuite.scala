package auk.llm.endpoint

import auk.utils.Result
import auk.utils.Result.ok

class MessageSuite extends munit.FunSuite:

  test("Message.user builds a user text message"):
    val m = Message.user("hi")
    assertEquals(m.role, Role.User)
    assertEquals(m.text, "hi")

  test("text and thinking concatenate only their own content kinds"):
    val m = Message(
      Role.Assistant,
      List(
        Content.Thinking("think-a"),
        Content.Text("say-"),
        Content.Thinking("think-b"),
        Content.Text("hi"),
        Content.ToolUse("id1", "tool", "{}")
      )
    )
    assertEquals(m.text, "say-hi")
    assertEquals(m.thinking, "think-athink-b")

  test("Result.ok returns the value on Right"):
    val r: Result[Int, String] = Result:
      val a = (Right(1): Result[Int, String]).ok
      val b = (Right(2): Result[Int, String]).ok
      a + b
    assertEquals(r, Right(3))

  test("Result.ok short-circuits on Left"):
    val r: Result[Int, String] = Result:
      val a = (Right(1): Result[Int, String]).ok
      val b = (Left("boom"): Result[Int, String]).ok
      a + b
    assertEquals(r, Left("boom"))
