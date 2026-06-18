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

  // -- coalesce: the wire-boundary merge of adjacent same-role messages, so
  // real-time steering never sends two consecutive same-role turns ------------

  import Content.{Text, ToolUse, ToolResult}

  test("coalesce merges adjacent same-role messages by concatenating content in order"):
    val in = List(Message(Role.User, List(Text("a"))), Message(Role.User, List(Text("b"))))
    assertEquals(Message.coalesce(in), List(Message(Role.User, List(Text("a"), Text("b")))))

  test("coalesce collapses a run of three same-role messages into one"):
    val in = List(
      Message(Role.User, List(Text("a"))),
      Message(Role.User, List(Text("b"))),
      Message(Role.User, List(Text("c")))
    )
    assertEquals(Message.coalesce(in), List(Message(Role.User, List(Text("a"), Text("b"), Text("c")))))

  test("coalesce leaves an already-alternating history unchanged"):
    val in = List(Message.user("hi"), Message.assistant("yo"), Message.user("bye"))
    assertEquals(Message.coalesce(in), in)

  test("coalesce merges a tool-results user message with a following steer into one user turn"):
    val in = List(
      Message(Role.Assistant, List(ToolUse("t1", "read", "{}"))),
      Message(Role.User, List(ToolResult("t1", "contents"))),
      Message(Role.User, List(Text("also do X")))
    )
    val out = Message.coalesce(in)
    assertEquals(out.size, 2)
    assertEquals(out(1), Message(Role.User, List(ToolResult("t1", "contents"), Text("also do X"))))
    assert(out.sliding(2).forall { case Seq(a, b) => a.role != b.role; case _ => true }, out.toString)

  test("coalesce passes empty and singleton inputs through, and is idempotent"):
    assertEquals(Message.coalesce(Nil), Nil)
    val one = List(Message.user("solo"))
    assertEquals(Message.coalesce(one), one)
    val in = List(Message.user("q"), Message(Role.User, List(Text("steer"))), Message.assistant("a"))
    val once = Message.coalesce(in)
    assertEquals(Message.coalesce(once), once)
