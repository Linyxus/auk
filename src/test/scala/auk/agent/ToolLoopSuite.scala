package auk.agent

import auk.llm.endpoint.{ChatResponse, Content, FinishReason, Message, Role}

/** Tests for the shared tool-calling loop, focused on the `onWouldFinish` hook
  * that lets a driver keep the model going past a tool-free turn (used by the
  * workflow bridge to nudge a sub-agent that forgot to submit its result). */
class ToolLoopSuite extends munit.FunSuite:

  private def prose(text: String): ChatResponse =
    ChatResponse(Message(Role.Assistant, List(Content.Text(text))), FinishReason.Stop)
  private def callTool(id: String): ChatResponse =
    ChatResponse(Message(Role.Assistant, List(Content.ToolUse(id, "do", "{}"))), FinishReason.ToolUse)

  /** A driver replaying a fixed script of turns, recording the messages it saw and
    * delegating tool runs to a trivial echo. `wouldFinish` overrides the hook. */
  private class ScriptDriver(
      script: List[ChatResponse],
      wouldFinish: ChatResponse => List[Message] = _ => Nil
  ) extends ToolLoop.Driver:
    var turns = 0
    val seen = scala.collection.mutable.ListBuffer.empty[List[Message]]
    def turn(messages: List[Message]): Option[ChatResponse] =
      seen += messages
      val r = script.lift(turns)
      turns += 1
      r
    def runTools(toolUses: List[Content.ToolUse]): List[Content.ToolResult] =
      toolUses.map(tu => Content.ToolResult(tu.id, "ok"))
    override def onWouldFinish(response: ChatResponse): List[Message] =
      wouldFinish(response)

  test("by default a tool-free turn ends the loop immediately"):
    val d = new ScriptDriver(List(prose("done")))
    val out = ToolLoop.run(List(Message.user("hi")), d)
    assertEquals(out.rounds, 1)
    assertEquals(out.finalResponse.map(_.message.text), Some("done"))
    assertEquals(d.turns, 1)

  test("onWouldFinish injecting a message appends it and keeps the loop going"):
    var injected = 0
    val d = new ScriptDriver(
      List(prose("first"), prose("second")),
      wouldFinish = _ => if injected < 1 then { injected += 1; List(Message.user("you forgot — try again")) } else Nil
    )
    val out = ToolLoop.run(List(Message.user("hi")), d)
    // Two turns: the first would-finish was overridden, the second finished.
    assertEquals(out.rounds, 2)
    assertEquals(out.finalResponse.map(_.message.text), Some("second"))
    // The injected user message is threaded into the conversation between turns.
    assert(out.messages.exists {
      case Message(Role.User, content) => content.exists { case Content.Text(t) => t.contains("you forgot"); case _ => false }
      case _                           => false
    }, out.messages.toString)
    // The second turn saw the injected message in its prompt.
    assert(d.seen(1).exists(_.text.contains("you forgot")), d.seen.toString)

  test("onWouldFinish can nudge several times until it stops"):
    var nudges = 0
    val d = new ScriptDriver(
      List.fill(4)(prose("nope")),
      wouldFinish = _ => if nudges < 3 then { nudges += 1; List(Message.user(s"nudge $nudges")) } else Nil
    )
    val out = ToolLoop.run(List(Message.user("hi")), d)
    assertEquals(nudges, 3)
    assertEquals(out.rounds, 4) // 3 nudged turns + 1 final
    assertEquals(out.finalResponse.map(_.message.text), Some("nope"))

  test("a tool call still drives a normal round; onWouldFinish only fires on tool-free turns"):
    var finishChecks = 0
    val d = new ScriptDriver(
      List(callTool("c1"), prose("done")),
      wouldFinish = r => { finishChecks += 1; Nil }
    )
    val out = ToolLoop.run(List(Message.user("hi")), d)
    assertEquals(out.rounds, 2)
    assertEquals(out.finalResponse.map(_.message.text), Some("done"))
    // The hook is consulted once — on the tool-free second turn, not the tool turn.
    assertEquals(finishChecks, 1)
    // The tool result is threaded back as a user message.
    assert(out.messages.exists {
      case Message(Role.User, content) => content.exists { case _: Content.ToolResult => true; case _ => false }
      case _                           => false
    }, out.messages.toString)
