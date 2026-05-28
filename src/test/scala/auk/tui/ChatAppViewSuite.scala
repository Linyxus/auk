package auk.tui

import auk.tui.app.Layout
import gears.async.UnboundedChannel
import auk.agent.UserCommand
import auk.llm.endpoint.{StreamEvent, LLMError}
import auk.utils.Result

class ChatAppViewSuite extends munit.FunSuite:

  private def appUI: ChatApp =
    val events = UnboundedChannel[Result[StreamEvent, LLMError]]()
    val commands = UnboundedChannel[UserCommand]()
    ChatApp(events.asReadable, commands)

  private def plainLines(state: ChatState, width: Int = 60): (Vector[String], Vector[String]) =
    val screen = appUI.view(state)
    val committed = screen.committed.flatMap(Layout.lay(_, width)).map(_.plain)
    val live = Layout.lay(screen.live, width).map(_.plain)
    (committed, live)

  test("initial view: header is committed; hint, prompt, and footer are live") {
    val (committed, live) = plainLines(ChatState.initial)
    assert(committed.exists(_.contains("Auk")), committed.mkString("|"))
    assert(committed.exists(_.contains("a coding agent")))
    assert(live.exists(_.contains("Type a message and press Enter")), live.mkString("|"))
    assert(live.exists(_.contains("›")), "prompt arrow missing")
    assert(live.exists(_.contains("ctrl+q to quit")), "footer missing")
  }

  test("a submitted message commits a You entry; the hint disappears") {
    val state = ChatState.initial.submitted("hello there").copy(phase = Phase.Waiting)
    val (committed, live) = plainLines(state)
    assert(committed.exists(_.contains("You")), committed.mkString("|"))
    assert(committed.exists(_.contains("hello there")))
    assert(!live.exists(_.contains("Type a message")), "hint should be gone once history is non-empty")
    // Waiting phase shows the spinner label in the live region.
    assert(live.exists(_.contains("auk is thinking")), live.mkString("|"))
  }

  test("divider rule expands to the layout width") {
    val (_, live) = plainLines(ChatState.initial, width = 30)
    assert(live.exists(l => l.nonEmpty && l.forall(_ == '─') && l.length == 30), live.mkString("|"))
  }

  test("a finalized assistant turn is committed, not live") {
    val streamed = ChatState.initial
      .submitted("q")
      .copy(phase = Phase.Waiting)
      .appendReply("the answer", now = 1000)
      .completeReply("the answer", now = 2000)
    val (committed, live) = plainLines(streamed)
    assert(committed.exists(_.contains("the answer")), committed.mkString("|"))
    // once committed, the live region no longer carries the answer
    assert(!live.exists(_.contains("the answer")), live.mkString("|"))
  }
