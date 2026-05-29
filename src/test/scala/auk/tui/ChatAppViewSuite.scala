package auk.tui

import auk.tui.app.{Cmd, Key, Layout, Sub}
import auk.tui.render.Width
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

  private def overlayLines(state: ChatState, width: Int = 60): Vector[String] =
    appUI.view(state).overlay.toVector.flatMap(Layout.lay(_, width)).map(_.plain)

  private def keyEvent(state: ChatState, key: Key): Option[Event] =
    keyEventFor(appUI, state, key)

  private def keyEventFor(app: ChatApp, state: ChatState, key: Key): Option[Event] =
    def collect(sub: Sub[Event]): List[Key => Option[Event]] =
      sub match
        case Sub.Batch(ss)     => ss.flatMap(collect)
        case Sub.OnKeyPress(h) => List(h)
        case _                 => Nil

    collect(app.subscriptions(state)).foldLeft(Option.empty[Event])((acc, h) => acc.orElse(h(key)))

  test("initial view: header is committed; hint, prompt, and footer are live") {
    val (committed, live) = plainLines(ChatState.initial)
    assert(committed.exists(_.contains("Auk")), committed.mkString("|"))
    assert(committed.exists(_.contains("a coding agent")))
    assert(live.exists(_.contains("Type a message and press Enter")), live.mkString("|"))
    assert(live.exists(_.contains("›")), "prompt arrow missing")
    assert(live.exists(_.contains("ctrl+c for keys")), "ctrl+c footer hint missing")
    assert(live.exists(_.contains("ctrl+q quit")), "footer missing")
  }

  test("Ctrl-C opens key bindings; c exits; other keys dismiss"):
    val open = ChatState.initial.showKeyBindings
    assertEquals(keyEvent(ChatState.initial, Key.Ctrl('C')), Some(Event.ShowKeyBindings))
    assertEquals(keyEvent(open, Key.Char('c')), Some(Event.RunCommand("c")))
    assertEquals(keyEvent(open, Key.Char('C')), Some(Event.RunCommand("c")))
    assertEquals(keyEvent(open, Key.Char('q')), Some(Event.RunCommand("q")))
    assertEquals(keyEvent(open, Key.Char('x')), Some(Event.HideKeyBindings))
    assertEquals(keyEvent(open, Key.Esc), Some(Event.HideKeyBindings))

  test("key bindings overlay renders separately from the live region"):
    assert(overlayLines(ChatState.initial).isEmpty)
    val overlay = overlayLines(ChatState.initial.showKeyBindings)
    assert(overlay.head.startsWith("┌"), overlay.mkString("|"))
    assert(overlay.last.startsWith("└"), overlay.mkString("|"))
    assert(overlay(1).contains("Key bindings"), overlay.mkString("|"))
    assert(overlay.exists(_.contains("c, q    exit")), overlay.mkString("|"))
    assert(!overlay.exists(_.contains("Enter")), overlay.mkString("|"))
    assert(!overlay.exists(_.contains("Ctrl+Q")), overlay.mkString("|"))
    assert(overlay.map(_.length).distinct.size == 1, overlay.mkString("|"))

  test("command exit returns a quit command and closes the overlay"):
    val (next, cmd) = appUI.update(Event.RunCommand("c"), ChatState.initial.showKeyBindings)
    assert(!next.keyBindingsOpen)
    cmd match
      case Cmd.Quit => ()
      case other    => fail(s"expected Cmd.Quit, got $other")

    val (nextFromAlias, aliasCmd) = appUI.update(Event.RunCommand("q"), ChatState.initial.showKeyBindings)
    assert(!nextFromAlias.keyBindingsOpen)
    aliasCmd match
      case Cmd.Quit => ()
      case other    => fail(s"expected Cmd.Quit from alias, got $other")

  test("key command registry drives dispatch and overlay rows"):
    val customApp =
      val events = UnboundedChannel[Result[StreamEvent, LLMError]]()
      val commands = UnboundedChannel[UserCommand]()
      ChatApp(
        events.asReadable,
        commands,
        keyCommands = Vector(ChatApp.Command(Vector("m", "n"), "mock command")(state => (state.copy(input = "ran"), Cmd.none)))
      )
    val state = ChatState.initial.showKeyBindings
    val screen = customApp.view(state)
    val overlay = screen.overlay.toVector.flatMap(Layout.lay(_, 60)).map(_.plain)
    assert(overlay.exists(_.contains("m, n    mock command")), overlay.mkString("|"))
    assert(!overlay.exists(_.contains("c, q    exit")), overlay.mkString("|"))

    val event = keyEventFor(customApp, state, Key.Char('m'))
    assertEquals(event, Some(Event.RunCommand("m")))
    assertEquals(keyEventFor(customApp, state, Key.Char('n')), Some(Event.RunCommand("n")))
    val (next, cmd) = customApp.update(Event.RunCommand("m"), state)
    assertEquals(next.input, "ran")
    assert(!next.keyBindingsOpen)
    assertEquals(cmd, Cmd.none)

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

  test("input prompt wraps inside the frame width") {
    val input = "abcdefghijklmnopqrstuvwxyz"
    val (_, live) = plainLines(ChatState.initial.copy(input = input, cursor = input.length), width = 12)
    val start = live.indexWhere(_.contains("›"))
    assert(start >= 0, live.mkString("|"))

    def isRule(line: String): Boolean = line.nonEmpty && line.forall(_ == '─')
    val promptRows = live.drop(start).takeWhile(line => !isRule(line))
    val content = promptRows.map(_.drop(4)).mkString

    assert(promptRows.length > 1, promptRows.mkString("|"))
    assert(promptRows.forall(line => Width.stringWidth(line) <= 12), promptRows.mkString("|"))
    assert(promptRows.head.startsWith("  › "), promptRows.head)
    assert(promptRows.tail.forall(_.startsWith("    ")), promptRows.mkString("|"))
    assert(content.startsWith(input), content)
  }

  test("input prompt renders explicit newlines") {
    val input = "alpha\nbeta"
    val (_, live) = plainLines(ChatState.initial.copy(input = input, cursor = input.length), width = 40)
    val start = live.indexWhere(_.contains("›"))
    assert(start >= 0, live.mkString("|"))

    def isRule(line: String): Boolean = line.nonEmpty && line.forall(_ == '─')
    val promptRows = live.drop(start).takeWhile(line => !isRule(line))

    assertEquals(promptRows.map(_.stripTrailing()), Vector("  › alpha", "    beta"))
  }

  test("newline event inserts a line break into the draft") {
    val (next, _) = appUI.update(Event.Newline, ChatState.initial.copy(input = "ab", cursor = 1))
    assertEquals(next.input, "a\nb")
    assertEquals(next.cursor, 2)
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
