package auk.tui.render

import scala.collection.mutable
import gears.async.UnboundedChannel
import auk.agent.{AgentEvent, Inbox, TeamMemberView, UserCommand}
import auk.tui.{ChatApp, ChatState, DisplayMode, Entry, Block, Event, Overlay}
import auk.tui.app.{Layout, Viewport}
import auk.workflow.TranscriptEvent

/** A tiny glyph-grid VT emulator: enough of the renderer's own vocabulary
  * (relative moves, CR, CRLF scroll, erase, clear, alt-screen switch; SGR and
  * sync markers are skipped) to replay emitted frames and compare the resulting
  * grid against a freshly painted frame. All glyphs count one cell, matching
  * the renderer's own width model, so any mismatch is a diff-logic bug. */
final class GridEmu(cols: Int, rows: Int):
  private def blank() = Array.fill(rows)(Array.fill(cols)(' '))
  private var main = blank()
  private var alt = blank()
  private var onAlt = false
  private var row = 0
  private var col = 0
  private var savedRow = 0
  private var savedCol = 0
  private def grid = if onAlt then alt else main

  def screen: Vector[String] = grid.map(_.mkString).toVector
  def isAlt: Boolean = onAlt

  def feed(s: String): Unit =
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      if c == 27 && i + 1 < s.length && s.charAt(i + 1) == '[' then
        var j = i + 2
        while j < s.length && !s.charAt(j).isLetter do j += 1
        val params = s.substring(i + 2, j)
        val fin = if j < s.length then s.charAt(j) else ' '
        applyCsi(params, fin)
        i = j + 1
      else if c == '\r' then { col = 0; i += 1 }
      else if c == '\n' then
        if row == rows - 1 then { grid.foreach(_ => ()); scrollUp() } else row += 1
        i += 1
      else if c == 27 then i += 1 // bare ESC (unused sequences)
      else
        if row < rows && col < cols then grid(row)(col) = c
        col = math.min(cols - 1, col + 1) // note: no pending-wrap; renderer never relies on wrap
        i += 1

  private def scrollUp(): Unit =
    val g = grid
    var r = 0
    while r < rows - 1 do { g(r) = g(r + 1); r += 1 }
    g(rows - 1) = Array.fill(cols)(' ')

  private def applyCsi(params: String, fin: Char): Unit =
    def n = if params.isEmpty then 1 else params.takeWhile(_.isDigit).toIntOption.getOrElse(1)
    fin match
      case 'A' => row = math.max(0, row - n)
      case 'B' => row = math.min(rows - 1, row + n)
      case 'C' => col = math.min(cols - 1, col + n)
      case 'H' => row = 0; col = 0
      case 'J' =>
        if params == "2" then { val g = grid; for r <- 0 until rows do g(r) = Array.fill(cols)(' ') }
        else if params == "3" then () // clear scrollback: not modelled
        else // 0J erase to end of screen
          for cc <- col until cols do grid(row)(cc) = ' '
          for r <- row + 1 until rows do grid(r) = Array.fill(cols)(' ')
      case 'h' if params == "?1049" => onAlt = true; savedRow = row; savedCol = col; alt = blank(); row = 0; col = 0
      case 'l' if params == "?1049" => onAlt = false; row = savedRow; col = savedCol
      case 'm' => () // SGR: styles not modelled
      case _   => ()

class EpisodeReproSuite extends munit.FunSuite:

  private def fullscreenApp: ChatApp =
    ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox](),
      mode = DisplayMode.Fullscreen
    )

  test("fullscreen: chat → team transcript → back leaves the screen exactly repainted"):
    val W = 60
    val R = 20
    val app = fullscreenApp
    val out = new StringBuilder
    val renderer = Renderer(s => { out.append(s); () })
    val emu = GridEmu(W, R)

    var st = auk.tui.ChatState.initial.copy(
      history = Vector(Entry.User("hello"), Entry.Assistant(Vector(Block.shownAnswer("world")))),
      team = Vector(TeamMemberView("poet", "writes poems", working = true, 10, 12345)),
      modelName = "glm-5.2"
    )
    st = st.applyActivity(TranscriptEvent.ToolCalled("team", "poet", "c1", "grep", "NodeProcess"))

    def paint(s: ChatState): Unit =
      val el = app.view(s, Viewport(W, R)).fullscreen.getOrElse(fail("expected fullscreen"))
      out.setLength(0)
      renderer.renderFullscreen(W, R, Layout.lay(el, W))
      emu.feed(out.toString)

    def expected(s: ChatState): Vector[String] =
      val el = app.view(s, Viewport(W, R)).fullscreen.getOrElse(fail("expected fullscreen"))
      val lines = Layout.lay(el, W).map(_.plain)
      lines.take(R).map(l => (l + " " * W).take(W)).padTo(R, " " * W)

    // Frame 1: the chat.
    paint(st)
    assertEquals(emu.screen, expected(st), "frame 1 (chat) mismatch")

    // Frame 2: focus the panel, open the member transcript, clock advanced.
    st = st.copy(teamSel = Some(0), clockMs = 150)
    val opened = st.copy(overlay = Overlay.TeamTranscript("poet", 0))
    paint(opened)
    assertEquals(emu.screen, expected(opened), "frame 2 (transcript) mismatch")

    // Frame 3: Esc back to the chat, clock advanced again.
    val back = st.copy(clockMs = 300)
    paint(back)
    assertEquals(emu.screen, expected(back), "frame 3 (back to chat) mismatch")

  test("inline: chat → team transcript episode → back repaints the live region exactly"):
    val W = 60
    val R = 24
    val inlineApp = ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox]()
    )

    var st = auk.tui.ChatState.initial.copy(
      team = Vector(TeamMemberView("poet", "writes poems", working = true, 10, 12345)),
      modelName = "glm-5.2"
    )
    st = st.applyActivity(TranscriptEvent.ToolCalled("team", "poet", "c1", "grep", "NodeProcess"))

    /** Replay `frames` (inline states or transcript-overlay states) through one
      * stateful renderer into an emulator, mirroring Runtime.render's dispatch. */
    def replay(frames: Vector[ChatState]): Vector[String] =
      val out = new StringBuilder
      val renderer = Renderer(s => { out.append(s); () })
      val emu = GridEmu(W, R)
      var flushed = 0
      frames.foreach { s =>
        val screen = inlineApp.view(s, Viewport(W, R))
        out.setLength(0)
        screen.fullscreen match
          case Some(el) => renderer.renderFullscreen(W, R, Layout.lay(el, W))
          case None =>
            val committed = screen.committed
            val fresh = if committed.length > flushed then committed.drop(flushed) else Vector.empty
            flushed = committed.length
            renderer.render(W, fresh.flatMap(Layout.lay(_, W)), Layout.lay(screen.live, W))
        emu.feed(out.toString)
      }
      assert(!emu.isAlt, "must have returned to the primary buffer")
      emu.screen

    val opened = st.copy(teamSel = Some(0), clockMs = 150, overlay = Overlay.TeamTranscript("poet", 0))
    val back = st.copy(teamSel = Some(0), clockMs = 300)

    // The episode path: chat → transcript (alt screen) → back to the chat.
    val episode = replay(Vector(st, opened, back))
    // The reference: the same final state painted without the episode.
    val direct = replay(Vector(st, back))
    assertEquals(episode, direct, "the post-episode screen must equal a direct paint")

  test("fullscreen sweep: team-transcript roundtrips leave the screen equal to a direct paint across states"):
    val W = 100
    val R = 28
    def member(working: Boolean) = TeamMemberView("poet", "writes poems", working, 10, 2800)
    def entries(n: Int): Vector[Entry] =
      (1 to n).flatMap(i => Vector(Entry.User(s"question $i"), Entry.Assistant(Vector(Block.shownAnswer(s"answer $i\n\nmore prose for $i"))))).toVector

    var poemState = auk.tui.ChatState.initial.copy(team = Vector(member(true)), modelName = "glm-5.2")
    for i <- 1 to 60 do
      poemState = poemState.applyActivity(TranscriptEvent.Said("team", "poet", s"poem line $i\n"))

    for
      rounds <- List(0, 2, 20)
      scroll <- List(Option.empty[Int], Some(0), Some(7))
      focusedSel <- List(Option.empty[Int], Some(0))
      growDuring <- List(false, true)
    do
      val app = fullscreenApp
      val out = new StringBuilder
      val renderer = Renderer(s => { out.append(s); () })
      val emu = GridEmu(W, R)
      def paint(s: ChatState): Unit =
        val el = app.view(s, Viewport(W, R)).fullscreen.getOrElse(fail("expected fullscreen"))
        out.setLength(0)
        renderer.renderFullscreen(W, R, Layout.lay(el, W))
        emu.feed(out.toString)
      def expectedOf(s: ChatState): Vector[String] =
        val el = app.view(s, Viewport(W, R)).fullscreen.getOrElse(fail("expected fullscreen"))
        Layout.lay(el, W).map(_.plain).take(R).map(l => (l + " " * W).take(W)).padTo(R, " " * W)

      val base = poemState.copy(history = entries(rounds), chatScroll = scroll, teamSel = focusedSel)
      paint(base)
      val opened = base.copy(overlay = Overlay.TeamTranscript("poet", 0), clockMs = 100)
      paint(opened)
      val evolved =
        if !growDuring then opened
        else
          opened
            .applyActivity(TranscriptEvent.Said("team", "poet", "one more line\n"))
            .copy(
              history = entries(rounds) ++ entries(1),
              team = Vector(member(false)),
              notices = Vector("poet is idle"),
              clockMs = 400
            )
      if growDuring then paint(evolved)
      val back = evolved.copy(overlay = Overlay.None, clockMs = 600)
      paint(back)
      assertEquals(
        emu.screen,
        expectedOf(back),
        s"mismatch at rounds=$rounds scroll=$scroll sel=$focusedSel grow=$growDuring"
      )

  test("inline: entries committed and notices grown DURING the episode flush correctly on return"):
    val W = 60
    val R = 24
    val inlineApp = ChatApp(
      UnboundedChannel[AgentEvent]().asReadable,
      UnboundedChannel[UserCommand](),
      UnboundedChannel[Unit](),
      UnboundedChannel[Inbox]()
    )

    var st = auk.tui.ChatState.initial.copy(
      team = Vector(TeamMemberView("poet", "writes poems", working = true, 10, 12345)),
      modelName = "glm-5.2"
    )
    st = st.applyActivity(TranscriptEvent.ToolCalled("team", "poet", "c1", "grep", "NodeProcess"))

    def replay(frames: Vector[ChatState]): Vector[String] =
      val out = new StringBuilder
      val renderer = Renderer(s => { out.append(s); () })
      val emu = GridEmu(W, R)
      var flushed = 0
      frames.foreach { s =>
        val screen = inlineApp.view(s, Viewport(W, R))
        out.setLength(0)
        screen.fullscreen match
          case Some(el) => renderer.renderFullscreen(W, R, Layout.lay(el, W))
          case None =>
            val committed = screen.committed
            val fresh = if committed.length > flushed then committed.drop(flushed) else Vector.empty
            flushed = committed.length
            renderer.render(W, fresh.flatMap(Layout.lay(_, W)), Layout.lay(screen.live, W))
        emu.feed(out.toString)
      }
      assert(!emu.isAlt, "must have returned to the primary buffer")
      emu.screen

    // While the transcript is open the member finishes: activity grows, the
    // lead runs a turn that commits entries, a notice appears, the roster goes
    // idle — all flushed in the same frame as the alt-screen exit.
    val opened1 = st.copy(overlay = Overlay.TeamTranscript("poet", 0), teamSel = Some(0), clockMs = 100)
    val opened2 = opened1.applyActivity(TranscriptEvent.Said("team", "poet", "the poem is done\nline two"))
    val grown = opened2.copy(
      history = Vector(Entry.User("write a poem"), Entry.Assistant(Vector(Block.shownAnswer("asked the poet")))),
      notices = Vector("poet is idle"),
      team = Vector(TeamMemberView("poet", "writes poems", working = false, 20, 20000)),
      clockMs = 400
    )
    val back = grown.copy(overlay = Overlay.None, clockMs = 500)

    val episode = replay(Vector(st, opened1, opened2, grown, back))
    val direct = replay(Vector(st, back))
    assertEquals(episode, direct, "post-episode screen with mid-episode commits must equal a direct paint")
