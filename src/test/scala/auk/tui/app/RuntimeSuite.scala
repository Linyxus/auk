package auk.tui.app

import auk.tui.render.{Ansi, Terminal}
import gears.async.Async
import gears.async.default.given
import scala.collection.mutable

/** Runtime tests for the Scala.js (push-based, single-threaded) model.
  *
  * The JVM suite spawned threads and used a blocking `readByte`; the JS runtime
  * is push-based (`Terminal.onByte`) and cooperative, so input is injected via a
  * [[FakeTerminal]] that buffers bytes until the runtime registers its sink, and
  * tests run inside `Async.fromSync` (returning a `Future` munit awaits).
  *
  * Three former tests covered JVM blocking-read semantics (a timed-read `-2`
  * sentinel, a reader thread blocked across the quit key, mid-read epoch resets)
  * that have no analogue in the single-threaded push model and were dropped.
  */
class RuntimeSuite extends munit.FunSuite:

  /** A scripted terminal: queue input bytes (flushed once the runtime registers
    * its sink) and record writes/lifecycle. */
  private final class FakeTerminal extends Terminal:
    val writes = new StringBuilder
    var rawEntered = false
    var closedFlag = false
    /** Invoked with each frame string as it is written, so a test can react to a
      * paint (e.g. inject the quit byte once the expected frame appears). */
    var onWrite: String => Unit = _ => ()
    private var sink: Option[Int => Unit] = None
    private val buffer = mutable.Queue[Int]()

    /** Inject an input byte (buffered until the sink is registered). */
    def push(b: Int): Unit = sink match
      case Some(s) => s(b)
      case None    => buffer.enqueue(b)

    /** The reported terminal size; a test can mutate it to simulate a resize the
      * runtime's poller then picks up (with no key/msg input). */
    var currentSize: (Int, Int) = (40, 10)

    def enterRawMode(): Unit = rawEntered = true
    def exitRawMode(): Unit = ()
    def onByte(s: Int => Unit): Unit =
      sink = Some(s)
      while buffer.nonEmpty do s(buffer.dequeue())
    def size(): (Int, Int) = currentSize
    def hideCursor(): Unit = ()
    def showCursor(): Unit = ()
    def write(str: String): Unit = { writes.append(str); onWrite(str); () }
    def close(): Unit = closedFlag = true

  test("runtime renders, processes a key, and quits cleanly on the quit key") {
    var seen = 0
    val app = new App[Int, Int]:
      def init: (Int, Cmd[Int]) = (0, Cmd.none)
      def update(m: Int, s: Int): (Int, Cmd[Int]) =
        val n = s + m
        seen = n
        (n, Cmd.none)
      def subscriptions(s: Int): Sub[Int] =
        Sub.onKeyPress { case Key.Char(_) => Some(1); case _ => None }
      def view(s: Int, viewport: Viewport): Screen = Screen(Vector.empty, Text(s"count: $s"))

    val term = FakeTerminal()
    // Type 'a' (handled -> +1), then Ctrl-Q (0x11) to quit.
    term.push('a'.toInt)
    term.push(0x11)

    Async.fromSync:
      Runtime.run(app, term, RuntimeConfig(frameMs = 5, widthPollMs = 50))
      assert(term.rawEntered, "raw mode was not entered")
      assert(term.closedFlag, "terminal was not closed on teardown")
      assert(term.writes.toString.contains("count:"), "no frame was rendered")
      assertEquals(seen, 1, "the typed key was not folded before quit")
  }

  test("runtime exits when an app command requests quit") {
    val app = new App[Int, Int]:
      def init: (Int, Cmd[Int]) = (0, Cmd.none)
      def update(m: Int, s: Int): (Int, Cmd[Int]) = (s, Cmd.quit)
      def subscriptions(s: Int): Sub[Int] =
        Sub.onKeyPress { case Key.Char('x') => Some(1); case _ => None }
      def view(s: Int, viewport: Viewport): Screen = Screen(Vector.empty, Text("running"))

    val term = FakeTerminal()
    term.push('x'.toInt)

    Async.fromSync:
      Runtime.run(app, term, RuntimeConfig(frameMs = 5, widthPollMs = 50))
      assert(term.closedFlag, "terminal was not closed on teardown")
  }

  /** A counter app: each key folds `+1`; the view shows `count: N`. `record` is
    * called with each new count so a test can observe what was folded. */
  private def counterApp(record: Int => Unit): App[Int, Int] =
    new App[Int, Int]:
      def init: (Int, Cmd[Int]) = (0, Cmd.none)
      def update(m: Int, s: Int): (Int, Cmd[Int]) = { val n = s + m; record(n); (n, Cmd.none) }
      def subscriptions(s: Int): Sub[Int] = Sub.onKeyPress { case Key.Char(_) => Some(1); case _ => None }
      def view(s: Int, viewport: Viewport): Screen = Screen(Vector.empty, Text(s"count: $s"))

  test("a keystroke repaints immediately without waiting for a frame tick") {
    var seen = 0
    val app = counterApp(n => seen = n)
    val term = FakeTerminal()
    // The ticker is effectively disabled (huge frameMs), so the only paint after
    // the startup paint can be the keystroke painting on its own. `write` is one
    // call per frame; quit once that second frame lands. (The renderer cell-diffs,
    // so we detect the paint itself, not its text.)
    var frames = 0
    term.onWrite = _ =>
      frames += 1
      if frames >= 2 then term.push(0x11)
    term.push('a'.toInt)

    Async.fromSync:
      Runtime.run(app, term, RuntimeConfig(frameMs = 10_000_000, widthPollMs = 10_000_000))
      assertEquals(seen, 1, "the key was not folded")
      assertEquals(frames, 2, "the keystroke did not repaint immediately (no second frame)")
  }

  test("a burst of buffered keys coalesces into a single repaint") {
    var seen = 0
    val app = counterApp(n => seen = n)
    val term = FakeTerminal()
    var frames = 0
    term.onWrite = _ =>
      frames += 1
      if frames >= 2 then term.push(0x11)
    // Three chars buffered together → flushed to the keys channel in one shot at
    // sink registration, so all three are queued before the loop's first select.
    term.push('a'.toInt); term.push('b'.toInt); term.push('c'.toInt)

    Async.fromSync:
      Runtime.run(app, term, RuntimeConfig(frameMs = 10_000_000, widthPollMs = 10_000_000))
      // All three fold in one keyCase (the drain), then one paint — not one per
      // char. So: startup frame + one coalesced frame = 2 (the drain must not
      // produce a frame per character).
      assertEquals(seen, 3, "all three keys should fold before the paint")
      assertEquals(frames, 2, s"expected startup + one coalesced frame, got $frames")
  }

  /** An app that toggles a fullscreen takeover on `f` (also committing a line) and
    * leaves it on `g`. State is `(fullscreen?, committedCount)`. */
  private def fullscreenApp: App[(Boolean, Int), Int] =
    new App[(Boolean, Int), Int]:
      def init: ((Boolean, Int), Cmd[Int]) = ((false, 0), Cmd.none)
      def update(m: Int, s: (Boolean, Int)): ((Boolean, Int), Cmd[Int]) = m match
        case 1 => ((true, s._2 + 1), Cmd.none)  // enter fullscreen and commit a line
        case 2 => ((false, s._2), Cmd.none)     // leave fullscreen
        case _ => (s, Cmd.none)
      def subscriptions(s: (Boolean, Int)): Sub[Int] =
        Sub.onKeyPress { case Key.Char('f') => Some(1); case Key.Char('g') => Some(2); case _ => None }
      def view(s: (Boolean, Int), viewport: Viewport): Screen =
        val committed = (0 until s._2).toVector.map(i => Text(s"commit-$i"))
        Screen(committed, Text("live"), fullscreen = if s._1 then Some(Text("FULLSCREEN")) else None)

  test("fullscreen: enter/exit sequences appear in order; committed lines flush only after the exit") {
    val term = FakeTerminal()
    var stage = 0
    term.onWrite = s =>
      if stage == 0 && s.contains(Ansi.AltScreenEnter) then { stage = 1; term.push('g'.toInt) }
      else if stage == 1 && s.contains(Ansi.AltScreenExit) then { stage = 2; term.push(0x11) }
    term.push('f'.toInt)

    Async.fromSync:
      Runtime.run(fullscreenApp, term, RuntimeConfig(frameMs = 5, widthPollMs = 10_000_000))
      val log = term.writes.toString
      val enterAt = log.indexOf(Ansi.AltScreenEnter)
      val exitAt = log.indexOf(Ansi.AltScreenExit)
      assert(enterAt >= 0, "never entered the alt buffer")
      assert(exitAt > enterAt, s"exit ($exitAt) should follow enter ($enterAt)")
      // The line committed while fullscreen must not reach the primary stream until
      // the inline return frame (which is where the exit sequence is emitted).
      assert(log.indexOf("commit-0") > exitAt, "committed line leaked before the exit frame")
  }

  test("fullscreen: quitting while fullscreen exits the alt buffer during teardown") {
    val term = FakeTerminal()
    var entered = false
    term.onWrite = s => if !entered && s.contains(Ansi.AltScreenEnter) then { entered = true; term.push(0x11) }
    term.push('f'.toInt)

    Async.fromSync:
      Runtime.run(fullscreenApp, term, RuntimeConfig(frameMs = 5, widthPollMs = 10_000_000))
      val log = term.writes.toString
      assert(log.contains(Ansi.AltScreenEnter), "never entered the alt buffer")
      assert(log.lastIndexOf(Ansi.AltScreenExit) > log.indexOf(Ansi.AltScreenEnter), "teardown must exit the alt buffer")
      assert(term.closedFlag, "terminal was not closed on teardown")
  }

  test("fullscreen: a resize with no key or msg events still repaints the alt buffer at the new dims") {
    // Regression: the fullscreen empty-list state has no tick timer, so nothing
    // sets `dirty`; a resize there must still drive a repaint on the poller alone.
    val term = FakeTerminal()
    var entered = false
    var resizeRepainted = false
    term.onWrite = s =>
      if !entered && s.contains(Ansi.AltScreenEnter) then
        entered = true
        term.currentSize = (30, 8) // resize while fullscreen; no key/msg follows
      else if entered && !resizeRepainted && s.contains(Ansi.ClearScreen) && !s.contains(Ansi.AltScreenEnter) then
        // A dim-change alt repaint (clear without re-entering) driven purely by the poller.
        resizeRepainted = true
        term.push(0x11)
    term.push('f'.toInt)

    Async.fromSync:
      Runtime.run(fullscreenApp, term, RuntimeConfig(frameMs = 5, widthPollMs = 5))
      assert(entered, "never entered fullscreen")
      assert(resizeRepainted, "a resize with no input did not repaint the alt buffer at the new dims")
  }

  test("fullscreen: a resize during the episode hard-resets the transcript on the inline return") {
    val term = FakeTerminal()
    var stage = 0
    term.onWrite = s =>
      stage match
        case 0 if s.contains(Ansi.AltScreenEnter) => stage = 1; term.currentSize = (30, 8) // resize while fullscreen
        case 1 if s.contains(Ansi.ClearScreen) && !s.contains(Ansi.AltScreenEnter) => stage = 2; term.push('g'.toInt) // leave fullscreen
        case 2 if s.contains(Ansi.AltScreenExit) => stage = 3; term.push(0x11) // inline return happened; quit
        case _ => ()
    term.push('f'.toInt)

    Async.fromSync:
      Runtime.run(fullscreenApp, term, RuntimeConfig(frameMs = 5, widthPollMs = 5))
      assertEquals(stage, 3, "expected enter → alt resize repaint → inline exit")
      val log = term.writes.toString
      val exitAt = log.indexOf(Ansi.AltScreenExit)
      // The return frame (from the exit onward) hard-resets: the deferred resize
      // reflows the transcript (clear screen + scrollback), not an incremental diff.
      assert(log.substring(exitAt).contains(Ansi.ClearScrollback), "the inline return after a fullscreen resize must hard-reset")
  }
