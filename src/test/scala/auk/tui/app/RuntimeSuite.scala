package auk.tui.app

import auk.tui.render.Terminal
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

    def enterRawMode(): Unit = rawEntered = true
    def exitRawMode(): Unit = ()
    def onByte(s: Int => Unit): Unit =
      sink = Some(s)
      while buffer.nonEmpty do s(buffer.dequeue())
    def size(): (Int, Int) = (40, 10)
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
      def view(s: Int): Screen = Screen(Vector.empty, Text(s"count: $s"))

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
      def view(s: Int): Screen = Screen(Vector.empty, Text("running"))

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
      def view(s: Int): Screen = Screen(Vector.empty, Text(s"count: $s"))

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
