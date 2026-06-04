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
    def write(str: String): Unit = { writes.append(str); () }
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
