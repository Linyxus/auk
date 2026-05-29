package auk.tui.app

import auk.tui.render.{Ansi, Terminal}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

class RuntimeSuite extends munit.FunSuite:

  /** A scripted terminal: yields queued input bytes, records writes and lifecycle. */
  private final class FakeTerminal(script: Seq[Int]) extends Terminal:
    val writes = new StringBuilder
    val rawEntered = new AtomicBoolean(false)
    val closedFlag = new AtomicBoolean(false)
    private val queue = new ConcurrentLinkedQueue[Integer]()
    script.foreach(b => queue.add(b))

    def enterRawMode(): Unit = rawEntered.set(true)
    def exitRawMode(): Unit = ()
    // Yield scripted bytes, then -1 (end of input) so the reader loop finishes.
    def readByte(): Int =
      val v = queue.poll()
      if v == null then -1 else v.intValue
    def size(): (Int, Int) = (40, 10)
    def hideCursor(): Unit = ()
    def showCursor(): Unit = ()
    def write(s: String): Unit = writes.synchronized { writes.append(s); () }
    def close(): Unit = closedFlag.set(true)

  test("runtime renders, processes a key, and quits cleanly on the quit key") {
    val seen = new AtomicInteger(0)
    val app = new App[Int, Int]:
      def init: (Int, Cmd[Int]) = (0, Cmd.none)
      def update(m: Int, s: Int): (Int, Cmd[Int]) =
        val n = s + m
        seen.set(n)
        (n, Cmd.none)
      def subscriptions(s: Int): Sub[Int] =
        Sub.onKeyPress { case Key.Char(_) => Some(1); case _ => None }
      def view(s: Int): Screen = Screen(Vector.empty, Text(s"count: $s"))

    // Script: type 'a' (handled -> +1), then Ctrl-Q (0x11) to quit.
    val term = FakeTerminal(Seq('a'.toInt, 0x11))
    val t = new Thread(() => Runtime.run(app, term, RuntimeConfig(frameMs = 5, widthPollMs = 50)))
    t.start()
    t.join(5000)

    assert(!t.isAlive, "runtime did not terminate on the quit key")
    assert(term.rawEntered.get(), "raw mode was not entered")
    assert(term.closedFlag.get(), "terminal was not closed on teardown")
    assert(term.writes.synchronized(term.writes.toString).contains("count:"), "no frame was rendered")
    assertEquals(seen.get(), 1, "the typed key was not folded before quit")
  }

  test("runtime ignores terminal read timeouts while waiting for the quit key") {
    val app = new App[Int, Int]:
      def init: (Int, Cmd[Int]) = (0, Cmd.none)
      def update(m: Int, s: Int): (Int, Cmd[Int]) = (s + m, Cmd.none)
      def subscriptions(s: Int): Sub[Int] = Sub.none
      def view(s: Int): Screen = Screen(Vector.empty, Text(s"count: $s"))

    // -2 is SttyTerminal's timed-read sentinel: no byte arrived yet, not EOF.
    val term = FakeTerminal(Seq(-2, -2, 0x11))
    val t = new Thread(() => Runtime.run(app, term, RuntimeConfig(frameMs = 5, widthPollMs = 50)))
    t.start()
    t.join(5000)

    assert(!t.isAlive, "runtime treated read timeouts as terminal EOF or got stuck before Ctrl-Q")
    assert(term.closedFlag.get(), "terminal was not closed on teardown")
  }

  test("runtime exits when an app command requests quit") {
    val app = new App[Int, Int]:
      def init: (Int, Cmd[Int]) = (0, Cmd.none)
      def update(m: Int, s: Int): (Int, Cmd[Int]) = (s, Cmd.quit)
      def subscriptions(s: Int): Sub[Int] =
        Sub.onKeyPress { case Key.Char('x') => Some(1); case _ => None }
      def view(s: Int): Screen = Screen(Vector.empty, Text("running"))

    val term = FakeTerminal(Seq('x'.toInt))
    val t = new Thread(() => Runtime.run(app, term, RuntimeConfig(frameMs = 5, widthPollMs = 50)))
    t.start()
    t.join(5000)

    assert(!t.isAlive, "runtime did not terminate on Cmd.Quit")
    assert(term.closedFlag.get(), "terminal was not closed on teardown")
  }

  test("runtime hard-resets when the committed transcript epoch changes") {
    final class BlockingSecondReadTerminal extends Terminal:
      val secondReadStarted = CountDownLatch(1)
      val releaseSecondRead = CountDownLatch(1)
      val writes = new StringBuilder
      val closedFlag = AtomicBoolean(false)
      private val reads = AtomicInteger(0)

      def enterRawMode(): Unit = ()
      def exitRawMode(): Unit = ()
      def readByte(): Int =
        reads.getAndIncrement() match
          case 0 => 'r'.toInt
          case 1 =>
            secondReadStarted.countDown()
            releaseSecondRead.await()
            0x11
          case _ => -1
      def size(): (Int, Int) = (40, 10)
      def hideCursor(): Unit = ()
      def showCursor(): Unit = ()
      def write(s: String): Unit = writes.synchronized { writes.append(s); () }
      def close(): Unit = closedFlag.set(true)

    val app = new App[Int, Int]:
      def init: (Int, Cmd[Int]) = (0, Cmd.none)
      def update(m: Int, s: Int): (Int, Cmd[Int]) = (s + m, Cmd.none)
      def subscriptions(s: Int): Sub[Int] =
        Sub.onKeyPress { case Key.Char('r') => Some(1); case _ => None }
      def view(s: Int): Screen =
        val label = if s == 0 then "old transcript" else "resumed transcript"
        Screen(Vector(Text(label)), Text("live"), committedEpoch = s.toLong)

    val term = BlockingSecondReadTerminal()
    val t = new Thread(() => Runtime.run(app, term, RuntimeConfig(frameMs = 5, widthPollMs = 50)))
    t.start()

    assert(term.secondReadStarted.await(2, TimeUnit.SECONDS), "reader did not reach the blocking read")
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2)
    var renderedReset = false
    while !renderedReset && System.nanoTime() < deadline do
      renderedReset = term.writes.synchronized:
        val out = term.writes.toString
        out.contains(Ansi.ClearScreen) && out.contains("resumed transcript")
      if !renderedReset then Thread.sleep(10)

    term.releaseSecondRead.countDown()
    t.join(5000)

    assert(renderedReset, "epoch change did not clear and reprint the committed transcript")
    assert(!t.isAlive, "runtime did not terminate on the quit key")
    assert(term.closedFlag.get(), "terminal was not closed on teardown")
  }

  test("runtime does not wait for a terminal read blocked after the quit key") {
    final class BlockingAfterQuitTerminal extends Terminal:
      val secondReadStarted = CountDownLatch(1)
      val releaseSecondRead = CountDownLatch(1)
      val closedFlag = AtomicBoolean(false)
      private val reads = AtomicInteger(0)

      def enterRawMode(): Unit = ()
      def exitRawMode(): Unit = ()
      def readByte(): Int =
        if reads.getAndIncrement() == 0 then 0x11
        else
          secondReadStarted.countDown()
          releaseSecondRead.await()
          -1
      def size(): (Int, Int) = (40, 10)
      def hideCursor(): Unit = ()
      def showCursor(): Unit = ()
      def write(s: String): Unit = ()
      def close(): Unit = closedFlag.set(true)

    val app = new App[Int, Int]:
      def init: (Int, Cmd[Int]) = (0, Cmd.none)
      def update(m: Int, s: Int): (Int, Cmd[Int]) = (s + m, Cmd.none)
      def subscriptions(s: Int): Sub[Int] = Sub.none
      def view(s: Int): Screen = Screen(Vector.empty, Text(s"count: $s"))

    val term = BlockingAfterQuitTerminal()
    val t = new Thread(() => Runtime.run(app, term, RuntimeConfig(frameMs = 5, widthPollMs = 50)))
    t.start()

    val readerBlocked = term.secondReadStarted.await(2, TimeUnit.SECONDS)
    t.join(1000)
    val exitedWhileReaderBlocked = !t.isAlive
    term.releaseSecondRead.countDown()
    t.join(2000)

    assert(readerBlocked, "terminal reader did not reach the blocking read after Ctrl-Q")
    assert(exitedWhileReaderBlocked, "runtime waited for the terminal reader after Ctrl-Q")
    assert(term.closedFlag.get(), "terminal was not closed on teardown")
  }
