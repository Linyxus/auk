package auk.tui.render

/** The terminal I/O seam: raw-mode byte input, size queries, and a single
  * atomic write for each rendered frame. A separate layer parses the bytes
  * delivered to the [[onByte]] sink into key events.
  *
  * Concrete implementations: `auk.platform.js.NodeTerminal` for a real TTY, and
  * [[HeadlessTerminal]] for piped/CI runs.
  */
trait Terminal:
  def enterRawMode(): Unit
  def exitRawMode(): Unit

  /** Register a sink for input bytes (0..255), invoked as input arrives. */
  def onByte(sink: Int => Unit): Unit

  /** Current `(cols, rows)`. */
  def size(): (Int, Int)

  /** Current width in columns (convenience over [[size]]). */
  def width(): Int = size()._1

  def hideCursor(): Unit
  def showCursor(): Unit

  /** Enable / disable mouse reporting. Default no-ops: only a real TTY overrides
    * them, so headless and test terminals compile unchanged. */
  def enableMouse(): Unit = ()
  def disableMouse(): Unit = ()

  /** Register a sink invoked when the terminal is resized. Default no-op; a real
    * TTY wires it to the OS resize signal for push-style repaints. */
  def onResize(sink: () => Unit): Unit = ()

  /** Write a fully-formed frame in one shot (and flush). */
  def write(s: String): Unit

  /** Restore the terminal (cooked mode, cursor shown). Idempotent. */
  def close(): Unit

/** A no-op terminal for when there is no controlling TTY (piped / CI): yields no
  * input, reports a fixed 80x24, and writes to stdout. Lets the app run without
  * crashing in non-interactive contexts. */
object HeadlessTerminal extends Terminal:
  def enterRawMode(): Unit = ()
  def exitRawMode(): Unit = ()
  def onByte(sink: Int => Unit): Unit = ()
  def size(): (Int, Int) = (80, 24)
  def hideCursor(): Unit = ()
  def showCursor(): Unit = ()
  def write(s: String): Unit = { System.out.nn.print(s); System.out.nn.flush() }
  def close(): Unit = ()
