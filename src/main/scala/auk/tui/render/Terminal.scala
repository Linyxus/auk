package auk.tui.render

import java.io.{FileDescriptor, FileInputStream, FileOutputStream, InputStream}
import java.nio.charset.StandardCharsets.UTF_8

/** The terminal I/O seam: raw-mode byte input, size queries, and a single
  * atomic write for each rendered frame. A separate layer parses the bytes from
  * [[readByte]] into key events. */
trait Terminal:
  def enterRawMode(): Unit
  def exitRawMode(): Unit

  /** Read one byte, blocking until input arrives; -1 on EOF / closed. */
  def readByte(): Int

  /** Current `(cols, rows)`. May shell out, so callers sample it off the render
    * hot path. */
  def size(): (Int, Int)

  /** Current width in columns (convenience over [[size]]). */
  def width(): Int = size()._1

  def hideCursor(): Unit
  def showCursor(): Unit

  /** Write a fully-formed frame in one shot (and flush). */
  def write(s: String): Unit

  /** Restore the terminal (cooked mode, cursor shown). Idempotent. */
  def close(): Unit

/** A [[Terminal]] backed by stdin/stdout and the `stty` utility.
  *
  * Raw mode and size come from `stty … </dev/tty`, so they work even if stdin
  * is piped. Input and output go straight to the controlling terminal device
  * (`/dev/tty` and the stdout FD), bypassing `System.in`/`System.out` — a
  * launcher like `sbt run` replaces `System.in` with a proxy that never
  * forwards keystrokes, and `System.out`'s per-newline autoflush was the old
  * flicker source. `close` is idempotent and also runs as a JVM shutdown hook,
  * so the terminal is restored even on an exception or Ctrl-C. */
final class SttyTerminal private (originalStty: String) extends Terminal:
  // Read from /dev/tty directly — the same device we set raw mode on — so input
  // is independent of how the process's stdin happens to be wired. Fall back to
  // System.in only if the controlling terminal can't be opened.
  private val in: InputStream =
    try new FileInputStream("/dev/tty")
    catch case _: Throwable => System.in.nn
  private val out = new FileOutputStream(FileDescriptor.out)
  @volatile private var closed = false
  @volatile private var cachedSize: (Int, Int) = (80, 24)

  private val shutdown = new Thread(() => close())
  Runtime.getRuntime.nn.addShutdownHook(shutdown)

  def enterRawMode(): Unit =
    SttyTerminal.run("stty raw -echo </dev/tty")
    ()

  def exitRawMode(): Unit =
    SttyTerminal.run(s"stty $originalStty </dev/tty")
    ()

  def readByte(): Int = in.read()

  def size(): (Int, Int) =
    SttyTerminal.run("stty size </dev/tty") match
      case Some(text) =>
        text.trim.nn.split("\\s+").nn match
          case Array(rows, cols) =>
            (cols.toIntOption, rows.toIntOption) match
              case (Some(c), Some(r)) if c > 0 && r > 0 =>
                cachedSize = (c, r)
                cachedSize
              case _ => cachedSize
          case _ => cachedSize
      case None => cachedSize

  def hideCursor(): Unit = write(Ansi.HideCursor)
  def showCursor(): Unit = write(Ansi.ShowCursor)

  def write(s: String): Unit =
    out.write(s.getBytes(UTF_8))
    out.flush()

  def close(): Unit =
    if !closed then
      closed = true
      try write(Ansi.ShowCursor) catch case _: Throwable => ()
      exitRawMode()
      // Unblocks the reader fiber parked in readByte() during teardown.
      try in.close() catch case _: Throwable => ()

/** A no-op terminal for when there is no controlling TTY (piped / CI): yields no
  * input, reports a fixed 80x24, and writes to stdout. Lets the app run without
  * crashing in non-interactive contexts. */
object HeadlessTerminal extends Terminal:
  def enterRawMode(): Unit = ()
  def exitRawMode(): Unit = ()
  def readByte(): Int = -1
  def size(): (Int, Int) = (80, 24)
  def hideCursor(): Unit = ()
  def showCursor(): Unit = ()
  def write(s: String): Unit = { System.out.nn.print(s); System.out.nn.flush() }
  def close(): Unit = ()

object SttyTerminal:
  /** Create a terminal, capturing the current `stty` settings to restore later.
    * `Left` if there is no controlling TTY (piped/CI), so the caller can fall
    * back to a fixed size and no raw mode. */
  def create(): Either[String, SttyTerminal] =
    run("stty -g </dev/tty") match
      case Some(orig) if orig.nonEmpty => Right(new SttyTerminal(orig))
      case _                           => Left("no controlling tty (stty unavailable)")

  /** Run a shell command, returning its trimmed stdout, or `None` on failure. */
  private def run(cmd: String): Option[String] =
    try
      val p = new ProcessBuilder("sh", "-c", cmd).start()
      val bytes = p.getInputStream.nn.readAllBytes().nn
      p.waitFor()
      if p.exitValue() == 0 then Some(new String(bytes, UTF_8).trim.nn) else None
    catch case _: Throwable => None
