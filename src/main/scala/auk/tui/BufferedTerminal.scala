package auk.tui

import layoutz.Terminal

/** A [[layoutz.Terminal]] decorator that emits each rendered frame atomically.
  *
  * layoutz repaints the whole frame on any change, writing one line at a time
  * (`write(line + "…\n")`) and only calling `flush()` at the end. The default
  * terminal prints to `System.out`, whose `autoFlush` fires on every '\n' — so a
  * full repaint is flushed line-by-line and the terminal paints it
  * progressively, which reads as a flicker on every keystroke.
  *
  * This wrapper buffers all writes and emits them in a single `print` on
  * `flush()`, bracketed by the synchronized-update sequence (DEC private mode
  * 2026) so supporting terminals swap the frame in one go. Terminals that don't
  * understand 2026 simply ignore it, and the single atomic write is enough on
  * its own. Everything other than the frame-writing methods delegates to the
  * real terminal. */
final class BufferedTerminal(underlying: Terminal) extends Terminal:
  private val buf = new StringBuilder
  private val BeginSync = "[?2026h"
  private val EndSync = "[?2026l"

  def write(text: String): Unit = buf.append(text)

  def flush(): Unit =
    if buf.nonEmpty then
      val frame = buf.toString
      buf.setLength(0)
      System.out.print(BeginSync + frame + EndSync)
      System.out.flush()

  def writeLine(text: String): Unit =
    buf.append(text).append('\n')
    flush()

  def enterRawMode(): Unit = underlying.enterRawMode()
  def exitRawMode(): Unit = underlying.exitRawMode()
  def clearScreen(): Unit = underlying.clearScreen()
  def clearScrollback(): Unit = underlying.clearScrollback()
  def hideCursor(): Unit = underlying.hideCursor()
  def showCursor(): Unit = underlying.showCursor()
  def readInput(): Int = underlying.readInput()
  def readInputNonBlocking(): Option[Int] = underlying.readInputNonBlocking()
  def close(): Unit = underlying.close()
  def terminalWidth(): Int = underlying.terminalWidth()
