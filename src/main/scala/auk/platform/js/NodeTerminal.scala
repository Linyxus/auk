package auk.platform.js

import scala.scalajs.js
import java.nio.charset.StandardCharsets.UTF_8
import auk.tui.render.{Terminal, Ansi}

/** A [[Terminal]] backed by `process.stdin`/`process.stdout`, following ink's
  * Bun-proven input model.
  *
  * Input uses `setRawMode(true)` (disables echo, canonical mode, and ISIG) plus
  * a `'readable'` + `read()` pull loop (not flowing `'data'`, which can
  * double-deliver in raw mode). The read loop is wrapped in try/catch and
  * re-attaches its listener on error, because in Bun an uncaught throw inside a
  * `readable` handler can permanently wedge the stream.
  *
  * The Kitty keyboard protocol is enabled only on terminals known to implement
  * it ([[supportsExtendedKeys]]); enabling it everywhere makes some terminals
  * emit key-event codepoints (press/release, all-keys-as-escapes) that show up
  * as doubled keystrokes or unrecognised Ctrl combos. Elsewhere keys arrive as
  * legacy bytes, which the [[auk.tui.app.KeyParser]] handles directly.
  *
  * Output (relative cursor moves + explicit `\r\n`) does not depend on `-opost`,
  * so plain `setRawMode` is sufficient.
  */
final class NodeTerminal private () extends Terminal:
  private val stdin = GlobalProcess.stdin
  private val encoder = new TextEncoder()
  private var closed = false
  private var mouseOn = false
  private var readable: js.Function1[js.Any, Unit] | Null = null
  private val extendedKeys = NodeTerminal.supportsExtendedKeys()

  GlobalProcess.on("exit", (_: js.Any) => close())

  def enterRawMode(): Unit =
    stdin.setEncoding("utf8")
    stdin.setRawMode(true)
    stdin.ref()
    if extendedKeys then write(Ansi.PushKeyboardEnhancement)
    // Like mouse reporting, bracketed paste is near-universal and ignored where
    // unsupported, so it is not gated by `supportsExtendedKeys`. It is a global
    // DEC mode (not per screen buffer, unlike the kitty push above), so once is
    // enough for both the inline and the alternate-screen UI.
    write(Ansi.BracketedPasteEnable)

  def exitRawMode(): Unit =
    try stdin.setRawMode(false)
    catch case _: Throwable => ()

  def onByte(sink: Int => Unit): Unit =
    def drain(): Unit =
      var chunk = stdin.read()
      while chunk != null do
        val bytes = encoder.encode(chunk.asInstanceOf[String])
        var i = 0
        while i < bytes.length do
          sink(bytes(i) & 0xff)
          i += 1
        chunk = stdin.read()
    val cb: js.Function1[js.Any, Unit] = (_: js.Any) =>
      try drain()
      catch
        case _: Throwable =>
          // Bun: an error in a 'readable' handler can wedge the stream; re-attach.
          if readable != null then
            stdin.removeListener("readable", readable.nn)
            stdin.on("readable", readable.nn)
    readable = cb
    stdin.on("readable", cb)

  def size(): (Int, Int) =
    (GlobalProcess.stdout.columns.getOrElse(80), GlobalProcess.stdout.rows.getOrElse(24))

  def hideCursor(): Unit = write(Ansi.HideCursor)
  def showCursor(): Unit = write(Ansi.ShowCursor)

  // SGR mouse reporting is near-universal and ignored where unsupported, so it is
  // NOT gated by `supportsExtendedKeys`. Disable only writes when currently on, so
  // teardown never emits a stray reset on terminals that never enabled it.
  override def enableMouse(): Unit =
    if !mouseOn then { write(Ansi.MouseEnable); mouseOn = true }

  override def disableMouse(): Unit =
    if mouseOn then { write(Ansi.MouseDisable); mouseOn = false }

  // OSC 52 carries the copy to the user's clipboard through the terminal (and any
  // multiplexer that has clipboard passthrough enabled), so no external process is
  // spawned. Base64 via the JDK encoder, which Scala.js's javalib provides.
  override def copyToClipboard(text: String): Unit =
    val base64 = java.util.Base64.getEncoder.nn.encodeToString(text.getBytes(UTF_8))
    write(Ansi.osc52Copy(base64))

  override def onResize(sink: () => Unit): Unit =
    GlobalProcess.stdout.on("resize", (_: js.Any) => sink())

  // Kitty keyboard mode is per screen buffer, so the main-screen push in
  // enterRawMode does not reach the alternate buffer; the renderer re-pushes it on
  // alt entry and pops it on exit. Gated by `extendedKeys` exactly as enterRawMode
  // is, so non-kitty terminals never see the push (which would double keystrokes).
  override def altScreenSetup: String = if extendedKeys then Ansi.PushKeyboardEnhancement else ""
  override def altScreenTeardown: String = if extendedKeys then Ansi.PopKeyboardEnhancement else ""

  def write(s: String): Unit = GlobalProcess.stdout.write(s); ()

  def close(): Unit =
    if !closed then
      closed = true
      // Mouse off first, in its own guard: the constructor's 'exit' registration
      // then still covers hard exits even if a later step throws.
      try disableMouse() catch case _: Throwable => ()
      try write(Ansi.BracketedPasteDisable) catch case _: Throwable => ()
      try if extendedKeys then write(Ansi.PopKeyboardEnhancement) catch case _: Throwable => ()
      try write(Ansi.ShowCursor) catch case _: Throwable => ()
      exitRawMode()
      if readable != null then try stdin.removeListener("readable", readable.nn) catch case _: Throwable => ()
      try stdin.unref() catch case _: Throwable => ()

object NodeTerminal:
  /** A real terminal when stdin/stdout are TTYs; `None` when piped/CI (the caller
    * falls back to a headless terminal). */
  def create(): Option[Terminal] =
    if GlobalProcess.stdin.isTTY.getOrElse(false) && GlobalProcess.stdout.isTTY.getOrElse(false)
    then Some(new NodeTerminal())
    else None

  /** Terminals known to implement the Kitty keyboard protocol correctly. On
    * others, enabling it produces doubled/unrecognised keys, so we leave keys in
    * their legacy byte form. */
  private def supportsExtendedKeys(): Boolean =
    def env(name: String): Option[String] = GlobalProcess.env.get(name).filter(_ != null)
    val termProgram = env("TERM_PROGRAM").getOrElse("")
    val term = env("TERM").getOrElse("")
    termProgram == "iTerm.app" || termProgram == "WezTerm" || termProgram == "ghostty" ||
      term.contains("kitty") || term == "xterm-ghostty" || env("KITTY_WINDOW_ID").isDefined
