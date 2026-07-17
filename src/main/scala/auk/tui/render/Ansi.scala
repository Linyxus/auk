package auk.tui.render

/** Raw ANSI / CSI escape sequences and small builders for the renderer.
  *
  * Pure: every member is a constant or a total function `Int => String`. The
  * renderer composes these into one frame string and writes it atomically.
  */
object Ansi:
  val ESC: String = ""
  val CSI: String = ESC + "["

  /** DEC private mode 2026 (synchronized output): a supporting terminal buffers
    * everything between Begin and End and swaps it in one repaint, so the user
    * never sees a half-drawn frame. Terminals that don't understand it ignore
    * the sequence, and our single atomic write is enough on its own. */
  val SyncBegin: String = CSI + "?2026h"
  val SyncEnd: String = CSI + "?2026l"

  val HideCursor: String = CSI + "?25l"
  val ShowCursor: String = CSI + "?25h"

  /** Switch to the alternate screen buffer, saving the cursor (`CSI ?1049h`).
    * The primary buffer and its scrollback are untouched while active. */
  val AltScreenEnter: String = CSI + "?1049h"

  /** Return to the primary screen buffer, restoring the saved cursor (`CSI ?1049l`). */
  val AltScreenExit: String = CSI + "?1049l"

  /** Kitty keyboard protocol: request all-key reporting with event types and
    * associated text, then restore the prior terminal keyboard state on exit.
    * This lets terminals report Shift+Enter distinctly while still telling us
    * what normal text keys should insert. Terminals that do not support it ignore
    * these sequences. */
  val PushKeyboardEnhancement: String = CSI + ">27u"
  val PopKeyboardEnhancement: String = CSI + "<u"

  /** Return to column 0 of the current row. Unambiguous regardless of any
    * pending-wrap state, which is why horizontal repositioning prefers it. */
  val CarriageReturn: String = "\r"

  /** Erase from the cursor to the end of the line (`CSI 0K`). */
  val EraseToEol: String = CSI + "0K"

  /** Erase from the cursor to the end of the screen (`CSI 0J`). */
  val EraseToEos: String = CSI + "0J"

  /** Erase the whole visible screen (`CSI 2J`). */
  val ClearScreen: String = CSI + "2J"

  /** Erase the scrollback buffer (`CSI 3J`, xterm extension). */
  val ClearScrollback: String = CSI + "3J"

  /** Move the cursor to the top-left (`CSI H`). */
  val CursorHome: String = CSI + "H"

  def cursorUp(n: Int): String = if n <= 0 then "" else CSI + n + "A"
  def cursorDown(n: Int): String = if n <= 0 then "" else CSI + n + "B"
  def cursorRight(n: Int): String = if n <= 0 then "" else CSI + n + "C"
  def cursorLeft(n: Int): String = if n <= 0 then "" else CSI + n + "D"

  /** Reset all SGR attributes (`CSI 0m`). */
  val Reset: String = CSI + "0m"
