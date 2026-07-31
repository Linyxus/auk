package auk.tui.render

/** Raw ANSI / CSI escape sequences and small builders for the renderer.
  *
  * Pure: every member is a constant or a total function `Int => String`. The
  * renderer composes these into one frame string and writes it atomically.
  */
object Ansi:
  val ESC: String = ""
  val CSI: String = ESC + "["

  /** Operating System Command introducer (`ESC ]`). Precedes OSC strings such as
    * the clipboard-write sequence built by [[osc52Copy]]. */
  val OSC: String = ESC + "]"

  /** The BEL control byte, the widest-compatible OSC string terminator
    * (accepted where the two-byte ST `ESC \` is not). */
  val Bell: String = "\u0007"

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

  /** Mouse reporting: button events (`?1000`), button-held motion (`?1002`, so a
    * drag streams position reports while a button is down), and SGR-1006 extended
    * coordinates (`?1006`, so column/row aren't capped at 223 and press/release
    * are distinct). Disable reverses the enable order exactly. Terminals without
    * SGR 1006 ignore these. */
  val MouseEnable: String = CSI + "?1000h" + CSI + "?1002h" + CSI + "?1006h"
  val MouseDisable: String = CSI + "?1006l" + CSI + "?1002l" + CSI + "?1000l"

  /** Bracketed paste (`?2004`): a supporting terminal wraps pasted text in
    * `CSI 200~` … `CSI 201~` instead of replaying it as keystrokes, so the key
    * parser can deliver the paste as one event and a pasted newline inserts
    * rather than submits. Near-universal and ignored where unsupported, like
    * mouse reporting; where it is missing a paste falls back to plain keys. */
  val BracketedPasteEnable: String = CSI + "?2004h"
  val BracketedPasteDisable: String = CSI + "?2004l"

  /** An OSC 52 clipboard-write sequence for the `c` (clipboard) selection, given
    * the payload already Base64-encoded. Terminated with BEL for the widest
    * terminal/multiplexer compatibility. Terminals without OSC 52 ignore it. */
  def osc52Copy(base64: String): String = OSC + "52;c;" + base64 + Bell

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

  /** DECAWM autowrap off/on (`CSI ?7l` / `?7h`). Off inside the alternate
    * screen: every fullscreen row is explicitly addressed, so wrapping can only
    * ever be a terminal disagreeing with [[Width]] about a glyph — clipping at
    * the margin keeps such a row's damage inside the row instead of pushing
    * every following row down. Restored on every alt-screen exit because the
    * mode is global, not per-buffer. */
  val WrapOff: String = CSI + "?7l"
  val WrapOn: String = CSI + "?7h"

  /** Reset all SGR attributes (`CSI 0m`). */
  val Reset: String = CSI + "0m"
