package auk.tui.app

/** A decoded key press. Covers the keys the app binds; anything else parses to
  * [[Key.Unknown]] and is ignored. */
enum Key:
  case Char(c: scala.Char)
  case Enter
  case Newline
  case Backspace
  case Tab
  case Delete
  case Up, Down, Left, Right
  case Home, End
  case PageUp, PageDown
  /** An emacs-style control chord; `c` is the uppercase letter (e.g. `Ctrl('Q')`). */
  case Ctrl(c: scala.Char)
  case Esc
  case Unknown
  /** Mouse wheel notches. `col`/`row` are 1-based terminal cells (SGR 1006). */
  case WheelUp(col: Int, row: Int)
  case WheelDown(col: Int, row: Int)
  /** Mouse button events. `button` is the low-two-bit SGR button code (0=left,
    * 1=middle, 2=right); `col`/`row` are 1-based cells. Parsed and available;
    * no binding consumes them yet. */
  case MousePress(button: Int, col: Int, row: Int)
  case MouseRelease(button: Int, col: Int, row: Int)
  /** Mouse motion while `button` is held down (a drag). `button` is the low-two-
    * bit SGR code (0=left, 1=middle, 2=right); `col`/`row` are 1-based cells.
    * Buttonless hover motion (`?1003`) is not reported. */
  case MouseDrag(button: Int, col: Int, row: Int)
