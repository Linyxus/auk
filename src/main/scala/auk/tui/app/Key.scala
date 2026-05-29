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
  /** An emacs-style control chord; `c` is the uppercase letter (e.g. `Ctrl('Q')`). */
  case Ctrl(c: scala.Char)
  case Esc
  case Unknown
