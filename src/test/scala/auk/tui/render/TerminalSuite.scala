package auk.tui.render

class TerminalSuite extends munit.FunSuite:

  // Raw-mode setup moved from the `stty` utility to `process.stdin.setRawMode`
  // in NodeTerminal, so the old stty-command-string test no longer applies.

  test("keyboard enhancement uses kitty push/pop sequences"):
    assertEquals(Ansi.PushKeyboardEnhancement, Ansi.CSI + ">27u")
    assertEquals(Ansi.PopKeyboardEnhancement, Ansi.CSI + "<u")
