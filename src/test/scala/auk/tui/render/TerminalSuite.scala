package auk.tui.render

class TerminalSuite extends munit.FunSuite:

  // Raw-mode setup moved from the `stty` utility to `process.stdin.setRawMode`
  // in NodeTerminal, so the old stty-command-string test no longer applies.

  test("keyboard enhancement uses kitty push/pop sequences"):
    assertEquals(Ansi.PushKeyboardEnhancement, Ansi.CSI + ">27u")
    assertEquals(Ansi.PopKeyboardEnhancement, Ansi.CSI + "<u")

  test("mouse enable/disable use SGR-1006 button+motion reporting, disable order reversed"):
    assertEquals(Ansi.MouseEnable, Ansi.CSI + "?1000h" + Ansi.CSI + "?1002h" + Ansi.CSI + "?1006h")
    assertEquals(Ansi.MouseDisable, Ansi.CSI + "?1006l" + Ansi.CSI + "?1002l" + Ansi.CSI + "?1000l")

  test("osc52Copy wraps a base64 payload in an OSC 52 clipboard write terminated with BEL"):
    assertEquals(Ansi.osc52Copy("aGk="), Ansi.OSC + "52;c;aGk=" + Ansi.Bell)
