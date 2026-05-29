package auk.tui.render

class TerminalSuite extends munit.FunSuite:

  test("raw mode disables line buffering and Ctrl+Q flow control"):
    val cmd = SttyTerminal.RawModeCommand
    assert(cmd.contains("-icanon"), cmd)
    assert(cmd.contains("min 0"), cmd)
    assert(cmd.contains("time 1"), cmd)
    assert(cmd.contains("-echo"), cmd)
    assert(cmd.contains("-ixon"), cmd)
    assert(cmd.contains("-ixoff"), cmd)
