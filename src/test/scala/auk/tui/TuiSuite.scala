package auk.tui

/** The display-mode selection: `--inline` on the command line falls back to the
  * inline transcript; its absence (the product default) is fullscreen. Argv is
  * scanned, not indexed, so the flag is honored wherever it appears. */
class TuiSuite extends munit.FunSuite:

  test("fromArgv: empty argv defaults to Fullscreen"):
    assertEquals(DisplayMode.fromArgv(Nil), DisplayMode.Fullscreen)

  test("fromArgv: --inline selects Inline"):
    assertEquals(DisplayMode.fromArgv(List("--inline")), DisplayMode.Inline)

  test("fromArgv: --inline anywhere among other args selects Inline"):
    val argv = List("/usr/bin/node", "/path/to/auk.js", "--inline", "--other")
    assertEquals(DisplayMode.fromArgv(argv), DisplayMode.Inline)

  test("fromArgv: no --inline among other args stays Fullscreen"):
    val argv = List("/usr/bin/node", "/path/to/auk.js", "--other")
    assertEquals(DisplayMode.fromArgv(argv), DisplayMode.Fullscreen)
