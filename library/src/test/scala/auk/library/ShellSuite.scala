package auk.library

/** [[Shell]] / `ShellImpl` / `ShellCommandImpl` and the `Sh` runner: output and
  * exit-code capture, literal (un-split) arguments, closed stdin, working
  * directory ([[Shell.at]]), the kill deadline ([[Shell.withTimeout]]), the
  * `sh` shell-line escape hatch, and the file-operation program denylist.
  *
  * Uses ordinary POSIX tools (echo/printf/false/wc/pwd/sleep), which the Node
  * jsEnv inherits from the host. */
class ShellSuite extends LibSuite:

  private def shell: Shell = lib.shell

  // -- output capture & exit codes -------------------------------------------

  test("run captures stdout and reports a clean exit"):
    val r = shell.run("echo", "hello")
    assertEquals(r.stdout.trim, "hello")
    assertEquals(r.exitCode, 0)
    assert(r.ok)

  test("a non-zero exit is reported in the result, not thrown"):
    val r = shell.run("false")
    assertNotEquals(r.exitCode, 0)
    assert(!r.ok)

  test("arguments are passed literally, with no shell word-splitting"):
    assertEquals(shell.run("printf", "%s", "a b").stdout, "a b")

  test("multi-line stdout is preserved verbatim"):
    assertEquals(shell.run("printf", "one\ntwo\n").stdout, "one\ntwo\n")

  test("non-ASCII UTF-8 output round-trips"):
    assertEquals(shell.run("printf", "café ✓").stdout, "café ✓")

  test("a program that does not exist reports exit 127"):
    assertEquals(shell.run("definitely-not-a-real-program-xyz").exitCode, 127)

  test("stderr is captured separately from stdout on failure"):
    val r = shell.run("wc", "/no-such-file-xyz")
    assertNotEquals(r.exitCode, 0)
    assertEquals(r.stdout, "")
    assert(r.stderr.nonEmpty)

  // -- reusable command handle -----------------------------------------------

  test("a command handle can be run repeatedly"):
    val echo = shell.command("echo")
    assertEquals(echo.execute("a").stdout.trim, "a")
    assertEquals(echo.execute("b").stdout.trim, "b")

  test("run is shorthand for command().execute()"):
    assertEquals(shell.command("echo").execute("hi").stdout, shell.run("echo", "hi").stdout)

  // -- stdin is closed -------------------------------------------------------

  test("stdin is closed so a reader sees EOF instead of hanging"):
    val r = shell.run("wc", "-c")
    assert(r.ok)
    assertEquals(r.stdout.trim, "0")

  // -- working directory -----------------------------------------------------

  tmp.test("at runs the command in the given absolute directory"): d =>
    val sub = d.dir("atabs"); sub.makedir()
    assert(shell.at(sub.path).run("pwd").stdout.trim.endsWith("/atabs"))

  tmp.test("at resolves a relative directory against the shell's cwd"): d =>
    val sub = d.dir("atrel"); sub.makedir()
    val rooted = shell.at(d.path).at(lib.path("atrel"))
    assert(rooted.run("pwd").stdout.trim.endsWith("/atrel"))

  tmp.test("at returns a new shell and leaves the original's cwd unchanged"): d =>
    val sub = d.dir("here"); sub.makedir()
    val moved = shell.at(sub.path)
    assertEquals(moved.run("pwd").stdout.trim.endsWith("/here"), true)
    // The original shell still runs in the process cwd, not /here.
    assert(!shell.run("pwd").stdout.trim.endsWith("/here"))

  // -- timeout ---------------------------------------------------------------

  test("a command exceeding the timeout is killed and flagged"):
    val r = shell.withTimeout(300).run("sleep", "5")
    assert(r.timedOut)
    assert(!r.ok)

  test("withTimeout returns a new shell without affecting the original"):
    // A generous timeout on the derived shell; the original keeps the default.
    assert(shell.withTimeout(10_000).run("echo", "ok").ok)

  // -- sh: the shell-line escape hatch ---------------------------------------

  test("sh interprets a shell command line"):
    assertEquals(shell.sh("echo hi && echo bye").stdout, "hi\nbye\n")

  test("sh supports pipes"):
    assertEquals(shell.sh("printf 'a\\nb\\nc\\n' | wc -l").stdout.trim, "3")

  test("sh reports the command line's exit status"):
    assertEquals(shell.sh("exit 3").exitCode, 3)

  tmp.test("sh respects the shell's working directory"): d =>
    val sub = d.dir("shdir"); sub.makedir()
    assert(shell.at(sub.path).sh("pwd").stdout.trim.endsWith("/shdir"))

  // -- the file-operation denylist -------------------------------------------

  test("every file-operation program in the denylist is rejected by command"):
    for n <- List("rm", "ls", "mkdir", "mv", "cat", "touch") do
      intercept[RuntimeException](shell.command(n))

  test("an absolute path to a forbidden program is rejected (matched by base name)"):
    intercept[RuntimeException](shell.command("/bin/rm"))
    intercept[RuntimeException](shell.command("./mv"))

  test("run enforces the denylist too"):
    intercept[RuntimeException](shell.run("ls", "-la"))

  test("the rejection points the caller at the file-system interface"):
    interceptContains("lib.fs")(shell.command("rm"))

  test("a permitted program is not rejected"):
    shell.command("echo")
    shell.command("wc") // must not throw

  // -- CommandResult conveniences, end to end --------------------------------

  test("output falls back to stderr when stdout is empty"):
    val r = shell.run("wc", "/no-such-file-xyz")
    assertEquals(r.stdout, "")
    assertEquals(r.output, r.stderr)

  test("toString renders the output followed by an exit footer"):
    val s = shell.run("echo", "hi").toString
    assert(s.contains("hi"), s)
    assert(s.contains("[exit 0]"), s)
