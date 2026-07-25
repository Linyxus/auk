package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}

import gears.async.Async
import gears.async.default.given

import auk.platform.js.ReplArtifacts
import auk.runtime.repl.{ReplProtocol, ScalaRepl}

/** End-to-end tests for the auk runtime filesystem library (`auk.library`),
  * exercised the way the agent uses it: as Scala evaluated in a real REPL worker
  * with the packed `library.bin` preloaded and `lib` bound by the preamble.
  *
  * Each case runs against a unique temp directory (`base`, created once in
  * [[beforeAll]] and removed in [[afterAll]]) and ends its snippet in a `Boolean`
  * so the assertion is a simple `Boolean = true` check — the REPL truncates long
  * rendered values (e.g. lists of absolute paths), so tests derive short values
  * (line numbers, base names, counts) rather than rendering paths.
  *
  * Skipped via `assume` when the REPL artifacts are absent
  * (`sbt vendorRepl packLibraryBin`).
  */
class FsLibrarySuite extends munit.FunSuite:

  override def munitTimeout: Duration = 120.seconds

  private lazy val artifactsAvailable = ReplArtifacts.resolve().isRight
  private val repl = ScalaRepl()

  override def beforeAll(): Unit =
    if artifactsAvailable then Async.fromSync(setup())

  override def afterAll(): Unit =
    Async.fromSync:
      if artifactsAvailable then
        try { val _ = repl.eval("base.delete()", Some(30_000)) }
        catch { case _: Throwable => () }
      repl.close()

  /** Create the shared temp directory and bind it as the REPL-session val
    * `base`, used by every snippet below. */
  private def setup()(using Async): Unit =
    val r = completed(
      """val osMod = scala.scalajs.js.Dynamic.global.require("node:os")
        |val base = lib.path(osMod.tmpdir().asInstanceOf[String]) / ("auk-fstest-" + System.nanoTime())
        |base.openAsDir.makedir()""".stripMargin
    )
    assert(r.ok, s"fs test setup failed: ${r.error.getOrElse(r.output)}")

  private def completed(code: String)(using Async): ReplProtocol.Response =
    repl.eval(code, Some(30_000)).status match
      case ScalaRepl.Status.Completed(r) => r
      case other                         => fail(s"unexpected REPL status: $other")

  /** Register a test whose snippet must evaluate to `true`. */
  private def check(name: String)(code: String): Unit =
    test(name):
      assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl packLibraryBin`")
      Async.fromSync:
        val r = completed(code)
        assert(r.ok, s"eval errored: ${r.error.getOrElse(r.output)}")
        assert(r.output.contains("Boolean = true"), s"expected `Boolean = true`, got: ${r.output}")

  /** Register a test whose snippet's REPL *rendering* must contain `substr` —
    * for pinning what the agent actually sees when it echoes a bare value. */
  private def checkRendered(name: String, substr: String)(code: String): Unit =
    test(name):
      assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl packLibraryBin`")
      Async.fromSync:
        val r = completed(code)
        assert(r.ok, s"eval errored: ${r.error.getOrElse(r.output)}")
        val rendered = ReplProtocol.stripAnsi(r.output)
        assert(rendered.contains(substr), s"expected '$substr' in: $rendered")

  /** Register a test whose snippet must fail with a message containing `substr`. */
  private def checkError(name: String, substr: String)(code: String): Unit =
    test(name):
      assume(artifactsAvailable, "REPL artifacts not found; run `sbt vendorRepl packLibraryBin`")
      Async.fromSync:
        val r = completed(code)
        assert(!r.ok, s"expected an error, got: ${r.output}")
        val msg = r.error.getOrElse("") + r.output + r.stderr
        assert(msg.toLowerCase.contains(substr.toLowerCase), s"expected '$substr' in: $msg")

  // -- Path (pure, no IO) ----------------------------------------------------------

  check("Path `/` joins segments"):
    """(lib.path("/foo") / "bar" / "baz").toString == "/foo/bar/baz""""

  check("Path `/` normalizes `..`"):
    """(lib.path("/foo/bar") / ".." / "qux").toString == "/foo/qux""""

  check("Path baseName is the final segment"):
    """lib.path("/a/b/c.txt").baseName == "c.txt""""

  check("Path parent drops the final segment"):
    """lib.path("/a/b/c.txt").parent.toString == "/a/b""""

  check("Path compares by value"):
    """lib.path("/a/b") == lib.path("/a/b") && lib.path("/a") != lib.path("/b")"""

  // -- FileSystem facade -----------------------------------------------------------

  check("fs.cwd is a directory at an absolute path"):
    """lib.fs.cwd.isDir && lib.fs.cwd.path.toString.startsWith("/")"""

  check("fs.access/accessFile/accessDir resolve a path"):
    """{ val d = (base / "acc").openAsDir; d.makedir()
      |  val f = (base / "acc.txt").openAsFile; f.write("x")
      |  lib.fs.access(d.path).isDir && lib.fs.access(f.path).isFile &&
      |    lib.fs.accessFile(f.path).rawContent == "x" && lib.fs.accessDir(d.path).isDir }""".stripMargin

  // -- FsFile: reading -------------------------------------------------------------

  check("read prints content with 1-based line numbers"):
    """{ val f = (base / "w.txt").openAsFile; f.write("hello\nworld")
      |  val sw = new java.io.ByteArrayOutputStream()
      |  Console.withOut(sw) { f.read() }
      |  sw.toString.trim == "1@ hello\n2@ world" }""".stripMargin

  check("rawContent has no prefixes"):
    """{ val f = (base / "r.txt").openAsFile; f.write("a\nb"); f.rawContent == "a\nb" }"""

  check("lines and lineCount split content"):
    """{ val f = (base / "l.txt").openAsFile; f.write("a\nb\nc"); f.lines == List("a","b","c") && f.lineCount == 3 }"""

  check("a trailing newline does not add a phantom line"):
    """{ val f = (base / "tn.txt").openAsFile; f.write("a\nb\n"); f.lines == List("a","b") && f.lineCount == 2 }"""

  check("size is the byte length"):
    """{ val f = (base / "s.txt").openAsFile; f.write("12345"); f.size == 5L }"""

  check("ext is the extension without the dot"):
    """(base / "e.scala").openAsFile.ext == "scala" && (base / "noext").openAsFile.ext == """""

  check("read prints a window with absolute line numbers"):
    """{ val f = (base / "sl.txt").openAsFile; f.write("a\nb\nc\nd")
      |  val sw = new java.io.ByteArrayOutputStream()
      |  Console.withOut(sw) { f.read(2, 2) }
      |  sw.toString.trim == "2@ b\n3@ c" }""".stripMargin

  // -- FsFile: writing -------------------------------------------------------------

  check("write overwrites existing content"):
    """{ val f = (base / "ow.txt").openAsFile; f.write("first"); f.write("second"); f.rawContent == "second" }"""

  check("append adds to the end and creates if missing"):
    """{ val f = (base / "ap.txt").openAsFile; f.append("x"); f.append("y"); f.rawContent == "xy" }"""

  check("touch creates empty but leaves existing content"):
    """{ val f = (base / "t.txt").openAsFile; f.touch(); val a = f.exists && f.rawContent == ""
      |  f.write("data"); f.touch(); a && f.rawContent == "data" }""".stripMargin

  check("replace swaps the single occurrence"):
    """{ val f = (base / "rep.txt").openAsFile; f.write("a b a"); f.replace("b", "B"); f.rawContent == "a B a" }"""

  checkError("replace fails when there is no match", "no occurrence"):
    """{ val f = (base / "rep0.txt").openAsFile; f.write("abc"); f.replace("zzz", "q") }"""

  checkError("replace fails when there are several matches", "exactly one"):
    """{ val f = (base / "repn.txt").openAsFile; f.write("a a a"); f.replace("a", "b") }"""

  check("replaceAll replaces all and returns the count"):
    """{ val f = (base / "all.txt").openAsFile; f.write("a a a"); val n = f.replaceAll("a", "b")
      |  n == 3 && f.rawContent == "b b b" }""".stripMargin

  check("replaceAll returns 0 when nothing matches"):
    """{ val f = (base / "all0.txt").openAsFile; f.write("xyz"); f.replaceAll("q", "w") == 0 }"""

  // -- FsFile: grep ----------------------------------------------------------------

  check("grep returns matching lines with line numbers"):
    """{ val f = (base / "g.txt").openAsFile; f.write("foo\nbar\nfoobar"); val m = f.grep("foo").matches
      |  m.map(_.lineNumber) == List(1, 3) && m.map(_.line) == List("foo", "foobar") }""".stripMargin

  check("grep honours regular expressions"):
    """{ val f = (base / "gr.txt").openAsFile; f.write("a1\nb2\ncc"); f.grep("[0-9]").matches.map(_.lineNumber) == List(1, 2) }"""

  check("a Match renders as path:line@ content"):
    """{ val f = (base / "mt.txt").openAsFile; f.write("hello"); val m = f.grep("ell").matches.head
      |  m.toString == f.path.toString + ":1@ hello" && m.file == f }""".stripMargin

  // -- FsEntry: common -------------------------------------------------------------

  check("exists / isFile / isDir reflect the filesystem"):
    """{ val f = (base / "x.txt").openAsFile; f.write("x"); val d = base.openAsDir
      |  f.exists && f.isFile && !f.isDir && d.isDir && !d.isFile && !(base / "nope").openAsFile.exists }""".stripMargin

  check("name is the final segment"):
    """(base / "n.txt").openAsFile.name == "n.txt""""

  check("parent is the containing directory"):
    """{ val f = (base / "p.txt").openAsFile; f.write("x"); f.parent.path == base }"""

  check("lastModified is a formatted datetime and lastModifiedMs is positive"):
    """{ val f = (base / "lm.txt").openAsFile; f.write("x"); val s = f.lastModified
      |  f.lastModifiedMs > 0L && s.length == 19 && s.charAt(4) == '-' && s.charAt(7) == '-' &&
      |    s.charAt(10) == ' ' && s.charAt(13) == ':' && s.charAt(16) == ':' }""".stripMargin

  check("delete removes a file"):
    """{ val f = (base / "del.txt").openAsFile; f.write("x"); val a = f.exists; f.delete(); a && !f.exists }"""

  check("delete removes a directory recursively"):
    """{ val d = (base / "deldir").openAsDir; d.makedir(); d.file("inner.txt").write("x")
      |  val a = d.exists; d.delete(); a && !d.exists }""".stripMargin

  check("moveTo relocates an entry"):
    """{ val f = (base / "mv1.txt").openAsFile; f.write("data"); val dest = base / "mv2.txt"
      |  f.moveTo(dest); !f.exists && dest.openAsFile.rawContent == "data" }""".stripMargin

  check("copyTo duplicates a file"):
    """{ val f = (base / "cp1.txt").openAsFile; f.write("data"); val dest = base / "cp2.txt"
      |  f.copyTo(dest); f.exists && dest.openAsFile.rawContent == "data" }""".stripMargin

  check("copyTo duplicates a directory recursively"):
    """{ val d = (base / "cpd").openAsDir; d.makedir(); d.file("k.txt").write("v"); val dest = base / "cpd2"
      |  d.copyTo(dest); dest.openAsDir.file("k.txt").rawContent == "v" }""".stripMargin

  // -- FsDir -----------------------------------------------------------------------

  check("makedir creates missing parents and is idempotent"):
    """{ val d = (base / "x1" / "x2" / "x3").openAsDir; d.makedir(); d.makedir(); d.exists && d.isDir }"""

  check("entries / files / dirs list immediate children"):
    """{ val d = (base / "listing").openAsDir; d.makedir()
      |  d.file("a.txt").write("a"); d.file("b.txt").write("b"); d.dir("sub").makedir()
      |  d.entries.length == 3 && d.files.map(_.name).sorted == List("a.txt","b.txt") &&
      |    d.dirs.map(_.name) == List("sub") }""".stripMargin

  check("walk lists the whole subtree"):
    """{ val d = (base / "tree").openAsDir; d.makedir(); d.file("top.txt").write("t")
      |  val s = d.dir("s"); s.makedir(); s.file("deep.txt").write("d")
      |  d.walk.map(_.name).sorted == List("deep.txt", "s", "top.txt") }""".stripMargin

  check("glob with a single segment stays in this directory"):
    """{ val d = (base / "gl").openAsDir; d.makedir(); d.file("A.scala").write(""); d.file("B.txt").write("")
      |  d.dir("p").makedir(); d.dir("p").file("C.scala").write("")
      |  d.glob("*.scala").entries.map(_.path.baseName).sorted == List("A.scala") }""".stripMargin

  check("glob with ** matches recursively"):
    """{ val d = (base / "gl2").openAsDir; d.makedir(); d.file("A.scala").write("")
      |  d.dir("p").makedir(); d.dir("p").file("C.scala").write("")
      |  d.glob("**/*.scala").entries.map(_.path.baseName).sorted == List("A.scala", "C.scala") }""".stripMargin

  check("file/dir return child handles"):
    """{ val d = (base / "handles").openAsDir; d.makedir()
      |  d.file("f.txt").path == (d.path / "f.txt") && d.dir("sub").path == (d.path / "sub") }""".stripMargin

  check("grep searches file contents recursively"):
    """{ val d = (base / "dgrep").openAsDir; d.makedir(); d.file("a.txt").write("TODO one\nok")
      |  d.dir("s").makedir(); d.dir("s").file("b.txt").write("TODO two")
      |  d.grep("TODO").matches.map(_.line).sorted == List("TODO one", "TODO two") }""".stripMargin

  check("grep with a file glob restricts which files are searched"):
    """{ val d = (base / "dgrepf").openAsDir; d.makedir()
      |  d.file("a.scala").write("TODO s"); d.file("b.txt").write("TODO t")
      |  d.grep("TODO", "*.scala").matches.map(_.line) == List("TODO s") }""".stripMargin

  // -- GrepResult / GlobResult -----------------------------------------------
  //
  // What the agent sees is the point of these: a search hands back a result
  // object whose *rendering* is a one-line summary (so echoing a grep over a
  // huge tree cannot flood the transcript) and whose matches are printed on
  // demand by `display()`, through the captured stdout.

  check("grep hands back a result object with a count and its matches"):
    """{ val d = (base / "gres").openAsDir; d.makedir(); d.file("a.txt").write("hit\nmiss\nhit")
      |  val r = d.grep("hit")
      |  r.length == 2 && r.nonEmpty && !r.isEmpty && r.matches.map(_.lineNumber) == List(1, 3) }""".stripMargin

  checkRendered(
    "a bare grep echoes a one-line summary, never its matches",
    "GrepResult(2 matches — use .display() or .matches)"
  ):
    """{ val d = (base / "gecho").openAsDir; d.makedir(); d.file("a.txt").write("hit\nmiss\nhit")
      |  d.grep("hit") }""".stripMargin

  check("display prints the matches as path:linenum@ line"):
    """{ val d = (base / "gdisp").openAsDir; d.makedir(); d.file("a.txt").write("hit\nmiss\nhit")
      |  val sw = new java.io.ByteArrayOutputStream()
      |  Console.withOut(sw) { d.grep("hit").display() }
      |  val p = d.file("a.txt").path.toString
      |  sw.toString.trim == p + ":1@ hit\n" + p + ":3@ hit" }""".stripMargin

  check("display of a window marks what it skipped and what it held back"):
    """{ val d = (base / "gwin").openAsDir; d.makedir()
      |  d.file("a.txt").write((1 to 5).map(i => "hit " + i).mkString("\n"))
      |  val sw = new java.io.ByteArrayOutputStream()
      |  Console.withOut(sw) { d.grep("hit").display(1, 2) }
      |  val ls = sw.toString.trim.split("\n").toList
      |  ls.length == 4 && ls.head == "(... 1 match skipped ...)" &&
      |    ls(1).endsWith(":2@ hit 2") && ls.last == "(... 2 more matches ...)" }""".stripMargin

  check("a grep that matches nothing displays as no matches"):
    """{ val d = (base / "gnone").openAsDir; d.makedir(); d.file("a.txt").write("quiet")
      |  val sw = new java.io.ByteArrayOutputStream()
      |  Console.withOut(sw) { d.grep("needle").display() }
      |  sw.toString.trim == "(no matches)" }""".stripMargin

  checkRendered(
    "a bare glob echoes a one-line summary too",
    "GlobResult(1 entry — use .display() or .entries)"
  ):
    """{ val d = (base / "glecho").openAsDir; d.makedir(); d.file("A.scala").write("")
      |  d.glob("*.scala") }""".stripMargin

  check("glob display prints one path per line, directories with a trailing slash"):
    """{ val d = (base / "gldisp").openAsDir; d.makedir(); d.file("A.scala").write(""); d.dir("p").makedir()
      |  val sw = new java.io.ByteArrayOutputStream()
      |  Console.withOut(sw) { d.glob("*").display() }
      |  sw.toString.trim.split("\n").toList.sorted ==
      |    List(d.path.toString + "/A.scala", d.path.toString + "/p/").sorted }""".stripMargin
