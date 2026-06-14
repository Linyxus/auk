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
        try { val _ = repl.eval("base.delete()", 30_000) }
        catch { case _: Throwable => () }
      repl.close()

  /** Create the shared temp directory and bind it as the REPL-session val
    * `base`, used by every snippet below. */
  private def setup()(using Async): Unit =
    val r = completed(
      """val osMod = scala.scalajs.js.Dynamic.global.require("node:os")
        |val base = lib.Path(osMod.tmpdir().asInstanceOf[String]) / ("auk-fstest-" + System.nanoTime())
        |base.asDir.makedir()""".stripMargin
    )
    assert(r.ok, s"fs test setup failed: ${r.error.getOrElse(r.output)}")

  private def completed(code: String)(using Async): ReplProtocol.Response =
    repl.eval(code, 30_000).status match
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
    """(lib.Path("/foo") / "bar" / "baz").toString == "/foo/bar/baz""""

  check("Path `/` normalizes `..`"):
    """(lib.Path("/foo/bar") / ".." / "qux").toString == "/foo/qux""""

  check("Path baseName is the final segment"):
    """lib.Path("/a/b/c.txt").baseName == "c.txt""""

  check("Path parent drops the final segment"):
    """lib.Path("/a/b/c.txt").parent.toString == "/a/b""""

  check("Path compares by value"):
    """lib.Path("/a/b") == lib.Path("/a/b") && lib.Path("/a") != lib.Path("/b")"""

  // -- FileSystem facade -----------------------------------------------------------

  check("fs.cwd is an absolute path"):
    """lib.fs.cwd.toString.startsWith("/")"""

  check("fs.access/accessFile/accessDir resolve a path"):
    """{ val d = (base / "acc").asDir; d.makedir()
      |  val f = (base / "acc.txt").asFile; f.write("x")
      |  lib.fs.access(d.path).isDir && lib.fs.access(f.path).isFile &&
      |    lib.fs.accessFile(f.path).rawContent == "x" && lib.fs.accessDir(d.path).isDir }""".stripMargin

  // -- FsFile: reading -------------------------------------------------------------

  check("read prints content with 1-based line numbers"):
    """{ val f = (base / "w.txt").asFile; f.write("hello\nworld")
      |  val sw = new java.io.ByteArrayOutputStream()
      |  Console.withOut(sw) { f.read() }
      |  sw.toString.trim == "1@ hello\n2@ world" }""".stripMargin

  check("rawContent has no prefixes"):
    """{ val f = (base / "r.txt").asFile; f.write("a\nb"); f.rawContent == "a\nb" }"""

  check("lines and lineCount split content"):
    """{ val f = (base / "l.txt").asFile; f.write("a\nb\nc"); f.lines == List("a","b","c") && f.lineCount == 3 }"""

  check("a trailing newline does not add a phantom line"):
    """{ val f = (base / "tn.txt").asFile; f.write("a\nb\n"); f.lines == List("a","b") && f.lineCount == 2 }"""

  check("size is the byte length"):
    """{ val f = (base / "s.txt").asFile; f.write("12345"); f.size == 5L }"""

  check("ext is the extension without the dot"):
    """(base / "e.scala").asFile.ext == "scala" && (base / "noext").asFile.ext == """""

  check("read prints a window with absolute line numbers"):
    """{ val f = (base / "sl.txt").asFile; f.write("a\nb\nc\nd")
      |  val sw = new java.io.ByteArrayOutputStream()
      |  Console.withOut(sw) { f.read(2, 2) }
      |  sw.toString.trim == "2@ b\n3@ c" }""".stripMargin

  // -- FsFile: writing -------------------------------------------------------------

  check("write overwrites existing content"):
    """{ val f = (base / "ow.txt").asFile; f.write("first"); f.write("second"); f.rawContent == "second" }"""

  check("append adds to the end and creates if missing"):
    """{ val f = (base / "ap.txt").asFile; f.append("x"); f.append("y"); f.rawContent == "xy" }"""

  check("touch creates empty but leaves existing content"):
    """{ val f = (base / "t.txt").asFile; f.touch(); val a = f.exists && f.rawContent == ""
      |  f.write("data"); f.touch(); a && f.rawContent == "data" }""".stripMargin

  check("replace swaps the single occurrence"):
    """{ val f = (base / "rep.txt").asFile; f.write("a b a"); f.replace("b", "B"); f.rawContent == "a B a" }"""

  checkError("replace fails when there is no match", "no occurrence"):
    """{ val f = (base / "rep0.txt").asFile; f.write("abc"); f.replace("zzz", "q") }"""

  checkError("replace fails when there are several matches", "exactly one"):
    """{ val f = (base / "repn.txt").asFile; f.write("a a a"); f.replace("a", "b") }"""

  check("replaceAll replaces all and returns the count"):
    """{ val f = (base / "all.txt").asFile; f.write("a a a"); val n = f.replaceAll("a", "b")
      |  n == 3 && f.rawContent == "b b b" }""".stripMargin

  check("replaceAll returns 0 when nothing matches"):
    """{ val f = (base / "all0.txt").asFile; f.write("xyz"); f.replaceAll("q", "w") == 0 }"""

  // -- FsFile: grep ----------------------------------------------------------------

  check("grep returns matching lines with line numbers"):
    """{ val f = (base / "g.txt").asFile; f.write("foo\nbar\nfoobar"); val m = f.grep("foo")
      |  m.map(_.lineNumber) == List(1, 3) && m.map(_.line) == List("foo", "foobar") }""".stripMargin

  check("grep honours regular expressions"):
    """{ val f = (base / "gr.txt").asFile; f.write("a1\nb2\ncc"); f.grep("[0-9]").map(_.lineNumber) == List(1, 2) }"""

  check("a Match renders as path:line@ content"):
    """{ val f = (base / "mt.txt").asFile; f.write("hello"); val m = f.grep("ell").head
      |  m.toString == f.path.toString + ":1@ hello" && m.file == f.path }""".stripMargin

  // -- FsEntry: common -------------------------------------------------------------

  check("exists / isFile / isDir reflect the filesystem"):
    """{ val f = (base / "x.txt").asFile; f.write("x"); val d = base.asDir
      |  f.exists && f.isFile && !f.isDir && d.isDir && !d.isFile && !(base / "nope").asFile.exists }""".stripMargin

  check("name is the final segment"):
    """(base / "n.txt").asFile.name == "n.txt""""

  check("parent is the containing directory"):
    """{ val f = (base / "p.txt").asFile; f.write("x"); f.parent.path == base }"""

  check("lastModified is a formatted datetime and lastModifiedMs is positive"):
    """{ val f = (base / "lm.txt").asFile; f.write("x"); val s = f.lastModified
      |  f.lastModifiedMs > 0L && s.length == 19 && s.charAt(4) == '-' && s.charAt(7) == '-' &&
      |    s.charAt(10) == ' ' && s.charAt(13) == ':' && s.charAt(16) == ':' }""".stripMargin

  check("delete removes a file"):
    """{ val f = (base / "del.txt").asFile; f.write("x"); val a = f.exists; f.delete(); a && !f.exists }"""

  check("delete removes a directory recursively"):
    """{ val d = (base / "deldir").asDir; d.makedir(); d.file("inner.txt").write("x")
      |  val a = d.exists; d.delete(); a && !d.exists }""".stripMargin

  check("moveTo relocates an entry"):
    """{ val f = (base / "mv1.txt").asFile; f.write("data"); val dest = base / "mv2.txt"
      |  f.moveTo(dest); !f.exists && dest.asFile.rawContent == "data" }""".stripMargin

  check("copyTo duplicates a file"):
    """{ val f = (base / "cp1.txt").asFile; f.write("data"); val dest = base / "cp2.txt"
      |  f.copyTo(dest); f.exists && dest.asFile.rawContent == "data" }""".stripMargin

  check("copyTo duplicates a directory recursively"):
    """{ val d = (base / "cpd").asDir; d.makedir(); d.file("k.txt").write("v"); val dest = base / "cpd2"
      |  d.copyTo(dest); dest.asDir.file("k.txt").rawContent == "v" }""".stripMargin

  // -- FsDir -----------------------------------------------------------------------

  check("makedir creates missing parents and is idempotent"):
    """{ val d = (base / "x1" / "x2" / "x3").asDir; d.makedir(); d.makedir(); d.exists && d.isDir }"""

  check("entries / files / dirs list immediate children"):
    """{ val d = (base / "listing").asDir; d.makedir()
      |  d.file("a.txt").write("a"); d.file("b.txt").write("b"); d.dir("sub").makedir()
      |  d.entries.length == 3 && d.files.map(_.name).sorted == List("a.txt","b.txt") &&
      |    d.dirs.map(_.name) == List("sub") }""".stripMargin

  check("walk lists the whole subtree"):
    """{ val d = (base / "tree").asDir; d.makedir(); d.file("top.txt").write("t")
      |  val s = d.dir("s"); s.makedir(); s.file("deep.txt").write("d")
      |  d.walk.map(_.name).sorted == List("deep.txt", "s", "top.txt") }""".stripMargin

  check("glob with a single segment stays in this directory"):
    """{ val d = (base / "gl").asDir; d.makedir(); d.file("A.scala").write(""); d.file("B.txt").write("")
      |  d.dir("p").makedir(); d.dir("p").file("C.scala").write("")
      |  d.glob("*.scala").map(_.path.baseName).sorted == List("A.scala") }""".stripMargin

  check("glob with ** matches recursively"):
    """{ val d = (base / "gl2").asDir; d.makedir(); d.file("A.scala").write("")
      |  d.dir("p").makedir(); d.dir("p").file("C.scala").write("")
      |  d.glob("**/*.scala").map(_.path.baseName).sorted == List("A.scala", "C.scala") }""".stripMargin

  check("file/dir return child handles"):
    """{ val d = (base / "handles").asDir; d.makedir()
      |  d.file("f.txt").path == (d.path / "f.txt") && d.dir("sub").path == (d.path / "sub") }""".stripMargin

  check("grep searches file contents recursively"):
    """{ val d = (base / "dgrep").asDir; d.makedir(); d.file("a.txt").write("TODO one\nok")
      |  d.dir("s").makedir(); d.dir("s").file("b.txt").write("TODO two")
      |  d.grep("TODO").map(_.line).sorted == List("TODO one", "TODO two") }""".stripMargin

  check("grep with a file glob restricts which files are searched"):
    """{ val d = (base / "dgrepf").asDir; d.makedir()
      |  d.file("a.scala").write("TODO s"); d.file("b.txt").write("TODO t")
      |  d.grep("TODO", "*.scala").map(_.line) == List("TODO s") }""".stripMargin
