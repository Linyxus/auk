package auk.grep

import scala.scalajs.js

/** The naive engine in isolation: recursive search, glob restriction, binary
  * and error handling, walk semantics. The `auk.library` suites cover the same
  * behavior end-to-end through `FsDir`/`FsFile`; this suite pins the engine's
  * own contract so optimization passes can be checked against it directly. */
class GrepSuite extends munit.FunSuite:

  private def fs = Node.fs
  private def join(parts: String*): String =
    parts.reduce((a, b) => Node.path.join(a, b).asInstanceOf[String])

  /** A unique, already-created temp directory, removed after the test. */
  private val tmp: FunFixture[String] = FunFixture[String](
    setup = _ =>
      val base = js.Dynamic.global.require("node:os").tmpdir().asInstanceOf[String]
      fs.mkdtempSync(join(base, "auk-grep-test-")).asInstanceOf[String],
    teardown = d =>
      try fs.rmSync(d, js.Dynamic.literal(recursive = true, force = true))
      catch case _: Throwable => ()
  )

  private def write(dir: String, rel: String, content: String): String =
    val p = join(dir, rel)
    fs.mkdirSync(Node.path.dirname(p), js.Dynamic.literal(recursive = true))
    fs.writeFileSync(p, content, "utf8")
    p

  // -- search (recursive) ----------------------------------------------------

  tmp.test("search finds matching lines recursively with path, 1-based line, and text"): d =>
    write(d, "a.txt", "TODO one\nok")
    val deep = write(d, "s/b.txt", "x\nTODO two")
    val ms = Grep.search(d, "TODO").sortBy(_.path)
    assertEquals(ms.map(_.text), List("TODO one", "TODO two"))
    assertEquals(ms.find(_.path == deep).map(_.line), Some(2))

  tmp.test("search that finds nothing returns an empty list"): d =>
    write(d, "a.txt", "nothing here")
    assertEquals(Grep.search(d, "absent"), Nil)

  tmp.test("search skips NUL-marked binary files but still searches text siblings"): d =>
    write(d, "text.txt", "needle here")
    write(d, "bin.dat", "needle\u0000x")
    assertEquals(Grep.search(d, "needle").map(_.text), List("needle here"))

  tmp.test("a malformed pattern raises a clear error, not an empty list"): d =>
    write(d, "a.txt", "x")
    val ex = intercept[RuntimeException](Grep.search(d, "(unclosed"))
    assert(ex.getMessage.contains("invalid regular expression"), ex.getMessage)

  tmp.test("search with a glob restricts which files are searched"): d =>
    write(d, "a.scala", "TODO s")
    write(d, "b.txt", "TODO t")
    write(d, "p/c.scala", "TODO deep")
    assertEquals(Grep.search(d, "TODO", "*.scala").map(_.text), List("TODO s"))
    assertEquals(
      Grep.search(d, "TODO", "**/*.scala").map(_.text).sorted,
      List("TODO deep", "TODO s")
    )

  // -- searchFile (strict) ---------------------------------------------------

  tmp.test("searchFile reports every matching line with 1-based numbers"): d =>
    val p = write(d, "g.txt", "foo\nbar\nfoobar")
    val ms = Grep.searchFile(p, "foo")
    assertEquals(ms.map(m => (m.line, m.text)), List((1, "foo"), (3, "foobar")))

  tmp.test("searchFile does not skip binary content — an explicitly-named file is always searched"): d =>
    val p = write(d, "bin.dat", "a\u0000b\nneedle")
    assertEquals(Grep.searchFile(p, "needle").map(_.line), List(2))

  tmp.test("searchFile propagates a read error instead of swallowing it"): d =>
    intercept[Throwable](Grep.searchFile(join(d, "missing.txt"), "x"))

  // -- walk ------------------------------------------------------------------

  tmp.test("walk lists dirs and files depth-first, each dir before its descendants"): d =>
    write(d, "p/c.txt", "")
    write(d, "a.txt", "")
    val es = Walker.walk(d)
    assertEquals(es.map(e => (e.path.stripPrefix(d + "/"), e.dir)).sortBy(_._1),
      List(("a.txt", false), ("p", true), ("p/c.txt", false)))
    // The dir entry precedes its child in walk order.
    assert(es.indexWhere(_.dir) < es.indexWhere(_.path.endsWith("c.txt")))

  tmp.test("walk cuts symlink cycles instead of looping forever"): d =>
    write(d, "p/a.txt", "")
    fs.symlinkSync(d, join(d, "p", "loop"))
    val es = Walker.walk(d)
    // The link is listed (as a dir) but the cycle is not descended endlessly.
    assert(es.exists(e => e.path.endsWith("loop") && e.dir))

  // -- Lines / Glob ----------------------------------------------------------

  test("Lines.split handles LF, CRLF, lone CR, and a trailing newline"):
    assertEquals(Lines.split("a\nb"), List("a", "b"))
    assertEquals(Lines.split("a\r\nb\rc"), List("a", "b", "c"))
    assertEquals(Lines.split("a\n"), List("a"))
    assertEquals(Lines.split(""), Nil)

  test("Glob.toRegex: `*` is segment-bounded, `**/` spans (or vanishes), `.` is literal"):
    assert(Glob.toRegex("*.scala").matches("a.scala"))
    assert(!Glob.toRegex("*.scala").matches("p/c.scala"))
    assert(Glob.toRegex("**/*.scala").matches("p/q/e.scala"))
    assert(Glob.toRegex("**/*.scala").matches("a.scala"))
    assert(!Glob.toRegex("a.b").matches("axb"))
