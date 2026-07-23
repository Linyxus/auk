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

  tmp.test("search skips a binary whose first NUL is past the 8 KB sniff window"): d =>
    write(d, "text.txt", "needle here")
    // 10 KB of non-NUL text precedes the NUL, so the 8 KB head sniff sees none;
    // only the full-content check catches it. The NUL is built from 0.toChar so
    // no raw NUL byte lives in this source. "needle" after it must not surface.
    val big = "x" * 10000 + 0.toChar + "needle after nul"
    write(d, "big.dat", big)
    assertEquals(Grep.search(d, "needle").map(_.text), List("needle here"))

  tmp.test("search handles an empty file: no match, no error"): d =>
    write(d, "empty.txt", "")
    assertEquals(Grep.search(d, "anything"), Nil)

  tmp.test("search reads a file of exactly 8192 bytes correctly"): d =>
    // Exactly the head length: the head read fills, and the 'read the rest'
    // path runs with nothing left to read.
    val content = "needle" + "a" * 8186
    assertEquals(content.length, 8192)
    write(d, "exact.txt", content)
    assertEquals(Grep.search(d, "needle").map(_.line), List(1))

  tmp.test("search decodes and matches text past the 8 KB sniff window"): d =>
    // Positive counterpart to the binary case: a text file larger than the head
    // is fully read and searched, including a match beyond 8 KB.
    val content = "x" * 9000 + "needle far past the head"
    write(d, "long.txt", content)
    assertEquals(Grep.search(d, "needle far past the head").map(_.line), List(1))

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

  // -- ignore-aware pruning (stage 2) ----------------------------------------

  tmp.test("search prunes files matched by a basename .gitignore rule at any depth"): d =>
    write(d, ".gitignore", "*.log")
    write(d, "keep.txt", "needle")
    write(d, "noise.log", "needle")
    write(d, "sub/deep.log", "needle")
    write(d, "sub/keep2.txt", "needle")
    assertEquals(Grep.search(d, "needle").map(_.path.stripPrefix(d + "/")).sorted,
      List("keep.txt", "sub/keep2.txt"))

  tmp.test("an anchored .gitignore rule matches only at that path"): d =>
    write(d, ".gitignore", "/sub/x.txt")
    write(d, "sub/x.txt", "needle")
    write(d, "other/x.txt", "needle")
    write(d, "x.txt", "needle")
    assertEquals(Grep.search(d, "needle").map(_.path.stripPrefix(d + "/")).sorted,
      List("other/x.txt", "x.txt"))

  tmp.test("a dir-only rule prunes a directory and its subtree at any depth"): d =>
    write(d, ".gitignore", "node_modules/")
    write(d, "src/app.js", "needle")
    write(d, "node_modules/pkg/index.js", "needle")
    write(d, "src/node_modules/dep/index.js", "needle")
    assertEquals(Grep.search(d, "needle").map(_.path.stripPrefix(d + "/")).sorted,
      List("src/app.js"))
    assert(!Walker.walk(d).exists(_.path.contains("node_modules")))

  tmp.test("negation with last-match-wins un-ignores a file"): d =>
    write(d, ".gitignore", "*.log" + 10.toChar + "!keep.log")
    write(d, "drop.log", "needle")
    write(d, "keep.log", "needle")
    assertEquals(Grep.search(d, "needle").map(_.path.stripPrefix(d + "/")), List("keep.log"))

  tmp.test("a nested .gitignore refines an outer one"): d =>
    write(d, ".gitignore", "*.txt")
    write(d, "a.txt", "needle")
    write(d, "sub/.gitignore", "!b.txt")
    write(d, "sub/b.txt", "needle")
    write(d, "sub/c.txt", "needle")
    assertEquals(Grep.search(d, "needle").map(_.path.stripPrefix(d + "/")), List("sub/b.txt"))

  tmp.test("walk skips .git but walkAll lists it"): d =>
    write(d, ".git/HEAD", "ref: refs/heads/main")
    write(d, "a.txt", "x")
    assert(!Walker.walk(d).exists(e => e.path.endsWith("/.git") || e.path.contains("/.git/")))
    assert(Walker.walkAll(d).exists(e => e.path.endsWith("/.git") || e.path.contains("/.git/")))

  tmp.test("an explicitly-named root is searched even if an outer tree ignores it"): d =>
    write(d, ".gitignore", "sub/")
    write(d, "sub/f.txt", "needle")
    assertEquals(Grep.search(d, "needle"), Nil)
    assertEquals(Grep.search(join(d, "sub"), "needle").map(_.path.endsWith("f.txt")), List(true))

  tmp.test("searchAll finds what search prunes (.git and gitignored)"): d =>
    write(d, ".gitignore", "*.log")
    write(d, ".git/config", "needle")
    write(d, "noise.log", "needle")
    write(d, "keep.txt", "needle")
    assertEquals(Grep.search(d, "needle").map(_.path.stripPrefix(d + "/")), List("keep.txt"))
    assertEquals(Grep.searchAll(d, "needle").map(_.path.stripPrefix(d + "/")).sorted,
      List(".git/config", "keep.txt", "noise.log"))

  // -- whole-content fast path (stage 4) -------------------------------------
  // Line/CR/backslash bytes are built from N.toChar (10=LF, 13=CR, 92=`\`) so no
  // raw control byte or backslash escape lives in this source.

  tmp.test("fast path rejects a cross-line candidate confirmed per line"): d =>
    // x[^a]y matches across the newline in whole content ([^a] eats the LF), but
    // no single line contains x[^a]y — both paths must report nothing.
    write(d, "f.txt", "x" + 10.toChar + "y")
    assertEquals(Grep.search(d, "x[^a]y"), Nil)

  tmp.test("^$ finds a blank line but never a phantom line past a trailing newline"): d =>
    // "a\n" is one line "a" (no blank line) -> ^$ matches nothing, not a phantom
    // line after the final newline. "a\n\n" has a blank line 2 -> ^$ matches it.
    write(d, "one.txt", "a" + 10.toChar)
    write(d, "two.txt", "a" + 10.toChar + 10.toChar)
    assertEquals(
      Grep.search(d, "^$").map(m => (m.path.stripPrefix(d + "/"), m.line, m.text)),
      List(("two.txt", 2, ""))
    )

  tmp.test("a zero-width pattern matches each line exactly once"): d =>
    // x* matches empty at every line start; "a\n" is one line, so exactly one hit
    // on line 1 — never a second, phantom hit past the trailing newline.
    val p = write(d, "z.txt", "a" + 10.toChar)
    assertEquals(Grep.searchFile(p, "x*").map(m => (m.line, m.text)), List((1, "a")))

  tmp.test("a bare ^ anchor is hazard-routed and still anchors per line"): d =>
    // ^ is routed to the reference (an unanchored fast-path scan can't reproduce
    // per-line ^ without MULTILINE); the reference anchors per line, so ^ok
    // matches line 3 — NOT only line 1, which a plain fast-path scan would give.
    val p = write(d, "anc.txt", "no ok" + 10.toChar + "still no" + 10.toChar + "ok yes")
    assertEquals(Grep.searchFile(p, "^ok").map(m => (m.line, m.text)), List((3, "ok yes")))

  tmp.test("a bare $ anchor is hazard-routed and still anchors per line"): d =>
    val p = write(d, "end.txt", "ends here" + 10.toChar + "not this" + 10.toChar + "also here")
    assertEquals(
      Grep.searchFile(p, "here$").map(m => (m.line, m.text)),
      List((1, "ends here"), (3, "also here"))
    )

  tmp.test("a negated class keeps the fast path — [^a] is not mistaken for an anchor"): d =>
    // [^a]'s ^ is class negation, not an anchor, so this stays on the fast path.
    // "b" (line 2) and the space/letters elsewhere match; "a" alone does not.
    val p = write(d, "neg.txt", "a" + 10.toChar + "b" + 10.toChar + "aaa")
    assertEquals(Grep.searchFile(p, "[^a]").map(m => (m.line, m.text)), List((2, "b")))

  tmp.test("a backslash-A pattern is hazard-routed and anchors per line, not per file"): d =>
    // \A is whole-input on the fast path but per-line on the reference; routing it
    // to the reference makes it match EVERY line starting "ab", not just line 1.
    val backslashAab = "" + 92.toChar + "Aab" // the regex \Aab
    val p = write(d, "h.txt", "abc" + 10.toChar + "xab" + 10.toChar + "abd")
    assertEquals(Grep.searchFile(p, backslashAab).map(_.line), List(1, 3))

  tmp.test("a lookbehind pattern is hazard-routed and matches per line"): d =>
    // (?<=a)b -> a 'b' preceded by 'a'. Lookbehind is hazard-routed to the
    // reference, which handles it per line.
    val p = write(d, "lb.txt", "ab" + 10.toChar + "xb" + 10.toChar + "cab")
    assertEquals(Grep.searchFile(p, "(?<=a)b").map(m => (m.line, m.text)), List((1, "ab"), (3, "cab")))

  tmp.test("CR content is routed to the reference and split on CR like before"): d =>
    // Lone-CR separators: Lines.split breaks on them, but the LF-only fast path
    // would not. The CR gate sends this file to the reference, so both lines show.
    val p = write(d, "cr.txt", "a" + 13.toChar + "b")
    assertEquals(Grep.searchFile(p, "[ab]").map(m => (m.line, m.text)), List((1, "a"), (2, "b")))

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
