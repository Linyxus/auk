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

  /** Write exact bytes — for content no `String` could express, such as a lone
    * 0x80 that is not valid UTF-8 and decodes to U+FFFD. */
  private def writeBytes(dir: String, rel: String, bytes: Int*): String =
    val p = join(dir, rel)
    fs.mkdirSync(Node.path.dirname(p), js.Dynamic.literal(recursive = true))
    fs.writeFileSync(p, js.Dynamic.global.Buffer.from(js.Array(bytes*)))
    p

  /** Run `body` with the engine's literal prefilter forced on or off, restoring
    * the flag afterwards. Production never touches it; tests use it to run the
    * exact same engine both ways and assert the results are identical. */
  private def withPrefilter[A](on: Boolean)(body: => A): A =
    val saved = Grep.prefilterEnabled
    Grep.prefilterEnabled = on
    try body
    finally Grep.prefilterEnabled = saved

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

  tmp.test("a lookbehind pattern is hazard-routed, or errors clearly where unsupported"): d =>
    // (?<=a)b -> a 'b' preceded by 'a'. Lookbehind is hazard-routed to the
    // per-line reference — but Scala.js can only COMPILE lookbehind under an
    // ES2018+ linker target, and this project (like the REPL worker) links
    // below that. Pin the contract in either environment: where the regex
    // compiles, matching is per line; where it does not, the engine raises its
    // clear "invalid regular expression" error, never a raw engine exception.
    val supported =
      try { "(?<=a)b".r; true }
      catch case _: Throwable => false
    val p = write(d, "lb.txt", "ab" + 10.toChar + "xb" + 10.toChar + "cab")
    if supported then
      assertEquals(Grep.searchFile(p, "(?<=a)b").map(m => (m.line, m.text)), List((1, "ab"), (3, "cab")))
    else
      val ex = intercept[RuntimeException](Grep.searchFile(p, "(?<=a)b"))
      assert(ex.getMessage.contains("invalid regular expression"), ex.getMessage)

  tmp.test("CR content is routed to the reference and split on CR like before"): d =>
    // Lone-CR separators: Lines.split breaks on them, but the LF-only fast path
    // would not. The CR gate sends this file to the reference, so both lines show.
    val p = write(d, "cr.txt", "a" + 13.toChar + "b")
    assertEquals(Grep.searchFile(p, "[ab]").map(m => (m.line, m.text)), List((1, "a"), (2, "b")))

  // -- literal prefilter (stage 5) -------------------------------------------

  test("requiredLiteral pins: what the prefilter will and will not extract"):
    // Each case is (pattern, the literal every match must contain). `None` means
    // no prefilter at all — either nothing long enough is required, or the
    // pattern holds a construct the extractor refuses to reason about.
    val cases: List[(String, Option[String])] = List(
      // -- runs, and how they are chosen
      ("foobar", Some("foobar")),                 // one plain run
      ("foo.*bar", Some("foo")),                  // two 3-char runs: the tie goes to the earlier
      ("foo(bar)?bazz", Some("bazz")),            // a group contributes nothing; the longer run wins
      ("ab", None),                               // shorter than the 3-char minimum
      ("", None),
      (".*", None),
      ("a.b.c", None),                            // runs of one
      // -- quantifiers
      ("fo?obar", Some("obar")),                  // `?` drops the character it follows
      ("x{0,3}yzw", Some("yzw")),                 // a zero minimum drops it too
      ("foo+", Some("foo")),                      // `+` keeps one, then ends the run
      ("abc{2}d", Some("abc")),                   // `{2}` likewise: "abcd" is NOT required
      ("a{2}b", None),                            // ... leaving two runs of one
      // -- what breaks a run vs. what abandons extraction
      ("\\d{3}-\\d{4}", None),                    // predefined classes break; nothing long survives
      ("[abc]def", Some("def")),                  // a class breaks the run
      ("foo\\.bar", Some("foo.bar")),             // an escaped metacharacter IS the character
      ("^import\\b", Some("import")),             // anchors and boundaries break the run
      ("foo$", Some("foo")),
      ("\\bword\\b", Some("word")),
      ("(foo)bar", Some("bar")),                  // a group's contents are never literal
      ("(?:xy)abc", Some("abc")),                 // ... including a non-capturing one
      ("(?:a|b)hello", Some("hello")),            // an alternation INSIDE a group is fine
      ("a|bc", None),                             // ... but a top-level one requires nothing
      ("(?i)foo", None),                          // inline flags: give up
      ("(?=foo)bar", None),                       // lookaround: give up
      ("((?i)x)abcd", None),                      // ... at any depth
      ("foo\\Qbar\\E", None),                     // an escape with no plain meaning: give up
      ("x[]y", None),                             // `[]`: an empty class in JS, a class of `]` in Java
      ("x[a[b]y", None),                          // a nested class: Java syntax, plain chars in JS
      ("ab{x}cd", None),                          // a brace that is not a quantifier: dialects differ
      // -- non-ASCII: the needle is encoded to UTF-8, so this is 9 bytes, not 3
      ("日本語のテキスト", Some("日本語のテキスト")),
      // -- the U+FFFD guard is NOT here: extraction yields the literal, and the
      // encode step (which alone can see the hazard) drops it. See the search
      // test below.
      ("ab" + 0xFFFD.toChar + "cd", Some("ab" + 0xFFFD.toChar + "cd"))
    )
    assertEquals(cases.map((p, _) => (p, Grep.requiredLiteral(p))), cases)

  tmp.test("the prefilter skips files that cannot hold the required literal"): d =>
    write(d, "hit.txt", "needle here")
    write(d, "miss1.txt", "nothing at all")
    write(d, "miss2.txt", "also nothing")
    val before = Grep.prefilterSkips
    assertEquals(Grep.search(d, "needle").map(_.text), List("needle here"))
    assertEquals(Grep.prefilterSkips - before, 2, "both needle-free files should be dropped unread")
    // Forced off, the same search reads everything and still returns the same thing.
    val off = Grep.prefilterSkips
    assertEquals(withPrefilter(false)(Grep.search(d, "needle")).map(_.text), List("needle here"))
    assertEquals(Grep.prefilterSkips - off, 0)

  tmp.test("a pattern with no extractable literal still searches every file"): d =>
    write(d, "a.txt", "alpha")
    write(d, "b.txt", "beta")
    val before = Grep.prefilterSkips
    assertEquals(Grep.search(d, "a|b").map(_.text).sorted, List("alpha", "beta"))
    assertEquals(Grep.prefilterSkips - before, 0)

  tmp.test("a multi-byte literal is prefiltered on its UTF-8 bytes, not its characters"): d =>
    write(d, "jp.txt", "preamble" + 10.toChar + "日本語のテキスト")
    write(d, "ascii.txt", "plain ascii, no match")
    assertEquals(Grep.search(d, "日本語").map(m => (m.line, m.text)), List((2, "日本語のテキスト")))

  tmp.test("a literal containing U+FFFD disables the prefilter instead of dropping the file"): d =>
    // Invalid UTF-8 decodes to U+FFFD, so a pattern containing that character can
    // match content whose RAW bytes hold nothing like the needle's encoding. Here
    // the file is the bytes "ab", 0x80 (invalid on its own), "cd" — it decodes to
    // "ab<FFFD>cd" and must match, though it contains none of that needle's
    // UTF-8 (0xEF 0xBF 0xBD). Without the guard in the encode step, it is skipped.
    writeBytes(d, "invalid.dat", 0x61, 0x62, 0x80, 0x63, 0x64)
    val pattern = "ab" + 0xFFFD.toChar + "cd"
    assertEquals(Grep.search(d, pattern).map(_.path.stripPrefix(d + "/")), List("invalid.dat"))

  tmp.test("a literal straddling the 8 KB head boundary is still found"): d =>
    // The prefilter runs on the whole read buffer, not the sniffed head, so a
    // needle beginning inside the head and ending past it is a hit.
    write(d, "straddle.txt", "x" * 8190 + "needle past the boundary")
    assertEquals(Grep.search(d, "needle past the boundary").map(_.line), List(1))

  tmp.test("prefiltered and unprefiltered searches agree — every path, every content shape"): d =>
    write(d, "src/app.scala", "import foo" + 10.toChar + "  import bar" + 10.toChar + "handler_42 = 1")
    write(d, "src/notes.txt", "TODO the thing" + 10.toChar + "nothing here" + 10.toChar + "foo and bar")
    write(d, "cr.txt", "aa" + 13.toChar + "the needle" + 13.toChar + "zz") // lone-CR: reference path
    write(d, "unicode.txt", "日本語のテキスト" + 10.toChar + "café")
    write(d, "big.txt", "y" * 9000 + 10.toChar + "needle far past the head")
    write(d, "bin.dat", "needle" + 0.toChar + "x")
    writeBytes(d, "invalid.dat", 0x61, 0x62, 0x80, 0x63, 0x64)
    write(d, "empty.txt", "")
    val patterns = List(
      "needle",             // a plain literal
      "^import",            // hazardous (anchor): the reference path is prefiltered too
      "import\\b",          // a boundary after a run
      "handler_[0-9]+",     // a literal then a class
      "TODO|FIXME",         // an alternation: no prefilter
      "日本語",              // a multi-byte needle
      "x[^a]y",             // a cross-line candidate on the fast path
      "\\bthe\\b",
      "a{2}b",
      "foo.*bar",
      "café",
      "ab" + 0xFFFD.toChar + "cd", // the U+FFFD guard, under equivalence
      "nothing-matches-this"
    )
    patterns.foreach { p =>
      assertEquals(withPrefilter(true)(Grep.search(d, p)), withPrefilter(false)(Grep.search(d, p)), p)
      assertEquals(withPrefilter(true)(Grep.searchAll(d, p)), withPrefilter(false)(Grep.searchAll(d, p)), p)
      assertEquals(
        withPrefilter(true)(Grep.search(d, p, "**/*.txt")),
        withPrefilter(false)(Grep.search(d, p, "**/*.txt")),
        p
      )
    }

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
