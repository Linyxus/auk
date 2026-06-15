package auk.library

/** [[FsFile]] / `FsFileImpl`: reading (read/rawContent/lines/lineCount/size/ext),
  * writing (write/append/touch), in-place editing (replace/replaceAll), and
  * per-file grep — all against a real file in a fresh temp directory. */
class FsFileSuite extends LibSuite:

  // -- read (printed, 1-based line numbers) ----------------------------------

  tmp.test("read prints every line prefixed with its 1-based number"): d =>
    val f = d.file("w.txt"); f.write("hello\nworld")
    assertEquals(captured(f.read()), "1@ hello\n2@ world\n")

  tmp.test("read of an empty file prints a single blank line"): d =>
    val f = d.file("empty.txt"); f.write("")
    assertEquals(captured(f.read()), "\n")

  tmp.test("read prints a window with absolute line numbers"): d =>
    val f = d.file("win.txt"); f.write("a\nb\nc\nd")
    assertEquals(captured(f.read(2, 2)), "2@ b\n3@ c\n")

  tmp.test("read limit -1 reads from the offset to the end"): d =>
    val f = d.file("toend.txt"); f.write("a\nb\nc\nd")
    assertEquals(captured(f.read(3, -1)), "3@ c\n4@ d\n")

  tmp.test("read clamps an offset below 1 up to the first line"): d =>
    val f = d.file("clamp.txt"); f.write("a\nb")
    assertEquals(captured(f.read(0)), "1@ a\n2@ b\n")

  tmp.test("read past the end prints nothing"): d =>
    val f = d.file("past.txt"); f.write("a\nb")
    assertEquals(captured(f.read(99)), "\n")

  // -- rawContent / lines / lineCount ----------------------------------------

  tmp.test("rawContent returns the bytes verbatim, including a trailing newline"): d =>
    val f = d.file("raw.txt"); f.write("a\nb\n")
    assertEquals(f.rawContent, "a\nb\n")

  tmp.test("lines splits on newlines with no prefixes"): d =>
    val f = d.file("l.txt"); f.write("a\nb\nc")
    assertEquals(f.lines, List("a", "b", "c"))
    assertEquals(f.lineCount, 3)

  tmp.test("a single trailing newline does not add a phantom final line"): d =>
    val f = d.file("tn.txt"); f.write("a\nb\n")
    assertEquals(f.lines, List("a", "b"))
    assertEquals(f.lineCount, 2)

  tmp.test("a second trailing newline does count as a blank line"): d =>
    val f = d.file("tn2.txt"); f.write("a\nb\n\n")
    assertEquals(f.lines, List("a", "b", ""))

  tmp.test("interior blank lines are preserved"): d =>
    val f = d.file("blank.txt"); f.write("a\n\nb")
    assertEquals(f.lines, List("a", "", "b"))

  tmp.test("CRLF line endings are normalized away"): d =>
    val f = d.file("crlf.txt"); f.write("a\r\nb\r\n")
    assertEquals(f.lines, List("a", "b"))

  tmp.test("an empty file has no lines"): d =>
    val f = d.file("e.txt"); f.write("")
    assertEquals(f.lines, Nil)
    assertEquals(f.lineCount, 0)

  tmp.test("a file holding just a newline is one empty line"): d =>
    val f = d.file("nl.txt"); f.write("\n")
    assertEquals(f.lines, List(""))
    assertEquals(f.lineCount, 1)

  // -- size / ext ------------------------------------------------------------

  tmp.test("size is the byte length, not the character count"): d =>
    val f = d.file("utf8.txt"); f.write("café") // é is two UTF-8 bytes
    assertEquals(f.size, 5L)
    assertEquals(f.rawContent.length, 4)

  tmp.test("ext is the extension without the leading dot"): d =>
    assertEquals(d.file("main.scala").ext, "scala")
    assertEquals(d.file("archive.tar.gz").ext, "gz")

  tmp.test("ext is empty when there is no extension"): d =>
    assertEquals(d.file("noext").ext, "")

  tmp.test("a dotfile has no extension"): d =>
    assertEquals(d.file(".gitignore").ext, "")

  // -- write / append / touch ------------------------------------------------

  tmp.test("write creates a missing file"): d =>
    val f = d.file("new.txt"); f.write("data")
    assert(f.exists)
    assertEquals(f.rawContent, "data")

  tmp.test("write overwrites existing content"): d =>
    val f = d.file("ow.txt"); f.write("first"); f.write("second")
    assertEquals(f.rawContent, "second")

  tmp.test("write fails when the parent directory does not exist"): d =>
    val f = d.dir("missing").file("x.txt")
    intercept[Throwable](f.write("data"))

  tmp.test("append adds to the end, creating the file if missing"): d =>
    val f = d.file("ap.txt"); f.append("x"); f.append("y")
    assertEquals(f.rawContent, "xy")

  tmp.test("touch creates an empty file but leaves existing content alone"): d =>
    val f = d.file("t.txt")
    f.touch()
    assert(f.exists)
    assertEquals(f.rawContent, "")
    f.write("data"); f.touch()
    assertEquals(f.rawContent, "data")

  // -- replace / replaceAll --------------------------------------------------

  tmp.test("replace swaps the single occurrence"): d =>
    val f = d.file("rep.txt"); f.write("a b a"); f.replace("b", "B")
    assertEquals(f.rawContent, "a B a")

  tmp.test("replace fails, and leaves the file untouched, when there is no match"): d =>
    val f = d.file("rep0.txt"); f.write("abc")
    interceptContains("no occurrence")(f.replace("zzz", "q"))
    assertEquals(f.rawContent, "abc")

  tmp.test("replace fails when there is more than one match"): d =>
    val f = d.file("repn.txt"); f.write("a a a")
    interceptContains("exactly one")(f.replace("a", "b"))
    assertEquals(f.rawContent, "a a a")

  tmp.test("replace of an empty target is treated as no occurrence"): d =>
    val f = d.file("repe.txt"); f.write("abc")
    interceptContains("no occurrence")(f.replace("", "x"))

  tmp.test("replaceAll replaces every occurrence and returns the count"): d =>
    val f = d.file("all.txt"); f.write("a a a")
    assertEquals(f.replaceAll("a", "b"), 3)
    assertEquals(f.rawContent, "b b b")

  tmp.test("replaceAll returns 0 and leaves the file alone when nothing matches"): d =>
    val f = d.file("all0.txt"); f.write("xyz")
    assertEquals(f.replaceAll("q", "w"), 0)
    assertEquals(f.rawContent, "xyz")

  tmp.test("replaceAll counts non-overlapping occurrences"): d =>
    val f = d.file("over.txt"); f.write("aaaa")
    assertEquals(f.replaceAll("aa", "b"), 2)
    assertEquals(f.rawContent, "bb")

  tmp.test("replaceAll with an empty target makes no change"): d =>
    val f = d.file("alle.txt"); f.write("abc")
    assertEquals(f.replaceAll("", "x"), 0)
    assertEquals(f.rawContent, "abc")

  // -- grep ------------------------------------------------------------------

  tmp.test("grep returns one Match per matching line with 1-based line numbers"): d =>
    val f = d.file("g.txt"); f.write("foo\nbar\nfoobar")
    val ms = f.grep("foo")
    assertEquals(ms.map(_.lineNumber), List(1, 3))
    assertEquals(ms.map(_.line), List("foo", "foobar"))

  tmp.test("grep honours regular-expression syntax"): d =>
    val f = d.file("gr.txt"); f.write("a1\nb2\ncc")
    assertEquals(f.grep("[0-9]").map(_.lineNumber), List(1, 2))

  tmp.test("grep matches a substring, not the whole line"): d =>
    val f = d.file("sub.txt"); f.write("hello world")
    assertEquals(f.grep("wor").map(_.line), List("hello world"))

  tmp.test("grep that finds nothing returns an empty list"): d =>
    val f = d.file("none.txt"); f.write("abc")
    assertEquals(f.grep("zzz"), Nil)

  tmp.test("a Match points back at the file and renders as path:line@ content"): d =>
    val f = d.file("mt.txt"); f.write("hello")
    val m = f.grep("ell").head
    assertEquals(m.file, f.path)
    assertEquals(m.lineNumber, 1)
    assertEquals(m.toString, s"${f.path}:1@ hello")
