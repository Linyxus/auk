package auk.library

/** [[FsFile]] / `FsFileImpl`: reading (read/rawContent/lines/lineCount/size/ext),
  * writing (write/append/touch), line-range editing against a read-pinned frame
  * (patch/insertAfter), and per-file grep — all against a real file in a fresh
  * temp directory. */
class FsFileSuite extends LibSuite:

  // -- read (printed, addressed as N#hh) -------------------------------------

  tmp.test("read prints every line with its line number and content hash"): d =>
    val f = d.file("w.txt"); f.write("hello\nworld")
    assertEquals(captured(f.read()), "1#cr@ hello\n2#k3@ world\n")

  tmp.test("read of an empty file prints nothing"): d =>
    val f = d.file("empty.txt"); f.write("")
    assertEquals(captured(f.read()), "")

  tmp.test("read prints a window with absolute line numbers"): d =>
    val f = d.file("win.txt"); f.write("a\nb\nc\nd")
    assertEquals(captured(f.read(2, 2)), "2#b9@ b\n3#ma@ c\n")

  tmp.test("read limit -1 reads from the offset to the end"): d =>
    val f = d.file("toend.txt"); f.write("a\nb\nc\nd")
    assertEquals(captured(f.read(3, -1)), "3#ma@ c\n4#5f@ d\n")

  tmp.test("read clamps an offset below 1 up to the first line"): d =>
    val f = d.file("clamp.txt"); f.write("a\nb")
    assertEquals(captured(f.read(0)), "1#8c@ a\n2#b9@ b\n")

  tmp.test("read past the end prints nothing"): d =>
    val f = d.file("past.txt"); f.write("a\nb")
    assertEquals(captured(f.read(99)), "")

  tmp.test("read with a zero limit prints nothing"): d =>
    val f = d.file("lim0.txt"); f.write("a\nb\nc")
    assertEquals(captured(f.read(1, 0)), "")

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

  tmp.test("lone-CR (classic Mac) line endings split into separate lines"): d =>
    val f = d.file("cr.txt"); f.write("l1\rl2\rl3")
    assertEquals(f.lines, List("l1", "l2", "l3"))
    assertEquals(f.lineCount, 3)

  tmp.test("a file holding just a carriage return is one empty line"): d =>
    val f = d.file("cronly.txt"); f.write("\r")
    assertEquals(f.lines, List(""))
    assertEquals(f.lineCount, 1)
    assertEquals(f.size, 1L)

  tmp.test("mixed CR, LF, and CRLF endings all split"): d =>
    val f = d.file("mixed.txt"); f.write("a\rb\nc\r\nd")
    assertEquals(f.lines, List("a", "b", "c", "d"))

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

  tmp.test("a trailing dot yields an empty extension"): d =>
    assertEquals(d.file("foo.").ext, "")

  tmp.test("a dotfile with an extension keeps the trailing extension"): d =>
    assertEquals(d.file(".env.local").ext, "local")

  tmp.test("size of a non-existent file fails with a clear message"): d =>
    interceptContains("cannot stat")(d.file("ghost.txt").size)

  // -- binary guard (a NUL byte refuses every text decode) ---------------------

  // n e e d l e NUL x — the NUL at offset 6 marks the file binary, the same
  // heuristic the grep engine uses to skip files.
  private def binaryBytes = Array[Byte](110, 101, 101, 100, 108, 101, 0, 120)

  tmp.test("read of a binary file fails hard and prints nothing"): d =>
    writeBytes(d.path / "bin.dat", binaryBytes)
    val f = d.file("bin.dat")
    val out = captured(interceptContains("looks binary")(f.read()))
    assertEquals(out, "")

  tmp.test("the binary refusal names the file and the NUL's offset"): d =>
    writeBytes(d.path / "bin.dat", binaryBytes)
    val ex = interceptContains("looks binary")(d.file("bin.dat").read())
    val msg = Option(ex.getMessage).getOrElse("")
    assert(msg.contains("bin.dat"), s"message does not name the file: '$msg'")
    assert(msg.contains("offset 6"), s"message does not give the offset: '$msg'")

  tmp.test("rawContent, lines and lineCount refuse a binary file the same way"): d =>
    writeBytes(d.path / "bin.dat", binaryBytes)
    val f = d.file("bin.dat")
    interceptContains("looks binary")(f.rawContent)
    interceptContains("looks binary")(f.lines)
    interceptContains("looks binary")(f.lineCount)

  tmp.test("patch and insertAfter refuse a binary file before looking at the ref"): d =>
    writeBytes(d.path / "bin.dat", binaryBytes)
    val f = d.file("bin.dat")
    interceptContains("looks binary")(f.patch("1#ab", "text"))
    interceptContains("looks binary")(f.insertAfter("0", "text"))

  tmp.test("size and ext still work on a binary file"): d =>
    writeBytes(d.path / "bin.dat", binaryBytes)
    val f = d.file("bin.dat")
    assertEquals(f.size, 8L)
    assertEquals(f.ext, "dat")

  tmp.test("write replaces binary content and the file reads again"): d =>
    writeBytes(d.path / "bin.dat", binaryBytes)
    val f = d.file("bin.dat")
    interceptContains("looks binary")(f.read())
    f.write("plain text")
    assertEquals(f.lines, List("plain text"))

  tmp.test("a text file with multi-byte characters is not mistaken for binary"): d =>
    val f = d.file("utf8.txt"); f.write("café — naïve\n")
    assertEquals(f.lineCount, 1)

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

  // -- patch / insertAfter (addressing by the token you were shown) -----------

  /** `l1\nl2\n…\ln` — content whose every line names itself, so an assertion on
    * the result says which original line ended up where. */
  private def numbered(n: Int): String = (1 to n).map(i => s"l$i").mkString("\n")

  /** The `N#hh` token printed for the line whose content is exactly `content` —
    * the only place an address legitimately comes from. */
  private def token(printed: String, content: String): String =
    val line = printed.linesIterator
      .find(_.endsWith(s"@ $content"))
      .getOrElse(fail(s"no line holding '$content' in:\n$printed"))
    line.take(line.indexOf('@'))

  /** An edit's two channels: everything it printed, and the head line it returned. */
  private def edit(body: => String): (String, String) =
    var head = ""
    val printed = captured { head = body }
    (printed, head)

  tmp.test("a patch replaces the line, printing the region with fresh tokens"): d =>
    val f = d.file("t-one.txt"); f.write("a\nb\nc\nd")
    val shown = captured(f.read())
    val (printed, head) = edit(f.patch(token(shown, "b"), "B1\nB2"))
    assertEquals(f.rawContent, "a\nB1\nB2\nc\nd")
    assertEquals(head, "patched lines 2-2 (1 line) with 2 lines; file now has 5 lines")
    assertEquals(
      printed,
      """patched lines 2-2 (1 line) with 2 lines; file now has 5 lines
        |1#8c@ a
        |2#n0@ B1
        |3#px@ B2
        |4#ma@ c
        |""".stripMargin
    )

  tmp.test("what a patch prints can be patched again straight away"): d =>
    val f = d.file("t-echo.txt"); f.write("a\nb\nc")
    val shown = captured(f.read())
    val (printed, _) = edit(f.patch(token(shown, "b"), "B1\nB2"))
    captured(f.patch(token(printed, "B2"), "B2x")) // no re-read: the echo is addressable
    assertEquals(f.lines, List("a", "B1", "B2x", "c"))

  tmp.test("a token still finds its line after an earlier patch moved it"): d =>
    val f = d.file("t-drift.txt"); f.write(numbered(10))
    val shown = captured(f.read())
    val eight = token(shown, "l8")
    captured(f.patch(token(shown, "l2"), "X\nY\nZ")) // everything below drifts down two
    captured(f.patch(eight, "EIGHT")) // the token says line 8; the content is at 10
    assertEquals(f.lines, List("l1", "X", "Y", "Z", "l3", "l4", "l5", "l6", "l7", "EIGHT", "l9", "l10"))

  tmp.test("a token from an early window survives a later read of another window"): d =>
    // The hole this addressing closes: with numbers alone, the second read would
    // redefine what "line 3" means and the held token would hit the wrong line.
    val f = d.file("t-interleave.txt"); f.write(numbered(20))
    val early = captured(f.read(1, 5))
    val three = token(early, "l3")
    captured(f.patch(token(early, "l1"), "A1\nA2\nA3")) // grow above it
    val later = captured(f.read(10, 3)) // a later read of somewhere else entirely
    captured(f.patch(token(later, "l9"), "NINE"))
    captured(f.patch(three, "THREE")) // still lands on l3, wherever it now sits
    assertEquals(f.lines.take(6), List("A1", "A2", "A3", "l2", "THREE", "l4"))
    assertEquals(f.lines(10), "NINE")

  tmp.test("a repeated line is told apart by the lines it was shown with"): d =>
    val f = d.file("t-dup.txt"); f.write("a\n}\nb\n}\nc")
    val shown = captured(f.read())
    val second = shown.linesIterator.toList(3) // the second `}`
    captured(f.patch(second.take(second.indexOf('@')), "CLOSE"))
    assertEquals(f.lines, List("a", "}", "b", "CLOSE", "c"))

  tmp.test("an address that could mean two places is refused, showing the candidates"): d =>
    val f = d.file("t-amb.txt"); f.write("x\nx\ny")
    val shown = captured(f.read(1, 1)) // a one-line window: no neighbours remembered
    val ref = token(shown, "x")
    val ex = interceptContains("nothing tells the copies apart")(f.patch(ref, "Z"))
    assert(ex.getMessage.contains("1#kn@ x"), ex.getMessage)
    assert(ex.getMessage.contains("2#kn@ x"), ex.getMessage)
    assertEquals(f.rawContent, "x\nx\ny") // nothing was changed on a guess

  tmp.test("the candidates a refusal prints are themselves addressable"): d =>
    val f = d.file("t-amb2.txt"); f.write("x\nx\ny")
    val shown = captured(f.read(1, 1))
    val ex = intercept[RuntimeException](f.patch(token(shown, "x"), "Z"))
    // The refusal showed each candidate in context, which is exactly the evidence
    // that tells them apart — so its own tokens resolve.
    captured(f.patch("2#kn", "SECOND"))
    assertEquals(f.lines, List("x", "SECOND", "y"))

  tmp.test("a range moves as a block, and an edit elsewhere does not disturb it"): d =>
    val f = d.file("t-block.txt"); f.write(numbered(10))
    val shown = captured(f.read())
    val (from, to) = (token(shown, "l6"), token(shown, "l8"))
    writeText(d.path / "t-block.txt", "NEW\n" + numbered(10)) // someone prepends a line
    captured(f.patch(from, to, "SIX\nSEVEN"))
    assertEquals(f.lines, List("NEW", "l1", "l2", "l3", "l4", "l5", "SIX", "SEVEN", "l9", "l10"))

  tmp.test("a token for a line a block patch swallowed cannot mistarget elsewhere"): d =>
    val f = d.file("t-mistarget.txt")
    // Two identical `}` lines: one inside the region about to be patched, one far off.
    f.write("head\n}\nmiddle\ntail\n}\nend")
    val shown = captured(f.read())
    val brace = shown.linesIterator.toList(1) // the FIRST `}`, shown at line 2
    val braceRef = brace.take(brace.indexOf('@'))
    captured(f.insertAfter("0", "NEW")) // everything drifts down one
    // Replace the block CONTAINING that `}` without naming it: the addressed refs
    // are its neighbours, so the line it named dies while the caller still holds a
    // token for it.
    captured(f.patch(token(shown, "head"), token(shown, "middle"), "REPLACED"))
    assertEquals(f.lines, List("NEW", "REPLACED", "tail", "}", "end"))
    // The one `}` left is a DIFFERENT line, which the caller never addressed.
    // Patching it would be a silent mistarget, so this has to refuse.
    intercept[RuntimeException](f.patch(braceRef, "OOPS"))
    assertEquals(f.lines, List("NEW", "REPLACED", "tail", "}", "end"))

  tmp.test("a token from an earlier read cannot mistarget onto a look-alike line"): d =>
    val f = d.file("t-crossrun.txt")
    f.write("head\n}\nmiddle\ntail\n}\nend")
    val first = captured(f.read()) // `}` shown here at line 2 ...
    val stale = first.linesIterator.toList(1)
    val staleRef = stale.take(stale.indexOf('@'))
    captured(f.insertAfter("0", "NEW"))
    val second = captured(f.read()) // ... and here at line 3: a different token
    captured(f.patch(token(second, "head"), token(second, "middle"), "REPLACED"))
    // The current token for that `}` was killed with the block. The stale one from
    // the first read was not, and a look-alike `}` is still in the file — but
    // nothing about it confirms it is the line the stale token named.
    intercept[RuntimeException](f.patch(staleRef, "OOPS"))
    assertEquals(f.lines, List("NEW", "REPLACED", "tail", "}", "end"))

  tmp.test("a line unique when it was shown still relocates freely"): d =>
    val f = d.file("t-unique.txt"); f.write(numbered(6))
    val shown = captured(f.read())
    val five = token(shown, "l5")
    captured(f.insertAfter("0", "NEW")) // drift, and no look-alikes anywhere
    captured(f.patch(five, "FIVE")) // still resolves on content alone
    assertEquals(f.lines, List("NEW", "l1", "l2", "l3", "l4", "FIVE", "l6"))

  tmp.test("a range whose block has been broken up is refused"): d =>
    val f = d.file("t-broken.txt"); f.write(numbered(6))
    val shown = captured(f.read())
    val (from, to) = (token(shown, "l2"), token(shown, "l4"))
    writeText(d.path / "t-broken.txt", "l1\nl2\nMID\nl3\nl4\nl5\nl6")
    val ex = interceptContains("no longer together")(f.patch(from, to, "X"))
    assert(ex.getMessage.contains("MID"), ex.getMessage) // and shows what is there now
    assertEquals(f.lineCount, 7) // untouched

  tmp.test("a range whose ends come from different reads is refused"): d =>
    val f = d.file("t-runs.txt"); f.write(numbered(10))
    val a = token(captured(f.read(1, 3)), "l2")
    val b = token(captured(f.read(5, 3)), "l6")
    interceptContains("different reads")(f.patch(a, b, "X"))
    assertEquals(f.lineCount, 10)

  tmp.test("a token you already replaced says so, and shows what took its place"): d =>
    val f = d.file("t-tomb.txt"); f.write("a\nb\nc")
    val shown = captured(f.read())
    val b = token(shown, "b")
    captured(f.patch(b, "B1\nB2"))
    val ex = interceptContains("you replaced")(f.patch(b, "again"))
    assert(ex.getMessage.contains("B1"), ex.getMessage)
    assertEquals(f.lines, List("a", "B1", "B2", "c")) // and changed nothing

  tmp.test("a line that was changed behind your back is refused, not patched by number"): d =>
    val f = d.file("t-gone.txt"); f.write("a\nb\nc")
    val shown = captured(f.read())
    val b = token(shown, "b")
    writeText(d.path / "t-gone.txt", "a\nSOMEONE ELSE\nc")
    val ex = interceptContains("no longer in the file")(f.patch(b, "B"))
    assert(ex.getMessage.contains("SOMEONE ELSE"), ex.getMessage)
    // Distinct from the tombstone case: this is not a line you replaced yourself.
    assert(ex.getMessage.contains("not a line you replaced yourself"), ex.getMessage)
    assertEquals(f.rawContent, "a\nSOMEONE ELSE\nc") // their edit is intact

  tmp.test("an edit elsewhere in the file is simply irrelevant"): d =>
    val f = d.file("t-tolerant.txt"); f.write(numbered(5))
    val shown = captured(f.read())
    val three = token(shown, "l3")
    writeText(d.path / "t-tolerant.txt", "PREPENDED\n" + numbered(5) + "\nAPPENDED")
    captured(f.patch(three, "THREE")) // no refusal: nothing it addressed was touched
    assertEquals(f.lines, List("PREPENDED", "l1", "l2", "THREE", "l4", "l5", "APPENDED"))

  tmp.test("insertAfter(\"0\") writes at the top; a token inserts after that line"): d =>
    val f = d.file("t-ins.txt"); f.write("a\nb")
    val shown = captured(f.read())
    val (printed, head) = edit(f.insertAfter("0", "top"))
    assertEquals(head, "inserted 1 line after line 0; file now has 3 lines")
    assertEquals(printed, "inserted 1 line after line 0; file now has 3 lines\n1#j0@ top\n2#8c@ a\n")
    captured(f.insertAfter(token(shown, "b"), "bottom"))
    assertEquals(f.lines, List("top", "a", "b", "bottom"))

  tmp.test("write retires the tokens it invalidates; append leaves them working"): d =>
    val f = d.file("t-inval.txt"); f.write("a\nb")
    val shown = captured(f.read())
    val b = token(shown, "b")
    f.append("\nc") // everything above is untouched, so the token still names its line
    captured(f.patch(b, "B"))
    assertEquals(f.lines, List("a", "B", "c"))
    f.write("a\nB\nc") // a wholesale rewrite retires everything shown of the old file
    interceptContains("no line was shown")(f.patch(b, "again"))

  tmp.test("a bare line number is not an address"): d =>
    val f = d.file("t-bare.txt"); f.write("a\nb")
    captured(f.read())
    val ex = interceptContains("not a line reference")(f.patch("2", "B"))
    assert(ex.getMessage.contains("N#hh"), ex.getMessage)
    interceptContains("not a line reference")(f.patch("2#", "B"))
    interceptContains("not a line reference")(f.insertAfter("b", "B"))
    assertEquals(f.rawContent, "a\nb")

  tmp.test("an unknown token suggests the ones the line was shown as"): d =>
    val f = d.file("t-didyoumean.txt"); f.write("a\nb")
    captured(f.read())
    val ex = interceptContains("did you mean")(f.patch("2#zz", "B"))
    assert(ex.getMessage.contains("2#b9"), ex.getMessage)
    assert(ex.getMessage.contains("'b'"), ex.getMessage)

  tmp.test("a token for a line never shown asks for a read"): d =>
    val f = d.file("t-unseen.txt"); f.write("a\nb")
    interceptContains("read the file")(f.patch("2#b9", "B"))

  tmp.test("a file of any size is patchable: addressing costs nothing per byte"): d =>
    val f = d.file("t-huge.txt")
    // Well past the 4 MB a whole-file frame used to be capped at: nothing is
    // remembered per byte now, only per line actually displayed.
    f.write("first\n" + "x".repeat(4 * 1024 * 1024) + "\nlast")
    val shown = captured(f.read(1, 1))
    captured(f.patch(token(shown, "first"), "FIRST"))
    assertEquals(f.lines.head, "FIRST")
    assertEquals(f.lines.last, "last")

  tmp.test("a file remembers only its most recent sightings"): d =>
    val f = d.file("t-lru.txt")
    f.write((1 to 6000).map(i => s"line $i").mkString("\n"))
    val early = captured(f.read(1, 10))
    val ref = token(early, "line 5")
    captured(f.read(1000, 5000)) // pushes the first window out of the store
    interceptContains("no line was shown")(f.patch(ref, "x"))

  // -- patch / insertAfter: payload and byte fidelity -------------------------

  tmp.test("an empty replacement deletes the line"): d =>
    val f = d.file("t-del.txt"); f.write("a\nb\nc\nd")
    val shown = captured(f.read())
    val (_, head) = edit(f.patch(token(shown, "b"), token(shown, "c"), ""))
    assertEquals(f.rawContent, "a\nd")
    assertEquals(head, "patched lines 2-3 (2 lines) with 0 lines; file now has 2 lines")

  tmp.test("a trailing newline in the replacement adds no phantom line"): d =>
    val f = d.file("t-tn.txt"); f.write("a\nb")
    val shown = captured(f.read())
    captured(f.patch(token(shown, "a"), "x\ny\n"))
    assertEquals(f.lines, List("x", "y", "b"))

  tmp.test("blank lines inside the replacement are kept"): d =>
    val f = d.file("t-blank.txt"); f.write("a\nb")
    val shown = captured(f.read())
    captured(f.patch(token(shown, "a"), "x\n\ny"))
    assertEquals(f.lines, List("x", "", "y", "b"))

  tmp.test("a long replacement is echoed with its middle elided"): d =>
    val f = d.file("t-long.txt"); f.write("a\nb")
    val shown = captured(f.read())
    val (printed, head) = edit(f.patch(token(shown, "a"), (1 to 20).map(i => s"n$i").mkString("\n")))
    assertEquals(head, "patched lines 1-1 (1 line) with 20 lines; file now has 21 lines")
    val body = printed.linesIterator.toList
    assert(body.contains("…"), printed)
    assert(body.exists(_.endsWith("@ n1")), printed)
    assert(body.exists(_.endsWith("@ n20")), printed)
    assert(!body.exists(_.endsWith("@ n10")), printed) // the middle is not shown
    // Head and tail are recorded as two runs — they are not adjacent in the file,
    // and one run claiming they were would feed a later tie-break a neighbour that
    // does not exist. Both blocks are addressable ...
    captured(f.patch(token(printed, "n20"), "LAST"))
    assertEquals(f.lines(19), "LAST")
    captured(f.patch(token(printed, "n1"), "FIRST"))
    assertEquals(f.lines.head, "FIRST")
    // ... and the elided middle is not: it was never shown, so it has no token.
    assertEquals(f.lines(9), "n10") // it is line 10, and `10#ao` would name it ...
    interceptContains("no line was shown")(f.patch("10#ao", "x")) // ... if it had been shown

  tmp.test("a patched CRLF file keeps every byte outside the range, endings included"): d =>
    val f = d.file("t-crlf.txt"); f.write("a\r\nb\r\nc\r\n")
    val shown = captured(f.read())
    captured(f.patch(token(shown, "b"), "B1\nB2")) // the payload's own newline is not the file's
    assertEquals(f.rawContent, "a\r\nB1\r\nB2\r\nc\r\n")

  tmp.test("a file of mixed endings keeps each one it did not touch"): d =>
    val f = d.file("t-mixed.txt"); f.write("a\r\nb\nc\rd")
    val shown = captured(f.read())
    assertEquals(f.lines, List("a", "b", "c", "d"))
    captured(f.patch(token(shown, "c"), "C")) // line 3 ends with a lone CR
    assertEquals(f.rawContent, "a\r\nb\nC\rd")

  tmp.test("an unterminated last line stays unterminated when patched"): d =>
    val f = d.file("t-noterm.txt"); f.write("a\nb")
    val shown = captured(f.read())
    captured(f.patch(token(shown, "b"), "B1\nB2"))
    assertEquals(f.rawContent, "a\nB1\nB2")

  tmp.test("appending after an unterminated last line closes that line first"): d =>
    val f = d.file("t-append.txt"); f.write("a\nb")
    val shown = captured(f.read())
    captured(f.insertAfter(token(shown, "b"), "x"))
    assertEquals(f.rawContent, "a\nb\nx") // the one byte an insertion adds outside its range

  tmp.test("appending after a terminated last line keeps the file terminated"): d =>
    val f = d.file("t-append2.txt"); f.write("a\nb\n")
    val shown = captured(f.read())
    captured(f.insertAfter(token(shown, "b"), "x"))
    assertEquals(f.rawContent, "a\nb\nx\n")

  tmp.test("the file keeps its trailing newline across a patch"): d =>
    val f = d.file("t-keepnl.txt"); f.write("a\nb\n")
    val shown = captured(f.read())
    captured(f.patch(token(shown, "a"), "A"))
    assertEquals(f.rawContent, "A\nb\n")

  tmp.test("deleting the last lines leaves the line above them exactly as it was"): d =>
    val f = d.file("t-deltail.txt"); f.write("a\nb\nc")
    val shown = captured(f.read())
    captured(f.patch(token(shown, "b"), token(shown, "c"), ""))
    assertEquals(f.rawContent, "a\n")

  tmp.test("read's numbering and lines agree on every line-ending shape"): d =>
    val f = d.file("t-seg.txt")
    val shapes =
      List("", "\n", "\r", "a", "a\n", "a\nb", "a\r\nb\r\n", "l1\rl2\rl3", "a\rb\nc\r\nd", "a\n\n", "a\n\nb\n")
    for c <- shapes do
      f.write(c)
      val rows = captured(f.read()).linesIterator.toList
      val shape = c.replace("\r", "CR").replace("\n", "LF")
      assertEquals(rows.map(r => r.substring(r.indexOf("@ ") + 2)), f.lines, s"content: $shape")
      assertEquals(rows.map(r => r.take(r.indexOf('#')).toInt), (1 to f.lineCount).toList, s"content: $shape")

  tmp.test("any handle to the file can patch what any other displayed"): d =>
    d.file("t-shared.txt").write("a\nb\nc\nd")
    val shown = captured(d.file("t-shared.txt").read()) // shown through one handle ...
    captured(d.file("t-shared.txt").patch(token(shown, "b"), "B")) // ... patched through another
    captured((d.path / "t-shared.txt").openAsFile.patch(token(shown, "c"), "C"))
    captured(d.file("t-shared.txt").grep("B").matches.head.file.patch(token(shown, "d"), "D"))
    assertEquals(d.file("t-shared.txt").lines, List("a", "B", "C", "D"))

  tmp.test("a refused patch leaves everything else addressable"): d =>
    val f = d.file("t-refused.txt"); f.write("a\nb")
    val shown = captured(f.read())
    interceptContains("not a line reference")(f.patch("nonsense", "A"))
    captured(f.patch(token(shown, "a"), "A")) // the refusal cost nothing
    assertEquals(f.lines, List("A", "b"))

  // -- grep ------------------------------------------------------------------

  tmp.test("grep returns one Match per matching line with 1-based line numbers"): d =>
    val f = d.file("g.txt"); f.write("foo\nbar\nfoobar")
    val ms = f.grep("foo").matches
    assertEquals(ms.map(_.lineNumber), List(1, 3))
    assertEquals(ms.map(_.line), List("foo", "foobar"))

  tmp.test("grep honours regular-expression syntax"): d =>
    val f = d.file("gr.txt"); f.write("a1\nb2\ncc")
    assertEquals(f.grep("[0-9]").matches.map(_.lineNumber), List(1, 2))

  tmp.test("grep matches a substring, not the whole line"): d =>
    val f = d.file("sub.txt"); f.write("hello world")
    assertEquals(f.grep("wor").matches.map(_.line), List("hello world"))

  tmp.test("grep that finds nothing is empty"): d =>
    val f = d.file("none.txt"); f.write("abc")
    val r = f.grep("zzz")
    assert(r.isEmpty)
    assertEquals(r.length, 0)
    assertEquals(r.matches, Nil)

  tmp.test("grep of a malformed pattern raises a clear error"): d =>
    val f = d.file("bad.txt"); f.write("abc")
    interceptContains("invalid regular expression")(f.grep("(unclosed"))

  tmp.test("a Match points back at the file and renders as path:line@ content"): d =>
    val f = d.file("mt.txt"); f.write("hello")
    val m = f.grep("ell").matches.head
    assertEquals(m.file, f)
    assertEquals(m.lineNumber, 1)
    assertEquals(m.toString, s"${f.path}:1@ hello")

  tmp.test("grep reports the correct 1-based number for a non-first line"): d =>
    val f = d.file("ml.txt"); f.write("a\nb\nNEEDLE\nd")
    val m = f.grep("NEEDLE").matches.head
    assertEquals(m.lineNumber, 3)
    assertEquals(m.toString, s"${f.path}:3@ NEEDLE")

  tmp.test("grep with an empty pattern matches every line, including blanks"): d =>
    val f = d.file("ge.txt"); f.write("a\n\nb")
    assertEquals(f.grep("").matches.map(_.lineNumber), List(1, 2, 3))
