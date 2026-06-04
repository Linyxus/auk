package auk.runtime

import gears.async.Async
import gears.async.default.given

import auk.TestFs
import auk.llm.tools.{RuntimeContext, ApprovalPolicy, ApprovalRequest, ToolResult}

class ReadSuite extends munit.FunSuite:

  private def tempDir(): String = TestFs.tempDir("auk-read")

  private def write(dir: String, name: String, content: String): String =
    val p = TestFs.join(dir, name)
    TestFs.write(p, content)
    p

  private def run(path: String, offset: Option[Int] = None, limit: Option[Int] = None)(using
      dir: String,
      a: Async
  ): ToolResult =
    given RuntimeContext = RuntimeContext(dir)
    Read.execute(ReadParams(path, offset, limit))

  private def asyncTest(name: String)(body: Async ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  asyncTest("numbers every line from 0 with the n@ prefix"):
    given dir: String = tempDir()
    write(dir, "f.txt", "alpha\nbeta\ngamma\n")
    val r = run("f.txt")
    assertEquals(r.isError, false)
    assertEquals(r.output, "0@ alpha\n1@ beta\n2@ gamma")
    assertEquals(r.metadata("totalLines"), "3")

  asyncTest("a file without a trailing newline counts the same number of lines"):
    given dir: String = tempDir()
    write(dir, "f.txt", "alpha\nbeta")
    val r = run("f.txt")
    assertEquals(r.output, "0@ alpha\n1@ beta")
    assertEquals(r.metadata("totalLines"), "2")

  asyncTest("preserves indentation after the single delimiter space"):
    given dir: String = tempDir()
    write(dir, "f.txt", "def f():\n    return 1\n")
    val r = run("f.txt")
    assertEquals(r.output, "0@ def f():\n1@     return 1")

  asyncTest("blank lines render as a bare prefix and keep their number"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\n\nb\n")
    val r = run("f.txt")
    assertEquals(r.output, "0@ a\n1@ \n2@ b")

  asyncTest("offset starts the window but keeps real line numbers"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", offset = Some(2))
    assertEquals(r.output, "2@ c\n3@ d")
    assertEquals(r.metadata("from"), "2")

  asyncTest("limit caps the number of lines read"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", offset = Some(1), limit = Some(2))
    assertEquals(r.output, "1@ b\n2@ c")

  asyncTest("an empty file reports it is empty and points to write"):
    given dir: String = tempDir()
    write(dir, "f.txt", "")
    val r = run("f.txt")
    assertEquals(r.isError, false)
    assert(r.output.contains("empty"), r.output)
    assert(r.output.contains("write"), r.output)
    assertEquals(r.metadata("totalLines"), "0")

  asyncTest("an offset past the end is reported, not an error"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", offset = Some(10))
    assertEquals(r.isError, false)
    assert(r.output.contains("past the end"), r.output)

  asyncTest("a missing file is reported (not an error) and points to write"):
    given dir: String = tempDir()
    val r = run("nope.txt")
    assertEquals(r.isError, false, r.output)
    assert(r.output.contains("does not exist"), r.output)
    assert(r.output.contains("write"), r.output)
    assertEquals(r.metadata("exists"), "false")

  asyncTest("a directory is rejected"):
    given dir: String = tempDir()
    val r = run(".")
    assert(r.isError)
    assert(r.output.contains("directory"), r.output)

  asyncTest("a negative offset is rejected"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\n")
    val r = run("f.txt", offset = Some(-1))
    assert(r.isError)

  // --- additional corner cases -------------------------------------------

  asyncTest("offset 0 is the same as no offset"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", offset = Some(0))
    assertEquals(r.isError, false, r.output)
    assertEquals(r.output, "0@ a\n1@ b\n2@ c")
    assertEquals(r.metadata("from"), "0")
    assertEquals(r.metadata("to"), "2")

  asyncTest("a negative limit is rejected"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", limit = Some(-1))
    assert(r.isError)
    assert(r.output.contains("limit must be >= 0"), r.output)

  asyncTest("limit 0 yields an empty window but is not an error"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", limit = Some(0))
    assertEquals(r.isError, false, r.output)
    // from (0) is within the file, so the past-the-end branch is not taken;
    // the render loop never runs, so the output is the empty string.
    assertEquals(r.output, "")
    assertEquals(r.metadata("totalLines"), "3")
    assertEquals(r.metadata("from"), "0")
    // to == (until-1).max(from) == (0-1).max(0) == 0
    assertEquals(r.metadata("to"), "0")
    assertEquals(r.metadata("truncated"), "false")

  asyncTest("limit 0 at a non-zero offset still reports that offset"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", offset = Some(2), limit = Some(0))
    assertEquals(r.isError, false, r.output)
    assertEquals(r.output, "")
    assertEquals(r.metadata("from"), "2")
    assertEquals(r.metadata("to"), "2")

  asyncTest("a limit larger than the file reads the whole file"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", limit = Some(1000))
    assertEquals(r.output, "0@ a\n1@ b\n2@ c")
    assertEquals(r.metadata("to"), "2")

  asyncTest("offset plus limit that overruns the file is clamped to the end"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", offset = Some(2), limit = Some(10))
    assertEquals(r.output, "2@ c\n3@ d")
    assertEquals(r.metadata("from"), "2")
    assertEquals(r.metadata("to"), "3")

  asyncTest("an interior offset+limit window keeps absolute line numbers"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\ne\n")
    val r = run("f.txt", offset = Some(1), limit = Some(3))
    assertEquals(r.output, "1@ b\n2@ c\n3@ d")
    assertEquals(r.metadata("from"), "1")
    assertEquals(r.metadata("to"), "3")

  asyncTest("offset exactly at EOF is reported as past the end, not empty"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", offset = Some(2))
    assertEquals(r.isError, false, r.output)
    assert(r.output.contains("past the end"), r.output)
    assert(r.output.contains("2 line(s)"), r.output)
    // from is clamped to total
    assertEquals(r.metadata("from"), "2")
    assertEquals(r.metadata("totalLines"), "2")

  asyncTest("offset past EOF clamps from/to metadata to totalLines"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", offset = Some(99))
    assertEquals(r.isError, false, r.output)
    assertEquals(r.metadata("from"), "3")
    assertEquals(r.metadata("to"), "3")

  asyncTest("offset at the last line reads only that line"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", offset = Some(2))
    assertEquals(r.output, "2@ c")
    assertEquals(r.metadata("from"), "2")
    assertEquals(r.metadata("to"), "2")

  asyncTest("a single line with a trailing newline is one line"):
    given dir: String = tempDir()
    write(dir, "f.txt", "only\n")
    val r = run("f.txt")
    assertEquals(r.output, "0@ only")
    assertEquals(r.metadata("totalLines"), "1")

  asyncTest("a single line without a trailing newline is one line"):
    given dir: String = tempDir()
    write(dir, "f.txt", "only")
    val r = run("f.txt")
    assertEquals(r.output, "0@ only")
    assertEquals(r.metadata("totalLines"), "1")

  asyncTest("a lone newline is a single empty line, not zero lines"):
    given dir: String = tempDir()
    write(dir, "f.txt", "\n")
    val r = run("f.txt")
    assertEquals(r.isError, false, r.output)
    // dropRight(1) leaves "" which splits to one empty line
    assertEquals(r.output, "0@ ")
    assertEquals(r.metadata("totalLines"), "1")

  asyncTest("a trailing blank line (two trailing newlines) is counted"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\n\n")
    val r = run("f.txt")
    // only the single final newline is dropped, leaving "a\n" -> [a, ""]
    assertEquals(r.output, "0@ a\n1@ ")
    assertEquals(r.metadata("totalLines"), "2")

  asyncTest("multiple consecutive blank lines each keep their own number"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\n\n\n\nb\n")
    val r = run("f.txt")
    assertEquals(r.output, "0@ a\n1@ \n2@ \n3@ \n4@ b")
    assertEquals(r.metadata("totalLines"), "5")

  asyncTest("a file of only blank lines reads as bare prefixes"):
    given dir: String = tempDir()
    write(dir, "f.txt", "\n\n\n")
    val r = run("f.txt")
    // "\n\n\n" -> drop one -> "\n\n" -> ["", "", ""]
    assertEquals(r.output, "0@ \n1@ \n2@ ")
    assertEquals(r.metadata("totalLines"), "3")

  asyncTest("a window over blank lines keeps absolute numbering"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\n\n\nb\n")
    val r = run("f.txt", offset = Some(1), limit = Some(2))
    assertEquals(r.output, "1@ \n2@ ")
    assertEquals(r.metadata("from"), "1")
    assertEquals(r.metadata("to"), "2")

  asyncTest("a very long line is rendered intact with its prefix"):
    given dir: String = tempDir()
    val long = "x" * 20000
    write(dir, "f.txt", s"$long\nshort\n")
    val r = run("f.txt")
    assertEquals(r.output, s"0@ $long\n1@ short")
    assertEquals(r.metadata("totalLines"), "2")

  asyncTest("leading and trailing spaces in a line are preserved"):
    given dir: String = tempDir()
    write(dir, "f.txt", "  spaced  \n")
    val r = run("f.txt")
    // Read just renders; all content spaces survive after the single "n@ " prefix
    assertEquals(r.output, "0@   spaced  ")

  asyncTest("a line that itself looks like a prefix is rendered verbatim"):
    given dir: String = tempDir()
    write(dir, "f.txt", "5@ already\n")
    val r = run("f.txt")
    assertEquals(r.output, "0@ 5@ already")
    assertEquals(r.metadata("totalLines"), "1")

  asyncTest("reading is read-only and never consults the approval policy"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\n")
    var consulted = false
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        consulted = true; false
    val r =
      given RuntimeContext = RuntimeContext(dir, recording)
      Read.execute(ReadParams("f.txt", None, None))
    assertEquals(r.isError, false, r.output)
    assertEquals(consulted, false)
    assertEquals(r.output, "0@ a\n1@ b")

  asyncTest("a relative path is resolved against the context working directory"):
    given dir: String = tempDir()
    val sub = TestFs.join(dir, "nested")
    TestFs.mkdir(sub)
    TestFs.write(TestFs.join(sub, "g.txt"), "hi\n")
    val r = run("nested/g.txt")
    assertEquals(r.isError, false, r.output)
    assertEquals(r.output, "0@ hi")

  asyncTest("an absolute path is read directly"):
    given dir: String = tempDir()
    val p = write(dir, "f.txt", "abs\n")
    val r = run(p.toString)
    assertEquals(r.isError, false, r.output)
    assertEquals(r.output, "0@ abs")

  asyncTest("the missing-file message echoes the path as given"):
    given dir: String = tempDir()
    val r = run("ghost.txt")
    assertEquals(r.isError, false, r.output)
    assert(r.output.contains("ghost.txt"), r.output)

  asyncTest("CRLF newlines leave a carriage return in the line content"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\r\nb\r\n")
    val r = run("f.txt")
    // split is on '\n' only; the '\r' stays attached to each line's content
    assertEquals(r.output, "0@ a\r\n1@ b\r")
    assertEquals(r.metadata("totalLines"), "2")
