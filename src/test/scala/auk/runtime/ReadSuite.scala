package auk.runtime

import java.nio.file.{Files, Path}

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.{RuntimeContext, ApprovalPolicy, ApprovalRequest, ToolResult}

class ReadSuite extends munit.FunSuite:

  private def tempDir(): Path = Files.createTempDirectory("auk-read").nn

  private def write(dir: Path, name: String, content: String): Path =
    val p = dir.resolve(name).nn
    Files.writeString(p, content)
    p

  private def run(path: String, offset: Option[Int] = None, limit: Option[Int] = None)(using
      dir: Path
  ): ToolResult =
    Async.blocking:
      given RuntimeContext = RuntimeContext(dir)
      Read.execute(ReadParams(path, offset, limit))

  test("numbers every line from 0 with the @n> prefix"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "alpha\nbeta\ngamma\n")
    val r = run("f.txt")
    assertEquals(r.isError, false)
    assertEquals(r.output, "@0> alpha\n@1> beta\n@2> gamma")
    assertEquals(r.metadata("totalLines"), "3")

  test("a file without a trailing newline counts the same number of lines"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "alpha\nbeta")
    val r = run("f.txt")
    assertEquals(r.output, "@0> alpha\n@1> beta")
    assertEquals(r.metadata("totalLines"), "2")

  test("preserves indentation after the single delimiter space"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "def f():\n    return 1\n")
    val r = run("f.txt")
    assertEquals(r.output, "@0> def f():\n@1>     return 1")

  test("blank lines render as a bare prefix and keep their number"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\n\nb\n")
    val r = run("f.txt")
    assertEquals(r.output, "@0> a\n@1> \n@2> b")

  test("offset starts the window but keeps real line numbers"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", offset = Some(2))
    assertEquals(r.output, "@2> c\n@3> d")
    assertEquals(r.metadata("from"), "2")

  test("limit caps the number of lines read"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", offset = Some(1), limit = Some(2))
    assertEquals(r.output, "@1> b\n@2> c")

  test("an empty file reads as (empty file)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "")
    val r = run("f.txt")
    assertEquals(r.isError, false)
    assertEquals(r.output, "(empty file)")
    assertEquals(r.metadata("totalLines"), "0")

  test("an offset past the end is reported, not an error"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", offset = Some(10))
    assertEquals(r.isError, false)
    assert(r.output.contains("past the end"), r.output)

  test("a missing file is an error"):
    given dir: Path = tempDir()
    val r = run("nope.txt")
    assert(r.isError)
    assert(r.output.contains("not found"), r.output)

  test("a directory is rejected"):
    given dir: Path = tempDir()
    val r = run(".")
    assert(r.isError)
    assert(r.output.contains("directory"), r.output)

  test("a negative offset is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\n")
    val r = run("f.txt", offset = Some(-1))
    assert(r.isError)

  // --- additional corner cases -------------------------------------------

  test("offset 0 is the same as no offset"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", offset = Some(0))
    assertEquals(r.isError, false, r.output)
    assertEquals(r.output, "@0> a\n@1> b\n@2> c")
    assertEquals(r.metadata("from"), "0")
    assertEquals(r.metadata("to"), "2")

  test("a negative limit is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", limit = Some(-1))
    assert(r.isError)
    assert(r.output.contains("limit must be >= 0"), r.output)

  test("limit 0 yields an empty window but is not an error"):
    given dir: Path = tempDir()
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

  test("limit 0 at a non-zero offset still reports that offset"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", offset = Some(2), limit = Some(0))
    assertEquals(r.isError, false, r.output)
    assertEquals(r.output, "")
    assertEquals(r.metadata("from"), "2")
    assertEquals(r.metadata("to"), "2")

  test("a limit larger than the file reads the whole file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", limit = Some(1000))
    assertEquals(r.output, "@0> a\n@1> b\n@2> c")
    assertEquals(r.metadata("to"), "2")

  test("offset plus limit that overruns the file is clamped to the end"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", offset = Some(2), limit = Some(10))
    assertEquals(r.output, "@2> c\n@3> d")
    assertEquals(r.metadata("from"), "2")
    assertEquals(r.metadata("to"), "3")

  test("an interior offset+limit window keeps absolute line numbers"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\ne\n")
    val r = run("f.txt", offset = Some(1), limit = Some(3))
    assertEquals(r.output, "@1> b\n@2> c\n@3> d")
    assertEquals(r.metadata("from"), "1")
    assertEquals(r.metadata("to"), "3")

  test("offset exactly at EOF is reported as past the end, not empty"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", offset = Some(2))
    assertEquals(r.isError, false, r.output)
    assert(r.output.contains("past the end"), r.output)
    assert(r.output.contains("2 line(s)"), r.output)
    // from is clamped to total
    assertEquals(r.metadata("from"), "2")
    assertEquals(r.metadata("totalLines"), "2")

  test("offset past EOF clamps from/to metadata to totalLines"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", offset = Some(99))
    assertEquals(r.isError, false, r.output)
    assertEquals(r.metadata("from"), "3")
    assertEquals(r.metadata("to"), "3")

  test("offset at the last line reads only that line"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", offset = Some(2))
    assertEquals(r.output, "@2> c")
    assertEquals(r.metadata("from"), "2")
    assertEquals(r.metadata("to"), "2")

  test("a single line with a trailing newline is one line"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "only\n")
    val r = run("f.txt")
    assertEquals(r.output, "@0> only")
    assertEquals(r.metadata("totalLines"), "1")

  test("a single line without a trailing newline is one line"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "only")
    val r = run("f.txt")
    assertEquals(r.output, "@0> only")
    assertEquals(r.metadata("totalLines"), "1")

  test("a lone newline is a single empty line, not zero lines"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "\n")
    val r = run("f.txt")
    assertEquals(r.isError, false, r.output)
    // dropRight(1) leaves "" which splits to one empty line
    assertEquals(r.output, "@0> ")
    assertEquals(r.metadata("totalLines"), "1")

  test("a trailing blank line (two trailing newlines) is counted"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\n\n")
    val r = run("f.txt")
    // only the single final newline is dropped, leaving "a\n" -> [a, ""]
    assertEquals(r.output, "@0> a\n@1> ")
    assertEquals(r.metadata("totalLines"), "2")

  test("multiple consecutive blank lines each keep their own number"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\n\n\n\nb\n")
    val r = run("f.txt")
    assertEquals(r.output, "@0> a\n@1> \n@2> \n@3> \n@4> b")
    assertEquals(r.metadata("totalLines"), "5")

  test("a file of only blank lines reads as bare prefixes"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "\n\n\n")
    val r = run("f.txt")
    // "\n\n\n" -> drop one -> "\n\n" -> ["", "", ""]
    assertEquals(r.output, "@0> \n@1> \n@2> ")
    assertEquals(r.metadata("totalLines"), "3")

  test("a window over blank lines keeps absolute numbering"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\n\n\nb\n")
    val r = run("f.txt", offset = Some(1), limit = Some(2))
    assertEquals(r.output, "@1> \n@2> ")
    assertEquals(r.metadata("from"), "1")
    assertEquals(r.metadata("to"), "2")

  test("a very long line is rendered intact with its prefix"):
    given dir: Path = tempDir()
    val long = "x" * 20000
    write(dir, "f.txt", s"$long\nshort\n")
    val r = run("f.txt")
    assertEquals(r.output, s"@0> $long\n@1> short")
    assertEquals(r.metadata("totalLines"), "2")

  test("leading and trailing spaces in a line are preserved"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "  spaced  \n")
    val r = run("f.txt")
    // exactly one delimiter space after '>' is consumed on parse, but Read just
    // renders, so all content spaces survive in the output
    assertEquals(r.output, "@0>   spaced  ")

  test("a line that itself looks like a prefix is rendered verbatim"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "@5> already\n")
    val r = run("f.txt")
    assertEquals(r.output, "@0> @5> already")
    assertEquals(r.metadata("totalLines"), "1")

  test("reading is read-only and never consults the approval policy"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    var consulted = false
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        consulted = true; false
    val r = Async.blocking:
      given RuntimeContext = RuntimeContext(dir, recording)
      Read.execute(ReadParams("f.txt", None, None))
    assertEquals(r.isError, false, r.output)
    assertEquals(consulted, false)
    assertEquals(r.output, "@0> a\n@1> b")

  test("a relative path is resolved against the context working directory"):
    given dir: Path = tempDir()
    val sub = Files.createDirectory(dir.resolve("nested")).nn
    Files.writeString(sub.resolve("g.txt"), "hi\n")
    val r = run("nested/g.txt")
    assertEquals(r.isError, false, r.output)
    assertEquals(r.output, "@0> hi")

  test("an absolute path is read directly"):
    given dir: Path = tempDir()
    val p = write(dir, "f.txt", "abs\n")
    val r = run(p.toString)
    assertEquals(r.isError, false, r.output)
    assertEquals(r.output, "@0> abs")

  test("the missing-file error echoes the path as given"):
    given dir: Path = tempDir()
    val r = run("ghost.txt")
    assert(r.isError)
    assert(r.output.contains("ghost.txt"), r.output)

  test("CRLF newlines leave a carriage return in the line content"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\r\nb\r\n")
    val r = run("f.txt")
    // split is on '\n' only; the '\r' stays attached to each line's content
    assertEquals(r.output, "@0> a\r\n@1> b\r")
    assertEquals(r.metadata("totalLines"), "2")
