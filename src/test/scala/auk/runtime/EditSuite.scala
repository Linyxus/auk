package auk.runtime

import java.nio.file.{Files, Path}

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.{RuntimeContext, ApprovalPolicy, ApprovalRequest, ToolResult}

class EditSuite extends munit.FunSuite:

  private def tempDir(): Path = Files.createTempDirectory("auk-edit").nn

  private def write(dir: Path, name: String, content: String): Path =
    val p = dir.resolve(name).nn
    Files.writeString(p, content)
    p

  private def read(dir: Path, name: String): String =
    Files.readString(dir.resolve(name)).nn

  private def run(
      path: String,
      startLine: Int,
      endLine: Int,
      content: String,
      approvals: ApprovalPolicy = ApprovalPolicy.AllowAll
  )(using dir: Path): ToolResult =
    Async.blocking:
      given RuntimeContext = RuntimeContext(dir, approvals)
      Edit.execute(EditParams(path, startLine, endLine, content))

  test("replaces a single line addressed by number"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 1, 1, "BEE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nBEE\nc\n")

  test("replaces a consecutive run of lines"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", 1, 2, "X\nY\nZ")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nX\nY\nZ\nd\n")

  test("empty content deletes the range"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 1, 1, "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nc\n")

  test("preserves the absence of a trailing newline"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb")
    val r = run("f.txt", 1, 1, "B")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nB")

  test("the replacement keeps its own indentation"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "def f():\n    return 1\n")
    val r = run("f.txt", 1, 1, "    return 2")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "def f():\n    return 2\n")

  test("an out-of-range endLine is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", 0, 5, "X")
    assert(r.isError)
    assert(r.output.contains("out of range"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\n")

  test("endLine exactly one past the end is out of range"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", 2, 2, "X")
    assert(r.isError)
    assert(r.output.contains("out of range"), r.output)

  test("a multi-line window whose tail spills past the end is out of range"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", 1, 2, "X")
    assert(r.isError)
    assert(r.output.contains("out of range"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\n")

  test("endLine before startLine is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 2, 1, "X")
    assert(r.isError)
    assert(r.output.contains("startLine"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\n")

  test("a negative startLine is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", -1, 0, "X")
    assert(r.isError)
    assert(r.output.contains("startLine must be >= 0"), r.output)

  test("a denied edit does not write and reports an error"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", 1, 1, "B", ApprovalPolicy.DenyAll)
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\n")

  test("approval carries the tool name and path"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    var seen: Option[ApprovalRequest] = None
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        seen = Some(req); true
    val _ = run("f.txt", 1, 1, "B", recording)
    assertEquals(seen.map(_.toolName), Some("edit"))
    assertEquals(seen.map(_.summary), Some("edit f.txt"))

  test("a missing file is an error that points to write"):
    given dir: Path = tempDir()
    val r = run("nope.txt", 0, 0, "X")
    assert(r.isError)
    assert(r.output.contains("does not exist"), r.output)
    assert(r.output.contains("write"), r.output)

  test("an empty file is an error that points to write"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "")
    val r = run("f.txt", 0, 0, "X")
    assert(r.isError)
    assert(r.output.contains("empty"), r.output)
    assert(r.output.contains("write"), r.output)

  test("editing a directory is rejected"):
    given dir: Path = tempDir()
    val sub = dir.resolve("sub").nn
    Files.createDirectory(sub)
    val r = run("sub", 0, 0, "Y")
    assert(r.isError)
    assert(r.output.contains("directory"), r.output)

  test("editing the first line (0)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 0, 0, "AYE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "AYE\nb\nc\n")

  test("editing the last line (highest valid index)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 2, 2, "SEE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nSEE\n")

  test("editing the sole line of a single-line file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "only\n")
    val r = run("f.txt", 0, 0, "changed")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "changed\n")

  test("deleting the only line leaves an empty file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "only\n")
    val r = run("f.txt", 0, 0, "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "")

  test("replacing the whole file in one range"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 0, 2, "X\nY")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "X\nY\n")

  test("inserting more lines than replaced grows the file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 1, 1, "B1\nB2\nB3")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nB1\nB2\nB3\nc\n")
    assertEquals(r.metadata("newLineCount"), "3")
    assertEquals(r.metadata("oldLineCount"), "1")
    assertEquals(r.metadata("startLine"), "1")
    assertEquals(r.metadata("endLine"), "1")

  test("multi-line content that itself looks like n@ prefixes is inserted verbatim"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 1, 1, "0@ not a prefix\n9@ still literal")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\n0@ not a prefix\n9@ still literal\nc\n")

  test("a no-op edit replacing a line with its own content still rewrites identically"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", 1, 1, "b")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\n")

  test("a trailing newline in content yields a blank line, not a stripped one"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    // content = "X\n" splits to Vector("X", "") -> X then a blank line
    val r = run("f.txt", 1, 1, "X\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nX\n\nc\n")
    assertEquals(r.metadata("newLineCount"), "2")

  test("a file lacking a trailing newline stays that way after a multi-line insert"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc") // no trailing newline
    val r = run("f.txt", 2, 2, "C1\nC2")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nC1\nC2")

  test("deleting the last line of a no-trailing-newline file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc") // no trailing newline
    val r = run("f.txt", 2, 2, "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb")

  test("CRLF: replacing a line by range drops that line's carriage return"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\r\nb\r\nc\r\n")
    // split is on \n only, so lines are "a\r", "b\r", "c\r"; the replacement has
    // no \r of its own, so the edited line loses it.
    val r = run("f.txt", 1, 1, "B")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\r\nB\nc\r\n")

  test("a relative path is resolved against the working directory"):
    given dir: Path = tempDir()
    val sub = dir.resolve("nested").nn
    Files.createDirectory(sub)
    Files.writeString(sub.resolve("g.txt").nn, "x\ny\n")
    val r = run("nested/g.txt", 0, 0, "X")
    assertEquals(r.isError, false, r.output)
    assertEquals(Files.readString(sub.resolve("g.txt").nn).nn, "X\ny\n")

  test("the success metadata reports the edited window for a deletion"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", 1, 2, "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nd\n")
    assertEquals(r.metadata("startLine"), "1")
    assertEquals(r.metadata("endLine"), "2")
    assertEquals(r.metadata("oldLineCount"), "2")
    assertEquals(r.metadata("newLineCount"), "0")

  test("a denied multi-line edit writes nothing"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", 1, 2, "X\nY", ApprovalPolicy.DenyAll)
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\nd\n")

  test("approval is consulted only after the edit is known to apply (no request when out of range)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("f.txt", 0, 9, "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested when the edit cannot apply")

  test("approval is not consulted for a missing file"):
    given dir: Path = tempDir()
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("nope.txt", 0, 0, "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested for a missing file")

  test("approval is not consulted for an empty file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "")
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("f.txt", 0, 0, "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested for an empty file")
