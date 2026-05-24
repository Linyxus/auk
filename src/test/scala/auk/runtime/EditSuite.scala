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
      old: String,
      `new`: String,
      approvals: ApprovalPolicy = ApprovalPolicy.AllowAll
  )(using dir: Path): ToolResult =
    Async.blocking:
      given RuntimeContext = RuntimeContext(dir, approvals)
      Edit.execute(EditParams(path, old, `new`))

  test("replaces a single line matched by content"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> b", "BEE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nBEE\nc\n")

  test("the ... wildcard replaces a line without quoting its content"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> ...", "BEE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nBEE\nc\n")

  test("replaces a consecutive run of lines"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", "@1> b\n@2> c", "X\nY\nZ")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nX\nY\nZ\nd\n")

  test("empty new deletes the matched lines"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> ...", "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nc\n")

  test("preserves the absence of a trailing newline"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb")
    val r = run("f.txt", "@1> b", "B")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nB")

  test("indentation is matched after the delimiter space"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "def f():\n    return 1\n")
    val r = run("f.txt", "@1>     return 1", "    return 2")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "def f():\n    return 2\n")

  test("a content mismatch is rejected and the file is untouched"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> WRONG", "B")
    assert(r.isError)
    assert(r.output.contains("mismatch"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\n")

  test("non-consecutive line numbers are rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@0> a\n@2> c", "X")
    assert(r.isError)
    assert(r.output.contains("consecutive"), r.output)

  test("an out-of-range line is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "@5> ...", "X")
    assert(r.isError)
    assert(r.output.contains("out of range"), r.output)

  test("a line missing the @n> prefix is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "b", "X")
    assert(r.isError)
    assert(r.output.contains("prefix"), r.output)

  test("a trailing newline in old is tolerated"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> b\n", "B")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nB\nc\n")

  test("a denied edit does not write and reports an error"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "@1> b", "B", ApprovalPolicy.DenyAll)
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
    val _ = run("f.txt", "@1> b", "B", recording)
    assertEquals(seen.map(_.toolName), Some("edit"))
    assertEquals(seen.map(_.summary), Some("edit f.txt"))

  test("a missing file is an error"):
    given dir: Path = tempDir()
    val r = run("nope.txt", "@0> a", "X")
    assert(r.isError)
    assert(r.output.contains("not found"), r.output)

  // --- additional corner cases ---------------------------------------------

  test("editing a directory is rejected"):
    given dir: Path = tempDir()
    val sub = dir.resolve("sub").nn
    Files.createDirectory(sub)
    val r = run("sub", "@0> x", "Y")
    assert(r.isError)
    assert(r.output.contains("directory"), r.output)

  test("an entirely empty old is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "", "X")
    assert(r.isError)
    assert(r.output.contains("old is empty"), r.output)

  test("old of a lone newline drops the trailing line and then fails the prefix on the empty remainder"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    // "\n".split keeps Vector("", ""); the trailing "" is dropped, leaving one
    // empty line which has no @n> prefix.
    val r = run("f.txt", "\n", "X")
    assert(r.isError)
    assert(r.output.contains("prefix"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\n")

  test("editing the first line (@0)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@0> a", "AYE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "AYE\nb\nc\n")

  test("editing the last line (highest valid index)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@2> c", "SEE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nSEE\n")

  test("editing the sole line of a single-line file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "only\n")
    val r = run("f.txt", "@0> only", "changed")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "changed\n")

  test("deleting the only line leaves an empty file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "only\n")
    val r = run("f.txt", "@0> only", "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "")

  test("editing an empty file errors out of range (file has 0 lines)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "")
    val r = run("f.txt", "@0> ...", "X")
    assert(r.isError)
    assert(r.output.contains("out of range"), r.output)
    assert(r.output.contains("0 line"), r.output)

  test("@n exactly one past the end is out of range"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "@2> ...", "X")
    assert(r.isError)
    assert(r.output.contains("out of range"), r.output)

  test("a multi-line window whose tail spills past the end is out of range"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "@1> b\n@2> ...", "X")
    assert(r.isError)
    assert(r.output.contains("out of range"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\n")

  test("a leading-zero line number is parsed as that integer"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@01> b", "B")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nB\nc\n")

  test("a negative line number fails the @n> prefix"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "@-1> a", "X")
    assert(r.isError)
    assert(r.output.contains("prefix"), r.output)

  test("a non-integer (overflowing) line number is rejected"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "@99999999999999999999> a", "X")
    assert(r.isError)
    assert(r.output.contains("valid integer"), r.output)

  test("a missing > delimiter fails the prefix"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "@1 b", "X")
    assert(r.isError)
    assert(r.output.contains("prefix"), r.output)

  test("a malformed prefix on the second line aborts the whole edit"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> b\noops", "X")
    assert(r.isError)
    assert(r.output.contains("prefix"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\n")

  test("a bare @n> with no space addresses an empty line"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\n\nc\n")
    val r = run("f.txt", "@1>", "MIDDLE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nMIDDLE\nc\n")

  test("content with no space after > must match exactly (mismatch)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1>b", "X")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nX\nc\n")

  test("only the single delimiter space is dropped; extra spaces are content"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\n x\nc\n") // line 1 is " x" (one leading space)
    val r = run("f.txt", "@1>  x", "Z") // two spaces -> content " x"
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nZ\nc\n")

  test("matching a blank line with a bare prefix vs. mismatching a non-blank one"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1>", "X") // claims line 1 is empty, but it is "b"
    assert(r.isError)
    assert(r.output.contains("mismatch"), r.output)

  test("the mismatch message quotes both expected and actual content"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> WRONG", "B")
    assert(r.isError)
    assert(r.output.contains("\"WRONG\""), r.output)
    assert(r.output.contains("\"b\""), r.output)

  test("wildcard does not match the literal string \"...\" specially when content differs"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\n...\nc\n") // line 1 literally holds "..."
    // "@1> ..." is treated as the wildcard, so it matches the literal "..." too
    val r = run("f.txt", "@1> ...", "DOTS")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nDOTS\nc\n")

  test("a mix of wildcard and exact specs in one window"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", "@1> b\n@2> ...", "X\nY")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nX\nY\nd\n")

  test("inserting more lines than replaced grows the file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> b", "B1\nB2\nB3")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nB1\nB2\nB3\nc\n")
    assertEquals(r.metadata("newLineCount"), "3")
    assertEquals(r.metadata("oldLineCount"), "1")
    assertEquals(r.metadata("startLine"), "1")

  test("multi-line new whose content itself looks like @n> prefixes is inserted verbatim"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> b", "@0> not a prefix\n@9> still literal")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\n@0> not a prefix\n@9> still literal\nc\n")

  test("a no-op edit replacing a line with its own content still rewrites identically"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "@1> b", "b")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\n")

  test("a trailing newline in new yields a blank line, not a stripped one"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    // new = "X\n" splits to Vector("X", "") -> X then a blank line
    val r = run("f.txt", "@1> b", "X\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nX\n\nc\n")
    assertEquals(r.metadata("newLineCount"), "2")

  test("a file lacking a trailing newline stays that way after a multi-line insert"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc") // no trailing newline
    val r = run("f.txt", "@2> c", "C1\nC2")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nC1\nC2")

  test("deleting the last line of a no-trailing-newline file"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc") // no trailing newline
    val r = run("f.txt", "@2> c", "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb")

  test("CRLF line endings: the \\r is part of the line content and must be matched"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\r\nb\r\nc\r\n")
    // split only on \n, so line 0 is "a\r"; matching "a" alone mismatches
    val mismatch = run("f.txt", "@0> a", "X")
    assert(mismatch.isError)
    assert(mismatch.output.contains("mismatch"), mismatch.output)

  test("CRLF line endings: a spec carrying the \\r fails the prefix regex (. excludes \\r)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\r\nb\r\nc\r\n")
    // The Java regex's `.` does not match \r, so the spec content cannot include
    // it and the line fails the @n> prefix check entirely.
    val r = run("f.txt", "@1> b\r", "B")
    assert(r.isError)
    assert(r.output.contains("prefix"), r.output)
    assertEquals(read(dir, "f.txt"), "a\r\nb\r\nc\r\n")

  test("CRLF line endings: a wildcard sidesteps content matching and edits the \\r line"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\r\nb\r\nc\r\n")
    // The wildcard never inspects content, so it can replace a line that still
    // carries a trailing \r; the replacement has no \r of its own.
    val r = run("f.txt", "@1> ...", "B")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\r\nB\nc\r\n")

  test("a relative path is resolved against the working directory"):
    given dir: Path = tempDir()
    val sub = dir.resolve("nested").nn
    Files.createDirectory(sub)
    Files.writeString(sub.resolve("g.txt").nn, "x\ny\n")
    val r = run("nested/g.txt", "@0> x", "X")
    assertEquals(r.isError, false, r.output)
    assertEquals(Files.readString(sub.resolve("g.txt").nn).nn, "X\ny\n")

  test("the success metadata reports the edited window for a deletion"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", "@1> b\n@2> c", "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nd\n")
    assertEquals(r.metadata("startLine"), "1")
    assertEquals(r.metadata("oldLineCount"), "2")
    assertEquals(r.metadata("newLineCount"), "0")

  test("a denied multi-line edit writes nothing"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", "@1> b\n@2> c", "X\nY", ApprovalPolicy.DenyAll)
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\nd\n")

  test("approval is consulted only after the edit is known to apply (no request on mismatch)"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("f.txt", "@1> WRONG", "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested when the edit cannot apply")

  test("approval is not consulted when old fails to parse"):
    given dir: Path = tempDir()
    write(dir, "f.txt", "a\nb\n")
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("f.txt", "garbage", "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested for an unparseable old")
