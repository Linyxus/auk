package auk.runtime

import gears.async.Async
import gears.async.default.given

import auk.TestFs
import auk.llm.tools.{RuntimeContext, ApprovalPolicy, ApprovalRequest, ToolResult}

class EditSuite extends munit.FunSuite:

  private def tempDir(): String = TestFs.tempDir("auk-edit")

  private def write(dir: String, name: String, content: String): String =
    val p = TestFs.join(dir, name)
    TestFs.write(p, content)
    p

  private def read(dir: String, name: String): String =
    TestFs.read(TestFs.join(dir, name))

  private def run(
      path: String,
      oldText: String,
      newText: String,
      approvals: ApprovalPolicy = ApprovalPolicy.AllowAll
  )(using dir: String, a: Async): ToolResult =
    given RuntimeContext = RuntimeContext(dir, approvals)
    Edit.execute(EditParams(path, oldText, newText))

  private def asyncTest(name: String)(body: Async ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  asyncTest("replaces a unique snippet"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "b", "BEE")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nBEE\nc\n")

  asyncTest("replaces a multi-line snippet"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\nd\n")
    val r = run("f.txt", "b\nc", "X\nY\nZ")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nX\nY\nZ\nd\n")

  asyncTest("empty newText deletes the matched text"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "b\n", "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nc\n")

  asyncTest("preserves the absence of a trailing newline"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb")
    val r = run("f.txt", "b", "B")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nB")

  asyncTest("matches indentation exactly"):
    given dir: String = tempDir()
    write(dir, "f.txt", "def f():\n    return 1\n")
    val r = run("f.txt", "    return 1", "    return 2")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "def f():\n    return 2\n")

  asyncTest("a snippet not present is rejected and the file is untouched"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "nope", "X")
    assert(r.isError)
    assert(r.output.contains("not found"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\n")

  asyncTest("a non-unique snippet is rejected and the file is untouched"):
    given dir: String = tempDir()
    write(dir, "f.txt", "x\ny\nx\n")
    val r = run("f.txt", "x", "Q")
    assert(r.isError)
    assert(r.output.contains("not unique"), r.output)
    assert(r.output.contains("2 matches"), r.output)
    assertEquals(read(dir, "f.txt"), "x\ny\nx\n")

  asyncTest("the non-unique error lists the matching lines with their read numbers"):
    given dir: String = tempDir()
    write(dir, "f.txt", "alpha\nbeta\nalpha\ngamma\nalpha\n")
    val r = run("f.txt", "alpha", "X")
    assert(r.isError)
    // The three matches are on lines 0, 2, and 4, shown in read's <n>@ format.
    assert(r.output.contains("0@ alpha"), r.output)
    assert(r.output.contains("2@ alpha"), r.output)
    assert(r.output.contains("4@ alpha"), r.output)

  asyncTest("repeated matches on the same line are listed once, with the full match count"):
    given dir: String = tempDir()
    write(dir, "f.txt", "ab ab\ncd\n")
    val r = run("f.txt", "ab", "Z")
    assert(r.isError)
    assert(r.output.contains("found 2 matches"), r.output)
    assert(r.output.contains("on 1 line(s)"), r.output)
    assert(r.output.contains("0@ ab ab"), r.output)

  asyncTest("surrounding context disambiguates an otherwise-repeated snippet"):
    given dir: String = tempDir()
    write(dir, "f.txt", "foo\nbar\nfoo\n")
    // "foo" alone is ambiguous; "foo\nbar" pins the first occurrence.
    val r = run("f.txt", "foo\nbar", "X")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "X\nfoo\n")

  asyncTest("an empty oldText is rejected and points to write"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "", "X")
    assert(r.isError)
    assert(r.output.contains("must not be empty"), r.output)
    assert(r.output.contains("write"), r.output)

  asyncTest("identical oldText and newText is rejected"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "b", "b")
    assert(r.isError)
    assert(r.output.contains("identical"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nc\n")

  asyncTest("a missing file is an error that points to write"):
    given dir: String = tempDir()
    val r = run("nope.txt", "a", "X")
    assert(r.isError)
    assert(r.output.contains("does not exist"), r.output)
    assert(r.output.contains("write"), r.output)

  asyncTest("an empty file is an error that points to write"):
    given dir: String = tempDir()
    write(dir, "f.txt", "")
    val r = run("f.txt", "a", "X")
    assert(r.isError)
    assert(r.output.contains("empty"), r.output)
    assert(r.output.contains("write"), r.output)

  asyncTest("editing a directory is rejected"):
    given dir: String = tempDir()
    val sub = TestFs.join(dir, "sub")
    TestFs.mkdir(sub)
    val r = run("sub", "x", "Y")
    assert(r.isError)
    assert(r.output.contains("directory"), r.output)

  asyncTest("a snippet at the start of the file"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "a\n", "AYE\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "AYE\nb\nc\n")

  asyncTest("a snippet at the end of the file"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "c\n", "SEE\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\nSEE\n")

  asyncTest("editing the sole content of a single-line file"):
    given dir: String = tempDir()
    write(dir, "f.txt", "only\n")
    val r = run("f.txt", "only", "changed")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "changed\n")

  asyncTest("replacing the whole file content"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "a\nb\nc\n", "X\nY\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "X\nY\n")

  asyncTest("inserting more lines than replaced grows the file"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "b", "B1\nB2\nB3")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\nB1\nB2\nB3\nc\n")
    assertEquals(r.metadata("oldLineCount"), "1")
    assertEquals(r.metadata("newLineCount"), "3")

  asyncTest("newText that itself looks like n@ prefixes is inserted verbatim"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    val r = run("f.txt", "b", "0@ not a prefix\n9@ still literal")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\n0@ not a prefix\n9@ still literal\nc\n")

  asyncTest("a substring match within a token is replaced (exact substring semantics)"):
    given dir: String = tempDir()
    write(dir, "f.txt", "foobar\n")
    val r = run("f.txt", "foo", "X")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "Xbar\n")

  asyncTest("CRLF: matching the bare char splices in place and preserves the \\r"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\r\nb\r\nc\r\n")
    // "b" occurs once (inside "b\r"); the splice leaves the surrounding \r intact.
    val r = run("f.txt", "b", "B")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "a\r\nB\r\nc\r\n")

  asyncTest("a relative path is resolved against the working directory"):
    given dir: String = tempDir()
    val sub = TestFs.join(dir, "nested")
    TestFs.mkdir(sub)
    TestFs.write(TestFs.join(sub, "g.txt"), "x\ny\n")
    val r = run("nested/g.txt", "x", "X")
    assertEquals(r.isError, false, r.output)
    assertEquals(TestFs.read(TestFs.join(sub, "g.txt")), "X\ny\n")

  asyncTest("consecutive edits stay valid without re-reading (the whole point)"):
    given dir: String = tempDir()
    write(dir, "f.txt", "line one\nline two\nline three\n")
    // First edit shifts line numbers, but the second anchors on content, so it
    // still applies correctly.
    val r1 = run("f.txt", "line one", "FIRST\ninserted")
    assertEquals(r1.isError, false, r1.output)
    val r2 = run("f.txt", "line three", "THIRD")
    assertEquals(r2.isError, false, r2.output)
    assertEquals(read(dir, "f.txt"), "FIRST\ninserted\nline two\nTHIRD\n")

  asyncTest("a denied edit does not write and reports an error"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\n")
    val r = run("f.txt", "b", "B", ApprovalPolicy.DenyAll)
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)
    assertEquals(read(dir, "f.txt"), "a\nb\n")

  asyncTest("approval carries the tool name and path"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\n")
    var seen: Option[ApprovalRequest] = None
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        seen = Some(req); true
    val _ = run("f.txt", "b", "B", recording)
    assertEquals(seen.map(_.toolName), Some("edit"))
    assertEquals(seen.map(_.summary), Some("edit f.txt"))

  asyncTest("approval is not consulted when the snippet is not found"):
    given dir: String = tempDir()
    write(dir, "f.txt", "a\nb\nc\n")
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("f.txt", "nope", "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested when no unique match exists")

  asyncTest("approval is not consulted when the snippet is not unique"):
    given dir: String = tempDir()
    write(dir, "f.txt", "x\nx\n")
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("f.txt", "x", "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested for an ambiguous match")

  asyncTest("approval is not consulted for a missing file"):
    given dir: String = tempDir()
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("nope.txt", "a", "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested for a missing file")

  asyncTest("approval is not consulted for an empty file"):
    given dir: String = tempDir()
    write(dir, "f.txt", "")
    var requested = false
    val watching = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        requested = true; true
    val r = run("f.txt", "a", "B", watching)
    assert(r.isError)
    assert(!requested, "approval should not be requested for an empty file")
