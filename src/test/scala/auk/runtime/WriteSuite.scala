package auk.runtime

import gears.async.Async
import gears.async.default.given

import auk.TestFs
import auk.llm.tools.{RuntimeContext, ApprovalPolicy, ApprovalRequest, ToolResult}

class WriteSuite extends munit.FunSuite:

  private def tempDir(): String = TestFs.tempDir("auk-write")

  private def read(dir: String, name: String): String =
    TestFs.read(TestFs.join(dir, name))

  private def run(
      path: String,
      content: String,
      approvals: ApprovalPolicy = ApprovalPolicy.AllowAll
  )(using dir: String, a: Async): ToolResult =
    given RuntimeContext = RuntimeContext(dir, approvals)
    Write.execute(WriteParams(path, content))

  private def asyncTest(name: String)(body: Async ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  asyncTest("creates a new file with the given content"):
    given dir: String = tempDir()
    val r = run("f.txt", "hello\nworld\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "hello\nworld\n")
    assert(r.output.contains("Created"), r.output)
    assertEquals(r.metadata("created"), "true")
    assertEquals(r.metadata("lineCount"), "2")

  asyncTest("overwrites an existing file entirely"):
    given dir: String = tempDir()
    TestFs.write(TestFs.join(dir, "f.txt"), "old\ncontent\nhere\n")
    val r = run("f.txt", "new\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "new\n")
    assert(r.output.contains("Overwrote"), r.output)
    assertEquals(r.metadata("created"), "false")

  asyncTest("creates missing parent directories"):
    given dir: String = tempDir()
    val r = run("a/b/c/deep.txt", "x\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(TestFs.read(TestFs.join(dir, "a/b/c/deep.txt")), "x\n")

  asyncTest("writes content verbatim, including the lack of a trailing newline"):
    given dir: String = tempDir()
    val r = run("f.txt", "no newline")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "no newline")
    assertEquals(r.metadata("lineCount"), "1")

  asyncTest("empty content creates an empty file"):
    given dir: String = tempDir()
    val r = run("f.txt", "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "")
    assertEquals(r.metadata("lineCount"), "0")
    assertEquals(r.metadata("bytes"), "0")

  asyncTest("a directory is rejected"):
    given dir: String = tempDir()
    TestFs.mkdir(TestFs.join(dir, "sub"))
    val r = run("sub", "x")
    assert(r.isError)
    assert(r.output.contains("directory"), r.output)

  asyncTest("a denied write does not create the file and reports an error"):
    given dir: String = tempDir()
    val r = run("f.txt", "x", ApprovalPolicy.DenyAll)
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)
    assert(!TestFs.exists(TestFs.join(dir, "f.txt")))

  asyncTest("approval carries the tool name and path"):
    given dir: String = tempDir()
    var seen: Option[ApprovalRequest] = None
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        seen = Some(req); true
    val _ = run("f.txt", "x", recording)
    assertEquals(seen.map(_.toolName), Some("write"))
    assertEquals(seen.map(_.summary), Some("write f.txt"))

  asyncTest("records the byte length in metadata"):
    given dir: String = tempDir()
    val r = run("f.txt", "abc")
    assertEquals(r.metadata("bytes"), "3")

  asyncTest("a relative path is resolved against the working directory"):
    given dir: String = tempDir()
    TestFs.mkdir(TestFs.join(dir, "nested"))
    val r = run("nested/g.txt", "hi\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(TestFs.read(TestFs.join(dir, "nested/g.txt")), "hi\n")

  asyncTest("an absolute path is written directly"):
    given dir: String = tempDir()
    val target = TestFs.join(dir, "abs.txt")
    val r = run(target, "abs\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(TestFs.read(target), "abs\n")
