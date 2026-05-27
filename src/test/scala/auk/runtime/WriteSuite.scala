package auk.runtime

import java.nio.file.{Files, Path}

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.{RuntimeContext, ApprovalPolicy, ApprovalRequest, ToolResult}

class WriteSuite extends munit.FunSuite:

  private def tempDir(): Path = Files.createTempDirectory("auk-write").nn

  private def read(dir: Path, name: String): String =
    Files.readString(dir.resolve(name)).nn

  private def run(
      path: String,
      content: String,
      approvals: ApprovalPolicy = ApprovalPolicy.AllowAll
  )(using dir: Path): ToolResult =
    Async.blocking:
      given RuntimeContext = RuntimeContext(dir, approvals)
      Write.execute(WriteParams(path, content))

  test("creates a new file with the given content"):
    given dir: Path = tempDir()
    val r = run("f.txt", "hello\nworld\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "hello\nworld\n")
    assert(r.output.contains("Created"), r.output)
    assertEquals(r.metadata("created"), "true")
    assertEquals(r.metadata("lineCount"), "2")

  test("overwrites an existing file entirely"):
    given dir: Path = tempDir()
    Files.writeString(dir.resolve("f.txt").nn, "old\ncontent\nhere\n")
    val r = run("f.txt", "new\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "new\n")
    assert(r.output.contains("Overwrote"), r.output)
    assertEquals(r.metadata("created"), "false")

  test("creates missing parent directories"):
    given dir: Path = tempDir()
    val r = run("a/b/c/deep.txt", "x\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(Files.readString(dir.resolve("a/b/c/deep.txt").nn).nn, "x\n")

  test("writes content verbatim, including the lack of a trailing newline"):
    given dir: Path = tempDir()
    val r = run("f.txt", "no newline")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "no newline")
    assertEquals(r.metadata("lineCount"), "1")

  test("empty content creates an empty file"):
    given dir: Path = tempDir()
    val r = run("f.txt", "")
    assertEquals(r.isError, false, r.output)
    assertEquals(read(dir, "f.txt"), "")
    assertEquals(r.metadata("lineCount"), "0")
    assertEquals(r.metadata("bytes"), "0")

  test("a directory is rejected"):
    given dir: Path = tempDir()
    Files.createDirectory(dir.resolve("sub").nn)
    val r = run("sub", "x")
    assert(r.isError)
    assert(r.output.contains("directory"), r.output)

  test("a denied write does not create the file and reports an error"):
    given dir: Path = tempDir()
    val r = run("f.txt", "x", ApprovalPolicy.DenyAll)
    assert(r.isError)
    assert(r.output.contains("not approved"), r.output)
    assert(!Files.exists(dir.resolve("f.txt").nn))

  test("approval carries the tool name and path"):
    given dir: Path = tempDir()
    var seen: Option[ApprovalRequest] = None
    val recording = new ApprovalPolicy:
      def request(req: ApprovalRequest)(using Async): Boolean =
        seen = Some(req); true
    val _ = run("f.txt", "x", recording)
    assertEquals(seen.map(_.toolName), Some("write"))
    assertEquals(seen.map(_.summary), Some("write f.txt"))

  test("records the byte length in metadata"):
    given dir: Path = tempDir()
    val r = run("f.txt", "abc")
    assertEquals(r.metadata("bytes"), "3")

  test("a relative path is resolved against the working directory"):
    given dir: Path = tempDir()
    Files.createDirectory(dir.resolve("nested").nn)
    val r = run("nested/g.txt", "hi\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(Files.readString(dir.resolve("nested/g.txt").nn).nn, "hi\n")

  test("an absolute path is written directly"):
    given dir: Path = tempDir()
    val target = dir.resolve("abs.txt").nn
    val r = run(target.toString, "abs\n")
    assertEquals(r.isError, false, r.output)
    assertEquals(Files.readString(target).nn, "abs\n")
