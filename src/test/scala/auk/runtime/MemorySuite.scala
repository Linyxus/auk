package auk.runtime

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import gears.async.Async
import gears.async.default.given

import auk.llm.tools.{RuntimeContext, ToolResult}

class MemorySuite extends munit.FunSuite:

  private def tempDir(): Path = Files.createTempDirectory("auk-memory").nn
  private def ctxAt(dir: Path): RuntimeContext = RuntimeContext(dir)

  private def write(key: String, content: String)(using RuntimeContext, Async): ToolResult =
    WriteMemory.execute(WriteMemoryParams(key, content))

  private def get(key: Option[String] = None)(using RuntimeContext, Async): ToolResult =
    GetMemory.execute(GetMemoryParams(key))

  private def storeFile(dir: Path): Path = dir.resolve(MemoryStore.RelativePath).nn

  test("get on an empty store reports nothing stored, not an error"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      val r = get()
      assertEquals(r.isError, false)
      assert(r.output.contains("no memories stored"))

  test("write then get round-trips the content"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      val w = write("build", "sbt test")
      assertEquals(w.isError, false)
      assert(w.output.contains("stored"))
      assertEquals(get(Some("build")).output, "sbt test")

  test("write creates the store file under .auk/"):
    Async.blocking:
      val dir = tempDir()
      given RuntimeContext = ctxAt(dir)
      write("k", "v")
      assert(Files.exists(storeFile(dir)))

  test("rewriting a key updates its value and reports 'updated'"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      write("k", "first")
      val again = write("k", "second")
      assert(again.output.contains("updated"))
      assertEquals(again.metadata.get("existed"), Some("true"))
      assertEquals(get(Some("k")).output, "second")

  test("listing renders every entry as a markdown section in insertion order"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      write("alpha", "one")
      write("beta", "two")
      val all = get()
      assertEquals(all.output, "## alpha\none\n\n## beta\ntwo")
      assertEquals(all.metadata.get("count"), Some("2"))

  test("getting a missing key is not an error and lists known keys"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      write("known", "x")
      val r = get(Some("missing"))
      assertEquals(r.isError, false)
      assert(r.output.contains("no memory found for 'missing'"))
      assert(r.output.contains("known"))

  test("an empty key is rejected"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      val r = write("   ", "v")
      assert(r.isError)
      assert(r.output.contains("key cannot be empty"))

  test("memory persists across fresh tool instances (same directory)"):
    Async.blocking:
      val dir = tempDir()
      locally:
        given RuntimeContext = ctxAt(dir)
        write("persisted", "value")
      // A new context over the same directory sees the prior write.
      given RuntimeContext = ctxAt(dir)
      assertEquals(get(Some("persisted")).output, "value")

  test("a corrupt store surfaces an error instead of pretending to be empty"):
    Async.blocking:
      val dir = tempDir()
      val f = storeFile(dir)
      Files.createDirectories(f.getParent)
      Files.writeString(f, "{ not json", UTF_8)
      given RuntimeContext = ctxAt(dir)
      val r = get()
      assert(r.isError)
      assert(r.output.contains("corrupt"))

  test("content with newlines and quotes round-trips through the JSON store"):
    Async.blocking:
      given RuntimeContext = ctxAt(tempDir())
      val tricky = "line1\nline2 with \"quotes\" and a \\ backslash"
      write("multiline", tricky)
      assertEquals(get(Some("multiline")).output, tricky)
