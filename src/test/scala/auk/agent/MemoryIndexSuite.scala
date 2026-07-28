package auk.agent

import gears.async.Async
import gears.async.default.given

import auk.platform.Platform

/** The pushed memory index: reads `.auk/memory` notes into a prompt section. */
class MemoryIndexSuite extends munit.FunSuite:

  private val root = s"target/memoryindex-test-${java.lang.System.currentTimeMillis()}"

  override def beforeAll(): Unit =
    Platform.fs.createDirectories(s"$root/.auk/memory")

  override def afterAll(): Unit =
    Platform.fs.removeAll(root)

  private def asyncTest(name: String)(body: Async ?=> Unit): Unit =
    test(name):
      Async.fromSync(body)

  private def env = PromptEnv(root, "test-model", "2026-07-28")

  asyncTest("no memory directory (or an empty one) renders nothing"):
    val bare = PromptEnv(s"$root/absent", "m", "d")
    assertEquals(DynamicSection.MemoryIndex.render(bare), None)
    assertEquals(DynamicSection.MemoryIndex.render(env), None)

  asyncTest("notes render one indexed line each, with frontmatter descriptions"):
    Platform.fs.writeString(
      s"$root/.auk/memory/build-commands.md",
      "---\ndescription: how to build and test\n---\n\nUse sbt test.\n"
    )
    Platform.fs.writeString(
      s"$root/.auk/memory/plain.md",
      "description: a headerless note\n\nBody.\n"
    )
    Platform.fs.writeString(s"$root/.auk/memory/nodesc.md", "Just a body.\n")
    Platform.fs.writeString(s"$root/.auk/memory/ignored.txt", "not markdown\n")
    DynamicSection.MemoryIndex.render(env) match
      case None => fail("expected a rendered index")
      case Some(body) =>
        assert(body.contains("- build-commands — how to build and test"), body)
        assert(body.contains("- plain — a headerless note"), body)
        assert(body.contains("- nodesc"), body)
        assert(!body.contains("ignored"), body)
        assert(body.contains("lib.memory.read"), body)
