package auk.config

import scala.scalajs.js

import auk.{TestEnv, TestFs}
import auk.platform.{PathOps, Platform}

/** The user-level API-key store behind /login: `~/.auk/credentials`. */
class CredentialsSuite extends munit.FunSuite:

  /** Each test runs against a fresh HOME with no masking env flag, the read
    * cache dropped on entry and exit so nothing leaks between homes. */
  private def withHome[A](body: String => A): A =
    val home = TestFs.tempDir("auk-cred-home")
    TestEnv.withEnv("HOME" -> Some(home), Credentials.NoKeysEnv -> None):
      Credentials.invalidate()
      try body(home)
      finally Credentials.invalidate()

  private def storePath(home: String): String =
    PathOps.join(PathOps.join(home, ".auk"), "credentials")

  test("save writes ~/.auk/credentials with mode 0600 and get reads it back, trimmed") {
    withHome: home =>
      assertEquals(Credentials.get("zai"), None)
      assertEquals(Credentials.save("ZAI", "  sk-test-123  "), Right(()))
      assertEquals(Credentials.get("zai"), Some("sk-test-123"))
      assertEquals(Credentials.get("ZAI"), Some("sk-test-123"))
      val path = storePath(home)
      val text = Platform.fs.readString(path)
      assert(text.startsWith("[keys]"), text)
      assert(text.contains("zai = sk-test-123"), text)
      val mode = js.Dynamic.global.require("node:fs").statSync(path).mode.asInstanceOf[Int] & 511
      assertEquals(mode, 384)
  }

  test("a second save preserves the other providers' keys") {
    withHome: home =>
      assertEquals(Credentials.save("zai", "sk-a"), Right(()))
      assertEquals(Credentials.save("kimi", "sk-b"), Right(()))
      assertEquals(Credentials.get("zai"), Some("sk-a"))
      assertEquals(Credentials.get("kimi"), Some("sk-b"))
      // …and replacing one leaves the other in place.
      assertEquals(Credentials.save("zai", "sk-c"), Right(()))
      assertEquals(Credentials.get("zai"), Some("sk-c"))
      assertEquals(Credentials.get("kimi"), Some("sk-b"))
  }

  test("AUK_NO_KEYS=1 makes reads see an empty store") {
    withHome: home =>
      assertEquals(Credentials.save("zai", "sk-a"), Right(()))
      TestEnv.withEnv(Credentials.NoKeysEnv -> Some("1")):
        assertEquals(Credentials.get("zai"), None)
      assertEquals(Credentials.get("zai"), Some("sk-a"))
  }

  test("a malformed file degrades reads to empty, surfaces via problem, and blocks save") {
    withHome: home =>
      TestFs.write(storePath(home), "[keys\nzai = broken\n")
      assertEquals(Credentials.get("zai"), None)
      assert(Credentials.problem.isDefined)
      assert(Credentials.save("zai", "sk-x").isLeft)
      // The malformed content was not clobbered by the refused save.
      assert(Platform.fs.readString(storePath(home)).startsWith("[keys\n"))
  }

  test("saveCustom writes the provider section and its key together") {
    withHome: home =>
      assertEquals(
        Credentials.saveCustom("anthropic", "https://api.example.com/anthropic", "glm-5.2", "sk-c"),
        Right(())
      )
      assertEquals(Credentials.get("custom"), Some("sk-c"))
      val entry = Credentials.customEntries.get("custom").get
      assertEquals(entry.kind, "anthropic")
      assertEquals(entry.url, "https://api.example.com/anthropic")
      assertEquals(entry.model, "glm-5.2")
      val text = Platform.fs.readString(storePath(home))
      assert(text.contains("[providers.custom]"), text)
      // A later key-only save keeps the provider section…
      assertEquals(Credentials.save("zai", "sk-z"), Right(()))
      assert(Credentials.customEntries.contains("custom"))
      // …and a fresh read off disk round-trips everything.
      Credentials.invalidate()
      assertEquals(Credentials.customEntries.get("custom").map(_.url), Some("https://api.example.com/anthropic"))
      assertEquals(Credentials.get("zai"), Some("sk-z"))
  }

  test("AUK_NO_KEYS masks custom providers too") {
    withHome: home =>
      assertEquals(Credentials.saveCustom("anthropic", "https://x", "m", "k"), Right(()))
      TestEnv.withEnv(Credentials.NoKeysEnv -> Some("1")):
        assert(Credentials.customEntries.isEmpty)
      assert(Credentials.customEntries.nonEmpty)
  }

  test("without HOME there is no store: reads empty, save refuses with a message") {
    TestEnv.withEnv("HOME" -> None, Credentials.NoKeysEnv -> None):
      Credentials.invalidate()
      try
        assertEquals(Credentials.get("zai"), None)
        assert(Credentials.save("zai", "sk-x").isLeft)
      finally Credentials.invalidate()
  }
