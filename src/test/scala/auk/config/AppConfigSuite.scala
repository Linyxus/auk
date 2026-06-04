package auk.config

import auk.platform.{Platform, PathOps}

class AppConfigSuite extends munit.FunSuite:

  private def tempDir(): String =
    val d = PathOps.join(Platform.tmpdir(), "auk-cfg-" + Platform.uuid.random())
    Platform.fs.createDirectories(PathOps.join(d, ".auk"))
    d

  private def writeConfig(dir: String, text: String): Unit =
    Platform.fs.writeString(PathOps.join(dir, AppConfig.RelativePath), text)

  test("a missing config file yields the empty AppConfig") {
    assertEquals(AppConfig.load(tempDir()), Right(AppConfig.empty))
  }

  test("loads and parses a [model] section") {
    val d = tempDir()
    writeConfig(d, """[model]
                     |provider = openrouter
                     |id = minimax/minimax-m3""".stripMargin)
    assertEquals(
      AppConfig.load(d),
      Right(AppConfig(Some(ModelConfig(Some("openrouter"), Some("minimax/minimax-m3")))))
    )
  }

  test("a partial section leaves the other field None") {
    val d = tempDir()
    writeConfig(d, "model.id = x")
    assertEquals(AppConfig.load(d), Right(AppConfig(Some(ModelConfig(None, Some("x"))))))
  }

  test("an unknown key is reported as an error") {
    val d = tempDir()
    writeConfig(d, "model.bogus = 1")
    assert(AppConfig.load(d).isLeft)
  }

  test("save then load round-trips a config") {
    val d = tempDir()
    val cfg = AppConfig(Some(ModelConfig(Some("openrouter"), Some("z-ai/glm-5.1"))))
    assertEquals(AppConfig.save(cfg, d), Right(()))
    assertEquals(AppConfig.load(d), Right(cfg))
  }

  test("save creates the .auk directory if absent") {
    val bare = PathOps.join(Platform.tmpdir(), "auk-cfg-bare-" + Platform.uuid.random())
    val cfg = AppConfig(Some(ModelConfig(Some("anthropic"), Some("claude-opus-4-8"))))
    assertEquals(AppConfig.save(cfg, bare), Right(()))
    assertEquals(AppConfig.load(bare), Right(cfg))
  }
