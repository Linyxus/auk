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
