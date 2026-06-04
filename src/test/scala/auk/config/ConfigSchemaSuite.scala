package auk.config

enum Vendor derives ConfigType:
  case OpenAI, Anthropic, OpenRouter

// A union leaf needs an explicit, ordered given (see ConfigType.oneOf).
given ConfigType[Int | String] = ConfigType.oneOf(ConfigType[Int], ConfigType[String])

case class Coord(lat: Double, lon: Double) derives ConfigSchema
case class ModelCfg(id: String, provider: Vendor, stop: List[String]) derives ConfigSchema
case class AppCfg(model: ModelCfg) derives ConfigSchema

case class TwoInts(a: Int, b: Int) derives ConfigSchema
case class Opt(name: String, nick: Option[String]) derives ConfigSchema
case class OptNested(model: Option[ModelCfg]) derives ConfigSchema
case class WithUnion(value: Int | String) derives ConfigSchema

class ConfigSchemaSuite extends munit.FunSuite:

  private def parseOk[A](text: String)(using ConfigSchema[A]): A =
    Config.parse[A](text) match
      case Right(a)   => a
      case Left(errs) => fail(s"unexpected errors: ${errs.map(_.render).mkString("; ")}")

  private def parseErr[A](text: String)(using ConfigSchema[A]): List[ConfigError] =
    Config.parse[A](text) match
      case Left(errs) => errs
      case Right(a)   => fail(s"expected errors but got: $a")

  test("flat record of leaves decodes") {
    val cfg = parseOk[TwoInts]("a = 1\nb = 2")
    assertEquals(cfg, TwoInts(1, 2))
  }

  test("nested record via dotted keys") {
    val cfg = parseOk[AppCfg]("""model.id = glm-5.1
                                |model.provider = openai
                                |model.stop = a b""".stripMargin)
    assertEquals(cfg, AppCfg(ModelCfg("glm-5.1", Vendor.OpenAI, List("a", "b"))))
  }

  test("nested record via [scope] header") {
    val cfg = parseOk[AppCfg]("""[model]
                                |id = glm-5.1
                                |provider = openrouter
                                |stop =
                                |  done
                                |  stop""".stripMargin)
    assertEquals(cfg, AppCfg(ModelCfg("glm-5.1", Vendor.OpenRouter, List("done", "stop"))))
  }

  test("headline example decodes end to end") {
    val cfg = parseOk[AppCfg]("""[model]
                                |id = glm-5.1
                                |provider = openrouter
                                |stop = x""".stripMargin)
    assertEquals(cfg, AppCfg(ModelCfg("glm-5.1", Vendor.OpenRouter, List("x"))))
  }

  test("optional leaf absent yields None, present yields Some") {
    assertEquals(parseOk[Opt]("name = bob"), Opt("bob", None))
    assertEquals(parseOk[Opt]("name = bob\nnick = bobby"), Opt("bob", Some("bobby")))
  }

  test("optional nested record absent yields None") {
    assertEquals(parseOk[OptNested](""), OptNested(None))
  }

  test("optional nested record present requires valid contents") {
    val errs = parseErr[OptNested]("model.id = x\nmodel.provider = openai")
    // present (keys under `model`) but `model.stop` missing
    assert(errs.exists(_.render.contains("model.stop")))
  }

  test("missing required leaf reports the full path") {
    val errs = parseErr[TwoInts]("a = 1")
    assertEquals(errs.length, 1)
    assertEquals(errs.head.path, List("b"))
  }

  test("all field errors accumulate across the record") {
    val errs = parseErr[TwoInts]("a = x\nb = y")
    assertEquals(errs.length, 2)
    assertEquals(errs.map(_.path).toSet, Set(List("a"), List("b")))
  }

  test("a decode error renders as 'line N: a.b.c: msg'") {
    val errs = parseErr[AppCfg]("""model.id = x
                                  |model.provider = bogus
                                  |model.stop = y""".stripMargin)
    assertEquals(errs.length, 1)
    assert(errs.head.render.startsWith("line 2: model.provider: "), errs.head.render)
  }

  test("list field accepts both inline and block forms") {
    val inlineForm = parseOk[ModelCfg]("id = a\nprovider = openai\nstop = x y z")
    assertEquals(inlineForm.stop, List("x", "y", "z"))
    val blk = parseOk[ModelCfg]("id = a\nprovider = openai\nstop =\n  x\n  y\n  z")
    assertEquals(blk.stop, List("x", "y", "z"))
  }

  test("union field decodes via the explicit oneOf given") {
    assertEquals(parseOk[WithUnion]("value = 42"), WithUnion(42))
    assertEquals(parseOk[WithUnion]("value = auto"), WithUnion("auto"))
  }

  test("unknown top-level key is reported with its line") {
    val errs = parseErr[TwoInts]("a = 1\nb = 2\nc = 3")
    assertEquals(errs.length, 1)
    assert(errs.head.message.contains("unknown key 'c'"))
    assertEquals(errs.head.line, Some(3))
  }

  test("unknown nested key is reported") {
    val errs = parseErr[AppCfg]("""model.id = x
                                  |model.provider = openai
                                  |model.stop = y
                                  |model.bogus = 1""".stripMargin)
    assert(errs.exists(_.message.contains("unknown key 'model.bogus'")))
  }

  test("decode errors and unknown-key errors are reported together") {
    val errs = parseErr[TwoInts]("a = x\nb = 2\nc = 3")
    assert(errs.exists(_.message.contains("not an integer")), errs.map(_.render).toString)
    assert(errs.exists(_.message.contains("unknown key 'c'")), errs.map(_.render).toString)
  }
