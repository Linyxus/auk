package auk.llm.provider

import scala.scalajs.js

import auk.config.{AppConfig, ModelConfig}
import auk.llm.endpoint.MissingKeyEndpoint

class ModelSelectionSuite extends munit.FunSuite:

  /** Run `body` with the given process-env entries forced (`Some` sets, `None`
    * deletes), restoring the originals after. `Platform.env` reads `process.env`
    * live, so this is the seam for exercising key-presence paths. */
  private def withEnv(pairs: (String, Option[String])*)(body: => Unit): Unit =
    val env = js.Dynamic.global.process.env
    val saved = pairs.map((name, _) => name -> env.selectDynamic(name))
    def put(name: String, value: Option[String]): Unit = value match
      case Some(v) => env.updateDynamic(name)(v)
      case None    => js.special.delete(env, name)
    pairs.foreach((name, value) => put(name, value))
    try body
    finally
      saved.foreach((name, value) =>
        if js.isUndefined(value) then js.special.delete(env, name)
        else env.updateDynamic(name)(value)
      )

  private def choose(
      config: AppConfig = AppConfig.empty,
      envProvider: Option[String] = None,
      envModel: Option[String] = None
  ): (Provider, Model) =
    ModelSelection.choose(config, envProvider, envModel) match
      case Right(pm) => pm
      case Left(err) => fail(s"unexpected error: $err")

  test("default selection is ZAI + its first model (glm-5.2)") {
    val (p, m) = choose()
    assertEquals(p.name, "ZAI")
    assertEquals(m.id, Providers.zai.models.head.id)
    assertEquals(m.id, "glm-5.2")
  }

  test("config supplies the provider and model") {
    val cfg = AppConfig(Some(ModelConfig(Some("zai"), Some("glm-5.1"))), None)
    val (p, m) = choose(config = cfg)
    assertEquals(p.name, "ZAI")
    assertEquals(m.id, "glm-5.1")
  }

  test("env overrides the config for provider and model") {
    // config names a still-disabled provider; the env override wins, so that is fine.
    val cfg = AppConfig(Some(ModelConfig(Some("anthropic"), Some("claude-opus-4-8"))), None)
    val (p, m) = choose(config = cfg, envProvider = Some("openrouter"), envModel = Some("deepseek/deepseek-v4-flash"))
    assertEquals(p.name, "OpenRouter")
    assertEquals(m.id, "deepseek/deepseek-v4-flash")
  }

  test("OpenRouter is selectable with its default (first) model") {
    val cfg = AppConfig(Some(ModelConfig(Some("openrouter"), None)), None)
    val (p, m) = choose(config = cfg)
    assertEquals(p.name, "OpenRouter")
    assertEquals(m.id, Providers.openRouter.models.head.id)
    assertEquals(m.id, "z-ai/glm-5.2")
  }

  test("config selects a specific OpenRouter model by id") {
    val (p, m) = choose(config = AppConfig(Some(ModelConfig(Some("openrouter"), Some("z-ai/glm-5.1"))), None))
    assertEquals(p.name, "OpenRouter")
    assertEquals(m.id, "z-ai/glm-5.1")
  }

  test("config provider with the provider's default model") {
    val cfg = AppConfig(Some(ModelConfig(Some("zai"), None)), None)
    val (p, m) = choose(config = cfg)
    assertEquals(p.name, "ZAI")
    assertEquals(m.id, Providers.zai.models.head.id)
  }

  test("an unknown provider is a helpful error") {
    val cfg = AppConfig(Some(ModelConfig(Some("bogus"), None)), None)
    val err = ModelSelection.choose(cfg, None, None).left.toOption.get
    assert(err.contains("Unknown provider 'bogus'"))
    assert(err.contains("ZAI"))
  }

  test("a model not offered by the provider is a helpful error") {
    val cfg = AppConfig(Some(ModelConfig(Some("zai"), Some("nope"))), None)
    val err = ModelSelection.choose(cfg, None, None).left.toOption.get
    assert(err.contains("offers no model 'nope'"))
  }

  test("byRef rejects an unknown provider") {
    val err = ModelSelection.byRef("bogus", "x").left.toOption.get
    assert(err.contains("Unknown provider 'bogus'"))
  }

  test("byRef rejects a model the provider does not offer") {
    val err = ModelSelection.byRef("zai", "nope").left.toOption.get
    assert(err.contains("offers no model 'nope'"))
  }

  test("resolve degrades to a stub endpoint when the API key is unset") {
    withEnv(
      "ZAI_API_KEY" -> None,
      ModelSelection.ProviderEnv -> None,
      ModelSelection.ModelEnv -> None
    ):
      val r = ModelSelection.resolve(AppConfig.empty) match
        case Right(r)  => r
        case Left(err) => fail(s"resolve must not fail on a missing key: $err")
      assertEquals(r.provider.name, "ZAI")
      assert(r.keyMissing.exists(_.contains("ZAI_API_KEY")), s"keyMissing was: ${r.keyMissing}")
      assert(r.endpoint.isInstanceOf[MissingKeyEndpoint])
  }

  test("resolve with the key present carries a live endpoint and no notice") {
    withEnv(
      "ZAI_API_KEY" -> Some("test-key"),
      ModelSelection.ProviderEnv -> None,
      ModelSelection.ModelEnv -> None
    ):
      val r = ModelSelection.resolve(AppConfig.empty).toOption.get
      assertEquals(r.keyMissing, None)
      assert(!r.endpoint.isInstanceOf[MissingKeyEndpoint])
  }

  test("byRef still refuses a provider whose key is unset") {
    withEnv("ZAI_API_KEY" -> None):
      val err = ModelSelection.byRef("zai", "glm-5.2").left.toOption.get
      assert(err.contains("ZAI_API_KEY"))
  }
