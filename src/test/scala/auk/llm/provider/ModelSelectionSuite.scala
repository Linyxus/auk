package auk.llm.provider

import auk.config.{AppConfig, ModelConfig}

class ModelSelectionSuite extends munit.FunSuite:

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
