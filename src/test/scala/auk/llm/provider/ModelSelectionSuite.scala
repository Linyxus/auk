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

  test("default selection is OpenRouter + its first model") {
    val (p, m) = choose()
    assertEquals(p.name, "OpenRouter")
    assertEquals(m.id, Providers.openRouter.models.head.id)
  }

  test("config supplies the provider and model") {
    val cfg = AppConfig(Some(ModelConfig(Some("anthropic"), Some("claude-opus-4-8"))))
    val (p, m) = choose(config = cfg)
    assertEquals(p.name, "Anthropic")
    assertEquals(m.id, "claude-opus-4-8")
  }

  test("env overrides the config for provider and model") {
    val cfg = AppConfig(Some(ModelConfig(Some("anthropic"), Some("claude-opus-4-8"))))
    val (p, m) = choose(config = cfg, envProvider = Some("openrouter"), envModel = Some("deepseek/deepseek-v4-flash"))
    assertEquals(p.name, "OpenRouter")
    assertEquals(m.id, "deepseek/deepseek-v4-flash")
  }

  test("config provider with the provider's default model") {
    val cfg = AppConfig(Some(ModelConfig(Some("openai"), None)))
    val (p, m) = choose(config = cfg)
    assertEquals(p.name, "OpenAI")
    assertEquals(m.id, Providers.openAI.models.head.id)
  }

  test("an unknown provider is a helpful error") {
    val cfg = AppConfig(Some(ModelConfig(Some("bogus"), None)))
    val err = ModelSelection.choose(cfg, None, None).left.toOption.get
    assert(err.contains("Unknown provider 'bogus'"))
    assert(err.contains("OpenRouter"))
  }

  test("a model not offered by the provider is a helpful error") {
    val cfg = AppConfig(Some(ModelConfig(Some("openai"), Some("nope"))))
    val err = ModelSelection.choose(cfg, None, None).left.toOption.get
    assert(err.contains("offers no model 'nope'"))
  }
