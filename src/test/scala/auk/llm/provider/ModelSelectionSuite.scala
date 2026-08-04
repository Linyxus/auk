package auk.llm.provider

import auk.{TestEnv, TestFs}
import auk.TestEnv.withEnv
import auk.config.{AppConfig, Credentials, ModelConfig}
import auk.llm.endpoint.MissingKeyEndpoint

class ModelSelectionSuite extends munit.FunSuite:

  /** Every catalog provider's key env var cleared, so keys in the developer's
    * real environment cannot leak into a test. */
  private def noProviderKeys: Seq[(String, Option[String])] =
    Providers.all.map(_.apiKeyEnv -> None)

  /** Only ZAI keyed (via env), the store masked: the setting for the tests
    * written against the catalog default. */
  private def onlyZaiKey: Seq[(String, Option[String])] =
    noProviderKeys ++ Seq("ZAI_API_KEY" -> Some("sk-test"), Credentials.NoKeysEnv -> Some("1"))

  /** No provider keyed anywhere (env or store). */
  private def noKeys: Seq[(String, Option[String])] =
    noProviderKeys :+ (Credentials.NoKeysEnv -> Some("1"))

  private def choose(
      config: AppConfig = AppConfig.empty,
      envProvider: Option[String] = None,
      envModel: Option[String] = None
  ): (Provider, Model) =
    ModelSelection.choose(config, envProvider, envModel) match
      case Right(pm) => pm
      case Left(err) => fail(s"unexpected error: $err")

  test("default selection is ZAI + its first model (glm-5.2) when ZAI is keyed") {
    withEnv(onlyZaiKey*):
      val (p, m) = choose()
      assertEquals(p.name, "ZAI")
      assertEquals(m.id, Providers.zai.models.head.id)
      assertEquals(m.id, "glm-5.2")
  }

  test("the default provider is the first with a key: only Kimi keyed → Kimi") {
    withEnv((noProviderKeys ++ Seq("KIMI_API_KEY" -> Some("sk-test"), Credentials.NoKeysEnv -> Some("1")))*):
      val (p, m) = choose()
      assertEquals(p.name, "Kimi")
      assertEquals(m.id, Providers.kimi.models.head.id)
  }

  test("no key anywhere → the catalog head, which then starts keyless") {
    withEnv(noKeys*):
      val (p, _) = choose()
      assertEquals(p.name, Providers.all.head.name)
  }

  test("the base-URL env var overrides the catalog endpoint URL") {
    withEnv("ZAI_BASE_URL" -> None):
      assertEquals(Providers.zai.effectiveBaseUrl, "https://api.z.ai/api/anthropic")
    withEnv("ZAI_BASE_URL" -> Some("https://proxy.local/anthropic")):
      assertEquals(Providers.zai.effectiveBaseUrl, "https://proxy.local/anthropic")
  }

  test("a stored credentials key counts toward apiKey, and the env var wins over it") {
    val home = TestFs.tempDir("auk-modelsel-home")
    withEnv("HOME" -> Some(home), "KIMI_API_KEY" -> None, Credentials.NoKeysEnv -> None):
      Credentials.invalidate()
      try
        assertEquals(Credentials.save("kimi", "sk-store"), Right(()))
        assertEquals(Providers.kimi.apiKey, Some("sk-store"))
        withEnv("KIMI_API_KEY" -> Some("sk-env")):
          assertEquals(Providers.kimi.apiKey, Some("sk-env"))
      finally Credentials.invalidate()
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

  test("resolve degrades to a stub endpoint when no key is found anywhere") {
    withEnv((noKeys ++ Seq(ModelSelection.ProviderEnv -> None, ModelSelection.ModelEnv -> None))*):
      val r = ModelSelection.resolve(AppConfig.empty) match
        case Right(r)  => r
        case Left(err) => fail(s"resolve must not fail on a missing key: $err")
      assertEquals(r.provider.name, "ZAI")
      assert(r.keyMissing.exists(_.contains("ZAI_API_KEY")), s"keyMissing was: ${r.keyMissing}")
      assert(r.keyMissing.exists(_.contains("/login")), s"keyMissing was: ${r.keyMissing}")
      assert(r.endpoint.isInstanceOf[MissingKeyEndpoint])
  }

  test("resolve with the key present carries a live endpoint and no notice") {
    withEnv((onlyZaiKey ++ Seq(ModelSelection.ProviderEnv -> None, ModelSelection.ModelEnv -> None))*):
      val r = ModelSelection.resolve(AppConfig.empty).toOption.get
      assertEquals(r.keyMissing, None)
      assert(!r.endpoint.isInstanceOf[MissingKeyEndpoint])
  }

  test("byRef still refuses a provider whose key is unset") {
    withEnv(noKeys*):
      val err = ModelSelection.byRef("zai", "glm-5.2").left.toOption.get
      assert(err.contains("ZAI_API_KEY"))
  }
