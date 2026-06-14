package auk.llm.provider

/** Built-in default catalog of providers and their models.
  *
  * These are starter entries: model ids, display names, and context windows are
  * best-effort defaults meant to be edited or extended (eventually from user
  * config). Adding a model here is enough to make it selectable.
  */
object Providers:

  /** OpenAI, via the native Responses API. */
  val openAI: Provider = Provider(
    name = "OpenAI",
    kind = ProviderKind.OpenAI(responses = true),
    baseUrl = "https://api.openai.com/v1",
    apiKeyEnv = "OPENAI_API_KEY",
    models = List(
      Model("gpt-5.5", "GPT-5.5", contextWindow = 1_050_000),
      Model("gpt-5.4", "GPT-5.4", contextWindow = 1_050_000),
      Model("gpt-5.4-mini", "GPT-5.4 mini", contextWindow = 400_000),
      Model("gpt-5.3-codex", "GPT-5.3-Codex", contextWindow = 400_000)
    )
  )

  /** Anthropic, via the Messages API. */
  val anthropic: Provider = Provider(
    name = "Anthropic",
    kind = ProviderKind.Anthropic,
    baseUrl = "https://api.anthropic.com",
    apiKeyEnv = "ANTHROPIC_API_KEY",
    models = List(
      Model("claude-opus-4-8", "Claude Opus 4.8", contextWindow = 1_000_000),
      Model("claude-sonnet-4-6", "Claude Sonnet 4.6", contextWindow = 1_000_000),
    )
  )

  /** OpenRouter: an OpenAI-compatible gateway (Chat Completions), so
    * `OpenAI(responses = false)`.
    */
  val openRouter: Provider = Provider(
    name = "OpenRouter",
    kind = ProviderKind.OpenAI(responses = false),
    baseUrl = "https://openrouter.ai/api/v1",
    apiKeyEnv = "OPENROUTER_API_KEY",
    models = List(
      // First entry is the default model for this provider.
      Model("minimax/minimax-m3", "MiniMax M3", contextWindow = 1_048_576),
      Model("deepseek/deepseek-v4-flash", "DeepSeek V4 Flash", contextWindow = 1_048_576),
      Model("deepseek/deepseek-v4-pro", "DeepSeek V4 Pro", contextWindow = 1_048_576),
      Model("z-ai/glm-5.1", "GLM 5.1", contextWindow = 202_752),
    )
  )

  /** ZAI: an OpenAI-compatible coding endpoint (Chat Completions).
    */
  val zai: Provider = Provider(
    name = "ZAI",
    kind = ProviderKind.OpenAI(responses = false),
    baseUrl = "https://api.z.ai/api/coding/paas/v4",
    apiKeyEnv = "ZAI_API_KEY",
    models = List(
      Model("glm-5.1", "GLM 5.1", contextWindow = 200_000),
      Model("glm-5.2", "GLM 5.2", contextWindow = 1_000_000),
    )
  )

  /** All built-in providers. */
  val all: List[Provider] = List(openAI, anthropic, openRouter, zai)

  /** Look up a provider by display name (case-insensitive). */
  def byName(name: String): Option[Provider] =
    all.find(_.name.equalsIgnoreCase(name))
