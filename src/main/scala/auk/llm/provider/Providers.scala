package auk.llm.provider

import auk.llm.endpoint.{ThinkingMode, EffortLevel}

/** Built-in default catalog of providers and their models.
  *
  * These are starter entries: model ids, display names, and context windows are
  * best-effort defaults meant to be edited or extended (eventually from user
  * config). Adding a model here is enough to make it selectable.
  */
object Providers:

  // Temporarily disabled (OpenAI, Anthropic) — ZAI and OpenRouter are active.
  // Uncomment to restore.
  // /** OpenAI, via the native Responses API. */
  // val openAI: Provider = Provider(
  //   name = "OpenAI",
  //   kind = ProviderKind.OpenAI(responses = true),
  //   baseUrl = "https://api.openai.com/v1",
  //   apiKeyEnv = "OPENAI_API_KEY",
  //   models = List(
  //     Model("gpt-5.5", "GPT-5.5", contextWindow = 1_050_000),
  //     Model("gpt-5.4", "GPT-5.4", contextWindow = 1_050_000),
  //     Model("gpt-5.4-mini", "GPT-5.4 mini", contextWindow = 400_000),
  //     Model("gpt-5.3-codex", "GPT-5.3-Codex", contextWindow = 400_000)
  //   )
  // )

  // /** Anthropic, via the Messages API. */
  // val anthropic: Provider = Provider(
  //   name = "Anthropic",
  //   kind = ProviderKind.Anthropic,
  //   baseUrl = "https://api.anthropic.com",
  //   apiKeyEnv = "ANTHROPIC_API_KEY",
  //   models = List(
  //     Model("claude-opus-4-8", "Claude Opus 4.8", contextWindow = 1_000_000),
  //     Model("claude-sonnet-4-6", "Claude Sonnet 4.6", contextWindow = 1_000_000),
  //   )
  // )

  /** OpenRouter: an OpenAI-compatible gateway (Chat Completions), so
    * `OpenAI(responses = false)`.
    */
  val openRouter: Provider = Provider(
    name = "OpenRouter",
    kind = ProviderKind.Anthropic,
    baseUrl = "https://openrouter.ai/api",
    apiKeyEnv = "OPENROUTER_API_KEY",
    models = List(
      Model("z-ai/glm-5.2", "GLM 5.2", contextWindow = 1_000_000),
      Model("z-ai/glm-5.1", "GLM 5.1", contextWindow = 202_752),
      Model(
        "moonshotai/kimi-k3",
        "Kimi K3",
        contextWindow = 1_000_000,
        thinking = ThinkingMode.Effort(EffortLevel.Max)
      ),
      Model("minimax/minimax-m3", "MiniMax M3", contextWindow = 1_048_576),
      Model("deepseek/deepseek-v4-flash", "DeepSeek V4 Flash", contextWindow = 1_048_576),
      Model("deepseek/deepseek-v4-pro", "DeepSeek V4 Pro", contextWindow = 1_048_576),
    )
  )

  /** ZAI (GLM coding plan), via its Anthropic Messages-compatible endpoint.
    *
    * z.ai exposes both an OpenAI Chat Completions surface (`/api/coding/paas/v4`)
    * and an Anthropic Messages surface (`/api/anthropic`, what the official
    * `@z_ai/coding-helper` configures Claude Code to use). We take the Anthropic
    * route because it returns *signed* thinking blocks that [[AnthropicEndpoint]]
    * replays across tool calls — so GLM's reasoning is preserved within a turn,
    * which the Chat Completions route cannot do. The SDK appends `/v1/messages`;
    * the API key is sent as `x-api-key` (z.ai also accepts it as a bearer token).
    */
  val zai: Provider = Provider(
    name = "ZAI",
    kind = ProviderKind.Anthropic,
    baseUrl = "https://api.z.ai/api/anthropic",
    apiKeyEnv = "ZAI_API_KEY",
    models = List(
      // First entry is the default model for this provider.
      Model(
        "glm-5.2",
        "GLM 5.2",
        contextWindow = 1_000_000,
        thinking = ThinkingMode.Effort(EffortLevel.Max)
      ),
      Model("glm-5.1", "GLM 5.1", contextWindow = 200_000),
    ),
    notes = Some("The GLM coding plan — subscribe at https://z.ai/subscribe")
  )

  /** Kimi (Moonshot coding plan), via its Anthropic Messages-compatible
    * endpoint. The SDK appends `/v1/messages` to the base URL.
    */
  val kimi: Provider = Provider(
    name = "Kimi",
    kind = ProviderKind.Anthropic,
    baseUrl = "https://api.kimi.com/coding/",
    apiKeyEnv = "KIMI_API_KEY",
    models = List(
      Model(
        "k3",
        "K3",
        contextWindow = 1_000_000,
        thinking = ThinkingMode.Effort(EffortLevel.Max)
      )
    ),
    notes = Some("The Kimi subscription — see https://www.kimi.com/membership/pricing")
  )

  /** All built-in providers, in default-preference order: when nothing names a
    * provider, `ModelSelection` picks the first entry here whose API key is
    * present. */
  val all: List[Provider] = List(zai, kimi, openRouter)

  /** Look up a provider by display name (case-insensitive). */
  def byName(name: String): Option[Provider] =
    all.find(_.name.equalsIgnoreCase(name))
