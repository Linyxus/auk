package auk.llm.provider

import auk.config.Credentials
import auk.llm.endpoint.{Endpoint, EndpointConfig, OpenAICompletionEndpoint, OpenAIEndpoint, AnthropicEndpoint, ThinkingMode}
import auk.platform.Platform
import auk.utils.Result

/** Which SDK client class backs a provider.
  *
  * This selects the concrete [[Endpoint]] implementation (and therefore the wire
  * protocol) used to talk to the provider:
  *   - [[OpenAI]] → the `openai` client. `responses = true` uses the newer
  *     Responses API ([[OpenAIEndpoint]]); `responses = false` uses Chat
  *     Completions ([[OpenAICompletionEndpoint]]), which also covers every
  *     OpenAI-compatible service (OpenRouter, Ollama, …).
  *   - [[Anthropic]] → the `@anthropic-ai/sdk` client speaking the Messages API.
  */
enum ProviderKind:
  case OpenAI(responses: Boolean)
  case Anthropic

/** A single model offered by a provider. */
case class Model(
    /** Wire identifier passed to the endpoint (e.g. `deepseek/deepseek-v4-flash`). */
    id: String,
    /** Human-facing display name. */
    name: String,
    /** Maximum context window, in tokens. */
    contextWindow: Int,
    /** Default reasoning effort for this model. Most models use
      * [[ThinkingMode.Auto]] (the provider's adaptive default); override per
      * model where a different effort is wanted. Must be a mode the provider's
      * endpoint accepts (e.g. Anthropic rejects `Effort`, OpenAI-family rejects
      * `Budget`). */
    thinking: ThinkingMode = ThinkingMode.Auto
)

/** A configurable LLM provider: a client kind, where to reach it, which env var
  * holds its API key, and the models it offers.
  */
case class Provider(
    /** Human-facing display name (e.g. `OpenRouter`). */
    name: String,
    /** SDK client class to use. */
    kind: ProviderKind,
    /** Base URL of the provider's API. */
    baseUrl: String,
    /** Name of the environment variable holding the API key (e.g. `OPENROUTER_API_KEY`). */
    apiKeyEnv: String,
    /** Models this provider offers. */
    models: List[Model]
):
  /** Look up the API key: the provider's env var, else the user-level
    * credentials store (`~/.auk/credentials`, written by /login). Env wins so
    * scripts and CI keep overriding per process. */
  def apiKey: Option[String] = Platform.env.get(apiKeyEnv).orElse(Credentials.get(name))

  /** Build a live [[Endpoint]] for this provider, reading the API key via
    * [[apiKey]]. Fails with a human-readable message if no key is found.
    */
  def endpoint: Result[Endpoint, String] =
    apiKey match
      case None =>
        Left(s"$name: no API key found — set $apiKeyEnv or run /login")
      case Some(key) =>
        val config = EndpointConfig(baseUrl = baseUrl, apiKey = key)
        val ep = kind match
          case ProviderKind.OpenAI(true)  => OpenAIEndpoint.create(config)
          case ProviderKind.OpenAI(false) => OpenAICompletionEndpoint.create(config)
          case ProviderKind.Anthropic     => AnthropicEndpoint.create(config)
        Right(ep)

  /** Find a model offered by this provider by its wire id. */
  def model(id: String): Option[Model] = models.find(_.id == id)
