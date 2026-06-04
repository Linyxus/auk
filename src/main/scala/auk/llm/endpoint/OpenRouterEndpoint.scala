package auk.llm.endpoint

/** OpenRouter endpoint factory.
  *
  * OpenRouter exposes an OpenAI Chat Completions-compatible API, so this
  * delegates entirely to [[OpenAICompletionEndpoint]] with the appropriate
  * base URL and API key.
  */
object OpenRouterEndpoint extends EndpointProvider:
  type EndpointType = OpenAICompletionEndpoint

  override def create(config: EndpointConfig): OpenAICompletionEndpoint =
    OpenAICompletionEndpoint.create(config)

  override def createFromEnv(): OpenAICompletionEndpoint =
    val apiKey = auk.platform.Platform.env
      .get("OPENROUTER_API_KEY")
      .getOrElse(throw RuntimeException("OPENROUTER_API_KEY environment variable is not set"))
    val baseUrl =
      auk.platform.Platform.env.get("OPENROUTER_BASE_URL").getOrElse("https://openrouter.ai/api/v1")
    create(EndpointConfig(baseUrl = baseUrl, apiKey = apiKey))
