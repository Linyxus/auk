package auk.llm.provider

import auk.llm.endpoint.Endpoint
import auk.platform.Platform
import auk.utils.Result
import auk.utils.Result.ok

/** A fully-resolved choice: which provider, which of its models, and a live
  * endpoint wired to that provider — everything the engine needs to run.
  */
final case class ResolvedModel(provider: Provider, model: Model, endpoint: Endpoint)

/** Resolves the active provider + model against the [[Providers]] catalog.
  *
  * Selection follows the same configuration style as the rest of Auk (API keys,
  * base URLs): environment overrides over a built-in default. This is the single
  * seam a future config file would plug into — callers never name a provider or
  * model string directly.
  *
  *   - `AUK_PROVIDER` — provider display name, matched case-insensitively
  *     (e.g. `OpenRouter`). Defaults to [[defaultProvider]].
  *   - `AUK_MODEL` — a model id offered by the chosen provider
  *     (e.g. `deepseek/deepseek-v4-flash`). Defaults to that provider's first
  *     listed model.
  *
  * Every failure path (unknown provider, unknown model, missing API key) yields
  * a human-readable message rather than throwing.
  */
object ModelSelection:
  val ProviderEnv = "AUK_PROVIDER"
  val ModelEnv = "AUK_MODEL"

  /** Provider used when `AUK_PROVIDER` is unset. */
  val defaultProvider: Provider = Providers.openRouter

  def resolve(): Result[ResolvedModel, String] = Result:
    val provider = Platform.env.get(ProviderEnv) match
      case Some(name) =>
        Providers
          .byName(name)
          .toRight(
            s"Unknown provider '$name' (from $ProviderEnv). " +
              s"Known providers: ${Providers.all.map(_.name).mkString(", ")}."
          )
          .ok
      case None => defaultProvider

    val model = Platform.env.get(ModelEnv) match
      case Some(id) =>
        provider
          .model(id)
          .toRight(
            s"Provider '${provider.name}' offers no model '$id' (from $ModelEnv). " +
              s"Available: ${provider.models.map(_.id).mkString(", ")}."
          )
          .ok
      case None =>
        provider.models.headOption
          .toRight(s"Provider '${provider.name}' has no models configured.")
          .ok

    val endpoint = provider.endpoint.ok
    ResolvedModel(provider, model, endpoint)
