package auk.llm.provider

import auk.config.AppConfig
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
  * Selection is layered: an **environment override** wins over the **`.auk/config`
  * file**, which wins over the **built-in catalog default** — callers never name a
  * provider or model string directly.
  *
  *   - provider: `AUK_PROVIDER` env, else `[model] provider` from config, else
  *     [[defaultProvider]]. Matched case-insensitively against [[Providers]].
  *   - model: `AUK_MODEL` env, else `[model] id` from config, else the chosen
  *     provider's first listed model.
  *
  * Every failure path (invalid config, unknown provider, unknown model, missing
  * API key) yields a human-readable message rather than throwing.
  */
object ModelSelection:
  val ProviderEnv = "AUK_PROVIDER"
  val ModelEnv = "AUK_MODEL"

  /** Provider used when neither env nor config names one. */
  val defaultProvider: Provider = Providers.openRouter

  /** Pick a provider + model from a loaded config and the env overrides. Pure
    * and total (no I/O, no endpoint construction) — the testable core of
    * [[resolve]].
    */
  def choose(
      config: AppConfig,
      envProvider: Option[String],
      envModel: Option[String]
  ): Result[(Provider, Model), String] = Result:
    val cfgModel = config.model

    val provider = envProvider.orElse(cfgModel.flatMap(_.provider)) match
      case Some(name) =>
        Providers
          .byName(name)
          .toRight(
            s"Unknown provider '$name'. " +
              s"Known providers: ${Providers.all.map(_.name).mkString(", ")}."
          )
          .ok
      case None => defaultProvider

    val model = envModel.orElse(cfgModel.flatMap(_.id)) match
      case Some(id) =>
        provider
          .model(id)
          .toRight(
            s"Provider '${provider.name}' offers no model '$id'. " +
              s"Available: ${provider.models.map(_.id).mkString(", ")}."
          )
          .ok
      case None =>
        provider.models.headOption
          .toRight(s"Provider '${provider.name}' has no models configured.")
          .ok

    (provider, model)

  /** Resolve an exact `(providerName, modelId)` selection into a live endpoint,
    * against the catalog — the resolver behind a runtime model switch.
    */
  def byRef(providerName: String, modelId: String): Result[ResolvedModel, String] = Result:
    val provider = Providers
      .byName(providerName)
      .toRight(
        s"Unknown provider '$providerName'. " +
          s"Known providers: ${Providers.all.map(_.name).mkString(", ")}."
      )
      .ok
    val model = provider
      .model(modelId)
      .toRight(
        s"Provider '${provider.name}' offers no model '$modelId'. " +
          s"Available: ${provider.models.map(_.id).mkString(", ")}."
      )
      .ok
    val endpoint = provider.endpoint.ok
    ResolvedModel(provider, model, endpoint)

  def resolve(): Result[ResolvedModel, String] = Result:
    val config = AppConfig
      .load()
      .left
      .map(errs =>
        s"Invalid ${AppConfig.RelativePath}:\n" + errs.map("  " + _.render).mkString("\n")
      )
      .ok
    val (provider, model) =
      choose(config, Platform.env.get(ProviderEnv), Platform.env.get(ModelEnv)).ok
    val endpoint = provider.endpoint.ok
    ResolvedModel(provider, model, endpoint)
