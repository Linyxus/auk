package auk.llm.provider

import auk.config.AppConfig
import auk.llm.endpoint.{Endpoint, MissingKeyEndpoint}
import auk.platform.Platform
import auk.utils.Result
import auk.utils.Result.ok

/** A fully-resolved choice: which provider, which of its models, and a live
  * endpoint wired to that provider — everything the engine needs to run.
  *
  * `keyMissing` is set when the provider's API key env var was unset at
  * startup: it carries the human-readable notice, and `endpoint` is then a
  * [[MissingKeyEndpoint]] that fails every request with that same message.
  */
final case class ResolvedModel(
    provider: Provider,
    model: Model,
    endpoint: Endpoint,
    keyMissing: Option[String] = None
)

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
  * Nothing here touches the filesystem: the config arrives already loaded. Every
  * failure path (unknown provider, unknown model, missing API key) yields a
  * human-readable message rather than throwing — with one deliberate exception:
  * [[resolve]] treats a missing API key as a degraded start, not a failure.
  */
object ModelSelection:
  val ProviderEnv = "AUK_PROVIDER"
  val ModelEnv = "AUK_MODEL"

  /** Provider used when neither env nor config names one. */
  val defaultProvider: Provider = Providers.zai

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

  /** [[choose]] against the ambient env overrides, plus the endpoint. The caller
    * supplies the already-loaded config — reading `.auk/config` is the entry
    * point's job, done once, so that a malformed file is diagnosed at one site.
    *
    * A missing API key is deliberately NOT a failure here: the session still
    * opens, on a [[MissingKeyEndpoint]] that fails each request with the
    * notice, and `keyMissing` carries that notice for the transcript. The key
    * can only arrive via the environment — the fix is to export it and restart
    * — so refusing to start would just trade a usable session for an error
    * print. Unknown provider/model names stay fatal: those are config typos,
    * and starting on them would silently run something else. A runtime model
    * switch ([[byRef]]) also stays strict — refusing the switch with the
    * message beats swapping onto an endpoint that cannot work.
    */
  def resolve(config: AppConfig): Result[ResolvedModel, String] = Result:
    val (provider, model) =
      choose(config, Platform.env.get(ProviderEnv), Platform.env.get(ModelEnv)).ok
    provider.endpoint match
      case Right(endpoint) => ResolvedModel(provider, model, endpoint)
      case Left(missing) =>
        ResolvedModel(provider, model, MissingKeyEndpoint(missing), keyMissing = Some(missing))
