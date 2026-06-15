package auk.llm.provider

import auk.llm.endpoint.{Endpoint, LLMConfig}

/** The live model an agent talks to: a provider [[Endpoint]], the base
  * [[LLMConfig]] naming the model (tool schemas are layered on per-consumer), and
  * a display label.
  */
final case class ActiveModel(
    endpoint: Endpoint,
    config: LLMConfig,
    label: String,
    contextWindow: Int = 0,
    provider: String = "",
    baseUrl: String = ""
)

/** The currently-selected model — the single source of truth for "which model is
  * in use," shared across the [[auk.agent.Engine]] and any sub-agents and
  * swappable at runtime.
  *
  * `resolve` turns a `(providerName, modelId)` selection into a new
  * [[ActiveModel]] (building the endpoint, which can fail — e.g. on a missing API
  * key). It is injected so this type stays independent of the provider catalog
  * and config-file layers.
  */
final class ModelSession(
    initial: ActiveModel,
    resolve: (String, String) => Either[String, ActiveModel]
):
  private var current: ActiveModel = initial

  /** The model in use right now; read fresh on every turn / sub-agent run. */
  def active: ActiveModel = current

  def label: String = current.label

  /** Switch to `(providerName, modelId)`. On success swaps the live model and
    * returns it; on failure leaves the current model untouched.
    */
  def switch(providerName: String, modelId: String): Either[String, ActiveModel] =
    resolve(providerName, modelId).map: next =>
      current = next
      next

object ModelSession:
  /** A session pinned to one model — `switch` always fails. For tests and any
    * non-interactive use that never changes models.
    */
  def of(endpoint: Endpoint, config: LLMConfig, label: String = ""): ModelSession =
    new ModelSession(
      ActiveModel(endpoint, config, label),
      (_, _) => Left("model switching is not configured")
    )
