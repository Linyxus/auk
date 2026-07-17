package auk.agent

import auk.llm.endpoint.ToolSchema

/** Rough, display-only token estimates for the context-window gauge.
  *
  * These are deliberately cheap heuristics, not a real tokenizer. The gauge they
  * feed is refreshed by *exact* usage the moment a model round reports it (see
  * [[auk.tui.ChatState.withContextUsage]]); an estimate only has to be in the
  * right ballpark until then. Two situations have no exact figure to lean on and
  * so must estimate: a fresh post-compaction checkpoint (before its first round
  * runs) and a resumed session's trailing, not-yet-answered events. */
object TokenEstimate:
  /** Estimate the tokens in `text`.
    *
    * Subword tokenizers pack roughly four ASCII characters into a token, but CJK
    * and other non-ASCII scripts run closer to one token per character. Counting
    * the two populations separately keeps the gauge from badly under-reading a
    * conversation held in a non-Latin language — the old chars/4 rule would have
    * shown such a session at a quarter of its true occupancy. Never returns 0 for
    * non-empty text, so a short summary or field still registers. */
  def estimate(text: String): Long =
    var ascii = 0
    var wide = 0
    text.foreach(c => if c < 128 then ascii += 1 else wide += 1)
    math.max(1L, math.round(ascii / 4.0) + wide)

  /** Estimate the tokens the advertised tool schemas add to every prompt.
    *
    * Tool definitions are serialized into each request, so a post-compaction
    * prompt is far larger than its summary alone: the model still sees the whole
    * tool catalogue. This sums, per tool, the text the model reads — the name,
    * the description, and each parameter's name, type, description, and enum
    * values (recursing into array element schemas) — plus a flat constant for the
    * JSON scaffolding (braces, the `type`/`properties`/`required` keys, quoting
    * and punctuation) that wraps every tool regardless of content. A display
    * heuristic for the gauge, not the true serialized size. */
  def toolSchemaTokens(schemas: List[ToolSchema]): Long =
    schemas.map(toolTokens).sum

  /** Per-tool token budget charged for JSON structure independent of the tool's
    * own text — a rough average across the endpoints' wire formats. */
  private val PerToolScaffolding = 25L

  private def toolTokens(schema: ToolSchema): Long =
    val params = schema.parameters.properties.map((name, prop) => estimate(name) + propertyTokens(prop)).sum
    estimate(schema.name) + estimate(schema.description) + params + PerToolScaffolding

  /** Tokens for one parameter's schema: its type, description, and enum values,
    * plus — for arrays — the element schema, recursively. The parameter's own
    * name is charged by the caller, as nested element schemas carry none. */
  private def propertyTokens(prop: ToolSchema.Property): Long =
    estimate(prop.`type`) + estimate(prop.description) +
      prop.enumValues.map(estimate).sum +
      prop.items.map(propertyTokens).getOrElse(0L)
