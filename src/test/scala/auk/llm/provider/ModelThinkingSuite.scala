package auk.llm.provider

import auk.llm.endpoint.{ThinkingMode, EffortLevel}

/** Per-model default reasoning effort (see [[Model.thinking]]). */
class ModelThinkingSuite extends munit.FunSuite:

  private def model(provider: Provider, id: String): Model =
    provider.model(id).getOrElse(fail(s"no model '$id' in ${provider.name}"))

  test("glm-5.2 defaults to maximum reasoning effort") {
    assertEquals(model(Providers.zai, "glm-5.2").thinking, ThinkingMode.Effort(EffortLevel.Max))
  }

  test("kimi k3 defaults to maximum reasoning effort") {
    assertEquals(model(Providers.kimi, "k3").thinking, ThinkingMode.Effort(EffortLevel.Max))
  }

  test("models without an override default to Auto") {
    assertEquals(model(Providers.zai, "glm-5.1").thinking, ThinkingMode.Auto)
    // OpenRouter models carry no per-model effort override (OpenAI-family kind).
    assertEquals(model(Providers.openRouter, "minimax/minimax-m3").thinking, ThinkingMode.Auto)
    // OpenAI/Anthropic remain disabled — see Providers.scala.
  }
