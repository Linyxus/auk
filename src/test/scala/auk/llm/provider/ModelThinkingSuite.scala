package auk.llm.provider

import auk.llm.endpoint.{ThinkingMode, EffortLevel}

/** Per-model default reasoning effort (see [[Model.thinking]]). */
class ModelThinkingSuite extends munit.FunSuite:

  private def model(provider: Provider, id: String): Model =
    provider.model(id).getOrElse(fail(s"no model '$id' in ${provider.name}"))

  test("glm-5.2 defaults to maximum reasoning effort") {
    assertEquals(model(Providers.zai, "glm-5.2").thinking, ThinkingMode.Effort(EffortLevel.Max))
  }

  test("models without an override default to Auto") {
    assertEquals(model(Providers.zai, "glm-5.1").thinking, ThinkingMode.Auto)
    assertEquals(model(Providers.openRouter, "deepseek/deepseek-v4-flash").thinking, ThinkingMode.Auto)
    assertEquals(model(Providers.anthropic, "claude-opus-4-8").thinking, ThinkingMode.Auto)
  }
