package auk.llm.endpoint

import gears.async.Async
import gears.async.default.given

/** The stub endpoint a keyless session runs on: every request fails at once,
  * non-transiently, with the provider's missing-key message — so the engine
  * surfaces it as the turn's end instead of riding the retry schedule. */
class MissingKeyEndpointSuite extends munit.FunSuite:

  private def asyncTest(name: String)(body: Async.Spawn ?=> Unit): Unit =
    test(name)(Async.fromSync(body))

  private val ep = MissingKeyEndpoint("ZAI: environment variable ZAI_API_KEY is not set")
  private val config = LLMConfig(model = "glm-5.2")

  asyncTest("invoke fails at once with a non-transient error"):
    ep.invoke(List(Message.user("hi")), config) match
      case Left(err) =>
        assert(err.description.contains("ZAI_API_KEY"))
        assert(!err.transient)
      case Right(r) => fail(s"unexpected success: $r")

  asyncTest("stream delivers a terminal non-transient error"):
    val ch = ep.stream(List(Message.user("hi")), config)
    ch.read() match
      case Right(Left(err)) =>
        assert(err.description.contains("ZAI_API_KEY"))
        assert(!err.transient)
      case other => fail(s"expected a terminal error, got: $other")
