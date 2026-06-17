package auk.webui.dev

import auk.workflow.OrchestrationEvent
import OrchestrationEvent.*

/** Scripted demo scenarios `(delayMs, event)`, faithful to what the real
  * `WorkflowBridge` emits. Drive the mock server's SSE endpoint and the tests. */
object Scenarios:
  type Script = Vector[(Int, OrchestrationEvent)]

  val names: List[String] = List("fanout", "flatMapFrontier", "loop", "failures", "bigFanout")

  def byName(name: String): Script = name match
    case "fanout"          => fanout
    case "flatMapFrontier" => flatMapFrontier
    case "loop"            => loop
    case "failures"        => failures
    case "bigFanout"       => bigFanout
    case _                 => fanout

  /** A node's full lifecycle from `t0` ms: declare+queue, then start+progress,
    * then finish. */
  private def life(run: String, group: Option[String], id: String, deps: List[String], t0: Int, ok: Boolean = true): Script =
    Vector(
      t0 -> NodeDeclared(run, id, group, deps),
      t0 -> NodeQueued(run, id),
      (t0 + 300) -> NodeStarted(run, id, s"work on $id"),
      (t0 + 600) -> NodeProgress(run, id, 120L, 480L, Some("eval_scala")),
      (t0 + 1200) -> NodeFinished(run, id, ok, if ok then s"$id: done" else s"$id: failed")
    )

  private def fanout: Script =
    val run = "fanout-1"
    (0 -> GroupDeclared(run, "g1", "scan", "Scan each file for issues", None)) +:
      (life(run, Some("g1"), "alpha", Nil, 100) ++
        life(run, Some("g1"), "beta", Nil, 300) ++
        life(run, Some("g1"), "gamma", Nil, 500))

  private def flatMapFrontier: Script =
    val run = "frontier-1"
    val head =
      (0 -> GroupDeclared(run, "g1", "scan", "Scan, then summarize", None)) +:
        (life(run, Some("g1"), "a", Nil, 100) ++ life(run, Some("g1"), "b", Nil, 200))
    // summary declared late (after the leaves finish), depending on them
    head ++ life(run, Some("g1"), "summary", List("a", "b"), 1600)

  private def loop: Script =
    val run = "loop-1"
    val g = 0 -> GroupDeclared(run, "revise", "revise", "Draft and revise until accepted", None)
    val rounds = (1 to 3).flatMap { r =>
      val base = (r - 1) * 2600 + 100
      val writerDeps = if r == 1 then Nil else List(s"reviewer-${r - 1}")
      life(run, Some("revise"), s"writer-$r", writerDeps, base) ++
        life(run, Some("revise"), s"reviewer-$r", List(s"writer-$r"), base + 1300)
    }.toVector
    g +: rounds

  private def failures: Script =
    val run = "fail-1"
    (0 -> GroupDeclared(run, "g1", "attempt", "Try things", None)) +:
      (life(run, Some("g1"), "ok-node", Nil, 100) ++
        life(run, Some("g1"), "bad-node", Nil, 300, ok = false) :+
        (1800 -> Log(run, "one node failed; see bad-node")))

  private def bigFanout: Script =
    val run = "big-1"
    val ids = (1 to 8).map(i => s"n$i").toVector
    val g = Vector[(Int, OrchestrationEvent)](0 -> GroupDeclared(run, "g1", "sweep", "Sweep many files (cap 2)", None))
    // declare + queue all up front
    val declares = ids.flatMap(id => Vector(0 -> NodeDeclared(run, id, Some("g1"), Nil), 50 -> NodeQueued(run, id)))
    // run in pairs (concurrency cap 2): only two are Running at any time
    val running = ids.grouped(2).zipWithIndex.flatMap { (pair, k) =>
      val base = 300 + k * 900
      pair.flatMap { id =>
        Vector(
          base -> NodeStarted(run, id, s"work $id"),
          (base + 300) -> NodeProgress(run, id, 100L, 300L, Some("grep")),
          (base + 700) -> NodeFinished(run, id, true, s"$id done")
        )
      }
    }.toVector
    g ++ declares ++ running
