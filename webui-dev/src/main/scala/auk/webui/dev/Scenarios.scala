package auk.webui.dev

import auk.workflow.{OrchestrationEvent, TranscriptEvent, WireMessage}
import OrchestrationEvent.*
import TranscriptEvent.*

/** Scripted demo scenarios as a timeline of `(delayMs, WireMessage)`, faithful to
  * what the real host emits: forest-structure [[OrchestrationEvent]]s interleaved
  * with per-agent transcript [[TranscriptEvent]]s. They drive the mock server's
  * SSE endpoint and the tests. */
object Scenarios:
  type Script = Vector[(Int, WireMessage)]

  val names: List[String] = List("fanout", "flatMapFrontier", "loop", "failures", "bigFanout", "multi", "paused")

  def byName(name: String): Script = name match
    case "fanout"          => fanout
    case "flatMapFrontier" => flatMapFrontier
    case "loop"            => loop
    case "failures"        => failures
    case "bigFanout"       => bigFanout
    case "multi"           => multi
    case "paused"          => paused
    case _                 => fanout

  private def ev(e: OrchestrationEvent): WireMessage = WireMessage.Event(e)
  private def act(e: TranscriptEvent): WireMessage = WireMessage.Activity(e)

  /** A node's full lifecycle from `t0` ms: declare + queue, start, then a streamed
    * transcript (prose deltas, a tool call whose output fills in later, a closing
    * line), then finish. `ok = false` makes the tool error and the node fail. */
  private def life(run: String, group: Option[String], id: String, deps: List[String], t0: Int, ok: Boolean = true): Script =
    val c = s"$id-c1"
    Vector(
      t0          -> ev(NodeDeclared(run, id, group, deps)),
      t0          -> ev(NodeQueued(run, id)),
      (t0 + 250)  -> ev(NodeStarted(run, id, s"Inspect `$id` and report any issues.")),
      // a reasoning block: streams open, then folds once the agent moves on to prose
      (t0 + 300)  -> act(Thought(run, id, s"Planning how to inspect `$id`. ")),
      (t0 + 370)  -> act(Thought(run, id, "I'll read the source, then run a quick check for severity-2 issues.")),
      (t0 + 420)  -> act(Said(run, id, s"Taking a look at `$id`. ")),
      (t0 + 560)  -> act(Said(run, id, "Let me read the source and run a check.")),
      (t0 + 600)  -> ev(NodeProgress(run, id, 120L, 480L, Some("eval_scala"))),
      // `raw` so \n and \" stay literal JSON escapes (a valid {"code": "<scala>"} the UI parses)
      (t0 + 700)  -> act(ToolCalled(run, id, c, "eval_scala",
        raw"""{"code": "// scan $id for issues\nval issues = Linter.check(\"src/$id.scala\")\nissues.count(i => i.severity >= 2)"}""")),
      (t0 + 1000) -> act(ToolReturned(run, id, c,
        if ok then s"src/$id.scala — 2 defs, 0 warnings" else s"error: unresolved reference in src/$id.scala", isError = !ok)),
      (t0 + 1040) -> ev(NodeProgress(run, id, 220L, 940L, None)),
      (t0 + 1100) -> act(Said(run, id, if ok then s"\n\nAll clear for `$id`." else s"\n\nFound a problem in `$id`.")),
      (t0 + 1200) -> ev(NodeFinished(run, id, ok, if ok then s"$id: done" else s"$id: failed"))
    )

  private def fanout: Script =
    val run = "fanout-1"
    val code =
      """wf.start[List[String]]:
        |  val scan = group("scan", "Scan each file for issues")
        |  inGroup(scan):
        |    Agent.all(List("alpha", "beta", "gamma").map(f => agent[String](s"Inspect $f", id = f)))""".stripMargin
    (0 -> ev(WorkflowCode(run, code))) +:
      (0 -> ev(GroupDeclared(run, "g1", "scan", "Scan each file for issues", None))) +:
      (life(run, Some("g1"), "alpha", Nil, 100) ++
        life(run, Some("g1"), "beta", Nil, 300) ++
        life(run, Some("g1"), "gamma", Nil, 500))

  private def flatMapFrontier: Script =
    val run = "frontier-1"
    val code =
      """wf.start[String]:
        |  val scan = group("scan", "Scan, then summarize")
        |  inGroup(scan):
        |    Agent.all(List("a", "b").map(f => agent[String](s"scan $f", id = f))).flatMap: rs =>
        |      agent[String](s"Summarize ${rs.mkString(", ")}", id = "summary")""".stripMargin
    val head =
      (0 -> ev(WorkflowCode(run, code))) +:
        (0 -> ev(GroupDeclared(run, "g1", "scan", "Scan, then summarize", None))) +:
        (life(run, Some("g1"), "a", Nil, 100) ++ life(run, Some("g1"), "b", Nil, 200))
    // summary declared late (after the leaves finish), depending on them
    head ++ life(run, Some("g1"), "summary", List("a", "b"), 1600)

  private def loop: Script =
    val run = "loop-1"
    val code =
      """wf.start[Draft]:
        |  val maxRounds = 5
        |  val revise = group("revise", "Draft and revise until accepted")
        |  def attempt(round: Int, prior: Option[(Draft, String)]): Agent[Draft] =
        |    val prompt = prior match
        |      case None => "Write the first draft."
        |      case Some((prev, feedback)) => s"Revise:\n${prev.content}\n\nFeedback: $feedback"
        |    inGroup(revise):
        |      agent[Draft](prompt, id = s"writer-$round").flatMap: draft =>
        |        agent[Review](s"Review:\n${draft.content}", id = s"reviewer-$round").flatMap: review =>
        |          if review.accepted || round >= maxRounds then Agent.pure(draft)
        |          else attempt(round + 1, Some((draft, review.feedback)))
        |  attempt(1, None)""".stripMargin
    val g = 0 -> ev(GroupDeclared(run, "revise", "revise", "Draft and revise until accepted", None))
    val rounds = (1 to 3).flatMap { r =>
      val base = (r - 1) * 2600 + 100
      val writerDeps = if r == 1 then Nil else List(s"reviewer-${r - 1}")
      life(run, Some("revise"), s"writer-$r", writerDeps, base) ++
        life(run, Some("revise"), s"reviewer-$r", List(s"writer-$r"), base + 1300)
    }.toVector
    (0 -> ev(WorkflowCode(run, code))) +: g +: rounds

  private def failures: Script =
    val run = "fail-1"
    val code =
      """wf.start[List[String]]:
        |  val attempt = group("attempt", "Try things")
        |  inGroup(attempt):
        |    Agent.all(List("ok-node", "bad-node").map(n => agent[String](n, id = n)))""".stripMargin
    (0 -> ev(WorkflowCode(run, code))) +:
      (0 -> ev(GroupDeclared(run, "g1", "attempt", "Try things", None))) +:
      (life(run, Some("g1"), "ok-node", Nil, 100) ++
        life(run, Some("g1"), "bad-node", Nil, 300, ok = false) :+
        (1800 -> act(Said(run, "ok-node", " (verified twice)"))) :+
        (1850 -> ev(Log(run, "one node failed; see bad-node"))))

  /** Three concurrent runs with distinct ids — the case the run-switcher dropdown
    * exists for. All three appear at once (so the dropdown shows immediately), then
    * finish staggered in different overall states (done / done / failed), so the
    * menu's status dots cycle through several colours as it plays. */
  private def multi: Script =
    def head(run: String, group: String, desc: String, ids: List[String]): Script =
      val list = ids.map("\"" + _ + "\"").mkString(", ")
      val code =
        s"""wf.start[List[String]]:
           |  val g = group("$group", "$desc")
           |  inGroup(g):
           |    Agent.all(List($list).map(f => agent[String](f, id = f)))""".stripMargin
      Vector(0 -> ev(WorkflowCode(run, code)), 0 -> ev(GroupDeclared(run, "g1", group, desc, None)))

    val review = "review-7a31"
    val audit = "audit-22c8"
    val build = "build-9f04"
    head(review, "review", "Review the diff", List("alpha", "beta", "gamma")) ++
      life(review, Some("g1"), "alpha", Nil, 100) ++
      life(review, Some("g1"), "beta", Nil, 350) ++
      life(review, Some("g1"), "gamma", Nil, 600) ++
    head(audit, "audit", "Audit for security issues", List("scan", "secrets")) ++
      life(audit, Some("g1"), "scan", Nil, 250) ++
      life(audit, Some("g1"), "secrets", Nil, 900, ok = false) ++
    head(build, "build", "Type-check and test", List("compile", "test")) ++
      life(build, Some("g1"), "compile", Nil, 500) ++
      life(build, Some("g1"), "test", List("compile"), 1700)

  /** A run paused with two sub-agents finished and a third still in flight — the
    * case the pause/resume control exists for. Its dot reads "paused", the side
    * head offers Resume, and `gamma` (running when paused) shows as interrupted
    * (amber) rather than failed. */
  private def paused: Script =
    val run = "review-paused"
    val code =
      """wf.start[List[String]]:
        |  val scan = group("scan", "Scan each file for issues")
        |  inGroup(scan):
        |    Agent.all(List("alpha", "beta", "gamma").map(f => agent[String](s"Inspect $f", id = f)))""".stripMargin
    (0 -> ev(WorkflowCode(run, code))) +:
      (0 -> ev(GroupDeclared(run, "g1", "scan", "Scan each file for issues", None))) +:
      (life(run, Some("g1"), "alpha", Nil, 100) ++
        life(run, Some("g1"), "beta", Nil, 350) ++
        List(
          600  -> ev(NodeDeclared(run, "gamma", Some("g1"), Nil)),
          700  -> ev(NodeStarted(run, "gamma", "Inspect gamma")),
          800  -> act(Said(run, "gamma", "Scanning gamma for issues…")),
          900  -> ev(NodeProgress(run, "gamma", 1200, 340, Some("eval_scala"))),
          1700 -> ev(NodeInterrupted(run, "gamma")),
          1700 -> ev(WorkflowPaused(run))
        ))

  private def bigFanout: Script =
    val run = "big-1"
    val ids = (1 to 8).map(i => s"n$i").toVector
    val code =
      """wf.start[List[String]]:
        |  val sweep = group("sweep", "Sweep many files (cap 2)")
        |  inGroup(sweep):
        |    Agent.all((1 to 8).toList.map(i => agent[String](s"Sweep n$i", id = s"n$i")))""".stripMargin
    val g = Vector[(Int, WireMessage)](
      0 -> ev(WorkflowCode(run, code)),
      0 -> ev(GroupDeclared(run, "g1", "sweep", "Sweep many files (cap 2)", None)))
    // declare + queue all up front
    val declares = ids.flatMap(id => Vector(0 -> ev(NodeDeclared(run, id, Some("g1"), Nil)), 50 -> ev(NodeQueued(run, id))))
    // run in pairs (concurrency cap 2): only two are Running at any time, each with a small transcript
    val running = ids.grouped(2).zipWithIndex.flatMap { (pair, k) =>
      val base = 300 + k * 900
      pair.flatMap { id =>
        val c = s"$id-c1"
        Vector(
          base -> ev(NodeStarted(run, id, s"Sweep `$id`.")),
          (base + 60) -> act(Said(run, id, s"Sweeping `$id`…")),
          (base + 120) -> ev(NodeProgress(run, id, 100L, 300L, Some("grep"))),
          (base + 200) -> act(ToolCalled(run, id, c, "grep", s"""{"pattern": "TODO", "path": "$id/"}""")),
          (base + 500) -> act(ToolReturned(run, id, c, s"$id/: 0 matches", isError = false)),
          (base + 560) -> act(Said(run, id, " clean.")),
          (base + 700) -> ev(NodeFinished(run, id, true, s"$id done"))
        )
      }
    }.toVector
    g ++ declares ++ running
