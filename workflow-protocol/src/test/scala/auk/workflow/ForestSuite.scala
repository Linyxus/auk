package auk.workflow

import OrchestrationEvent.*

/** The pure `Forest.update` fold — the single source of truth shared by the TUI
  * and the web UI. */
class ForestSuite extends munit.FunSuite:

  test("a node moves Pending -> Queued -> Running -> Done across its events"):
    val f0 = Forest.empty.update(NodeDeclared("r", "a", None, Nil))
    assertEquals(f0.nodes.head.status, NodeStatus.Pending)
    val f1 = f0.update(NodeQueued("r", "a"))
    assertEquals(f1.nodes.head.status, NodeStatus.Queued)
    val f2 = f1.update(NodeStarted("r", "a", "go"))
    assertEquals(f2.nodes.head.status, NodeStatus.Running)
    val f3 = f2.update(NodeFinished("r", "a", true, "ok"))
    assertEquals(f3.nodes.head.status, NodeStatus.Done)

  test("NodeFinished(ok=false) marks the node Failed and records the summary"):
    val f = Forest.empty
      .update(NodeDeclared("r", "a", None, Nil))
      .update(NodeStarted("r", "a", "go"))
      .update(NodeFinished("r", "a", false, "boom"))
    assertEquals(f.nodes.head.status, NodeStatus.Failed)
    assertEquals(f.nodes.head.summary, Some("boom"))

  test("groups and nodes are kept in declaration order"):
    val f = Forest.empty
      .update(GroupDeclared("r", "g1", "one", "", None))
      .update(GroupDeclared("r", "g2", "two", "", None))
      .update(NodeDeclared("r", "a", Some("g1"), Nil))
      .update(NodeDeclared("r", "b", Some("g2"), Nil))
    assertEquals(f.groups.map(_.id), Vector("g1", "g2"))
    assertEquals(f.nodes.map(_.id), Vector("a", "b"))

  test("a re-declared group is deduped"):
    val f = Forest.empty
      .update(GroupDeclared("r", "g1", "one", "", None))
      .update(GroupDeclared("r", "g1", "one-again", "", None))
    assertEquals(f.groups.map(_.name), Vector("one"))

  test("a re-declared node is deduped"):
    val f = Forest.empty
      .update(NodeDeclared("r", "a", None, Nil))
      .update(NodeDeclared("r", "a", Some("g"), List("x")))
    assertEquals(f.nodes.size, 1)
    assertEquals(f.nodes.head.group, None) // first declaration wins

  test("NodeQueued for an undeclared node upserts it as Queued"):
    val f = Forest.empty.update(NodeQueued("r", "z"))
    assertEquals(f.nodes.map(_.id), Vector("z"))
    assertEquals(f.nodes.head.status, NodeStatus.Queued)

  test("NodeStarted for an undeclared node upserts it as Running"):
    val f = Forest.empty.update(NodeStarted("r", "z", "go"))
    assertEquals(f.nodes.head.status, NodeStatus.Running)

  test("NodeStarted records the sub-agent's prompt on the node"):
    val f = Forest.empty
      .update(NodeDeclared("r", "a", None, Nil))
      .update(NodeStarted("r", "a", "investigate the parser"))
    assertEquals(f.nodes.head.prompt, Some("investigate the parser"))

  test("NodeProgress records tokens and tool, upserting if undeclared"):
    val f = Forest.empty.update(NodeProgress("r", "z", 11L, 22L, Some("eval_scala")))
    val n = f.nodes.head
    assertEquals(n.inputTokens, 11L)
    assertEquals(n.outputTokens, 22L)
    assertEquals(n.currentTool, Some("eval_scala"))

  test("NodeProgress with None currentTool keeps the prior tool (orElse)"):
    val f = Forest.empty
      .update(NodeProgress("r", "a", 1L, 1L, Some("grep")))
      .update(NodeProgress("r", "a", 2L, 2L, None))
    assertEquals(f.nodes.head.currentTool, Some("grep"))
    assertEquals(f.nodes.head.outputTokens, 2L)

  test("NodeFinished clears currentTool and sets summary"):
    val f = Forest.empty
      .update(NodeProgress("r", "a", 1L, 1L, Some("grep")))
      .update(NodeFinished("r", "a", true, "done"))
    assertEquals(f.nodes.head.currentTool, None)
    assertEquals(f.nodes.head.summary, Some("done"))

  test("NodeDeclared records deps"):
    val f = Forest.empty.update(NodeDeclared("r", "b", Some("g"), List("a", "c")))
    assertEquals(f.nodes.head.deps, List("a", "c"))

  test("Log appends to logs in order"):
    val f = Forest.empty.update(Log("r", "first")).update(Log("r", "second"))
    assertEquals(f.logs, Vector("first", "second"))

  test("WorkflowCode records the workflow source"):
    val f = Forest.empty.update(WorkflowCode("r", "wf.start(agent[String](\"go\"))"))
    assertEquals(f.code, Some("wf.start(agent[String](\"go\"))"))

  test("WorkflowFinished is a no-op on forest content (run removal is a UI concern)"):
    val f = Forest.empty
      .update(NodeDeclared("r", "a", None, Nil))
      .update(NodeFinished("r", "a", true, "ok"))
    assertEquals(f.update(WorkflowFinished("r", true, "all done")), f)

  test("update is a pure left-fold: folding a fixed event list is stable"):
    val events = List(
      GroupDeclared("r", "g1", "hunt", "", None),
      NodeDeclared("r", "a", Some("g1"), Nil),
      NodeQueued("r", "a"),
      NodeStarted("r", "a", "go"),
      NodeProgress("r", "a", 5L, 6L, Some("t")),
      NodeFinished("r", "a", true, "ok")
    )
    val once = events.foldLeft(Forest.empty)(_.update(_))
    val twice = events.foldLeft(Forest.empty)(_.update(_))
    assertEquals(once, twice)
