package auk.webui

import auk.workflow.{Forest, ForestGroup, ForestNode, NodeStatus}

class WorkflowViewSuite extends munit.FunSuite:

  private def stateWith(f: Forest, rid: String = "r"): AppState =
    AppState(forests = Map(rid -> f), order = Vector(rid), selected = Some(rid), conn = ConnStatus.Open)

  private def forestView(f: Forest): ForestView =
    WorkflowView.from(stateWith(f)) match
      case View.Forest(fv, _) => fv
      case other              => fail(s"expected a Forest view, got $other")

  test("no selected run renders the Empty view carrying the connection status"):
    WorkflowView.from(AppState(conn = ConnStatus.Connecting)) match
      case View.Empty(c) => assertEquals(c, ConnStatus.Connecting)
      case other         => fail(s"expected Empty, got $other")

  test("each NodeStatus maps to its glyph"):
    assertEquals(StatusKind.glyph(StatusKind.Pending), "·")
    assertEquals(StatusKind.glyph(StatusKind.Queued), "◌")
    assertEquals(StatusKind.glyph(StatusKind.Done), "✓")
    assertEquals(StatusKind.glyph(StatusKind.Failed), "✗")

  test("each NodeStatus maps to its css class"):
    assertEquals(StatusKind.cssClass(StatusKind.Pending), "node-pending")
    assertEquals(StatusKind.cssClass(StatusKind.Queued), "node-queued")
    assertEquals(StatusKind.cssClass(StatusKind.Running), "node-running")
    assertEquals(StatusKind.cssClass(StatusKind.Done), "node-done")
    assertEquals(StatusKind.cssClass(StatusKind.Failed), "node-failed")

  test("a node row reflects status, glyph, tokens, tool, and summary"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running, 1L, 1500L, Some("eval_scala"), Some("wip"))))
    val row = forestView(f).sections.head.nodes.head
    assertEquals(row.statusKind, StatusKind.Running)
    assertEquals(row.glyph, "◍")
    assertEquals(row.tokensText, "1.5k tokens")
    assertEquals(row.toolText, "eval_scala")
    assertEquals(row.summary, Some("wip"))

  test("zero output tokens and no tool render as empty text fields"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Pending, 0L, 0L, None, None)))
    val row = forestView(f).sections.head.nodes.head
    assertEquals(row.tokensText, "")
    assertEquals(row.toolText, "")

  test("fmtTokens matches the TUI byte-for-byte"):
    assertEquals(WorkflowView.fmtTokens(999L), "999")
    assertEquals(WorkflowView.fmtTokens(1000L), "1.0k")
    assertEquals(WorkflowView.fmtTokens(1234L), "1.2k")
    assertEquals(WorkflowView.fmtTokens(1250L), "1.3k")
    assertEquals(WorkflowView.fmtTokens(12345L), "12.3k")

  test("nodes are grouped into sections in declared-group order"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", ""), ForestGroup("g2", "two", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done), ForestNode("b", Some("g2"), Nil, NodeStatus.Done))
    )
    assertEquals(forestView(f).sections.map(_.name), Vector(Some("one"), Some("two")))

  test("the ungrouped section comes last and only when ungrouped nodes exist"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done), ForestNode("u", None, Nil, NodeStatus.Done))
    )
    assertEquals(forestView(f).sections.map(_.id), Vector(Some("g1"), None))

  test("a declared group with no nodes produces no section"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", ""), ForestGroup("g2", "two", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done))
    )
    assertEquals(forestView(f).sections.map(_.name), Vector(Some("one")))

  test("run tabs list every run with 8-char labels and mark the selected one"):
    val s = AppState(
      forests = Map("run-aaaaaaaa" -> Forest.empty, "run-b" -> Forest.empty),
      order = Vector("run-aaaaaaaa", "run-b"),
      selected = Some("run-b"),
      conn = ConnStatus.Open
    )
    WorkflowView.from(s) match
      case View.Forest(fv, _) =>
        assertEquals(fv.tabs.map(_.runId), Vector("run-aaaaaaaa", "run-b"))
        assertEquals(fv.tabs.map(_.label), Vector("run-aaaa", "run-b"))
        assertEquals(fv.tabs.find(_.selected).map(_.runId), Some("run-b"))
      case other => fail(s"expected Forest, got $other")

  test("logs and nodeCount are carried into the view"):
    val f = Forest(
      nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done), ForestNode("b", None, Nil, NodeStatus.Done)),
      logs = Vector("l1", "l2")
    )
    val fv = forestView(f)
    assertEquals(fv.logs, Vector("l1", "l2"))
    assertEquals(fv.nodeCount, 2)

  test("selecting a different run projects that run's forest"):
    val s = AppState(
      forests = Map(
        "r1" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done))),
        "r2" -> Forest(nodes = Vector(ForestNode("z", None, Nil, NodeStatus.Failed)))
      ),
      order = Vector("r1", "r2"),
      selected = Some("r2"),
      conn = ConnStatus.Open
    )
    WorkflowView.from(s) match
      case View.Forest(fv, _) => assertEquals(fv.sections.head.nodes.head.id, "z")
      case other              => fail(s"expected Forest, got $other")
