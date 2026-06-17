package auk.webui

import auk.workflow.{Forest, ForestGroup, ForestNode, NodeStatus, Transcript, TranscriptItem}

class WorkflowViewSuite extends munit.FunSuite:

  private def runState(f: Forest, rid: String = "r"): AppState =
    AppState(forests = Map(rid -> f), order = Vector(rid), selectedRun = Some(rid), conn = ConnStatus.Open)

  private def sidebarOf(f: Forest): SidebarView = WorkflowView.from(runState(f)).sidebar

  private def agentOf(s: AppState): AgentView =
    WorkflowView.from(s).main match
      case MainView.Agent(a) => a
      case other             => fail(s"expected an Agent main panel, got $other")

  private def selectedAgent(tr: Transcript, node: ForestNode = ForestNode("a", None, Nil, NodeStatus.Running)): AgentView =
    val f = Forest(nodes = Vector(node))
    agentOf(runState(f).copy(focus = Focus.Node(node.id), transcripts = Map("r" -> Map(node.id -> tr))))

  private def firstTool(a: AgentView): TranscriptRow.Tool =
    a.rows.collectFirst { case t: TranscriptRow.Tool => t }.getOrElse(fail("no Tool row"))

  // -- empty / status mapping --------------------------------------------------

  test("with no selected run the main panel is Waiting and the sidebar is empty"):
    val v = WorkflowView.from(AppState(conn = ConnStatus.Connecting))
    assertEquals(v.main, MainView.Waiting)
    assertEquals(v.sidebar.nodeCount, 0)
    assertEquals(v.conn, ConnStatus.Connecting)

  test("each StatusKind maps to its glyph"):
    assertEquals(StatusKind.glyph(StatusKind.Pending), "○")
    assertEquals(StatusKind.glyph(StatusKind.Queued), "◔")
    assertEquals(StatusKind.glyph(StatusKind.Running), "●")
    assertEquals(StatusKind.glyph(StatusKind.Done), "●")
    assertEquals(StatusKind.glyph(StatusKind.Failed), "●")

  test("each StatusKind maps to its css class"):
    assertEquals(StatusKind.cssClass(StatusKind.Pending), "is-pending")
    assertEquals(StatusKind.cssClass(StatusKind.Queued), "is-queued")
    assertEquals(StatusKind.cssClass(StatusKind.Running), "is-running")
    assertEquals(StatusKind.cssClass(StatusKind.Done), "is-done")
    assertEquals(StatusKind.cssClass(StatusKind.Failed), "is-failed")

  // -- sidebar tree ------------------------------------------------------------

  test("a sidebar node row reflects status, glyph, compact tokens, tool"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running, 1L, 1500L, Some("eval_scala"), None)))
    val row = sidebarOf(f).sections.head.nodes.head
    assertEquals(row.statusKind, StatusKind.Running)
    assertEquals(row.glyph, "●")
    assertEquals(row.tokensText, "1.5k")
    assertEquals(row.toolText, "eval_scala")

  test("zero output tokens and no tool render as empty text fields"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Pending, 0L, 0L, None, None)))
    val row = sidebarOf(f).sections.head.nodes.head
    assertEquals(row.tokensText, "")
    assertEquals(row.toolText, "")

  test("the selected node row is flagged selected; others are not"):
    val f = Forest(nodes = Vector(
      ForestNode("a", None, Nil, NodeStatus.Done), ForestNode("b", None, Nil, NodeStatus.Done)))
    val s = AppState(forests = Map("r" -> f), order = Vector("r"), selectedRun = Some("r"), focus = Focus.Node("b"), conn = ConnStatus.Open)
    val rows = WorkflowView.from(s).sidebar.sections.head.nodes
    assertEquals(rows.find(_.id == "b").map(_.selected), Some(true))
    assertEquals(rows.find(_.id == "a").map(_.selected), Some(false))

  test("nodes are grouped into sections in declared-group order"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", ""), ForestGroup("g2", "two", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done), ForestNode("b", Some("g2"), Nil, NodeStatus.Done))
    )
    assertEquals(sidebarOf(f).sections.map(_.name), Vector(Some("one"), Some("two")))

  test("the ungrouped section comes last and only when ungrouped nodes exist"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done), ForestNode("u", None, Nil, NodeStatus.Done))
    )
    assertEquals(sidebarOf(f).sections.map(_.id), Vector(Some("g1"), None))

  test("a declared group with no nodes produces no section"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", ""), ForestGroup("g2", "two", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done))
    )
    assertEquals(sidebarOf(f).sections.map(_.name), Vector(Some("one")))

  test("logs and nodeCount are carried into the sidebar"):
    val f = Forest(
      nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done), ForestNode("b", None, Nil, NodeStatus.Done)),
      logs = Vector("l1", "l2")
    )
    val sb = sidebarOf(f)
    assertEquals(sb.logs, Vector("l1", "l2"))
    assertEquals(sb.nodeCount, 2)

  // -- run switcher ------------------------------------------------------------

  test("runs list every run with 8-char labels and mark the selected one"):
    val s = AppState(
      forests = Map("run-aaaaaaaa" -> Forest.empty, "run-b" -> Forest.empty),
      order = Vector("run-aaaaaaaa", "run-b"), selectedRun = Some("run-b"), conn = ConnStatus.Open
    )
    val runs = WorkflowView.from(s).runs
    assertEquals(runs.map(_.runId), Vector("run-aaaaaaaa", "run-b"))
    assertEquals(runs.map(_.label), Vector("run-aaaa", "run-b"))
    assertEquals(runs.find(_.selected).map(_.runId), Some("run-b"))

  // -- main panel: transcript --------------------------------------------------

  test("a run with no selected node shows the Unselected main panel"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    assertEquals(WorkflowView.from(runState(f)).main, MainView.Unselected)

  test("selecting a node projects its header: id, status, header tokens, tool, prompt, summary"):
    val f = Forest(nodes = Vector(
      ForestNode("a", None, Nil, NodeStatus.Running, 10L, 2000L, Some("grep"), Some("looks good"), Some("do the task"))))
    val s = runState(f).copy(focus = Focus.Node("a"))
    WorkflowView.from(s).main match
      case MainView.Agent(a) =>
        assertEquals(a.id, "a")
        assertEquals(a.statusKind, StatusKind.Running)
        assertEquals(a.tokensText, "2.0k tokens")
        assertEquals(a.toolText, "grep")
        assertEquals(a.prompt, Some("do the task"))
        assertEquals(a.summary, Some("looks good"))
        assertEquals(a.streaming, true)
      case other => fail(s"expected Agent, got $other")

  test("a finished node is not streaming"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    val s = runState(f).copy(focus = Focus.Node("a"))
    WorkflowView.from(s).main match
      case MainView.Agent(a) => assertEquals(a.streaming, false)
      case other             => fail(s"expected Agent, got $other")

  // -- workflow code tab -------------------------------------------------------

  test("no code tab when the run has no code; a code tab appears when it does"):
    assertEquals(sidebarOf(Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))).codeTab, None)
    val withCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("wf.start(...)"))
    assertEquals(sidebarOf(withCode).codeTab, Some(CodeTab(selected = false)))

  test("focusing the code marks the code tab selected and shows highlighted Scala"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("val x = 1"))
    val s = runState(f).copy(focus = Focus.Code)
    val v = WorkflowView.from(s)
    assertEquals(v.sidebar.codeTab, Some(CodeTab(selected = true)))
    v.main match
      case MainView.Code(tokens) =>
        assertEquals(tokens.map(_.text).mkString, "val x = 1")
        assert(tokens.exists(t => t.kind == HlKind.Keyword && t.text == "val"))
      case other => fail(s"expected Code, got $other")

  test("focusing code on a run without code falls back to Unselected"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    assertEquals(WorkflowView.from(runState(f).copy(focus = Focus.Code)).main, MainView.Unselected)

  test("transcript items project to prose, thought, and tool rows in order"):
    val a = selectedAgent(Transcript(Vector(
      TranscriptItem.Said("hi"),
      TranscriptItem.Thought("hmm"),
      TranscriptItem.ToolCall("c1", "grep", "pat", Some("3 hits"), false)
    )))
    assertEquals(a.rows(0), TranscriptRow.Prose("hi"))
    // The thought is not the last row, so it is done (folded).
    assertEquals(a.rows(1), TranscriptRow.Thought("hmm", done = true))
    a.rows(2) match
      case TranscriptRow.Tool(id, name, input, output, isError) =>
        assertEquals(id, "c1")
        assertEquals(name, "grep")
        assertEquals(input.map(_.text).mkString, "pat")
        assertEquals(output, Some("3 hits"))
        assertEquals(isError, false)
      case other => fail(s"expected a Tool row, got $other")

  test("the last thought is active (open) while streaming, and folds once done"):
    val tr = Transcript(Vector(TranscriptItem.Said("hi"), TranscriptItem.Thought("reasoning")))
    // While the agent is Running and the thought is its last row, it stays open.
    val running = selectedAgent(tr, ForestNode("a", None, Nil, NodeStatus.Running))
    assertEquals(running.rows.last, TranscriptRow.Thought("reasoning", done = false))
    // Once the agent is Done, the same thought folds.
    val done = selectedAgent(tr, ForestNode("a", None, Nil, NodeStatus.Done))
    assertEquals(done.rows.last, TranscriptRow.Thought("reasoning", done = true))

  test("a tool call with no output yet projects to a Tool row with output None"):
    val a = selectedAgent(Transcript(Vector(TranscriptItem.ToolCall("c1", "eval_scala", """{"code":"1"}""", None, false))))
    assertEquals(firstTool(a).output, None)

  test("a non-eval_scala tool input is a single plain token shown verbatim"):
    val a = selectedAgent(Transcript(Vector(TranscriptItem.ToolCall("c1", "grep", """{"q":"x"}""", Some("ok"), false))))
    assertEquals(firstTool(a).input, Vector(HlToken(HlKind.Plain, """{"q":"x"}""")))

  test("an eval_scala tool input extracts the code field and highlights it as Scala"):
    val a = selectedAgent(Transcript(Vector(TranscriptItem.ToolCall("c1", "eval_scala", """{"code": "val x = 1"}""", None, false))))
    val tool = firstTool(a)
    assertEquals(tool.input.map(_.text).mkString, "val x = 1")
    assert(tool.input.exists(t => t.kind == HlKind.Keyword && t.text == "val"))

  test("eval_scala input with no code field falls back to the raw input verbatim"):
    val a = selectedAgent(Transcript(Vector(TranscriptItem.ToolCall("c1", "eval_scala", "not json at all", Some("x"), false))))
    assertEquals(firstTool(a).input.map(_.text).mkString, "not json at all")

  test("eval_scala extracts and highlights an escaped multi-line code field"):
    // value is {"code": "val x = 1\nval y = 2"} with \n a literal JSON escape
    val a = selectedAgent(Transcript(Vector(
      TranscriptItem.ToolCall("c1", "eval_scala", "{\"code\": \"val x = 1\\nval y = 2\"}", Some("ok"), false))))
    val tool = firstTool(a)
    assertEquals(tool.input.map(_.text).mkString, "val x = 1\nval y = 2")
    assert(tool.input.exists(t => t.kind == HlKind.Keyword && t.text == "val"))

  // -- fmtTokens ---------------------------------------------------------------

  test("fmtTokens matches the TUI byte-for-byte"):
    assertEquals(WorkflowView.fmtTokens(999L), "999")
    assertEquals(WorkflowView.fmtTokens(1000L), "1.0k")
    assertEquals(WorkflowView.fmtTokens(1234L), "1.2k")
    assertEquals(WorkflowView.fmtTokens(1250L), "1.3k")
    assertEquals(WorkflowView.fmtTokens(12345L), "12.3k")

  // -- run-scoped projection ---------------------------------------------------

  test("selecting a different run projects that run's tree"):
    val s = AppState(
      forests = Map(
        "r1" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done))),
        "r2" -> Forest(nodes = Vector(ForestNode("z", None, Nil, NodeStatus.Failed)))
      ),
      order = Vector("r1", "r2"), selectedRun = Some("r2"), conn = ConnStatus.Open
    )
    assertEquals(WorkflowView.from(s).sidebar.sections.head.nodes.head.id, "z")
