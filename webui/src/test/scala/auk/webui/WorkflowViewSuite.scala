package auk.webui

import auk.workflow.{Forest, ForestGroup, ForestNode, NodeStatus, Transcript, TranscriptEvent, TranscriptItem}

class WorkflowViewSuite extends munit.FunSuite:

  private def runState(f: Forest, rid: String = "r"): AppState =
    AppState(forests = Map(rid -> f), order = Vector(rid), selectedRun = Some(rid), conn = ConnStatus.Open)

  private def canvasOf(f: Forest): CanvasView = WorkflowView.from(runState(f)).canvas

  private def agentOf(s: AppState): AgentView =
    WorkflowView.from(s).panel match
      case PanelView.Agent(a) => a
      case other              => fail(s"expected an Agent panel, got $other")

  private def selectedAgent(tr: Transcript, node: ForestNode = ForestNode("a", None, Nil, NodeStatus.Running)): AgentView =
    val f = Forest(nodes = Vector(node))
    agentOf(runState(f).copy(focus = Focus.Node(node.id), transcripts = Map("r" -> Map(node.id -> tr))))

  private def firstTool(a: AgentView): TranscriptRow.Tool =
    a.rows.collectFirst { case t: TranscriptRow.Tool => t }.getOrElse(fail("no Tool row"))

  // -- empty / status mapping --------------------------------------------------

  test("with no selected run the panel is Closed and the canvas is empty"):
    val v = WorkflowView.from(AppState(conn = ConnStatus.Connecting))
    assertEquals(v.panel, PanelView.Closed)
    assertEquals(v.canvas.cards, Vector.empty)
    assertEquals(v.canvas.nodeCount, 0)
    assertEquals(v.conn, ConnStatus.Connecting)

  test("each StatusKind maps to its glyph"):
    assertEquals(StatusKind.glyph(StatusKind.Pending), "○")
    assertEquals(StatusKind.glyph(StatusKind.Queued), "◔")
    assertEquals(StatusKind.glyph(StatusKind.Running), "◐")
    assertEquals(StatusKind.glyph(StatusKind.Done), "✓")
    assertEquals(StatusKind.glyph(StatusKind.Failed), "✕")
    assertEquals(StatusKind.glyph(StatusKind.Interrupted), "❚")

  test("each StatusKind maps to its css class"):
    assertEquals(StatusKind.cssClass(StatusKind.Pending), "is-pending")
    assertEquals(StatusKind.cssClass(StatusKind.Queued), "is-queued")
    assertEquals(StatusKind.cssClass(StatusKind.Running), "is-running")
    assertEquals(StatusKind.cssClass(StatusKind.Done), "is-done")
    assertEquals(StatusKind.cssClass(StatusKind.Failed), "is-failed")
    assertEquals(StatusKind.cssClass(StatusKind.Interrupted), "is-interrupted")

  test("StatusKind.of maps an interrupted node to Interrupted"):
    assertEquals(StatusKind.of(NodeStatus.Interrupted), StatusKind.Interrupted)

  // -- canvas cards --------------------------------------------------------------

  test("an agent card reflects status, glyph, compact tokens, tool, prompt hint"):
    val f = Forest(nodes = Vector(
      ForestNode("a", None, Nil, NodeStatus.Running, 1L, 1500L, Some("eval_scala"), None, Some("Inspect  the\nthing"))))
    val card = canvasOf(f).cards.head.agents.head
    assertEquals(card.statusKind, StatusKind.Running)
    assertEquals(card.glyph, "◐")
    assertEquals(card.tokensText, "1.5k")
    assertEquals(card.toolText, "eval_scala")
    assertEquals(card.promptHint, "Inspect the thing")

  test("zero output tokens, no tool, and no prompt render as empty text fields"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Pending, 0L, 0L, None, None)))
    val card = canvasOf(f).cards.head.agents.head
    assertEquals(card.tokensText, "")
    assertEquals(card.toolText, "")
    assertEquals(card.promptHint, "")

  test("a long prompt is truncated to a one-line hint with an ellipsis"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running, prompt = Some("x" * 300))))
    val hint = canvasOf(f).cards.head.agents.head.promptHint
    assertEquals(hint, "x" * 110 + "…")

  test("the selected agent card is flagged selected; others are not"):
    val f = Forest(nodes = Vector(
      ForestNode("a", None, Nil, NodeStatus.Done), ForestNode("b", None, Nil, NodeStatus.Done)))
    val s = AppState(forests = Map("r" -> f), order = Vector("r"), selectedRun = Some("r"), focus = Focus.Node("b"), conn = ConnStatus.Open)
    val cards = WorkflowView.from(s).canvas.cards.head.agents
    assertEquals(cards.find(_.id == "b").map(_.selected), Some(true))
    assertEquals(cards.find(_.id == "a").map(_.selected), Some(false))

  test("groups become cards in declared-group order, carrying name and description"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", "first phase"), ForestGroup("g2", "two", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done), ForestNode("b", Some("g2"), Nil, NodeStatus.Done))
    )
    val cards = canvasOf(f).cards
    assertEquals(cards.map(_.name), Vector(Some("one"), Some("two")))
    assertEquals(cards.map(_.description), Vector("first phase", ""))
    assertEquals(cards.map(_.key), Vector("g:g1", "g:g2"))

  test("the ungrouped card comes last and only when ungrouped nodes exist"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done), ForestNode("u", None, Nil, NodeStatus.Done))
    )
    assertEquals(canvasOf(f).cards.map(_.key), Vector("g:g1", "~ungrouped"))
    assertEquals(canvasOf(f).cards.last.name, None)

  test("a declared group with no nodes produces no card"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", ""), ForestGroup("g2", "two", "")),
      nodes = Vector(ForestNode("a", Some("g1"), Nil, NodeStatus.Done))
    )
    assertEquals(canvasOf(f).cards.map(_.name), Vector(Some("one")))

  test("a group card counts settled (done or failed) agents out of its total"):
    val f = Forest(
      groups = Vector(ForestGroup("g1", "one", "")),
      nodes = Vector(
        ForestNode("a", Some("g1"), Nil, NodeStatus.Done),
        ForestNode("b", Some("g1"), Nil, NodeStatus.Failed),
        ForestNode("c", Some("g1"), Nil, NodeStatus.Running)
      )
    )
    val card = canvasOf(f).cards.head
    assertEquals(card.settled, 2)
    assertEquals(card.total, 3)

  test("logs and nodeCount are carried into the canvas"):
    val f = Forest(
      nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done), ForestNode("b", None, Nil, NodeStatus.Done)),
      logs = Vector("l1", "l2")
    )
    val c = canvasOf(f)
    assertEquals(c.logs, Vector("l1", "l2"))
    assertEquals(c.nodeCount, 2)

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

  // -- drawer panel: transcript --------------------------------------------------

  test("a run with nothing focused keeps the drawer Closed"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Running)))
    assertEquals(WorkflowView.from(runState(f)).panel, PanelView.Closed)

  test("selecting a node projects its header: id, status, header tokens, tool, prompt, summary"):
    val f = Forest(nodes = Vector(
      ForestNode("a", None, Nil, NodeStatus.Running, 10L, 2000L, Some("grep"), Some("looks good"), Some("do the task"))))
    val s = runState(f).copy(focus = Focus.Node("a"))
    WorkflowView.from(s).panel match
      case PanelView.Agent(a) =>
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
    WorkflowView.from(s).panel match
      case PanelView.Agent(a) => assertEquals(a.streaming, false)
      case other              => fail(s"expected Agent, got $other")

  // -- workflow code button ------------------------------------------------------

  test("no code button when the run has no code; a code button appears when it does"):
    assertEquals(WorkflowView.from(runState(Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done))))).codeButton, None)
    val withCode = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("wf.start(...)"))
    assertEquals(WorkflowView.from(runState(withCode)).codeButton, Some(CodeButton(selected = false)))

  test("focusing the code marks the button selected and fills the drawer with highlighted Scala"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("val x = 1"))
    val s = runState(f).copy(focus = Focus.Code)
    val v = WorkflowView.from(s)
    assertEquals(v.codeButton, Some(CodeButton(selected = true)))
    v.panel match
      case PanelView.Code(tokens) =>
        assertEquals(tokens.map(_.text).mkString, "val x = 1")
        assert(tokens.exists(t => t.kind == HlKind.Keyword && t.text == "val"))
      case other => fail(s"expected Code, got $other")

  test("focusing code on a run without code falls back to Closed"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)))
    assertEquals(WorkflowView.from(runState(f).copy(focus = Focus.Code)).panel, PanelView.Closed)

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

  // -- chunked transcript rows (incremental-rendering contract) -----------------

  test("a streamed prose row carries each delta as a chunk"):
    val tr = Transcript.empty.update(TranscriptEvent.Said("r", "a", "Hel")).update(TranscriptEvent.Said("r", "a", "lo"))
    selectedAgent(tr).rows.head match
      case TranscriptRow.Prose(cs) => assertEquals(cs, Vector("Hel", "lo"))
      case other                   => fail(s"expected a Prose row, got $other")

  test("a streamed thinking row carries each delta as a chunk"):
    val tr = Transcript.empty.update(TranscriptEvent.Thought("r", "a", "x")).update(TranscriptEvent.Thought("r", "a", "y"))
    selectedAgent(tr, ForestNode("a", None, Nil, NodeStatus.Done)).rows.head match
      case TranscriptRow.Thought(cs, done) =>
        assertEquals(cs, Vector("x", "y"))
        assertEquals(done, true)
      case other => fail(s"expected a Thought row, got $other")

  test("appending a delta leaves earlier rows identical (stable for incremental render)"):
    val base = Transcript.empty
      .update(TranscriptEvent.Said("r", "a", "prose "))
      .update(TranscriptEvent.ToolCalled("r", "a", "c1", "grep", "p"))
      .update(TranscriptEvent.ToolReturned("r", "a", "c1", "ok", false))
      .update(TranscriptEvent.Thought("r", "a", "think "))
    val grown = base.update(TranscriptEvent.Thought("r", "a", "more"))
    val rb = selectedAgent(base).rows
    val rg = selectedAgent(grown).rows
    assertEquals(rb.size, rg.size)                 // the open thinking run grew — no new row
    assertEquals(rb.dropRight(1), rg.dropRight(1)) // every earlier row is == (untouched)
    assert(rb.last != rg.last, "the open thinking row changed")

  test("a new run appends exactly one row and keeps prior rows identical"):
    val base = Transcript.empty.update(TranscriptEvent.Said("r", "a", "hi"))
    val next = base.update(TranscriptEvent.ToolCalled("r", "a", "c1", "grep", "p"))
    val rb = selectedAgent(base).rows
    val rn = selectedAgent(next).rows
    assertEquals(rn.size, rb.size + 1)
    assertEquals(rn.head, rb.head)                 // the prose row is unchanged

  test("a tool row's output filling leaves the input identical and only flips the output"):
    val base = Transcript.empty.update(TranscriptEvent.ToolCalled("r", "a", "c1", "grep", "p"))
    val done = base.update(TranscriptEvent.ToolReturned("r", "a", "c1", "3 hits", false))
    val tb = firstTool(selectedAgent(base))
    val td = firstTool(selectedAgent(done))
    assertEquals(tb.input, td.input)               // highlighted code unchanged
    assertEquals(tb.output, None)
    assertEquals(td.output, Some("3 hits"))

  // -- previewChunks (bounded folded-block hint) -------------------------------

  test("previewChunks matches preview over the joined text for short content"):
    assertEquals(WorkflowView.previewChunks(Vector("hello ", "world")), "hello world")
    assertEquals(WorkflowView.previewChunks(Vector.empty), "")
    assertEquals(WorkflowView.previewChunks(Vector("  a\tb  ")), "a b")

  test("previewChunks truncates with an ellipsis after scanning only enough chunks"):
    val chunks = Vector.fill(1000)("x") // 1000 one-char chunks; only ~max need scanning
    assertEquals(WorkflowView.previewChunks(chunks, max = 10), "x" * 10 + "…")

  // -- syntax-highlight memo (#3) ----------------------------------------------

  test("highlightScala memoizes: a repeated source yields the very same token vector"):
    val a = WorkflowView.highlightScala("val memo1 = 1")
    val b = WorkflowView.highlightScala("val memo1 = 1")
    assert(a eq b, "expected the cached token vector to be reused by reference")
    assert(!(WorkflowView.highlightScala("val memo2 = 2") eq a), "a different source must not alias")

  test("highlightScala is value-correct (memo does not change the tokenization)"):
    assertEquals(WorkflowView.highlightScala("def memoF = 1"), Highlight.scala("def memoF = 1"))
    val toks = WorkflowView.highlightScala("val memo3 = 1")
    assertEquals(toks.map(_.text).mkString, "val memo3 = 1")
    assert(toks.exists(t => t.kind == HlKind.Keyword && t.text == "val"))

  test("re-projecting an eval_scala tool reuses the cached highlight (makes the render gate O(1))"):
    val tr = Transcript(Vector(TranscriptItem.ToolCall("c1", "eval_scala", """{"code": "val memo4 = 1"}""", None, false)))
    val first  = firstTool(selectedAgent(tr)).input
    val second = firstTool(selectedAgent(tr)).input
    assert(first eq second, "two projections of the same eval_scala input should share the token vector")
    // and the shared instance makes the whole Tool row compare equal by reference fast-path
    assertEquals(firstTool(selectedAgent(tr)), firstTool(selectedAgent(tr)))

  test("the workflow-code panel reuses the cached highlight across projections"):
    val f = Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done)), code = Some("object MemoW"))
    val s = runState(f).copy(focus = Focus.Code)
    def codeTokens(): Vector[HlToken] = WorkflowView.from(s).panel match
      case PanelView.Code(t) => t
      case other             => fail(s"expected Code, got $other")
    assert(codeTokens() eq codeTokens(), "the code panel should reuse the cached tokens")

  // -- preview (folded-block hint) ---------------------------------------------

  test("preview collapses whitespace, trims, and truncates with an ellipsis"):
    assertEquals(WorkflowView.preview("  hello   world  "), "hello world")
    assertEquals(WorkflowView.preview("line one\nline two\tend"), "line one line two end")
    assertEquals(WorkflowView.preview(""), "")
    // short text passes through unchanged (no ellipsis)
    assertEquals(WorkflowView.preview("short", max = 80), "short")
    // long text is cut to `max` chars plus an ellipsis
    val p = WorkflowView.preview("x" * 100, max = 10)
    assertEquals(p, "x" * 10 + "…")

  // -- fmtTokens ---------------------------------------------------------------

  test("fmtTokens matches the TUI byte-for-byte"):
    assertEquals(WorkflowView.fmtTokens(999L), "999")
    assertEquals(WorkflowView.fmtTokens(1000L), "1.0k")
    assertEquals(WorkflowView.fmtTokens(1234L), "1.2k")
    assertEquals(WorkflowView.fmtTokens(1250L), "1.3k")
    assertEquals(WorkflowView.fmtTokens(12345L), "12.3k")

  // -- run-scoped projection ---------------------------------------------------

  test("selecting a different run projects that run's canvas"):
    val s = AppState(
      forests = Map(
        "r1" -> Forest(nodes = Vector(ForestNode("a", None, Nil, NodeStatus.Done))),
        "r2" -> Forest(nodes = Vector(ForestNode("z", None, Nil, NodeStatus.Failed)))
      ),
      order = Vector("r1", "r2"), selectedRun = Some("r2"), conn = ConnStatus.Open
    )
    assertEquals(WorkflowView.from(s).canvas.cards.head.agents.head.id, "z")
