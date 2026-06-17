package auk.webui

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** The thin Laminar binding: a pure `View -> HtmlElement` mapping. The view is
  * split into independent signals (sidebar head / tree / main), each `.distinct`,
  * so a transcript delta rebuilds only the main panel. The main panel keeps a
  * single stable scroll container and swaps its content, so the scroll position
  * survives streaming updates: it follows the stream only when already at the
  * bottom, and jumps to the top when the selection changes.
  *
  * Every decision is already a field on the [[View]], so this needs no unit tests
  * beyond the pure view-model tests; it is verified visually via `startTestWebUI`.
  */
object WorkflowRender:

  def app(view: Signal[View], onSelectRun: String => Unit, onSelectNode: String => Unit, onSelectCode: () => Unit): HtmlElement =
    div(
      cls := "app",
      div(cls := "sidebar",
        child <-- view.map(v => (v.conn, v.runs)).distinct.map((c, r) => renderSideHead(c, r, onSelectRun)),
        child <-- view.map(_.sidebar).distinct.map(renderSidebar(_, onSelectNode, onSelectCode))
      ),
      mainTag(cls := "main", mainPanel(view.map(_.main).distinct))
    )

  // -- main panel: one stable scroll container, content swapped on change -------

  private def mainPanel(main: Signal[MainView]): HtmlElement =
    var atBottom = true // whether the user is parked at the bottom (so we follow)
    var lastKey = ""    // the focused thing; a change means "scroll to top"
    val scroll = div(cls := "scroll")
    scroll.amend(
      onScroll --> (_ => atBottom = nearBottom(scroll.ref)),
      child <-- main.map: mv =>
        renderMainInner(mv).amend(onMountCallback: _ =>
          val el = scroll.ref
          val key = keyOf(mv)
          if key != lastKey then
            el.scrollTop = 0.0 // a new selection: start at the top
            lastKey = key
            atBottom = nearBottom(el)
          else if atBottom then el.scrollTop = el.scrollHeight.toDouble // same selection, growing: follow
        )
    )
    scroll

  private def keyOf(mv: MainView): String = mv match
    case MainView.Agent(a)     => s"node:${a.id}"
    case MainView.Code(_)      => "code"
    case MainView.Waiting      => "waiting"
    case MainView.Unselected   => "unselected"

  private def nearBottom(el: dom.Element): Boolean =
    el.scrollHeight - el.scrollTop - el.clientHeight <= 40.0

  private def renderMainInner(mv: MainView): HtmlElement = mv match
    case MainView.Waiting      => hint("Waiting for a workflow to start…")
    case MainView.Unselected   => hint("Select an agent or the workflow code on the left.")
    case MainView.Agent(a)     => renderAgent(a)
    case MainView.Code(tokens) => renderCode(tokens)

  // -- sidebar head (connection + run switcher) --------------------------------

  private def renderSideHead(conn: ConnStatus, runs: Vector[RunTab], onSelectRun: String => Unit): HtmlElement =
    div(cls := "side-head", connBadge(conn), renderRunSwitcher(runs, onSelectRun))

  private def renderRunSwitcher(runs: Vector[RunTab], onSelectRun: String => Unit): Modifier[HtmlElement] =
    if runs.size <= 1 then emptyNode
    else div(cls := "runs", runs.map(t =>
      button(cls := (if t.selected then "run is-active" else "run"), t.label, onClick --> (_ => onSelectRun(t.runId)))
    ))

  private def connBadge(c: ConnStatus): HtmlElement =
    val (label, klass) = c match
      case ConnStatus.Connecting => ("connecting", "is-connecting")
      case ConnStatus.Open       => ("connected", "is-open")
      case ConnStatus.Closed     => ("disconnected", "is-closed")
      case ConnStatus.Error(_)   => ("disconnected", "is-error")
    div(cls := s"conn $klass", span(cls := "conn-dot"), span(label))

  // -- sidebar tree ------------------------------------------------------------

  private def renderSidebar(s: SidebarView, onSelectNode: String => Unit, onSelectCode: () => Unit): HtmlElement =
    if s.nodeCount == 0 && s.codeTab.isEmpty then div(cls := "sidebar-empty", "No agents yet.")
    else
      div(cls := "tree",
        s.codeTab.map(renderCodeTab(_, onSelectCode)).getOrElse(emptyNode),
        s.sections.map(renderSection(_, onSelectNode)),
        renderLogs(s.logs)
      )

  private def renderCodeTab(ct: CodeTab, onSelectCode: () => Unit): HtmlElement =
    div(
      cls := s"tree-code${if ct.selected then " is-selected" else ""}",
      onClick --> (_ => onSelectCode()),
      span(cls := "code-glyph", "◇"),
      span(cls := "node-name", "workflow code")
    )

  private def renderSection(sec: GroupSection, onSelectNode: String => Unit): HtmlElement =
    div(
      cls := "tree-group",
      sec.name.map(n => div(cls := "label", n)).getOrElse(emptyNode),
      div(cls := "tree-nodes", sec.nodes.map(renderNodeRow(_, onSelectNode)))
    )

  private def renderNodeRow(r: NodeRow, onSelectNode: String => Unit): HtmlElement =
    div(
      cls := s"tree-node ${StatusKind.cssClass(r.statusKind)}${if r.selected then " is-selected" else ""}",
      dataAttr("node-id") := r.id,
      dataAttr("status") := StatusKind.name(r.statusKind),
      onClick --> (_ => onSelectNode(r.id)),
      span(cls := "node-glyph", r.glyph),
      span(cls := "node-name", r.id),
      if r.toolText.nonEmpty then span(cls := "node-tool", r.toolText) else emptyNode,
      if r.tokensText.nonEmpty then span(cls := "node-tokens", r.tokensText) else emptyNode
    )

  private def renderLogs(logs: Vector[String]): Modifier[HtmlElement] =
    if logs.isEmpty then emptyNode
    else div(cls := "tree-logs", div(cls := "label", "log"), logs.map(l => div(cls := "tree-log", l)))

  // -- main content ------------------------------------------------------------

  private def hint(text: String): HtmlElement =
    div(cls := "main-hint", div(cls := "main-hint-text", text))

  private def renderCode(tokens: Vector[HlToken]): HtmlElement =
    div(cls := "doc",
      div(cls := "code-head", "workflow code"),
      pre(cls := "code-block", code(tokens.map(renderToken)))
    )

  private def renderAgent(a: AgentView): HtmlElement =
    div(
      cls := s"doc agent ${StatusKind.cssClass(a.statusKind)}",
      div(cls := "agent-head",
        div(cls := "agent-id", a.id),
        div(cls := "agent-meta",
          span(cls := "agent-status", StatusKind.name(a.statusKind)),
          if a.toolText.nonEmpty then span(cls := "agent-tool", a.toolText) else emptyNode,
          if a.tokensText.nonEmpty then span(cls := "agent-tokens", a.tokensText) else emptyNode
        )
      ),
      a.prompt.map(p => div(cls := "task", div(cls := "label", "task"), div(cls := "task-body", p))).getOrElse(emptyNode),
      div(cls := "transcript", a.rows.map(renderRow)),
      if a.streaming then div(cls := "stream-caret") else emptyNode,
      a.summary.filter(_ => !a.streaming).map(s =>
        div(cls := "result", div(cls := "label", "result"), div(cls := "result-body", s))).getOrElse(emptyNode)
    )

  private def renderRow(r: TranscriptRow): HtmlElement = r match
    case TranscriptRow.Prose(text)   => div(cls := "row-prose", text)
    case TranscriptRow.Thought(text) => div(cls := "row-thought", text)
    case TranscriptRow.Tool(_, name, input, output, isError) =>
      val state = output match
        case None               => "running"
        case Some(_) if isError => "error"
        case Some(_)            => "done"
      div(
        cls := s"snippet is-$state",
        pre(cls := "snippet-code", code(input.map(renderToken))),
        div(cls := "snippet-strip",
          span(cls := "snippet-status", name),
          span(cls := "snippet-state", state),
          output match
            case Some(out) => div(cls := "snippet-detail", out)
            case None      => div(cls := "snippet-detail is-running", "running…")
        )
      )

  private def renderToken(t: HlToken): HtmlElement =
    if t.kind == HlKind.Plain then span(t.text) else span(cls := hlClass(t.kind), t.text)

  private def hlClass(k: HlKind): String = k match
    case HlKind.Keyword => "hl-kw"
    case HlKind.Soft    => "hl-soft"
    case HlKind.Type    => "hl-type"
    case HlKind.Str     => "hl-str"
    case HlKind.Num     => "hl-num"
    case HlKind.Def     => "hl-def"
    case HlKind.Comment => "hl-comment"
    case HlKind.Op      => "hl-op"
    case HlKind.Punct   => "hl-punct"
    case HlKind.Plain   => ""
