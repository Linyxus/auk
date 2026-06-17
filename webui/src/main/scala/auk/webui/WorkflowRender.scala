package auk.webui

import scala.scalajs.js
import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** The thin Laminar binding: a pure `View -> HtmlElement` mapping. The view is
  * split into independent signals (sidebar head / tree / main), each `.distinct`,
  * so a transcript delta rebuilds only the main panel. The main panel keeps a
  * single stable scroll container and swaps its content, so the scroll position
  * survives streaming updates: it follows the stream only when already at the
  * bottom, and jumps to the top when the selection changes.
  *
  * Foldable blocks (task, thinking, code, output) each carry a default open state,
  * but the agent view is rebuilt on every transcript delta, so a user's manual
  * fold/unfold would reset on the next update. To keep it stable, the open state is
  * persisted in a [[FoldStore]] keyed by a stable per-block id, outside the swapped
  * subtree; each block restores from it on (re)mount and writes back on toggle.
  *
  * Every other decision is a field on the [[View]], so this needs no unit tests
  * beyond the pure view-model tests; it is verified visually via `startTestWebUI`.
  */
object WorkflowRender:

  /** Per-page, mutable persistence of each foldable block's open state, keyed by a
    * stable id. Lives outside the rebuilt subtree so manual toggles survive the
    * per-delta rebuild of the agent view. */
  private type FoldStore = scala.collection.mutable.Map[String, Boolean]

  def app(view: Signal[View], onSelectRun: String => Unit, onSelectNode: String => Unit, onSelectCode: () => Unit): HtmlElement =
    val folds: FoldStore = scala.collection.mutable.Map.empty
    div(
      cls := "app",
      div(cls := "sidebar",
        child <-- view.map(v => (v.conn, v.runs)).distinct.map((c, r) => renderSideHead(c, r, onSelectRun)),
        child <-- view.map(_.sidebar).distinct.map(renderSidebar(_, onSelectNode, onSelectCode))
      ),
      mainTag(cls := "main", mainPanel(view.map(_.main).distinct, folds))
    )

  // -- main panel: one stable scroll container, content swapped on change -------

  private def mainPanel(main: Signal[MainView], folds: FoldStore): HtmlElement =
    // `atBottom` is reactive so the "go to latest" button hides itself once the
    // view is parked at the bottom and reappears when the reader scrolls up.
    val atBottom = Var(true)
    var lastKey = "" // the focused thing; a change means "scroll to top"
    val scroll = div(cls := "scroll")
    def jumpToLatest(): Unit =
      val el = scroll.ref
      el.scrollTop = el.scrollHeight.toDouble
      atBottom.set(true)
    scroll.amend(
      onScroll --> (_ => atBottom.set(nearBottom(scroll.ref))),
      child <-- main.map: mv =>
        renderMainInner(mv, folds).amend(onMountCallback: _ =>
          val el = scroll.ref
          val key = keyOf(mv)
          if key != lastKey then
            el.scrollTop = 0.0 // a new selection: start at the top
            lastKey = key
            atBottom.set(nearBottom(el))
          else if atBottom.now() then el.scrollTop = el.scrollHeight.toDouble // same selection, growing: follow
        )
    )
    div(cls := "main-body",
      button(
        cls := "go-latest",
        cls("is-hidden") <-- atBottom.signal.distinct,
        span(cls := "go-latest-arrow", "↓"),
        "latest",
        onClick --> (_ => jumpToLatest())
      ),
      scroll
    )

  private def keyOf(mv: MainView): String = mv match
    case MainView.Agent(a)     => s"node:${a.id}"
    case MainView.Code(_)      => "code"
    case MainView.Waiting      => "waiting"
    case MainView.Unselected   => "unselected"

  private def nearBottom(el: dom.Element): Boolean =
    el.scrollHeight - el.scrollTop - el.clientHeight <= 40.0

  /** Make a `<details>` element foldable with a stable, persisted open state.
    *
    * On (re)mount it restores `folds(key)` (or `defaultOpen` if the user hasn't
    * touched it) and writes back on every user toggle, so the choice survives the
    * per-delta rebuild. Setting `.open` programmatically queues at most one `toggle`
    * event; we swallow exactly that one (armed only when the set actually changes
    * the state, so it can't accidentally swallow the first real user toggle). */
  private def foldable(folds: FoldStore, key: String, defaultOpen: Boolean): Modifier[HtmlElement] =
    onMountCallback: ctx =>
      val el = ctx.thisNode.ref.asInstanceOf[js.Dynamic]
      val target = folds.getOrElse(key, defaultOpen)
      var swallow = el.open.asInstanceOf[Boolean] != target // our own set will fire one toggle
      el.open = target
      el.addEventListener("toggle", ((_: js.Any) =>
        if swallow then swallow = false
        else folds.update(key, el.open.asInstanceOf[Boolean])
      ): js.Function1[js.Any, Unit])

  /** One unified heading for every foldable block: a disclosure title, an optional
    * status chip, and a one-line content preview shown only while folded. */
  private def foldSummary(title: String, hint: String, status: Option[(String, String)] = None): HtmlElement =
    summaryTag(
      cls := "fold-summary",
      span(cls := "fold-title", title),
      status.map((t, c) => span(cls := s"fold-status $c", t)).getOrElse(emptyNode),
      if hint.nonEmpty then span(cls := "fold-hint", hint) else emptyNode
    )

  private def renderMainInner(mv: MainView, folds: FoldStore): HtmlElement = mv match
    case MainView.Waiting      => hint("Waiting for a workflow to start…")
    case MainView.Unselected   => hint("Select an agent or the workflow code on the left.")
    case MainView.Agent(a)     => renderAgent(a, folds)
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

  private def renderAgent(a: AgentView, folds: FoldStore): HtmlElement =
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
      // the task is foldable and folded by default; the heading hints its content
      a.prompt.map(p => detailsTag(cls := "fold task",
        foldable(folds, s"${a.id}::task", defaultOpen = false),
        foldSummary("task", WorkflowView.preview(p)),
        div(cls := "fold-body task-body", p))).getOrElse(emptyNode),
      div(cls := "transcript", a.rows.zipWithIndex.map((r, i) => renderRow(r, folds, a.id, i))),
      if a.streaming then div(cls := "stream-caret") else emptyNode,
      a.summary.filter(_ => !a.streaming).map(s =>
        div(cls := "result", div(cls := "label", "result"), div(cls := "result-body", s))).getOrElse(emptyNode)
    )

  private def renderRow(r: TranscriptRow, folds: FoldStore, agentId: String, idx: Int): HtmlElement = r match
    case TranscriptRow.Prose(text) => div(cls := "row-prose", text)
    case TranscriptRow.Thought(text, done) =>
      // Open while still being produced; folds once done (unless the user reopened it).
      detailsTag(cls := "fold row-thought",
        foldable(folds, s"$agentId::thought::$idx", defaultOpen = !done),
        foldSummary("thinking", WorkflowView.preview(text)),
        div(cls := "fold-body thought-body", text))
    case TranscriptRow.Tool(callId, name, input, output, isError) =>
      val state = output match
        case None               => "running"
        case Some(_) if isError => "error"
        case Some(_)            => "done"
      val title = name match
        case "eval_scala" => if state == "running" then "executing code" else "code executed"
        case other        => other
      val codeText = input.map(_.text).mkString
      detailsTag(
        cls := s"fold snippet is-$state",
        foldable(folds, s"$agentId::tool::$callId", defaultOpen = false),
        foldSummary(title, WorkflowView.preview(codeText), Some((state, s"is-$state"))),
        pre(cls := "snippet-code", code(input.map(renderToken))),
        // the output is foldable on its own, folded by default, even with the code open
        output match
          case Some(out) =>
            detailsTag(cls := s"fold snippet-output is-$state",
              foldable(folds, s"$agentId::tool::$callId::out", defaultOpen = false),
              foldSummary("output", WorkflowView.preview(out), Some((state, s"is-$state"))),
              div(cls := "snippet-detail", out))
          case None => div(cls := "snippet-detail is-running", "running…")
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
