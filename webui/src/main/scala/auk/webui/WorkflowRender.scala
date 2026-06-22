package auk.webui

import scala.scalajs.js
import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** The thin Laminar binding: a pure `View -> HtmlElement` mapping. The view is
  * split into independent signals (sidebar head / tree / main), each `.distinct`,
  * so a transcript delta rebuilds only the main panel.
  *
  * The main panel renders **incrementally**: the focused agent's subtree is built
  * once (when the selection changes) and then patched in place as the transcript
  * streams — header/result/task bind to signals, the row list is driven by
  * `splitByIndex` (so only NEW rows are inserted, never the whole list), and each
  * streamed prose/thinking run renders one node per chunk (so a delta only appends
  * a node). Nothing re-renders the rows that did not change, which is what keeps a
  * stream O(N) instead of O(N²) — the dominant cost in the performance audit. The
  * scroll container is stable, so position survives updates: it follows the stream
  * only when already at the bottom, and jumps to the top on a selection change.
  *
  * Foldable blocks (task, thinking, code, output) each carry a default open state.
  * Because elements now persist across deltas, a user's manual fold/unfold survives
  * naturally; the open state is still persisted in a [[FoldStore]] keyed by a stable
  * per-block id so it also survives a selection switch and back, and so a thinking
  * block can auto-fold once done without clobbering a manual choice.
  *
  * Every other decision is a field on the [[View]], so this needs no unit tests
  * beyond the pure view-model tests; it is verified visually via `startTestWebUI`.
  */
object WorkflowRender:

  /** Per-page, mutable persistence of each foldable block's open state, keyed by a
    * stable id. Lives outside the rebuilt subtree so manual toggles survive a
    * selection switch (and a thinking block's auto-fold-on-done). */
  private type FoldStore = scala.collection.mutable.Map[String, Boolean]

  def app(view: Signal[View], onSelectRun: String => Unit, onSelectNode: String => Unit, onSelectCode: () => Unit): HtmlElement =
    val folds: FoldStore = scala.collection.mutable.Map.empty
    div(
      cls := "app",
      div(cls := "sidebar",
        renderSideHead(view, onSelectRun),
        child <-- view.map(_.sidebar).distinct.map(renderSidebar(_, onSelectNode, onSelectCode))
      ),
      mainTag(cls := "main", mainPanel(view.map(_.main).distinct, folds))
    )

  // -- main panel: one stable scroll container, content patched in place ---------

  private def mainPanel(main: Signal[MainView], folds: FoldStore): HtmlElement =
    // `atBottom` is reactive so the "go to latest" button hides itself once the
    // view is parked at the bottom and reappears when the reader scrolls up.
    val atBottom = Var(true)
    val scroll = div(cls := "scroll")
    def jumpToLatest(): Unit =
      val el = scroll.ref
      el.scrollTop = el.scrollHeight.toDouble
      atBottom.set(true)
    scroll.amend(
      onScroll --> (_ => atBottom.set(nearBottom(scroll.ref))),
      // Switch the panel subtree only when the focused thing changes (waiting /
      // unselected / code / node:id) — NOT on every streaming delta. Within a
      // node's subtree, content is patched reactively (see renderAgentReactive).
      child <-- main.distinctBy(keyOf).map: mv0 =>
        renderMainFor(mv0, main, folds, scroll, atBottom).amend(
          onMountCallback: _ =>
            scroll.ref.scrollTop = 0.0 // a new selection starts at the top
            atBottom.set(nearBottom(scroll.ref))
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

  private def renderMainFor(mv0: MainView, main: Signal[MainView], folds: FoldStore, scrollEl: HtmlElement, atBottom: Var[Boolean]): HtmlElement =
    mv0 match
      case MainView.Waiting      => hint("Waiting for a workflow to start…")
      case MainView.Unselected   => hint("Select an agent or the workflow code on the left.")
      case MainView.Code(tokens) => renderCode(tokens)
      case MainView.Agent(a0)    =>
        // Under this key, `main` is always Agent(_) for the same node, so the
        // fallback is unreachable; it only satisfies totality.
        val agentSig = main.map { case MainView.Agent(a) => a; case _ => a0 }
        renderAgentReactive(a0, agentSig, folds, scrollEl, atBottom)

  // -- foldable blocks ---------------------------------------------------------

  /** Make a `<details>` foldable with a stable, persisted open state, restored on
    * mount from `folds(key)` (or `defaultOpen` if untouched) and written back on
    * every user toggle. Our own programmatic `.open` set queues one `toggle` event;
    * we swallow exactly that one so it is not mistaken for a user action. */
  private def foldable(folds: FoldStore, key: String, defaultOpen: Boolean): Modifier[HtmlElement] =
    onMountCallback: ctx =>
      val el = ctx.thisNode.ref.asInstanceOf[js.Dynamic]
      val target = folds.getOrElse(key, defaultOpen)
      var swallow = el.open.asInstanceOf[Boolean] != target
      el.open = target
      el.addEventListener("toggle", ((_: js.Any) =>
        if swallow then swallow = false
        else folds.update(key, el.open.asInstanceOf[Boolean])
      ): js.Function1[js.Any, Unit])

  /** Like [[foldable]] but with a reactive default (e.g. a thinking block whose
    * default flips to folded once it is done). The user's manual choice, once made,
    * is recorded in `folds` and always wins over the default thereafter. */
  private def foldable(folds: FoldStore, key: String, defaultOpen: Signal[Boolean]): Modifier[HtmlElement] =
    onMountBind: ctx =>
      val el = ctx.thisNode.ref.asInstanceOf[js.Dynamic]
      var swallow = false
      def setOpen(open: Boolean): Unit =
        if el.open.asInstanceOf[Boolean] != open then
          swallow = true
          el.open = open
      el.addEventListener("toggle", ((_: js.Any) =>
        if swallow then swallow = false
        else folds.update(key, el.open.asInstanceOf[Boolean])
      ): js.Function1[js.Any, Unit])
      // `defaultOpen` is distinct upstream, so this fires once on mount and again
      // only when the default actually flips; a recorded user choice overrides it.
      defaultOpen --> { (d: Boolean) => setOpen(folds.getOrElse(key, d)) }

  /** One unified heading for a foldable block: a disclosure title, an optional
    * status chip, and a one-line content preview shown only while folded. */
  private def foldSummary(title: String, hint: String, status: Option[(String, String)] = None): HtmlElement =
    summaryTag(
      cls := "fold-summary",
      span(cls := "fold-title", title),
      status.map((t, c) => span(cls := s"fold-status $c", t)).getOrElse(emptyNode),
      if hint.nonEmpty then span(cls := "fold-hint", hint) else emptyNode
    )

  /** A [[foldSummary]] whose preview hint updates as the block streams. */
  private def foldSummaryReactive(title: String, hint: Signal[String]): HtmlElement =
    summaryTag(
      cls := "fold-summary",
      span(cls := "fold-title", title),
      span(cls := "fold-hint", child.text <-- hint)
    )

  // -- main content ------------------------------------------------------------

  private def hint(text: String): HtmlElement =
    div(cls := "main-hint", div(cls := "main-hint-text", text))

  private def renderCode(tokens: Vector[HlToken]): HtmlElement =
    div(cls := "doc",
      div(cls := "code-head", "workflow code"),
      pre(cls := "code-block", code(tokens.map(renderToken)))
    )

  private def renderAgentReactive(a0: AgentView, agentSig: Signal[AgentView], folds: FoldStore, scrollEl: HtmlElement, atBottom: Var[Boolean]): HtmlElement =
    val streamingSig = agentSig.map(_.streaming).distinct
    val rowCountSig  = agentSig.map(_.rows.size).distinct
    // Keep the view pinned to the bottom as the typewriter grows height between
    // state updates — but only while the reader is parked there.
    val follow: () => Unit = () =>
      if atBottom.now() then scrollEl.ref.scrollTop = scrollEl.ref.scrollHeight.toDouble
    // `primed` is false during this panel's initial mount (so pre-existing rows
    // show their text at once) and flips true on the next frame (so a row born
    // from a later delta types from the start). See [[Typewriter.typed]].
    var primed = false
    div(
      cls := "doc agent",
      cls <-- agentSig.map(a => StatusKind.cssClass(a.statusKind)),
      div(cls := "agent-head",
        div(cls := "agent-id", a0.id),
        div(cls := "agent-meta",
          span(cls := "agent-status", child.text <-- agentSig.map(a => StatusKind.name(a.statusKind))),
          child <-- agentSig.map(_.toolText).distinct.map(t => if t.nonEmpty then span(cls := "agent-tool", t) else emptyNode),
          child <-- agentSig.map(_.tokensText).distinct.map(t => if t.nonEmpty then span(cls := "agent-tokens", t) else emptyNode)
        )
      ),
      // the task is foldable and folded by default; the heading hints its content.
      // The prompt is set once (on NodeStarted), so this builds the block at most once.
      child <-- agentSig.map(_.prompt).distinct.map:
        case Some(p) =>
          detailsTag(cls := "fold task",
            foldable(folds, s"${a0.id}::task", defaultOpen = false),
            foldSummary("task", WorkflowView.preview(p)),
            div(cls := "fold-body task-body", p))
        case None => emptyNode
      ,
      div(cls := "transcript",
        children <-- agentSig.map(_.rows).splitByIndex((idx, row0, rowSig) =>
          renderRow(idx, row0, rowSig, folds, a0.id, streamingSig, rowCountSig, follow, () => primed))
      ),
      // The inline typewriter caret owns the cursor on the last text row, so the
      // standalone caret only shows when the last row is not text (a tool card, or
      // nothing yet) — keeping exactly one caret on screen.
      child <-- agentSig.map(a => a.streaming && !lastRowIsText(a)).distinct.map(s => if s then div(cls := "stream-caret") else emptyNode),
      child <-- agentSig.map(a => a.summary.filter(_ => !a.streaming)).distinct.map:
        case Some(s) => div(cls := "result", div(cls := "label", "result"), div(cls := "result-body", s))
        case None    => emptyNode
      ,
      // Follow the stream as rows/chunks grow, but only when parked at the bottom.
      // Deferred to the next frame so we read layout after the DOM has been patched
      // (no synchronous forced reflow per delta).
      agentSig --> { _ =>
        if atBottom.now() then
          dom.window.requestAnimationFrame((_: Double) => { scrollEl.ref.scrollTop = scrollEl.ref.scrollHeight.toDouble; () })
      },
      // Rows present at the initial mount are pre-existing (show in full); flip
      // `primed` on the next frame so rows added later type from the start.
      onMountCallback(_ => dom.window.requestAnimationFrame((_: Double) => { primed = true; () }))
    )

  private def renderRow(
      idx: Int,
      row0: TranscriptRow,
      rowSig: Signal[TranscriptRow],
      folds: FoldStore,
      agentId: String,
      streamingSig: Signal[Boolean],
      rowCountSig: Signal[Int],
      follow: () => Unit,
      isPrimed: () => Boolean
  ): HtmlElement =
    // Animate this row only while the agent is streaming AND it is the live (last)
    // row; older rows and finished agents render in full at once.
    val animate = streamingSig.combineWith(rowCountSig.map(n => idx == n - 1)).map { case (s, l) => s && l }.distinct
    row0 match
      case _: TranscriptRow.Prose =>
        div(cls := "row-prose",
          Typewriter.typed(rowSig.map(r => proseChunks(r).mkString), animate, follow, caret = true, isPrimed = isPrimed))
      case _: TranscriptRow.Thought =>
        val chunksSig = rowSig.map(thoughtChunks)
        // Open while still being produced; folds once done (unless the user reopened it).
        val openSig = rowSig.map { case TranscriptRow.Thought(_, done) => !done; case _ => false }.distinct
        detailsTag(cls := "fold row-thought",
          foldable(folds, s"$agentId::thought::$idx", openSig),
          foldSummaryReactive("thinking", chunksSig.map(WorkflowView.previewChunks(_))),
          div(cls := "fold-body thought-body",
            Typewriter.typed(chunksSig.map(_.mkString), animate, follow, caret = true, isPrimed = isPrimed)))
      case t0: TranscriptRow.Tool =>
        renderToolRow(t0, rowSig, folds, agentId)

  private def lastRowIsText(a: AgentView): Boolean = a.rows.lastOption match
    case Some(_: TranscriptRow.Prose)   => true
    case Some(_: TranscriptRow.Thought) => true
    case _                              => false

  private def renderToolRow(t0: TranscriptRow.Tool, rowSig: Signal[TranscriptRow], folds: FoldStore, agentId: String): HtmlElement =
    // callId / name / input are fixed once the call appears; only `output` (and
    // hence the state) changes — so the highlighted code is rendered once.
    val toolSig  = rowSig.map { case t: TranscriptRow.Tool => t; case _ => t0 }
    val stateSig = toolSig.map(t => toolState(t.output, t.isError)).distinct
    val codeHint = WorkflowView.preview(t0.input.map(_.text).mkString)
    detailsTag(
      cls := "fold snippet",
      cls <-- stateSig.map(s => s"is-$s"),
      foldable(folds, s"$agentId::tool::${t0.callId}", defaultOpen = false),
      summaryTag(cls := "fold-summary",
        span(cls := "fold-title", child.text <-- stateSig.map(s => toolTitle(t0.name, s))),
        span(cls := "fold-status", cls <-- stateSig.map(s => s"is-$s"), child.text <-- stateSig),
        if codeHint.nonEmpty then span(cls := "fold-hint", codeHint) else emptyNode
      ),
      pre(cls := "snippet-code", code(t0.input.map(renderToken))),
      // the output is foldable on its own, folded by default, even with the code open
      child <-- toolSig.map(t => (t.output, toolState(t.output, t.isError))).distinct.map:
        case (Some(out), st) =>
          detailsTag(cls := s"fold snippet-output is-$st",
            foldable(folds, s"$agentId::tool::${t0.callId}::out", defaultOpen = false),
            foldSummary("output", WorkflowView.preview(out), Some((st, s"is-$st"))),
            div(cls := "snippet-detail", out))
        case (None, _) => div(cls := "snippet-detail is-running", "running…")
    )

  private def proseChunks(r: TranscriptRow): Vector[String] = r match
    case TranscriptRow.Prose(cs) => cs
    case _                       => Vector.empty

  private def thoughtChunks(r: TranscriptRow): Vector[String] = r match
    case TranscriptRow.Thought(cs, _) => cs
    case _                            => Vector.empty

  private def toolState(output: Option[String], isError: Boolean): String = output match
    case None               => "running"
    case Some(_) if isError => "error"
    case Some(_)            => "done"

  private def toolTitle(name: String, state: String): String = name match
    case "eval_scala" => if state == "running" then "executing code" else "code executed"
    case other        => other

  // -- sidebar head (connection + run switcher) --------------------------------

  /** Built once (not rebuilt per frame): the connection badge and the run
    * switcher each bind to their own `.distinct` slice of the view, so the
    * switcher's open state and the menu's element identities survive streaming
    * updates. */
  private def renderSideHead(view: Signal[View], onSelectRun: String => Unit): HtmlElement =
    val runsSig = view.map(_.runs).distinct
    val selectedSig = runsSig.map(rs => rs.find(_.selected).orElse(rs.headOption)).distinct
    div(cls := "side-head",
      child <-- view.map(_.conn).distinct.map(connBadge),
      div(cls := "side-head-right",
        renderRunControl(selectedSig),
        renderRunSwitcher(runsSig, onSelectRun)
      )
    )

  /** The pause/resume control for the selected run, always visible (the switcher
    * hides at one run, but a single run still needs a control). Shows Pause while
    * the run is live, Resume while paused, and nothing once it has settled. */
  private def renderRunControl(selectedSig: Signal[Option[RunTab]]): HtmlElement =
    div(cls := "run-control",
      child <-- selectedSig.map:
        case Some(t) =>
          t.statusKind match
            case StatusKind.Paused =>
              button(tpe := "button", cls := "run-ctl is-resume",
                title := s"Resume ${t.runId}", onClick --> (_ => Control.send("resume", t.runId)),
                span(cls := "run-ctl-glyph", "▶"), "Resume")
            case StatusKind.Done | StatusKind.Failed =>
              span(cls := "run-ctl is-done", "done")
            case _ =>
              button(tpe := "button", cls := "run-ctl is-pause",
                title := s"Pause ${t.runId}", onClick --> (_ => Control.send("pause", t.runId)),
                span(cls := "run-ctl-glyph", "❚❚"), "Pause")
        case None => emptyNode
    )

  /** The run switcher: a compact dropdown, shown only when more than one workflow
    * is live. The toggle carries the selected run's status dot and label; the menu
    * lists every run (status dot · label · settled/total) and highlights the
    * active one. Open state lives in a `Var` on this stable element, so a
    * streaming status update never tears the menu down. A transparent, viewport-
    * filling backdrop closes the menu on an outside click; selecting a run (or the
    * run set collapsing back to one) closes it too. */
  private def renderRunSwitcher(runsSig: Signal[Vector[RunTab]], onSelectRun: String => Unit): HtmlElement =
    val open = Var(false)
    val multiSig = runsSig.map(_.size > 1).distinct
    val selectedSig = runsSig.map(rs => rs.find(_.selected).orElse(rs.headOption)).distinct
    div(
      cls := "run-select",
      cls("is-multi") <-- multiSig,
      cls("is-open") <-- open.signal,
      // a run set collapsing back to a single run hides and closes the switcher
      multiSig --> { m => if !m then open.set(false) },
      button(
        tpe := "button",
        cls := "run-select-toggle",
        onClick --> (_ => open.update(!_)),
        child <-- selectedSig.map:
          case Some(t) => span(cls := "run-select-current", runDot(t), span(cls := "run-select-label", t.label))
          case None    => span(cls := "run-select-current", span(cls := "run-select-label", "—"))
        ,
        span(cls := "run-select-caret", "▾")
      ),
      div(cls := "run-select-backdrop", onClick --> (_ => open.set(false))),
      div(cls := "run-select-menu",
        children <-- runsSig.split(_.runId)((rid, _, tabSig) => renderRunItem(rid, tabSig, onSelectRun, open)))
    )

  /** One menu row: a status dot, the run label, and a dim `settled/total` count,
    * highlighting the active run. Selecting it switches runs and closes the menu. */
  private def renderRunItem(runId: String, tabSig: Signal[RunTab], onSelectRun: String => Unit, open: Var[Boolean]): HtmlElement =
    button(
      tpe := "button",
      cls := "run-select-item",
      cls("is-selected") <-- tabSig.map(_.selected),
      onClick --> (_ => { onSelectRun(runId); open.set(false) }),
      child <-- tabSig.map(runDot),
      span(cls := "run-select-label", child.text <-- tabSig.map(_.label)),
      child <-- tabSig.map(t => if t.total > 0 then span(cls := "run-select-count", s"${t.settled}/${t.total}") else emptyNode)
    )

  /** A small status dot coloured by the run's overall state (the node-status
    * palette + the running pulse, via the `is-*` class). */
  private def runDot(t: RunTab): HtmlElement =
    span(cls := s"run-dot ${StatusKind.cssClass(t.statusKind)}", StatusKind.glyph(t.statusKind))

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

  // -- tokens ------------------------------------------------------------------

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
