package auk.webui

import scala.scalajs.js
import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** The thin Laminar binding: a pure `View -> HtmlElement` mapping. The page is a
  * top bar (brand, run switcher, pause/resume, workflow-code button, connection
  * badge) over a board of group columns laid out left to right — one column per
  * group, agent cards stacked vertically inside — plus a floating window for the
  * focused thing (a sub-agent's streaming transcript, or the workflow code) over
  * a dimmed scrim. The view is split into independent signals (runs / canvas /
  * panel), each `.distinct`, so a transcript delta rebuilds only the window.
  *
  * The window renders **incrementally**: the focused agent's subtree is built
  * once (when the selection changes) and then patched in place as the transcript
  * streams — head/result/task bind to signals, the row list is driven by
  * `splitByIndex` (so only NEW rows are inserted, never the whole list), and each
  * streamed prose/thinking run renders one node per chunk (so a delta only appends
  * a node). Nothing re-renders the rows that did not change, which is what keeps a
  * stream O(N) instead of O(N²) — the dominant cost in the performance audit. The
  * scroll container is stable, so position survives updates: it follows the stream
  * only when already at the bottom, and jumps to the top on a selection change.
  *
  * Foldable blocks (task, thinking, code, output) each carry a default open state.
  * Because elements persist across deltas, a user's manual fold/unfold survives
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

  def app(
      view: Signal[View],
      onSelectRun: String => Unit,
      onSelectNode: String => Unit,
      onSelectCode: () => Unit,
      onClose: () => Unit
  ): HtmlElement =
    val folds: FoldStore = scala.collection.mutable.Map.empty
    val panelSig = view.map(_.panel).distinct
    div(
      cls := "app",
      windowEvents(_.onKeyDown).filter(_.key == "Escape") --> (_ => onClose()),
      renderTopbar(view, onSelectRun, onSelectCode),
      // the board is the stable scroll container; only its child is swapped, so
      // scroll position survives streaming updates
      div(cls := "board", child <-- view.map(_.canvas).distinct.map(renderBoard(_, onSelectNode))),
      renderWindow(panelSig, folds, onClose)
    )

  // -- top bar -------------------------------------------------------------------

  private def renderTopbar(view: Signal[View], onSelectRun: String => Unit, onSelectCode: () => Unit): HtmlElement =
    val runsSig = view.map(_.runs).distinct
    val selectedSig = runsSig.map(rs => rs.find(_.selected).orElse(rs.headOption)).distinct
    headerTag(cls := "topbar",
      div(cls := "brand", span(cls := "brand-mark"), span(cls := "brand-name", "Auk"), span(cls := "brand-sub", "Workflows")),
      div(cls := "topbar-runs",
        renderRunSwitcher(runsSig, onSelectRun),
        renderRunControl(selectedSig)
      ),
      div(cls := "topbar-right",
        child <-- view.map(_.codeButton).distinct.map:
          case Some(cb) =>
            button(
              tpe := "button",
              cls := s"code-btn${if cb.selected then " is-active" else ""}",
              onClick --> (_ => onSelectCode()),
              span(cls := "code-btn-glyph", "{ }"),
              "Workflow code"
            )
          case None => emptyNode
        ,
        child <-- view.map(_.conn).distinct.map(connBadge)
      )
    )

  /** The pause/resume control for the selected run, always visible (the switcher
    * hides at one run, but a single run still needs a control). Shows Pause while
    * the run is live, Resume while paused, and a settled marker once it is done. */
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
              span(cls := "run-ctl is-settled", runDot(t), if t.statusKind == StatusKind.Failed then "failed" else "done")
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
    * palette + the running spin, via the `is-*` class). */
  private def runDot(t: RunTab): HtmlElement =
    span(cls := s"run-dot ${StatusKind.cssClass(t.statusKind)}", StatusKind.glyph(t.statusKind))

  private def connBadge(c: ConnStatus): HtmlElement =
    val (label, klass) = c match
      case ConnStatus.Connecting => ("connecting", "is-connecting")
      case ConnStatus.Open       => ("connected", "is-open")
      case ConnStatus.Closed     => ("disconnected", "is-closed")
      case ConnStatus.Error(_)   => ("disconnected", "is-error")
    div(cls := s"conn $klass", span(cls := "conn-dot"), span(label))

  // -- board: group columns, agent cards stacked vertically -----------------------

  private def renderBoard(c: CanvasView, onSelectNode: String => Unit): HtmlElement =
    if c.cards.isEmpty then
      div(cls := "board-empty",
        span(cls := "board-empty-mark", "◐"),
        span("Waiting for the workflow…"))
    else
      div(cls := "columns",
        c.cards.map(renderGroupColumn(_, onSelectNode)),
        renderLogs(c.logs)
      )

  private def renderGroupColumn(g: GroupCard, onSelectNode: String => Unit): HtmlElement =
    sectionTag(
      cls := "group",
      div(cls := "group-head",
        span(cls := "group-name", g.name.getOrElse("agents")),
        span(cls := "group-count", s"${g.settled}/${g.total}")
      ),
      if g.description.nonEmpty then div(cls := "group-desc", g.description) else emptyNode,
      div(cls := "stack", g.agents.map(renderAgentCard(_, onSelectNode)))
    )

  private def renderAgentCard(a: AgentCard, onSelectNode: String => Unit): HtmlElement =
    button(
      tpe := "button",
      cls := s"agent-card ${StatusKind.cssClass(a.statusKind)}${if a.selected then " is-selected" else ""}",
      dataAttr("node-id") := a.id,
      dataAttr("status") := StatusKind.name(a.statusKind),
      onClick --> (_ => onSelectNode(a.id)),
      div(cls := "agent-card-head",
        span(cls := "agent-card-glyph", a.glyph),
        span(cls := "agent-card-id", a.id)
      ),
      if a.promptHint.nonEmpty then div(cls := "agent-card-hint", a.promptHint) else emptyNode,
      div(cls := "agent-card-foot",
        if a.toolText.nonEmpty then span(cls := "chip is-tool", a.toolText)
        else span(cls := "agent-card-state", StatusKind.name(a.statusKind)),
        if a.tokensText.nonEmpty then span(cls := "agent-card-tokens", a.tokensText) else emptyNode
      )
    )

  private def renderLogs(logs: Vector[String]): Modifier[HtmlElement] =
    if logs.isEmpty then emptyNode
    else sectionTag(cls := "group group-logs",
      div(cls := "group-head", span(cls := "group-name", "Log")),
      div(cls := "log-lines", logs.map(l => div(cls := "log-line", l))))

  // -- floating window: transcript / workflow code --------------------------------

  /** A non-modal floating panel pinned to the right edge of the page, at the same
    * level as the board: no scrim, so the board stays interactive and clicking
    * another agent card simply switches the panel's content in place. */
  private def renderWindow(panel: Signal[PanelView], folds: FoldStore, onClose: () => Unit): HtmlElement =
    val statusSig = panel.map(windowStatus).distinct
    div(
      cls := "window",
      role := "dialog",
      cls("is-open") <-- panel.map(_ != PanelView.Closed).distinct,
      div(cls := "window-head",
        span(cls := "window-title", child.text <-- panel.map(windowTitle).distinct),
        child <-- statusSig.map:
          case Some(k) => span(cls := s"pill ${StatusKind.cssClass(k)}", StatusKind.name(k))
          case None    => emptyNode
        ,
        span(cls := "window-meta", child.text <-- panel.map(windowMeta).distinct),
        button(tpe := "button", cls := "window-close", aria.label := "Close",
          onClick --> (_ => onClose()), "✕")
      ),
      windowBody(panel, folds)
    )

  private def windowTitle(pv: PanelView): String = pv match
    case PanelView.Agent(a) => a.id
    case PanelView.Code(_)  => "Workflow code"
    case PanelView.Closed   => ""

  private def windowStatus(pv: PanelView): Option[StatusKind] = pv match
    case PanelView.Agent(a) => Some(a.statusKind)
    case _                  => None

  private def windowMeta(pv: PanelView): String = pv match
    case PanelView.Agent(a) => Vector(a.toolText, a.tokensText).filter(_.nonEmpty).mkString(" · ")
    case _                  => ""

  private def windowBody(panel: Signal[PanelView], folds: FoldStore): HtmlElement =
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
      // Switch the window subtree only when the focused thing changes (closed /
      // code / node:id) — NOT on every streaming delta. Within a node's subtree,
      // content is patched reactively (see renderAgentReactive).
      child <-- panel.distinctBy(keyOf).map: pv0 =>
        renderPanelFor(pv0, panel, folds, scroll, atBottom).amend(
          onMountCallback: _ =>
            scroll.ref.scrollTop = 0.0 // a new selection starts at the top
            atBottom.set(nearBottom(scroll.ref))
        )
    )
    div(cls := "window-body",
      scroll,
      button(
        cls := "go-latest",
        cls("is-hidden") <-- atBottom.signal.distinct,
        aria.label := "Jump to latest",
        onClick --> (_ => jumpToLatest()),
        "↓"
      )
    )

  private def keyOf(pv: PanelView): String = pv match
    case PanelView.Agent(a) => s"node:${a.id}"
    case PanelView.Code(_)  => "code"
    case PanelView.Closed   => "closed"

  private def nearBottom(el: dom.Element): Boolean =
    el.scrollHeight - el.scrollTop - el.clientHeight <= 40.0

  private def renderPanelFor(pv0: PanelView, panel: Signal[PanelView], folds: FoldStore, scrollEl: HtmlElement, atBottom: Var[Boolean]): HtmlElement =
    pv0 match
      case PanelView.Closed       => div(cls := "doc")
      case PanelView.Code(tokens) => renderCode(tokens)
      case PanelView.Agent(a0)    =>
        // Under this key, `panel` is always Agent(_) for the same node, so the
        // fallback is unreachable; it only satisfies totality.
        val agentSig = panel.map { case PanelView.Agent(a) => a; case _ => a0 }
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

  // -- window content ------------------------------------------------------------

  private def renderCode(tokens: Vector[HlToken]): HtmlElement =
    div(cls := "doc",
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
      // the task is foldable and folded by default; the heading hints its content.
      // The prompt is set once (on NodeStarted), so this builds the block at most once.
      child <-- agentSig.map(_.prompt).distinct.map:
        case Some(p) =>
          detailsTag(cls := "fold task",
            foldable(folds, s"${a0.id}::task", defaultOpen = false),
            foldSummary("Task", WorkflowView.preview(p)),
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
        case Some(s) => div(cls := "result", div(cls := "label", "Result"), div(cls := "result-body", s))
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
          foldSummaryReactive("Thinking", chunksSig.map(WorkflowView.previewChunks(_))),
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
            foldSummary("Output", WorkflowView.preview(out), Some((st, s"is-$st"))),
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
    case "eval_scala" => if state == "running" then "Executing code" else "Code"
    case other        => other

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
