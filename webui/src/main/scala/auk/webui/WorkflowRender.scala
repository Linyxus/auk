package auk.webui

import scala.scalajs.js
import com.raquo.laminar.api.L.*
import org.scalajs.dom

import auk.workflow.TranscriptEvent

/** The thin Laminar binding: a pure `View -> HtmlElement` mapping. The page is a
  * top bar (brand, switcher, pause/resume, code button, connection badge) over a
  * board — group columns laid out left to right for a workflow, a lineage for a
  * loop — plus a floating window for the focused thing (a sub-agent's streaming
  * transcript, a generation, or the source code). The view is split into
  * independent signals (runs / board / panel), each `.distinct`, so a transcript
  * delta rebuilds only the window.
  *
  * The chrome is here and both boards' contents are elsewhere: [[LoopRender]] draws
  * everything a loop puts on the page, through the same window, the same fold store
  * and the same transcript rows a workflow uses — which is why several of this
  * object's helpers are `private[webui]` rather than private.
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
  private[webui] type FoldStore = scala.collection.mutable.Map[String, Boolean]

  def app(view: Signal[View], actions: Actions): HtmlElement =
    val folds: FoldStore = scala.collection.mutable.Map.empty
    val panelSig = view.map(_.panel).distinct
    val boardSig = view.map(_.board).distinct
    div(
      cls := "app",
      windowEvents(_.onKeyDown).filter(_.key == "Escape") --> (_ => actions.close()),
      renderTopbar(view, actions),
      // the board is the stable scroll container; only its child is swapped, so
      // scroll position survives streaming updates. The swap is keyed by WHAT is on
      // the board, not by its content, so a loop's lineage is not rebuilt (and its
      // animations restarted) every time the loop reports a new stage.
      div(cls := "board",
        child <-- boardSig.distinctBy(boardKey).map(b0 => div(cls := "board-host", boardContent(b0, boardSig, actions)))),
      renderWindow(panelSig, folds, actions)
    )

  private def boardKey(b: BoardView): String = b match
    case BoardView.Workflow(_) => "workflow"
    case BoardView.Loop(l)     => s"loop:${l.id}"

  private def boardContent(b0: BoardView, boardSig: Signal[BoardView], actions: Actions): Modifier[HtmlElement] =
    b0 match
      case BoardView.Loop(l0) =>
        LoopRender.board(l0, boardSig.map { case BoardView.Loop(l) => l; case _ => l0 }, actions)
      case BoardView.Workflow(c0) =>
        // The canvas is cheap and stateless to rebuild, and rebuilding it wholesale
        // is how it has always worked.
        child <-- boardSig.map { case BoardView.Workflow(c) => c; case _ => c0 }.distinct.map(renderBoard(_, actions))

  // -- top bar -------------------------------------------------------------------

  private def renderTopbar(view: Signal[View], actions: Actions): HtmlElement =
    val metricsSig = view.map(_.metrics).distinct
    headerTag(cls := "topbar",
      div(cls := "brand",
        span(cls := "brand-dot", cls <-- view.map(v => connClass(v.conn)).distinct),
        div(cls := "brand-ttl",
          span(cls := "brand-name", "Workflow Viewer"),
          span(cls := "brand-sub", child.text <-- view.map(brandSub).distinct)
        )
      ),
      // the switcher sits on the left, next to the title
      renderSwitcher(view, actions),
      // the selected thing's at-a-glance figures: serif numerals under mono
      // micro-labels, separated by hairlines (hidden until something is selected)
      div(cls := "metrics",
        cls("is-hidden") <-- metricsSig.map(_.isEmpty),
        children <-- metricsSig.map(keyedStrip(_)(_.label)).split(_._1)((_, _, cellSig) => metricCell(cellSig.map(_._2)))
      ),
      div(cls := "controls",
        renderRunControl(view.map(_.runControl).distinct),
        child <-- view.map(_.codeButton).distinct.map:
          case Some(cb) =>
            button(
              tpe := "button",
              cls := s"btn code-btn${if cb.selected then " is-active" else ""}",
              onClick --> (_ => actions.selectCode()),
              cb.label
            )
          case None => emptyNode
      )
    )

  /** Keys the cells of a numeric strip for a Laminar `split`. A label is not a usable
    * key on its own: the strips carry both built-in cells and cells named by a loop's
    * checker, so a checker metric called `tokens` (or `attempts`, or `generations`)
    * collides with a built-in one, and duplicate keys break the split at runtime.
    *
    * Position makes the key unique; the label rides along so that a cell whose
    * identity changes at a fixed position is rebuilt rather than left showing the
    * label it was created with — which is why this is not `splitByIndex`. Strip cells
    * never reorder, so keying by position costs nothing.
    */
  private[webui] def keyedStrip[A](cells: Vector[A])(label: A => String): Vector[(String, A)] =
    cells.zipWithIndex.map((c, i) => (s"$i|${label(c)}", c))

  /** One metrics cell: a mono micro-label over a serif numeral, with an optional dim
    * note beside it (the loop's "was 0.82"). */
  private def metricCell(cell: Signal[MetricCell]): HtmlElement =
    div(cls := "metric",
      cls("is-accent") <-- cell.map(_.accent),
      span(cls := "label", child.text <-- cell.map(_.label)),
      div(cls := "metric-line",
        span(cls := "metric-v", child.text <-- cell.map(_.value)),
        child <-- cell.map(_.note).distinct.map(n => if n.nonEmpty then span(cls := "metric-note", n) else emptyNode)
      ))

  /** The brand's second line: what the page holds — runs, loops, or both — and the
    * connection state. */
  private def brandSub(v: View): String =
    val connWord = v.conn match
      case ConnStatus.Connecting => "connecting"
      case ConnStatus.Open       => "live"
      case _                     => "offline"
    val parts = Vector(
      Option.when(v.runs.nonEmpty)(if v.runs.size == 1 then s"run ${v.runs.head.label}" else s"${v.runs.size} runs"),
      Option.when(v.loops.nonEmpty)(if v.loops.size == 1 then "1 loop" else s"${v.loops.size} loops")
    ).flatten
    if parts.isEmpty then s"workflows · $connWord" else s"${parts.mkString(" · ")} · $connWord"

  private def connClass(c: ConnStatus): String = c match
    case ConnStatus.Connecting => "is-connecting"
    case ConnStatus.Open       => "is-open"
    case ConnStatus.Closed     => "is-closed"
    case ConnStatus.Error(_)   => "is-error"

  /** The pause/resume control for the selected run, always visible while a run is
    * selected (the switcher hides at one run, but a single run still needs a
    * control). Shows Pause while the run is live, Resume while paused, and a settled
    * marker once it is done. A loop has none in v1, and the projection says so by
    * naming no run. */
  private def renderRunControl(selectedSig: Signal[Option[RunTab]]): HtmlElement =
    div(cls := "run-control",
      child <-- selectedSig.map:
        case Some(t) =>
          t.statusKind match
            case StatusKind.Paused =>
              button(tpe := "button", cls := "btn run-ctl is-resume",
                title := s"Resume ${t.runId}", onClick --> (_ => Control.send("resume", t.runId)),
                "Resume")
            case StatusKind.Done | StatusKind.Failed =>
              span(cls := "run-ctl is-settled", span(cls := s"sdot ${StatusKind.cssClass(t.statusKind)}"),
                if t.statusKind == StatusKind.Failed then "failed" else "done")
            case _ =>
              button(tpe := "button", cls := "btn run-ctl is-pause",
                title := s"Pause ${t.runId}", onClick --> (_ => Control.send("pause", t.runId)),
                "Pause")
        case None => emptyNode
    )

  /** The switcher: a compact dropdown, shown only when the page holds more than one
    * thing. The toggle carries the selected thing's status dot and label; the menu
    * lists the workflows and the loops under mono-caps section headings and
    * highlights the active one. A section with nothing in it shows neither heading
    * nor rows, so a project with only loops reads as a list rather than as an empty
    * category.
    *
    * Open state lives in a `Var` on this stable element, so a streaming status
    * update never tears the menu down. A transparent, viewport-filling backdrop
    * closes the menu on an outside click; selecting something (or the set collapsing
    * back to one) closes it too. */
  private def renderSwitcher(view: Signal[View], actions: Actions): HtmlElement =
    val open = Var(false)
    val runsSig = view.map(_.runs).distinct
    val loopsSig = view.map(_.loops).distinct
    val multiSig = view.map(v => v.runs.size + v.loops.size > 1).distinct
    val currentSig = view.map(currentEntry).distinct
    div(
      cls := "run-select",
      cls("is-multi") <-- multiSig,
      cls("is-open") <-- open.signal,
      // the set collapsing back to a single thing hides and closes the switcher
      multiSig --> { m => if !m then open.set(false) },
      button(
        tpe := "button",
        cls := "run-select-toggle",
        onClick --> (_ => open.update(!_)),
        child <-- currentSig.map:
          case Some((dotCls, label)) =>
            span(cls := "run-select-current", span(cls := s"sdot $dotCls"), span(cls := "run-select-label", label))
          case None => span(cls := "run-select-current", span(cls := "run-select-label", "—"))
        ,
        span(cls := "run-select-caret", "▾")
      ),
      div(cls := "run-select-backdrop", onClick --> (_ => open.set(false))),
      div(cls := "run-select-menu",
        switcherSection("workflows", runsSig.map(_.nonEmpty),
          children <-- runsSig.split(_.runId)((rid, _, tabSig) => renderRunItem(rid, tabSig, actions, open))),
        switcherSection("loops", loopsSig.map(_.nonEmpty),
          children <-- loopsSig.split(_.loopId)((id, _, tabSig) => renderLoopItem(id, tabSig, actions, open)))
      )
    )

  /** One labelled group of switcher rows, absent entirely when it holds nothing. */
  private def switcherSection(label: String, shown: Signal[Boolean], rows: Modifier[HtmlElement]): HtmlElement =
    div(cls := "run-select-group",
      cls("is-hidden") <-- shown.map(!_),
      div(cls := "run-select-section", span(cls := "label", label)),
      rows)

  /** What the toggle names: the selected loop, the selected run, or — before
    * anything is selected — the first run there is. */
  private def currentEntry(v: View): Option[(String, String)] =
    v.loops.find(_.selected).map(t => (LoopDot.cssClass(t.dot), t.label))
      .orElse(v.runs.find(_.selected).map(t => (StatusKind.cssClass(t.statusKind), t.label)))
      .orElse(v.runs.headOption.map(t => (StatusKind.cssClass(t.statusKind), t.label)))

  /** One menu row: a status dot, the run label, and a dim `settled/total` count,
    * highlighting the active run. Selecting it switches runs and closes the menu. */
  private def renderRunItem(runId: String, tabSig: Signal[RunTab], actions: Actions, open: Var[Boolean]): HtmlElement =
    button(
      tpe := "button",
      cls := "run-select-item",
      cls("is-selected") <-- tabSig.map(_.selected),
      onClick --> (_ => { actions.selectRun(runId); open.set(false) }),
      child <-- tabSig.map(t => span(cls := s"sdot ${StatusKind.cssClass(t.statusKind)}")),
      span(cls := "run-select-label", child.text <-- tabSig.map(_.label)),
      child <-- tabSig.map(t => if t.total > 0 then span(cls := "run-select-count", s"${t.settled}/${t.total}") else emptyNode)
    )

  /** The same row for a loop, counting accepted generations where a run counts
    * settled sub-agents. */
  private def renderLoopItem(loopId: String, tabSig: Signal[LoopTab], actions: Actions, open: Var[Boolean]): HtmlElement =
    button(
      tpe := "button",
      cls := "run-select-item",
      cls("is-selected") <-- tabSig.map(_.selected),
      onClick --> (_ => { actions.selectLoop(loopId); open.set(false) }),
      child <-- tabSig.map(t => span(cls := s"sdot ${LoopDot.cssClass(t.dot)}")),
      span(cls := "run-select-label", child.text <-- tabSig.map(_.label)),
      child <-- tabSig.map(t => if t.started > 0 then span(cls := "run-select-count", s"${t.accepted}/${t.started}") else emptyNode)
    )

  // -- board: group columns, agent cards stacked vertically -----------------------

  private def renderBoard(c: CanvasView, actions: Actions): HtmlElement =
    if c.cards.isEmpty then
      div(cls := "board-empty",
        span(cls := "sdot is-running"),
        span("Waiting for the workflow…"))
    else
      div(cls := "columns",
        c.cards.map(renderGroupColumn(_, actions.selectNode)),
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
        span(cls := s"sdot ${StatusKind.cssClass(a.statusKind)}"),
        span(cls := "agent-card-id", a.id),
        if a.tokensText.nonEmpty then span(cls := "agent-card-tokens", a.tokensText) else emptyNode
      ),
      if a.promptHint.nonEmpty then div(cls := "agent-card-hint", a.promptHint) else emptyNode,
      // the live action line: the tool being run, or the status word
      div(cls := "agent-card-action",
        if a.toolText.nonEmpty then a.toolText else StatusKind.name(a.statusKind))
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
  private def renderWindow(panel: Signal[PanelView], folds: FoldStore, actions: Actions): HtmlElement =
    val statusSig = panel.map(windowStatus).distinct
    val statsSig = panel.map(windowStats).distinct
    div(
      cls := "window",
      role := "dialog",
      cls("is-open") <-- panel.map(_ != PanelView.Closed).distinct,
      div(cls := "window-head",
        span(cls := "sdot window-dot",
          cls <-- statusSig.map(_.map((k, _) => StatusKind.cssClass(k)).getOrElse("is-none"))),
        div(cls := "window-ttl",
          span(cls := "window-title", child.text <-- panel.map(windowTitle).distinct),
          child <-- statusSig.map:
            case Some((k, word)) => span(cls := s"pill ${StatusKind.cssClass(k)}", word)
            case None            => emptyNode
        ),
        // the panel's own stat cells (an agent's tokens/steps/tool, a generation's
        // attempts and headline metric) — absent for the code panel
        div(cls := "window-stats",
          cls("is-hidden") <-- statsSig.map(_.isEmpty),
          children <-- statsSig.map(keyedStrip(_)(_._1)).split(_._1) { (_, initial, cellSig) =>
            div(cls := "wstat",
              span(cls := "label", initial._2._1),
              span(cls := "wstat-v", child.text <-- cellSig.map(_._2._2)))
          }
        ),
        button(tpe := "button", cls := "window-close", aria.label := "Close",
          onClick --> (_ => actions.close()), "✕")
      ),
      windowBody(panel, folds, actions)
    )

  private def windowTitle(pv: PanelView): String = pv match
    case PanelView.Agent(a)      => a.id
    case PanelView.Code(t, _)    => t
    case PanelView.Generation(g) => g.title
    case PanelView.Closed        => ""

  /** The head's dot and pill: the status kind that colours them, and the word the
    * pill reads — which for a generation is the ledger's own ("accepted",
    * "abandoned") rather than the palette's. */
  private def windowStatus(pv: PanelView): Option[(StatusKind, String)] = pv match
    case PanelView.Agent(a)      => Some((a.statusKind, StatusKind.name(a.statusKind)))
    case PanelView.Generation(g) => Some((g.stateKind, g.state))
    case _                       => None

  private def windowStats(pv: PanelView): Vector[(String, String)] = pv match
    case PanelView.Agent(a) =>
      Vector(
        "tokens" -> (if a.tokensText.isEmpty then "0" else a.tokensText.stripSuffix(" tokens")),
        "steps" -> a.rows.size.toString,
        "tool" -> (if a.toolText.isEmpty then "—" else a.toolText)
      )
    case PanelView.Generation(g) => g.stats
    case _                       => Vector.empty

  private def windowBody(panel: Signal[PanelView], folds: FoldStore, actions: Actions): HtmlElement =
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
        renderPanelFor(pv0, panel, folds, scroll, atBottom, actions).amend(
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

  /** What makes the window's subtree a different subtree. A generation's key carries
    * its pane and attempt as well as its identity: those two choose the whole body,
    * so switching either is a new panel rather than a patch of the old one — which
    * keeps the streaming path (the transcript rows) the only thing that patches. */
  private def keyOf(pv: PanelView): String = pv match
    case PanelView.Agent(a)      => s"node:${a.id}"
    case PanelView.Code(_, _)    => "code"
    case PanelView.Generation(g) => s"gen:${g.loopId}:${g.gen}:${paneKey(g.body)}:${g.attempt}"
    case PanelView.Closed        => "closed"

  private def paneKey(b: GenBody): String = b match
    case GenBody.Overview(_)   => "overview"
    case GenBody.Transcript(p) => p.label

  private[webui] def nearBottom(el: dom.Element): Boolean =
    el.scrollHeight - el.scrollTop - el.clientHeight <= 40.0

  private def renderPanelFor(
      pv0: PanelView,
      panel: Signal[PanelView],
      folds: FoldStore,
      scrollEl: HtmlElement,
      atBottom: Var[Boolean],
      actions: Actions
  ): HtmlElement =
    pv0 match
      case PanelView.Closed       => div(cls := "doc")
      case PanelView.Code(_, tokens) => renderCode(tokens)
      case PanelView.Agent(a0)    =>
        // Under this key, `panel` is always Agent(_) for the same node, so the
        // fallback is unreachable; it only satisfies totality.
        val agentSig = panel.map { case PanelView.Agent(a) => a; case _ => a0 }
        renderAgentReactive(a0, agentSig, folds, scrollEl, atBottom)
      case PanelView.Generation(g0) =>
        val genSig = panel.map { case PanelView.Generation(g) => g; case _ => g0 }
        LoopRender.generation(g0, genSig, folds, scrollEl, atBottom, actions)

  // -- foldable blocks ---------------------------------------------------------

  /** Make a `<details>` foldable with a stable, persisted open state, restored on
    * mount from `folds(key)` (or `defaultOpen` if untouched) and written back on
    * every user toggle. Our own programmatic `.open` set queues one `toggle` event;
    * we swallow exactly that one so it is not mistaken for a user action. */
  private[webui] def foldable(folds: FoldStore, key: String, defaultOpen: Boolean): Modifier[HtmlElement] =
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
  private[webui] def foldable(folds: FoldStore, key: String, defaultOpen: Signal[Boolean]): Modifier[HtmlElement] =
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
  private[webui] def foldSummary(title: String, hint: String, status: Option[(String, String)] = None): HtmlElement =
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

  private[webui] def renderRow(
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
      case r: TranscriptRow.Received =>
        // A message is atomic — it arrives whole and never streams — so it is
        // rendered once from the value the row was born with, like a tool card's
        // fixed fields. No typewriter: it is not this agent's own output.
        div(cls := "received",
          div(cls := "label", if r.from == TranscriptEvent.LeadSender then "Message" else s"Message from ${r.from}"),
          div(cls := "received-body", r.text))

  private def lastRowIsText(a: AgentView): Boolean = lastRowIsText(a.rows)

  private[webui] def lastRowIsText(rows: Vector[TranscriptRow]): Boolean = rows.lastOption match
    case Some(_: TranscriptRow.Prose)   => true
    case Some(_: TranscriptRow.Thought) => true
    case _                              => false

  private def renderToolRow(t0: TranscriptRow.Tool, rowSig: Signal[TranscriptRow], folds: FoldStore, agentId: String): HtmlElement =
    // callId / name / input are fixed once the call appears; only `output` (and
    // hence the state) changes — so the highlighted code is rendered once.
    val toolSig  = rowSig.map { case t: TranscriptRow.Tool => t; case _ => t0 }
    val stateSig = toolSig.map(t => toolState(t.output, t.isError)).distinct
    // `eval_scala` previews its highlighted code; any other tool previews the
    // projected argument digest, which is already one line and budgeted.
    val codeHint =
      if t0.name == "eval_scala" then WorkflowView.preview(t0.input.map(_.text).mkString) else t0.hint
    detailsTag(
      cls := "fold snippet",
      cls <-- stateSig.map(s => s"is-$s"),
      foldable(folds, s"$agentId::tool::${t0.callId}", defaultOpen = false),
      summaryTag(cls := "fold-summary",
        span(cls := "fold-title", child.text <-- stateSig.map(s => toolTitle(t0, s))),
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

  /** `eval_scala` keeps its verb-then-noun copy; every other tool shows the
    * display title the projection already computed. */
  private def toolTitle(t: TranscriptRow.Tool, state: String): String = t.name match
    case "eval_scala" => if state == "running" then "Executing code" else "Code"
    case _            => t.title

  // -- tokens ------------------------------------------------------------------

  private[webui] def renderToken(t: HlToken): HtmlElement =
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
