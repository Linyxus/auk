package auk.webui

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.L.svg as sv
import org.scalajs.dom

/** The Laminar binding for everything a loop puts on the page: the board's lineage,
  * and the generation window that opens when a circle on it is clicked.
  *
  * A sibling of [[WorkflowRender]] rather than part of it, and a guest inside its
  * chrome: the floating window, its scroll container, its fold store and its
  * transcript rows are all the workflow dashboard's, reused verbatim, so a
  * generation's worker reads exactly like a sub-agent because it is drawn by the
  * same code.
  *
  * Two things here are its own. The lineage is inline SVG, which is the only place
  * on the page that positions anything — [[LoopLineage]] does the arithmetic and
  * this turns numbers into `<circle>`s, so the layout is unit-tested and the drawing
  * is not. And the generation window's phase line doubles as its tab bar: the
  * Worker circle, the checker diamond and the Evaluator circle are three buttons
  * that choose the body below them, which is why a station is a `<button>` and not
  * an ornament.
  *
  * Like [[WorkflowRender]], every decision is a field on the view model, so this
  * needs no unit tests beyond the pure ones; it is verified visually via
  * `startTestWebUI`.
  */
object LoopRender:

  // -- the board ---------------------------------------------------------------

  /** The loop's header over its lineage. The header binds reactively and the lineage
    * is swapped only when its geometry actually changes, so a loop reporting a new
    * stage every few seconds does not restart the animations on a picture that has
    * not moved. */
  def board(b0: LoopBoard, sig: Signal[LoopBoard], actions: Actions): HtmlElement =
    div(cls := "loop-board",
      renderHead(sig),
      div(cls := "loop-lineage-band",
        div(cls := "label loop-lineage-label", "lineage"),
        // its own scroller, so a long lineage never carries the masthead off the
        // left edge with it
        div(cls := "loop-lineage",
          child <-- sig.map(_.lineage).distinct.map(renderLineage(_, actions)))
      )
    )

  /** The masthead: what the loop is for on the left, what it is judged by on the
    * right, and what it is doing along the top. */
  private def renderHead(sig: Signal[LoopBoard]): HtmlElement =
    div(cls := "loop-head",
      div(cls := "loop-head-line",
        child <-- sig.map(b => (b.dot, b.phase)).distinct.map((d, p) =>
          span(cls := s"pill loop-phase ${LoopDot.cssClass(d)}", p)),
        child <-- sig.map(_.activity).distinct.map(a =>
          if a.isEmpty then emptyNode else span(cls := "loop-activity", a))
      ),
      div(cls := "loop-masthead",
        div(cls := "loop-masthead-goal",
          div(cls := "loop-goal", child.text <-- sig.map(_.goal).distinct),
          child <-- sig.map(_.goalRest).distinct.map(r =>
            if r.isEmpty then emptyNode else div(cls := "loop-goal-rest", r))
        ),
        div(cls := "loop-masthead-def",
          child <-- sig.map(_.rubric).distinct.map(r =>
            if r.isEmpty then emptyNode
            else div(cls := "loop-def-field", div(cls := "label", "rubric"), div(cls := "loop-def-body", r))),
          div(cls := "loop-def-field",
            div(cls := "label", "budgets"),
            div(cls := "loop-def-body is-mono", child.text <-- sig.map(_.budgets).distinct))
        )
      )
    )

  // -- the lineage -------------------------------------------------------------

  private def renderLineage(l: Lineage, actions: Actions): SvgElement =
    sv.svg(
      sv.cls := s"loop-lineage-svg${if l.live then " is-live" else ""}",
      sv.width := l.width.toString,
      sv.height := l.height.toString,
      sv.viewBox := s"0 0 ${l.width} ${l.height}",
      sv.g(sv.cls := "loop-edges", l.edges.map(renderEdge)),
      l.endCap.map(renderCap).toList,
      sv.g(sv.cls := "loop-nodes", l.nodes.map(renderNode(_, actions)))
    )

  private def renderEdge(e: LineageEdge): SvgElement =
    val cls = s"loop-edge ${LineageEdgeKind.cssClass(e.kind)}${if e.dashed then " is-dashed" else ""}"
    e.kind match
      case LineageEdgeKind.Rise =>
        // Leave the parent along the line and arrive vertically into the stack: the
        // first control point extends the horizontal tangent, the second sits under
        // the target so the curve straightens as it lands.
        val dx = e.x2 - e.x1
        val dy = e.y1 - e.y2
        val d = s"M ${e.x1} ${e.y1} C ${e.x1 + dx * 0.55} ${e.y1}, ${e.x2} ${e.y1 - dy * 0.45}, ${e.x2} ${e.y2}"
        sv.path(sv.cls := cls, sv.d := d)
      case _ =>
        sv.line(sv.cls := cls, sv.x1 := e.x1.toString, sv.y1 := e.y1.toString, sv.x2 := e.x2.toString, sv.y2 := e.y2.toString)

  /** The terminal tick a loop that reached its goal earns: a short accent bar across
    * the end of the line, the way a well-set page marks the end of a piece. */
  private def renderCap(cap: (Double, Double)): SvgElement =
    val (x, y) = cap
    sv.line(sv.cls := "loop-cap", sv.x1 := x.toString, sv.y1 := (y - 8).toString, sv.x2 := x.toString, sv.y2 := (y + 8).toString)

  private def renderNode(n: LineageNode, actions: Actions): SvgElement =
    val selected = if n.selected then " is-selected" else ""
    sv.g(
      sv.cls := s"loop-node ${LineageKind.cssClass(n.kind)}$selected",
      n.gen match
        case Some(g) => onClick --> (_ => actions.selectGeneration(g))
        case None    => emptyMod
      ,
      // A generous invisible target, so a 6px abandoned circle is still a thing you
      // can click at the size a cursor actually lands.
      n.gen.map(_ =>
        sv.circle(sv.cls := "loop-node-hit", sv.cx := n.x.toString, sv.cy := n.y.toString, sv.r := "17")).toList,
      if n.kind == LineageKind.Running then
        sv.circle(sv.cls := "loop-node-halo", sv.cx := n.x.toString, sv.cy := n.y.toString, sv.r := n.r.toString)
      else emptyMod
      ,
      if n.selected then
        sv.circle(sv.cls := "loop-node-ring", sv.cx := n.x.toString, sv.cy := n.y.toString, sv.r := (n.r + 5).toString)
      else emptyMod
      ,
      sv.circle(sv.cls := "loop-node-dot", sv.cx := n.x.toString, sv.cy := n.y.toString, sv.r := n.r.toString),
      if n.metric.nonEmpty then
        sv.text(sv.cls := "loop-node-metric", sv.x := n.x.toString, sv.y := (n.y + LoopLineage.MetricDy).toString, n.metric)
      else emptyMod
      ,
      if n.label.nonEmpty then
        sv.text(sv.cls := "loop-node-label", sv.x := n.labelX.toString, sv.y := n.labelY.toString, n.label)
      else emptyMod
      ,
      sv.titleTag(n.title)
    )

  // -- the generation window ---------------------------------------------------

  def generation(
      g0: GenerationView,
      sig: Signal[GenerationView],
      folds: WorkflowRender.FoldStore,
      scrollEl: HtmlElement,
      atBottom: Var[Boolean],
      actions: Actions
  ): HtmlElement =
    div(cls := "doc loop-gen",
      renderPhaseLine(sig.map(_.phase).distinct, actions),
      renderAttempts(g0, sig, actions),
      g0.body match
        case GenBody.Overview(o0) =>
          renderOverview(g0, sig.map(_.body).map { case GenBody.Overview(o) => o; case _ => o0 }, folds, actions)
        case GenBody.Transcript(p0) =>
          renderTranscript(g0, p0, sig.map(_.body).map { case GenBody.Transcript(p) => p; case _ => p0 },
            folds, scrollEl, atBottom, actions)
    )

  /** Worker ●———◆———● Evaluator: the generation's three stations, which are also the
    * window's three tabs. The pulse walks it as the drive cycle moves, and each
    * station keeps what it made of the attempt once it is past — so a settled
    * generation's line still reads as the story of what happened to it. */
  private def renderPhaseLine(sig: Signal[PhaseLine], actions: Actions): HtmlElement =
    div(cls := "loop-phaseline",
      station(GenPane.Worker, "worker", "is-circle", sig.map(_.worker).distinct, actions),
      span(cls := "loop-rail"),
      station(GenPane.Overview, "check", "is-diamond", sig.map(_.checker).distinct, actions),
      span(cls := "loop-rail"),
      station(GenPane.Evaluator, "evaluator", "is-circle", sig.map(_.evaluator).distinct, actions)
    )

  private def station(pane: GenPane, label: String, shape: String, sig: Signal[PhaseStation], actions: Actions): HtmlElement =
    button(
      tpe := "button",
      cls := s"loop-station $shape",
      cls <-- sig.map(s => PhaseKind.cssClass(s.kind)),
      cls("is-selected") <-- sig.map(_.selected),
      onClick --> (_ => actions.selectPane(pane)),
      span(cls := "loop-station-mark"),
      span(cls := "loop-station-label", label)
    )

  /** The attempt pills, which scope everything below the phase line. */
  private def renderAttempts(g0: GenerationView, sig: Signal[GenerationView], actions: Actions): HtmlElement =
    div(cls := "loop-attempts",
      cls("is-hidden") <-- sig.map(_.attempts.isEmpty),
      span(cls := "label", "attempt"),
      children <-- sig.map(_.attempts).split(_.attempt): (n, _, pillSig) =>
        button(
          tpe := "button",
          cls := "loop-attempt",
          cls <-- pillSig.map(p => StatusKind.cssClass(p.kind)),
          cls("is-selected") <-- pillSig.map(_.selected),
          onClick --> (_ => actions.selectAttempt(n)),
          n.toString
        )
    )

  // -- the overview pane -------------------------------------------------------

  /** Bound to a signal rather than built from a value, because an overview keeps
    * arriving: the diff is fetched after the pane opens, and a generation still in
    * flight gains its checker's report and its verdict while it is on screen. */
  private def renderOverview(
      g: GenerationView,
      sig: Signal[GenOverview],
      folds: WorkflowRender.FoldStore,
      actions: Actions
  ): HtmlElement =
    val key = s"${g.loopId}::gen${g.gen}::a${g.attempt}"
    div(cls := "loop-pane loop-overview",
      child <-- sig.map(_.description).distinct.map: d =>
        if d.nonEmpty then div(cls := "loop-desc", d) else emptyNode
      ,
      // An attempt the driver is counting but the ledger has not written down yet:
      // saying so is the difference between "nothing came of this" and "this has not
      // happened", and it points at the pane where it IS happening.
      child <-- sig.map(_.submitted).distinct.map: submitted =>
        if submitted then emptyNode
        else
          div(cls := "loop-note is-waiting",
            span(cls := "sdot is-running"),
            span(s"Attempt ${g.attempt} has not been submitted yet — the worker is still on it."))
      ,
      child <-- sig.map(o => (o.commit, o.metrics)).distinct.map(renderFacts),
      child <-- sig.map(_.artifact).distinct.map: a =>
        if a.isEmpty then emptyNode
        else
          detailsTag(cls := "fold snippet loop-artifact",
            WorkflowRender.foldable(folds, s"$key::artifact", defaultOpen = false),
            WorkflowRender.foldSummary("Artifact", WorkflowView.preview(a)),
            pre(cls := "snippet-code", a))
      ,
      child <-- sig.map(_.verdict).distinct.map(_.map(renderVerdict).getOrElse(emptyNode)),
      child <-- sig.map(_.check).distinct.map(_.map(renderCheck).getOrElse(emptyNode)),
      child <-- sig.map(_.diff).distinct.map(renderDiff(g, _, folds, actions))
    )

  /** The generation's hard figures: what it committed and what it measured, in the
    * hairline-separated cells the top bar uses. The commit is set in mono — a serif
    * numeral is for a number, and a SHA is not one. */
  private def renderFacts(facts: (String, Vector[(String, String)])): Node =
    val (commit, metrics) = facts
    if commit.isEmpty && metrics.isEmpty then emptyNode
    else
      div(cls := "loop-facts",
        if commit.nonEmpty then fact("commit", commit, mono = true) else emptyNode,
        metrics.map((label, value) => fact(label, value, mono = false)))

  private def fact(label: String, value: String, mono: Boolean): HtmlElement =
    div(cls := s"loop-fact${if mono then " is-mono" else ""}",
      span(cls := "label", label),
      span(cls := "loop-fact-v", value))

  private def renderVerdict(v: VerdictNote): Node =
    div(cls := s"loop-panel loop-verdict ${if v.accepted then "is-ok" else "is-bad"}",
      div(cls := "loop-panel-head",
        span(cls := "label", "verdict"),
        span(cls := s"pill ${if v.accepted then "is-done" else "is-interrupted"}", if v.accepted then "accepted" else "declined"),
        if v.goalReached then span(cls := "pill is-done", "goal reached") else emptyNode
      ),
      if v.feedback.nonEmpty then div(cls := "loop-panel-body", v.feedback) else emptyNode
    )

  private def renderCheck(c: CheckReport): Node =
    div(cls := s"loop-panel loop-check ${if c.pass then "is-ok" else "is-bad"}",
      div(cls := "loop-panel-head",
        span(cls := "label", "checker"),
        span(cls := s"pill ${if c.pass then "is-done" else "is-failed"}", if c.pass then "passed" else "rejected")
      ),
      if c.reasons.isEmpty then emptyNode
      else ul(cls := "loop-reasons", c.reasons.map(r => li(r))),
      if c.metrics.isEmpty then emptyNode
      else div(cls := "loop-facts is-inline", c.metrics.map((label, value) => fact(label, value, mono = false)))
    )

  /** The attempt's patch. Fetched when this pane first mounts rather than pushed:
    * a diff is larger than everything else about a generation put together, and only
    * one reader ever wants it. */
  private def renderDiff(g: GenerationView, d: DiffPane, folds: WorkflowRender.FoldStore, actions: Actions): Node =
    if !d.available then emptyNode
    else
      detailsTag(
        cls := "fold snippet loop-diff",
        WorkflowRender.foldable(folds, s"${g.loopId}::gen${g.gen}::a${g.attempt}::diff", defaultOpen = true),
        onMountCallback(_ => if d.state.isEmpty then actions.needDiff(g.loopId, g.gen, g.attempt)),
        WorkflowRender.foldSummary("Diff", diffHint(d)),
        d.state match
          case Some(DiffState.Ready(text)) => pre(cls := "loop-diff-body", diffLines(text))
          case Some(DiffState.Failed(msg)) => div(cls := "snippet-detail", msg)
          case _                           => div(cls := "snippet-detail is-running", "fetching the patch…")
      )

  private def diffHint(d: DiffPane): String = d.state match
    case Some(DiffState.Ready(text)) =>
      val added = text.linesIterator.count(l => l.startsWith("+") && !l.startsWith("+++"))
      val removed = text.linesIterator.count(l => l.startsWith("-") && !l.startsWith("---"))
      s"+$added −$removed"
    case Some(DiffState.Failed(_)) => "unavailable"
    case _                         => "…"

  /** A unified diff, tinted the way a patch is read: additions and removals in the
    * accent and error hues at their quietest, hunk headers dim, everything else
    * plain. Deliberately not syntax-highlighted — two colours saying "more" and
    * "less" is the whole information a patch carries at a glance. */
  private def diffLines(text: String): Vector[HtmlElement] =
    text.linesIterator.toVector.map: line =>
      val kind =
        if line.startsWith("+++") || line.startsWith("---") || line.startsWith("diff ") || line.startsWith("index ")
        then "is-meta"
        else if line.startsWith("@@") then "is-hunk"
        else if line.startsWith("+") then "is-add"
        else if line.startsWith("-") then "is-del"
        else ""
      div(cls := s"loop-diff-line $kind", if line.isEmpty then " " else line)

  // -- the transcript panes ----------------------------------------------------

  /** A generation agent's transcript, rendered by exactly the rows a sub-agent's is.
    *
    * The live one arrives over the wire and types; a settled one is a file the host
    * still has, asked for the first time this pane is shown and rendered whole. The
    * reader is not told which — it is the same transcript either way. */
  private def renderTranscript(
      g: GenerationView,
      p0: TranscriptPane,
      paneSig: Signal[TranscriptPane],
      folds: WorkflowRender.FoldStore,
      scrollEl: HtmlElement,
      atBottom: Var[Boolean],
      actions: Actions
  ): HtmlElement =
    val streamingSig = paneSig.map(_.streaming).distinct
    val rowCountSig = paneSig.map(_.rows.size).distinct
    val follow: () => Unit = () =>
      if atBottom.now() then scrollEl.ref.scrollTop = scrollEl.ref.scrollHeight.toDouble
    var primed = false
    div(
      cls := "loop-pane loop-transcript",
      onMountCallback(_ => if p0.status == PaneStatus.Loading then actions.needTranscript(g.loopId, p0.label)),
      child <-- paneSig.map(_.status).distinct.map(statusNote),
      div(cls := "transcript",
        children <-- paneSig.map(_.rows).splitByIndex((idx, row0, rowSig) =>
          WorkflowRender.renderRow(idx, row0, rowSig, folds, s"${g.loopId}::${p0.label}",
            streamingSig, rowCountSig, follow, () => primed))
      ),
      child <-- paneSig.map(p => p.streaming && !WorkflowRender.lastRowIsText(p.rows)).distinct
        .map(s => if s then div(cls := "stream-caret") else emptyNode),
      // Follow the stream as rows grow, but only while parked at the bottom, and on
      // the next frame so layout is read after the DOM has been patched.
      paneSig --> { _ =>
        if atBottom.now() then
          dom.window.requestAnimationFrame((_: Double) => { scrollEl.ref.scrollTop = scrollEl.ref.scrollHeight.toDouble; () })
      },
      onMountCallback(_ => dom.window.requestAnimationFrame((_: Double) => { primed = true; () }))
    )

  private def statusNote(s: PaneStatus): Node = s match
    case PaneStatus.Loading      => div(cls := "loop-note is-waiting", span(cls := "sdot is-running"), span("Fetching the transcript…"))
    case PaneStatus.Missing(msg) => div(cls := "loop-note is-missing", msg)
    case PaneStatus.Live         => emptyNode
    case PaneStatus.Settled      => emptyNode
