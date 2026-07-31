package auk.webui

import auk.workflow.{LoopAttemptWire, LoopGenerationWire, LoopStageWire, LoopWire, Transcript}

/** Projection of the loop half of the page: the switcher's loop rows, the board's
  * lineage, the top bar's figures, and the generation window.
  *
  * The same division of labour [[WorkflowView]] keeps: every decision here, so the
  * Laminar binding is branch-free.
  *
  * A loop's wire says what it is doing right now twice over — as the host's own
  * sentence (`gen 3, attempt 2 — checking`) and as a [[LoopStageWire]]. The sentence
  * is printed as it stands and never read; everything that moves is driven by the
  * structure, so a rewording upstream changes a line of copy and nothing else. A
  * `step` this page does not know is simply not one of the three stations, which
  * costs the line its pulse rather than the window its existence.
  */
object LoopView:

  /** Whether a loop is doing anything — the switch on every animation the board and
    * the window draw. A parked or orphaned loop shows the same picture, still. */
  def isLive(l: LoopWire): Boolean = l.parked.isEmpty && !l.orphaned

  /** The reason string a loop that finished carries. Its own state rather than just
    * another park, because it is the one ending that is a success. */
  private val GoalReached = "goal reached"

  def dotOf(l: LoopWire): LoopDot =
    if l.orphaned then LoopDot.Orphaned
    else if l.parked.contains(GoalReached) then LoopDot.Reached
    else if l.parked.isDefined then LoopDot.Parked
    else LoopDot.Live

  // -- switcher ----------------------------------------------------------------

  def tabsOf(state: AppState): Vector[LoopTab] =
    state.loopOrder.flatMap(state.loops.get).map: l =>
      LoopTab(
        loopId = l.id,
        label = l.id,
        selected = state.selectedLoop.contains(l.id),
        dot = dotOf(l),
        accepted = l.generations.count(_.state == "accepted"),
        started = l.generations.size
      )

  // -- board -------------------------------------------------------------------

  def boardOf(l: LoopWire, focused: Option[Int]): LoopBoard =
    val b = l.budgets
    val (headline, rest) = splitGoal(l.goal)
    LoopBoard(
      id = l.id,
      goal = headline,
      goalRest = rest,
      rubric = flow(l.rubric),
      phase = l.phase,
      dot = dotOf(l),
      activity = l.activity.getOrElse(""),
      budgets = s"${b.maxGenerations} generations · patience ${b.patience} · ${b.maxAttemptsPerGeneration} attempts",
      lineage = LoopLineage.layout(
        l.generations,
        selected = focused,
        live = isLive(l),
        goalReached = l.parked.contains(GoalReached)
      )
    )

  /** A goal's first paragraph and the rest of it.
    *
    * What somebody typed into a goal is prose, hard-wrapped wherever their editor's
    * window ended — which is nothing to do with how wide this page is. So each
    * paragraph is un-wrapped and the first is kept apart: a headline, and the
    * standfirst under it. A single-paragraph goal leaves the second half empty and
    * the board simply does not draw it. */
  private[webui] def splitGoal(goal: String): (String, String) =
    val paragraphs = goal.trim.split("\n\\s*\n").iterator.map(flow).filter(_.nonEmpty).toVector
    (paragraphs.headOption.getOrElse(""), paragraphs.drop(1).mkString("\n\n"))

  /** One paragraph with its hard wrapping undone. */
  private[webui] def flow(text: String): String = text.trim.replaceAll("\\s*\n\\s*", " ")

  // -- top bar -----------------------------------------------------------------

  /** A loop's at-a-glance figures: how far the lineage has got, which attempt is in
    * hand, and the metric the whole thing is being read by — with where it stood
    * before, because a refinement loop is a story about a number moving. */
  def metricsOf(l: LoopWire): Vector[MetricCell] =
    val accepted = l.generations.count(_.state == "accepted")
    val values = LoopLineage.headlineValues(l.generations)
    val headline = LoopLineage.headlineMetric(l.generations)
    Vector(
      MetricCell("generations", s"$accepted/${l.generations.size}"),
      MetricCell("attempt", attemptNow(l).map(_.toString).getOrElse("—")),
      MetricCell(
        label = headline.map(clip(_, 14)).getOrElse("metric"),
        value = values.lastOption.map((_, v) => LoopLineage.fmtMetric(v)).getOrElse("—"),
        accent = true,
        note = values.dropRight(1).lastOption.map((_, v) => s"was ${LoopLineage.fmtMetric(v)}").getOrElse("")
      )
    )

  /** The attempt the loop is on: the driver's, if it is driving, else the last one
    * the newest generation recorded. */
  private def attemptNow(l: LoopWire): Option[Int] =
    l.stage.map(_.attempt).orElse(l.generations.maxByOption(_.gen).flatMap(_.attempts.lastOption).map(_.attempt))

  // -- the generation window ---------------------------------------------------

  def panelOf(state: AppState, l: LoopWire): PanelView = state.focus match
    case Focus.Code =>
      if l.defSource.nonEmpty then PanelView.Code("Loop code", WorkflowView.highlightScala(l.defSource))
      else PanelView.Closed
    case Focus.Generation(gen, pane, attempt) =>
      l.generations.find(_.gen == gen) match
        case Some(g) => PanelView.Generation(generationView(state, l, g, pane, attempt))
        case None    => PanelView.Closed
    case Focus.Node(_) | Focus.Unfocused => PanelView.Closed

  private def generationView(
      state: AppState,
      l: LoopWire,
      g: LoopGenerationWire,
      pane: GenPane,
      wanted: Option[Int]
  ): GenerationView =
    val stage = l.stage.filter(_.gen == g.gen && isLive(l))
    // The attempt in flight has no ledger record yet — it is only written down once
    // it has been submitted — so the driver's own count is what puts a pill on it.
    val recorded = g.attempts.toVector
    val inFlight = stage.map(_.attempt).filterNot(n => recorded.exists(_.attempt == n))
    val numbers = recorded.map(_.attempt) ++ inFlight
    val selected = wanted.filter(numbers.contains).orElse(numbers.lastOption).getOrElse(0)
    val attempt = recorded.find(_.attempt == selected)
    GenerationView(
      loopId = l.id,
      gen = g.gen,
      title = s"Generation ${g.gen}",
      state = g.state,
      stateKind = stateKind(g.state),
      stats = Vector(
        "attempts" -> numbers.size.toString,
        headlineOf(l).map(clip(_, 12)).getOrElse("metric") -> headlineValue(l, g).getOrElse("—")
      ),
      phase = phaseLine(g, attempt, stage.filter(_.attempt == selected), pane),
      attempts = numbers.map: n =>
        AttemptPill(n, s"attempt $n", attemptKind(recorded.find(_.attempt == n)), selected = n == selected),
      attempt = selected,
      body = pane match
        case GenPane.Overview  => GenBody.Overview(overview(state, l, g, attempt))
        case GenPane.Worker    => GenBody.Transcript(transcriptPane(state, l, g.gen, worker = true))
        case GenPane.Evaluator => GenBody.Transcript(transcriptPane(state, l, g.gen, worker = false))
    )

  private def stateKind(state: String): StatusKind = state match
    case "accepted"  => StatusKind.Done
    case "abandoned" => StatusKind.Interrupted
    case _           => StatusKind.Running

  /** An attempt's pill colour: its furthest gate. Blue while it is still moving,
    * red where the checker refused it, amber where the evaluator did, green once it
    * was accepted. */
  private def attemptKind(a: Option[LoopAttemptWire]): StatusKind = a match
    case None => StatusKind.Running
    case Some(x) =>
      x.check match
        case None                => StatusKind.Running
        case Some(c) if !c.pass  => StatusKind.Failed
        case Some(_) =>
          x.verdict match
            case None                  => StatusKind.Running
            case Some(v) if v.accepted => StatusKind.Done
            case Some(_)               => StatusKind.Interrupted

  /** Where the pulse is on the Worker —◆— Evaluator line, and what each station has
    * already made of this attempt. `stage` is `None` unless the driver is on this
    * very attempt, which is what keeps a settled generation's line still. */
  private def phaseLine(
      g: LoopGenerationWire,
      a: Option[LoopAttemptWire],
      stage: Option[LoopStageWire],
      pane: GenPane
  ): PhaseLine =
    val step = stage.map(_.step)
    val worker =
      if step.contains("working") then PhaseKind.Live
      else if a.isDefined then PhaseKind.Ok
      else PhaseKind.Idle
    val checker = a.flatMap(_.check) match
      case Some(c) => if c.pass then PhaseKind.Ok else PhaseKind.Bad
      case None    => if step.contains("checking") then PhaseKind.Live else PhaseKind.Idle
    val evaluator = a.flatMap(_.verdict) match
      case Some(v) => if v.accepted then PhaseKind.Ok else PhaseKind.Bad
      case None    => if step.contains("evaluating") then PhaseKind.Live else PhaseKind.Idle
    PhaseLine(
      PhaseStation(GenPane.Worker, "worker", worker, pane == GenPane.Worker),
      PhaseStation(GenPane.Overview, "check", checker, pane == GenPane.Overview),
      PhaseStation(GenPane.Evaluator, "evaluator", evaluator, pane == GenPane.Evaluator)
    )

  private def overview(
      state: AppState,
      l: LoopWire,
      g: LoopGenerationWire,
      a: Option[LoopAttemptWire]
  ): GenOverview =
    val attempt = a.map(_.attempt).getOrElse(0)
    val metrics = g.metrics.toVector.map((k, v) => k -> LoopLineage.fmtMetric(v))
    GenOverview(
      submitted = a.isDefined,
      description = a.map(_.description).filter(_.nonEmpty).getOrElse(g.description),
      commit = g.commit.map(_.take(9)).getOrElse(""),
      metrics = metrics,
      artifact = a.map(_.artifact).getOrElse(""),
      check = a.flatMap(_.check).map: c =>
        val measured = c.metrics.toVector.map((k, v) => k -> LoopLineage.fmtMetric(v))
        // An accepted generation's figures ARE the accepted attempt's, and printing
        // the same three numbers twice on one screen only makes a reader look for a
        // difference that is not there.
        CheckReport(c.pass, c.reasons.toVector, if measured == metrics then Vector.empty else measured)
      ,
      verdict = a.flatMap(_.verdict).map(v => VerdictNote(v.accepted, v.goalReached, v.feedback)),
      diff = DiffPane(
        available = a.exists(_.hasSnapshot),
        state = if a.exists(_.hasSnapshot) then state.diffs.get((l.id, g.gen, attempt)) else None
      )
    )

  /** One generation agent's transcript pane.
    *
    * A settled generation's transcript is not on the wire — the host keeps only the
    * one in flight — so `Loading` here is the render's cue to ask the API for it. A
    * transcript this session watched stream is still in hand afterwards, and is
    * `Settled` without a round trip. */
  private def transcriptPane(state: AppState, l: LoopWire, gen: Int, worker: Boolean): TranscriptPane =
    val label = if worker then s"gen-$gen-worker" else s"gen-$gen-eval"
    val live = isLive(l) && l.liveLabel.contains(label)
    val transcript = state.loopTranscript(l.id, label).getOrElse(Transcript.empty)
    val status =
      if live then PaneStatus.Live
      else
        state.fetches.get((l.id, label)) match
          case Some(FetchState.Ready)      => PaneStatus.Settled
          case Some(FetchState.Failed(m))  => PaneStatus.Missing(m)
          case Some(FetchState.Loading)    => PaneStatus.Loading
          case None                        => if transcript.items.nonEmpty then PaneStatus.Settled else PaneStatus.Loading
    val items = transcript.items
    TranscriptPane(
      label = label,
      rows = items.zipWithIndex.map((item, i) =>
        WorkflowView.transcriptRow(item, isLast = i == items.size - 1, streaming = live)),
      streaming = live,
      status = status
    )

  private def headlineOf(l: LoopWire): Option[String] = LoopLineage.headlineMetric(l.generations)

  private def headlineValue(l: LoopWire, g: LoopGenerationWire): Option[String] =
    headlineOf(l).flatMap(k => g.metrics.collectFirst { case (m, v) if m == k => LoopLineage.fmtMetric(v) })

  private def clip(s: String, max: Int): String = if s.length > max then s.take(max - 1) + "…" else s
