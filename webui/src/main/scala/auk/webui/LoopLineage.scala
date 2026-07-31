package auk.webui

import auk.workflow.LoopGenerationWire

/** What one circle on the lineage is. */
enum LineageKind:
  case Baseline, Accepted, Abandoned, Running

object LineageKind:
  def cssClass(k: LineageKind): String = k match
    case Baseline  => "is-baseline"
    case Accepted  => "is-accepted"
    case Abandoned => "is-abandoned"
    case Running   => "is-running"

/** One node of the lineage, positioned.
  *
  * `key` is a stable identity for keyed rendering; `label` is the mono micro-label
  * beneath the circle and `metric` the serif figure above it (empty when the
  * generation was never measured). `title` is the hover text, which is where a
  * description too long for the board goes.
  */
final case class LineageNode(
    key: String,
    gen: Option[Int],
    kind: LineageKind,
    x: Double,
    y: Double,
    r: Double,
    label: String,
    /** Where the generation number is set. On the main line it sits beneath the
      * circle; on a branch it sits beside it, because a stack has another circle
      * where "beneath" would be and a number between two circles belongs to
      * neither. */
    labelX: Double,
    labelY: Double,
    metric: String,
    title: String,
    selected: Boolean
)

/** How a lineage edge is drawn: a straight run along the main line, the curve that
  * lifts a branch off its parent, or the vertical spine a branch stacks along. */
enum LineageEdgeKind:
  case Line, Rise, Riser

object LineageEdgeKind:
  def cssClass(k: LineageEdgeKind): String = k match
    case Line  => "is-line"
    case Rise  => "is-rise"
    case Riser => "is-riser"

/** One edge, as its two endpoints. `dashed` marks the segment reaching towards a
  * generation that has not landed yet — the render marches it while the loop is
  * live. */
final case class LineageEdge(
    kind: LineageEdgeKind,
    x1: Double,
    y1: Double,
    x2: Double,
    y2: Double,
    dashed: Boolean
)

/** The lineage as pure geometry: everything the SVG needs and nothing about SVG.
  *
  * `live` is the whole animation switch — a parked or orphaned loop draws the same
  * picture, still. `endCap` is the small accent tick a loop that reached its goal
  * earns at the end of its line.
  */
final case class Lineage(
    nodes: Vector[LineageNode],
    edges: Vector[LineageEdge],
    endCap: Option[(Double, Double)],
    width: Double,
    height: Double,
    live: Boolean
)

/** Where every circle of a loop's lineage sits.
  *
  * The picture is a sentence read left to right: a hairline from the baseline
  * through each accepted generation, and, rising off whichever generation they were
  * tried from, the ones that did not work. Abandoned generations branch **up** and
  * to the right, stacked in the column their successor occupies — so the reader sees
  * "at this step we tried these three, and this is the one that stuck", with the
  * most recent failure nearest the line it eventually joined. A generation in flight
  * is the next node on the line, reached by a dashed segment: the loop is trying to
  * arrive at it.
  *
  * Pure geometry in an abstract unit space, so it is unit-tested without a DOM and
  * the render layer only turns numbers into `<circle>`s and `<path>`s. The
  * coordinate system's origin is the top-left of the SVG; `y` grows downward, as
  * SVG's does, which is why a branch rising is a subtraction.
  */
object LoopLineage:

  /** Horizontal distance between one step of the lineage and the next. */
  val StepX: Double = 132.0
  /** Space kept left of the baseline node and right of the last thing drawn. */
  val MarginX: Double = 52.0
  /** Vertical distance between two stacked abandoned generations. */
  val BranchDy: Double = 58.0
  /** How far along the step a branch stack stands. Halfway rather than a whole step:
    * a stem that travels a full step while rising one row is so shallow that it reads
    * as a second route between the two accepted generations, instead of as a
    * departure from the first. Standing the failures between the two steps also says
    * the true thing — they were tried on the way. */
  val BranchDx: Double = 0.52
  /** Space above the topmost circle, for the serif metric caption that sits there. */
  val TopPad: Double = 42.0
  /** Space below the main line, for the mono generation numbers. */
  val BottomPad: Double = 48.0

  val AcceptedR: Double = 11.0
  val RunningR: Double = 11.0
  val AbandonedR: Double = 7.0
  val BaselineR: Double = 4.0

  /** Where a node's caption and its number sit relative to its centre. Kept here
    * rather than in the stylesheet because they are geometry the SVG lays out, not
    * styling: an `<text>` in an SVG is positioned, not flowed. */
  val MetricDy: Double = -22.0
  val LabelDy: Double = 28.0

  /** The lineage of a loop, positioned.
    *
    * `live` suppresses every animation when false, which is what a parked or
    * orphaned loop needs: its dashed segment is still the truthful picture of a
    * generation that never landed, but nothing is marching towards it any more.
    */
  def layout(
      generations: List[LoopGenerationWire],
      selected: Option[Int] = None,
      live: Boolean = false,
      goalReached: Boolean = false
  ): Lineage =
    val all = generations.toVector.sortBy(_.gen)
    val accepted = all.filter(_.state == "accepted")
    val running = all.filter(_.state == "running")
    val headline = headlineMetric(generations)

    // Slot 0 is the baseline; the accepted generations follow in order, and anything
    // still in flight takes the slots after them.
    val mainline = accepted ++ running
    val slotOf: Map[Int, Int] = mainline.zipWithIndex.map((g, i) => g.gen -> (i + 1)).toMap
    def slotX(slot: Int): Double = MarginX + slot * StepX

    // A branch stands between its parent's slot and the next — see [[BranchDx]]. A
    // parent that is missing or was never accepted falls back to the baseline, which
    // is where a generation with no accepted ancestor genuinely started.
    val abandoned = all.filter(_.state == "abandoned")
    val branches: Vector[(Int, Vector[LoopGenerationWire])] =
      abandoned.groupBy(g => g.parent.flatMap(slotOf.get).getOrElse(0)).toVector.sortBy(_._1)
    val stackHeight = branches.map(_._2.size).maxOption.getOrElse(0)

    val lineY = TopPad + stackHeight * BranchDy

    val baseline = LineageNode(
      key = "baseline",
      gen = None,
      kind = LineageKind.Baseline,
      x = slotX(0),
      y = lineY,
      r = BaselineR,
      label = "",
      labelX = slotX(0),
      labelY = lineY + LabelDy,
      metric = "",
      title = "Where the loop started",
      selected = false
    )

    val mainNodes = mainline.map: g =>
      val slot = slotOf(g.gen)
      val kind = if g.state == "running" then LineageKind.Running else LineageKind.Accepted
      LineageNode(
        key = s"g${g.gen}",
        gen = Some(g.gen),
        kind = kind,
        x = slotX(slot),
        y = lineY,
        r = if kind == LineageKind.Running then RunningR else AcceptedR,
        label = g.gen.toString,
        labelX = slotX(slot),
        labelY = lineY + LabelDy,
        metric = headline.flatMap(k => g.metrics.collectFirst { case (m, v) if m == k => fmtMetric(v) }).getOrElse(""),
        title = titleOf(g),
        selected = selected.contains(g.gen)
      )

    val branchNodes = branches.flatMap: (parentSlot, gens) =>
      val x = slotX(parentSlot) + StepX * BranchDx
      // Most recent nearest the line: the last generation tried is the one whose
      // failure the accepted node below it answers.
      gens.zipWithIndex.map: (g, i) =>
        LineageNode(
          key = s"g${g.gen}",
          gen = Some(g.gen),
          kind = LineageKind.Abandoned,
          x = x,
          y = lineY - (gens.size - i) * BranchDy,
          r = AbandonedR,
          label = g.gen.toString,
          labelX = x + AbandonedR + 9,
          labelY = lineY - (gens.size - i) * BranchDy + 4,
          metric = "",
          title = titleOf(g),
          selected = selected.contains(g.gen)
        )

    val mainPoints = baseline +: mainNodes
    val mainEdges = mainPoints.sliding(2).collect { case Vector(a, b) =>
      LineageEdge(LineageEdgeKind.Line, a.x, a.y, b.x, b.y, dashed = b.kind == LineageKind.Running)
    }.toVector

    val branchEdges = branches.flatMap: (parentSlot, gens) =>
      val x = slotX(parentSlot) + StepX * BranchDx
      val bottom = lineY - BranchDy
      val stem = LineageEdge(LineageEdgeKind.Rise, slotX(parentSlot), lineY, x, bottom, dashed = false)
      val riser =
        if gens.size <= 1 then Vector.empty
        else Vector(LineageEdge(LineageEdgeKind.Riser, x, bottom, x, lineY - gens.size * BranchDy, dashed = false))
      stem +: riser

    val nodes = mainPoints ++ branchNodes
    val cap = Option.when(goalReached && mainNodes.nonEmpty)((mainNodes.last.x + 30.0, lineY))
    val rightmost = (nodes.map(_.x) ++ cap.map(_._1)).max

    Lineage(
      nodes = nodes,
      edges = mainEdges ++ branchEdges,
      endCap = cap,
      width = rightmost + MarginX,
      height = lineY + BottomPad,
      live = live
    )

  /** The metric a lineage is read by: of those the most recent measured generation
    * reports, the first in key order that has actually moved.
    *
    * A checker reports whatever it likes and the loop never declares one of them
    * primary, so the choice has to be made here — and it has to be the same choice
    * every frame, or the captions above the circles would swap around as generations
    * land. Preferring a metric that has changed is what makes the caption worth the
    * space: a gate metric that reads 0.98 at every generation says only that the gate
    * held, where the one the loop is actually moving is the story. Key order settles
    * the rest, including a loop young enough that nothing has moved yet. */
  def headlineMetric(generations: List[LoopGenerationWire]): Option[String] =
    val measured = generations.filter(_.metrics.nonEmpty).sortBy(_.gen)
    val keys = measured.lastOption.map(_.metrics.map(_._1).sorted).getOrElse(Nil)
    def valuesOf(key: String): List[Double] = measured.flatMap(_.metrics.collectFirst { case (k, v) if k == key => v })
    keys.find(k => valuesOf(k).distinct.sizeIs > 1).orElse(keys.headOption)

  /** The value of [[headlineMetric]] in each accepted generation, oldest first —
    * what the top bar's "was X" suffix reads its previous value from. */
  def headlineValues(generations: List[LoopGenerationWire]): Vector[(Int, Double)] =
    headlineMetric(generations) match
      case None => Vector.empty
      case Some(key) =>
        generations.toVector
          .sortBy(_.gen)
          .flatMap(g => g.metrics.collectFirst { case (k, v) if k == key => g.gen -> v })

  /** A metric as a caption: whole numbers stay whole, everything else keeps three
    * decimals with the trailing zeros trimmed — enough to tell two generations
    * apart, short enough to sit above a 9px circle. */
  def fmtMetric(v: Double): String =
    if v.isNaN then "—"
    else if v.isInfinite then (if v > 0 then "∞" else "-∞")
    else if v == v.round.toDouble && math.abs(v) < 1e15 then v.round.toString
    else
      val s = f"$v%.3f"
      val trimmed = s.reverse.dropWhile(_ == '0').reverse
      if trimmed.endsWith(".") then trimmed.dropRight(1) else trimmed

  private def titleOf(g: LoopGenerationWire): String =
    val head = s"Generation ${g.gen} — ${g.state}"
    if g.description.isEmpty then head else s"$head\n${g.description}"
