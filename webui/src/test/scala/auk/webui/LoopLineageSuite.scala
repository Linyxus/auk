package auk.webui

import auk.workflow.LoopGenerationWire

class LoopLineageSuite extends munit.FunSuite:

  private def gen(
      n: Int,
      state: String,
      parent: Option[Int] = None,
      metrics: List[(String, Double)] = Nil
  ): LoopGenerationWire =
    LoopGenerationWire(n, parent, state, "", metrics, None, Nil, "t0", None)

  private def nodeOf(l: Lineage, gen: Int): LineageNode =
    l.nodes.find(_.gen.contains(gen)).getOrElse(fail(s"no node for generation $gen"))

  private def baselineOf(l: Lineage): LineageNode =
    l.nodes.find(_.kind == LineageKind.Baseline).getOrElse(fail("no baseline node"))

  private def edges(l: Lineage, kind: LineageEdgeKind): Vector[LineageEdge] = l.edges.filter(_.kind == kind)

  // -- the main line -----------------------------------------------------------

  test("an empty loop is just the baseline"):
    val l = LoopLineage.layout(Nil)
    assertEquals(l.nodes.map(_.kind), Vector(LineageKind.Baseline))
    assertEquals(l.edges, Vector.empty)
    assertEquals(l.endCap, None)

  test("accepted generations sit left to right on one line, a step apart"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "accepted", Some(1)), gen(3, "accepted", Some(2))))
    val xs = Vector(1, 2, 3).map(nodeOf(l, _).x)
    assertEquals(xs, Vector(1, 2, 3).map(i => LoopLineage.MarginX + i * LoopLineage.StepX))
    assert(Vector(1, 2, 3).map(nodeOf(l, _).y).distinct.sizeIs == 1, "accepted generations share the line")
    assertEquals(baselineOf(l).x, LoopLineage.MarginX)

  test("generations are placed in generation order however the wire lists them"):
    val shuffled = List(gen(3, "accepted", Some(2)), gen(1, "accepted"), gen(2, "accepted", Some(1)))
    val l = LoopLineage.layout(shuffled)
    assert(nodeOf(l, 1).x < nodeOf(l, 2).x && nodeOf(l, 2).x < nodeOf(l, 3).x)

  test("the in-flight generation takes the slot after the last accepted one"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "running", Some(1))))
    assertEquals(nodeOf(l, 2).kind, LineageKind.Running)
    assertEquals(nodeOf(l, 2).x - nodeOf(l, 1).x, LoopLineage.StepX)
    assertEquals(nodeOf(l, 2).y, nodeOf(l, 1).y)

  test("the segment reaching the in-flight generation is the dashed one"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "running", Some(1))))
    val dashed = l.edges.filter(_.dashed)
    assertEquals(dashed.size, 1)
    assertEquals(dashed.head.x2, nodeOf(l, 2).x)
    assert(l.edges.filterNot(_.dashed).forall(_.kind != LineageEdgeKind.Line) || l.edges.count(_.dashed) == 1)

  test("a settled lineage has no dashed segment"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "accepted", Some(1))))
    assert(l.edges.forall(!_.dashed))

  test("the main line is one segment per step, baseline included"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "accepted", Some(1))))
    assertEquals(edges(l, LineageEdgeKind.Line).size, 2)
    assertEquals(edges(l, LineageEdgeKind.Line).head.x1, baselineOf(l).x)

  // -- branches ----------------------------------------------------------------

  test("an abandoned generation branches up and to the right of its parent"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "abandoned", Some(1)), gen(3, "accepted", Some(1))))
    val parent = nodeOf(l, 1)
    val branch = nodeOf(l, 2)
    assertEquals(branch.kind, LineageKind.Abandoned)
    assert(branch.x > parent.x, "a branch stands to the right of its parent")
    assert(branch.x < nodeOf(l, 3).x, "…and short of the generation that succeeded it")
    assert(branch.y < parent.y, "…and above the line (y grows downward)")
    assertEquals(parent.y - branch.y, LoopLineage.BranchDy)

  test("abandoned generations sharing a parent stack, the most recent nearest the line"):
    val l = LoopLineage.layout(List(
      gen(1, "accepted"),
      gen(2, "abandoned", Some(1)),
      gen(3, "abandoned", Some(1)),
      gen(4, "abandoned", Some(1))
    ))
    val stack = Vector(2, 3, 4).map(nodeOf(l, _))
    assertEquals(stack.map(_.x).distinct.size, 1, "one shared column")
    // generation 4 is the most recent, so it is the lowest — nearest the line
    assert(nodeOf(l, 4).y > nodeOf(l, 3).y && nodeOf(l, 3).y > nodeOf(l, 2).y)
    assertEquals(nodeOf(l, 4).y - nodeOf(l, 3).y, LoopLineage.BranchDy)

  test("a shared branch has one stem and one riser"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "abandoned", Some(1)), gen(3, "abandoned", Some(1))))
    assertEquals(edges(l, LineageEdgeKind.Rise).size, 1)
    assertEquals(edges(l, LineageEdgeKind.Riser).size, 1)
    val stem = edges(l, LineageEdgeKind.Rise).head
    assertEquals((stem.x1, stem.y1), (nodeOf(l, 1).x, nodeOf(l, 1).y))
    // the stem lands on the lowest of the stack; the riser carries on past it
    assertEquals(stem.y2, nodeOf(l, 3).y)
    assertEquals(edges(l, LineageEdgeKind.Riser).head.y2, nodeOf(l, 2).y)

  test("a lone branch needs no riser"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "abandoned", Some(1))))
    assertEquals(edges(l, LineageEdgeKind.Riser), Vector.empty)

  test("branches with different parents get their own columns"):
    val l = LoopLineage.layout(List(
      gen(1, "accepted"),
      gen(2, "abandoned", Some(1)),
      gen(3, "accepted", Some(1)),
      gen(4, "abandoned", Some(3))
    ))
    assert(nodeOf(l, 2).x < nodeOf(l, 4).x)
    assertEquals(edges(l, LineageEdgeKind.Rise).size, 2)

  test("a generation with no accepted parent branches off the baseline"):
    val l = LoopLineage.layout(List(gen(1, "abandoned"), gen(2, "abandoned", Some(99))))
    val stem = edges(l, LineageEdgeKind.Rise).head
    assertEquals(stem.x1, baselineOf(l).x)
    assertEquals(Vector(1, 2).map(nodeOf(l, _).x).distinct.size, 1)

  test("the canvas grows tall enough for the deepest stack"):
    val flat = LoopLineage.layout(List(gen(1, "accepted")))
    val deep = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "abandoned", Some(1)), gen(3, "abandoned", Some(1))))
    assertEquals(deep.height - flat.height, 2 * LoopLineage.BranchDy)
    assert(deep.nodes.map(_.y).min >= 0.0, "nothing is drawn above the canvas")

  test("the canvas is wide enough for a branch that has no successor"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "abandoned", Some(1))))
    assert(l.width > nodeOf(l, 2).x, "the trailing branch column is inside the canvas")

  // -- decoration --------------------------------------------------------------

  test("selection marks exactly the generation asked for"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "abandoned", Some(1))), selected = Some(2))
    assertEquals(l.nodes.filter(_.selected).flatMap(_.gen), Vector(2))

  test("live is carried through, and is what the render animates on"):
    assert(LoopLineage.layout(List(gen(1, "running")), live = true).live)
    assert(!LoopLineage.layout(List(gen(1, "running"))).live)

  test("reaching the goal earns an end cap past the last node"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "accepted", Some(1))), goalReached = true)
    val (x, y) = l.endCap.getOrElse(fail("expected an end cap"))
    assert(x > nodeOf(l, 2).x)
    assertEquals(y, nodeOf(l, 2).y)
    assert(l.width > x, "the cap is inside the canvas")

  test("a loop that reached its goal with no generations gets no cap"):
    assertEquals(LoopLineage.layout(Nil, goalReached = true).endCap, None)

  /** Nothing is drawn as text, so the title is the whole of what a node says — and it
    * is the accessible name the render gives the circle's button. */
  test("a node's title names its generation and what became of it"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "abandoned", Some(1))))
    assertEquals(nodeOf(l, 1).title, "Generation 1 — accepted")
    assertEquals(nodeOf(l, 2).title, "Generation 2 — abandoned")
    assert(baselineOf(l).title.nonEmpty, "even the baseline says what it is")

  test("the circles are drawn large enough to tell apart by size alone"):
    assert(LoopLineage.AcceptedR > LoopLineage.AbandonedR)
    assert(LoopLineage.AbandonedR > LoopLineage.BaselineR)
    assertEquals(LoopLineage.RunningR, LoopLineage.AcceptedR, "an arrival is an arrival, whatever its colour")

  test("node keys are stable and unique, so keyed rendering has an identity"):
    val l = LoopLineage.layout(List(gen(1, "accepted"), gen(2, "abandoned", Some(1)), gen(3, "running", Some(1))))
    assertEquals(l.nodes.map(_.key).distinct.size, l.nodes.size)
    assertEquals(nodeOf(l, 2).key, "g2")

  // -- the headline metric -----------------------------------------------------

  test("the headline metric is the first, in key order, that has moved"):
    val gens = List(
      gen(1, "accepted", metrics = List("accuracy" -> 0.98, "p99_ms" -> 41.0)),
      gen(2, "accepted", Some(1), metrics = List("accuracy" -> 0.98, "p99_ms" -> 33.0))
    )
    assertEquals(LoopLineage.headlineMetric(gens), Some("p99_ms"))

  test("with nothing moving yet the headline falls back to key order"):
    val gens = List(gen(1, "accepted", metrics = List("p99_ms" -> 41.0, "accuracy" -> 0.98)))
    assertEquals(LoopLineage.headlineMetric(gens), Some("accuracy"))

  test("the headline comes from the LATEST measured generation's keys"):
    val gens = List(
      gen(1, "accepted", metrics = List("old_metric" -> 1.0)),
      gen(2, "accepted", Some(1), metrics = List("new_metric" -> 2.0))
    )
    assertEquals(LoopLineage.headlineMetric(gens), Some("new_metric"))

  test("an unmeasured lineage has no headline"):
    assertEquals(LoopLineage.headlineMetric(List(gen(1, "abandoned"))), None)
    assertEquals(LoopLineage.headlineMetric(Nil), None)

  test("headlineValues follows the headline across generations, oldest first"):
    val gens = List(
      gen(2, "accepted", Some(1), metrics = List("p99_ms" -> 33.0)),
      gen(1, "accepted", metrics = List("p99_ms" -> 41.0))
    )
    assertEquals(LoopLineage.headlineValues(gens), Vector(1 -> 41.0, 2 -> 33.0))

  // -- number formatting -------------------------------------------------------

  test("fmtMetric keeps whole numbers whole and trims the rest"):
    assertEquals(LoopLineage.fmtMetric(41.0), "41")
    assertEquals(LoopLineage.fmtMetric(0.0), "0")
    assertEquals(LoopLineage.fmtMetric(33.4), "33.4")
    assertEquals(LoopLineage.fmtMetric(0.9812), "0.981")
    assertEquals(LoopLineage.fmtMetric(-2.5), "-2.5")
    assertEquals(LoopLineage.fmtMetric(1234567.0), "1234567")

  test("fmtMetric says something rather than nothing for a number that is not one"):
    assertEquals(LoopLineage.fmtMetric(Double.NaN), "—")
    assertEquals(LoopLineage.fmtMetric(Double.PositiveInfinity), "∞")
