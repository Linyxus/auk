package auk.webui

import auk.workflow.*

class LoopViewSuite extends munit.FunSuite:

  // -- fixtures ----------------------------------------------------------------

  private def check(pass: Boolean, metrics: (String, Double)*): LoopCheckWire =
    LoopCheckWire(pass, if pass then Nil else List("a reason"), metrics.toList)

  private def verdict(accepted: Boolean, goalReached: Boolean = false): LoopVerdictWire =
    LoopVerdictWire(accepted, "some feedback", goalReached)

  private def attempt(
      n: Int,
      check: Option[LoopCheckWire] = None,
      verdict: Option[LoopVerdictWire] = None,
      description: String = "",
      hasSnapshot: Boolean = true
  ): LoopAttemptWire =
    LoopAttemptWire(n, description, s"""{"n": $n}""", hasSnapshot, check, verdict, "t")

  private def gen(
      n: Int,
      state: String,
      parent: Option[Int] = None,
      description: String = "",
      metrics: List[(String, Double)] = Nil,
      commit: Option[String] = None,
      attempts: List[LoopAttemptWire] = Nil
  ): LoopGenerationWire =
    LoopGenerationWire(n, parent, state, description, metrics, commit, attempts, "t0", None)

  private def stage(gen: Int, attempt: Int, step: String): Option[LoopStageWire] =
    Some(LoopStageWire(gen, attempt, step))

  /** The activity line comes with the stage rather than beside it, because on the wire
    * it does too — the host writes both from the one stage. */
  private def loop(
      id: String = "L",
      generations: List[LoopGenerationWire] = Nil,
      phase: String = "running (gen 1)",
      parked: Option[String] = None,
      orphaned: Boolean = false,
      stage: Option[LoopStageWire] = None,
      liveLabel: Option[String] = None,
      goal: String = "make it fast",
      rubric: String = "it is faster",
      defSource: String = "lib.loop.start(...)"
  ): LoopWire =
    LoopWire(id, phase, goal, rubric, LoopBudgetsWire(20, 2, 3), defSource, 1,
      held = true, parked = parked, orphaned = orphaned,
      activity = stage.map(s => s"gen ${s.gen}, attempt ${s.attempt} — ${s.step}"), stage = stage,
      liveLabel = liveLabel, generations = generations, createdAt = "t")

  private def stateWith(l: LoopWire, focus: Focus = Focus.Unfocused): AppState =
    AppState(loops = Map(l.id -> l), loopOrder = Vector(l.id), selected = Some(Target.Loop(l.id)), focus = focus)

  private def genViewOf(s: AppState, l: LoopWire): GenerationView =
    LoopView.panelOf(s, l) match
      case PanelView.Generation(g) => g
      case other                   => fail(s"expected a Generation panel, got $other")

  // -- state vocabulary --------------------------------------------------------

  test("a loop's dot reads its state, with goal-reached its own"):
    assertEquals(LoopView.dotOf(loop()), LoopDot.Live)
    assertEquals(LoopView.dotOf(loop(parked = Some("patience exhausted"))), LoopDot.Parked)
    assertEquals(LoopView.dotOf(loop(parked = Some("goal reached"))), LoopDot.Reached)
    assertEquals(LoopView.dotOf(loop(orphaned = true)), LoopDot.Orphaned)

  test("orphaned outranks parked: nothing is driving it either way"):
    assertEquals(LoopView.dotOf(loop(parked = Some("goal reached"), orphaned = true)), LoopDot.Orphaned)

  test("only a loop with nothing wrong is live (which is what animates)"):
    assert(LoopView.isLive(loop()))
    assert(!LoopView.isLive(loop(parked = Some("user requested"))))
    assert(!LoopView.isLive(loop(orphaned = true)))

  test("each LoopDot maps to a css class the stylesheet has"):
    assertEquals(LoopDot.cssClass(LoopDot.Live), "is-running")
    assertEquals(LoopDot.cssClass(LoopDot.Parked), "is-paused")
    assertEquals(LoopDot.cssClass(LoopDot.Reached), "is-done")
    assertEquals(LoopDot.cssClass(LoopDot.Orphaned), "is-orphaned")

  // -- the switcher ------------------------------------------------------------

  test("loop tabs follow insertion order, marking the selected one"):
    val a = loop("a", List(gen(1, "accepted"), gen(2, "abandoned", Some(1))))
    val b = loop("b")
    val s = AppState(loops = Map("a" -> a, "b" -> b), loopOrder = Vector("a", "b"), selected = Some(Target.Loop("b")))
    val tabs = LoopView.tabsOf(s)
    assertEquals(tabs.map(_.loopId), Vector("a", "b"))
    assertEquals(tabs.find(_.selected).map(_.loopId), Some("b"))

  test("a loop tab counts accepted generations out of started"):
    val l = loop("a", List(gen(1, "accepted"), gen(2, "abandoned", Some(1)), gen(3, "running", Some(1))))
    val tab = LoopView.tabsOf(stateWith(l)).head
    assertEquals((tab.accepted, tab.started), (1, 3))

  // -- the board ---------------------------------------------------------------

  test("the goal is split into a headline and the rest, each un-wrapped"):
    val goal = "Cut p99 by 30%\nwithout losing accuracy.\n\nThe tokenizer is on\nthe hot path."
    val b = LoopView.boardOf(loop(goal = goal), None)
    assertEquals(b.goal, "Cut p99 by 30% without losing accuracy.")
    assertEquals(b.goalRest, "The tokenizer is on the hot path.")

  test("a one-paragraph goal leaves nothing for the standfirst"):
    assertEquals(LoopView.boardOf(loop(goal = "be fast"), None).goalRest, "")

  test("the board's lineage carries the selection, liveness and the goal-reached cap"):
    val gens = List(gen(1, "accepted", metrics = List("m" -> 1.0)), gen(2, "accepted", Some(1), metrics = List("m" -> 2.0)))
    val live = LoopView.boardOf(loop(generations = gens), Some(2))
    assert(live.lineage.live)
    assertEquals(live.lineage.nodes.filter(_.selected).flatMap(_.gen), Vector(2))
    assertEquals(live.lineage.endCap, None)
    val done = LoopView.boardOf(loop(generations = gens, parked = Some("goal reached")), None)
    assert(!done.lineage.live)
    assert(done.lineage.endCap.isDefined)

  test("the board states the definition's declared limits"):
    assertEquals(LoopView.boardOf(loop(), None).budgets, "20 generations · patience 2 · 3 attempts")

  test("a loop nobody is driving shows no activity line"):
    assertEquals(LoopView.boardOf(loop(parked = Some("x")), None).activity, "")

  // -- the figures strip -------------------------------------------------------

  test("the metrics strip counts generations, names the attempt, and moves the headline"):
    val gens = List(
      gen(1, "accepted", metrics = List("p99_ms" -> 41.8)),
      gen(2, "abandoned", Some(1)),
      gen(3, "accepted", Some(1), metrics = List("p99_ms" -> 33.4)),
      gen(4, "running", Some(3), attempts = List(attempt(1)))
    )
    val cells = LoopView.metricsOf(loop(generations = gens, stage = stage(4, 2, "working")))
    assertEquals(cells.map(_.label), Vector("generations", "attempt", "p99_ms"))
    assertEquals(cells(0).value, "2/4")
    assertEquals(cells(1).value, "2") // the driver's count, ahead of the ledger's
    assertEquals(cells(2).value, "33.4")
    assertEquals(cells(2).note, "was 41.8")
    assert(cells(2).accent)

  test("with no driver the attempt cell falls back to the newest generation's last"):
    val gens = List(gen(1, "abandoned", attempts = List(attempt(1), attempt(2))))
    assertEquals(LoopView.metricsOf(loop(generations = gens, parked = Some("x")))(1).value, "2")

  test("an unmeasured loop's headline cell says so rather than inventing a figure"):
    val cells = LoopView.metricsOf(loop(generations = List(gen(1, "running"))))
    assertEquals(cells(2).label, "metric")
    assertEquals(cells(2).value, "—")
    assertEquals(cells(2).note, "")

  test("one measured generation has no previous value to note"):
    val cells = LoopView.metricsOf(loop(generations = List(gen(1, "accepted", metrics = List("m" -> 3.0)))))
    assertEquals(cells(2).value, "3")
    assertEquals(cells(2).note, "")

  // -- the panel ---------------------------------------------------------------

  test("nothing focused leaves the drawer closed"):
    assertEquals(LoopView.panelOf(stateWith(loop()), loop()), PanelView.Closed)

  test("focusing the code shows the loop definition, titled as such"):
    val l = loop(defSource = "val x = 1")
    LoopView.panelOf(stateWith(l, Focus.Code), l) match
      case PanelView.Code(title, tokens) =>
        assertEquals(title, "Loop code")
        assertEquals(tokens.map(_.text).mkString, "val x = 1")
      case other => fail(s"expected Code, got $other")

  test("focusing code on a loop with no definition falls back to Closed"):
    val l = loop(defSource = "")
    assertEquals(LoopView.panelOf(stateWith(l, Focus.Code), l), PanelView.Closed)

  test("focusing a generation the loop does not have falls back to Closed"):
    val l = loop(generations = List(gen(1, "accepted")))
    assertEquals(LoopView.panelOf(stateWith(l, Focus.Generation(9, GenPane.Overview, None)), l), PanelView.Closed)

  // -- attempts ----------------------------------------------------------------

  private def openGen(l: LoopWire, gen: Int, pane: GenPane = GenPane.Overview, attempt: Option[Int] = None) =
    genViewOf(stateWith(l, Focus.Generation(gen, pane, attempt)), l)

  test("the last attempt is the one everything is scoped to by default"):
    val l = loop(generations = List(gen(1, "abandoned", attempts = List(attempt(1), attempt(2)))))
    assertEquals(openGen(l, 1).attempt, 2)
    assertEquals(openGen(l, 1).attempts.map(_.attempt), Vector(1, 2))
    assertEquals(openGen(l, 1).attempts.filter(_.selected).map(_.attempt), Vector(2))

  test("a chosen attempt wins, and one the generation lacks is clamped away"):
    val l = loop(generations = List(gen(1, "abandoned", attempts = List(attempt(1), attempt(2)))))
    assertEquals(openGen(l, 1, attempt = Some(1)).attempt, 1)
    assertEquals(openGen(l, 1, attempt = Some(9)).attempt, 2)

  test("the attempt in flight gets a pill of its own, ahead of the ledger"):
    val l = loop(
      generations = List(gen(1, "running", attempts = List(attempt(1, check = Some(check(false)))))),
      stage = stage(1, 2, "working")
    )
    val g = openGen(l, 1)
    assertEquals(g.attempts.map(_.attempt), Vector(1, 2))
    assertEquals(g.attempt, 2)
    assertEquals(g.attempts.last.kind, StatusKind.Running)

  test("a generation with nothing submitted has no pills at all"):
    val l = loop(generations = List(gen(1, "running")))
    assertEquals(openGen(l, 1).attempts, Vector.empty)
    assertEquals(openGen(l, 1).attempt, 0)

  test("an attempt's pill reads its furthest gate"):
    def kindOf(a: LoopAttemptWire): StatusKind =
      openGen(loop(generations = List(gen(1, "abandoned", attempts = List(a)))), 1).attempts.head.kind
    assertEquals(kindOf(attempt(1)), StatusKind.Running)
    assertEquals(kindOf(attempt(1, check = Some(check(false)))), StatusKind.Failed)
    assertEquals(kindOf(attempt(1, check = Some(check(true)))), StatusKind.Running)
    assertEquals(kindOf(attempt(1, check = Some(check(true)), verdict = Some(verdict(false)))), StatusKind.Interrupted)
    assertEquals(kindOf(attempt(1, check = Some(check(true)), verdict = Some(verdict(true)))), StatusKind.Done)

  // -- the phase line ----------------------------------------------------------

  private def phaseOf(l: LoopWire, gen: Int, pane: GenPane = GenPane.Overview): PhaseLine =
    openGen(l, gen, pane).phase

  test("a settled, accepted generation's line is green all the way across"):
    val l = loop(
      parked = Some("x"),
      generations = List(gen(1, "accepted",
        attempts = List(attempt(1, check = Some(check(true)), verdict = Some(verdict(true))))))
    )
    assertEquals(phaseOf(l, 1).stations.map(_.kind), Vector(PhaseKind.Ok, PhaseKind.Ok, PhaseKind.Ok))

  test("a checker rejection stops the line at the diamond"):
    val l = loop(parked = Some("x"),
      generations = List(gen(1, "abandoned", attempts = List(attempt(1, check = Some(check(false)))))))
    assertEquals(phaseOf(l, 1).stations.map(_.kind), Vector(PhaseKind.Ok, PhaseKind.Bad, PhaseKind.Idle))

  test("an evaluator declining marks the last station, not the checker"):
    val l = loop(parked = Some("x"), generations = List(gen(1, "abandoned",
      attempts = List(attempt(1, check = Some(check(true)), verdict = Some(verdict(false)))))))
    assertEquals(phaseOf(l, 1).stations.map(_.kind), Vector(PhaseKind.Ok, PhaseKind.Ok, PhaseKind.Bad))

  test("the pulse walks the line as the driver's stage moves"):
    def stationsFor(step: String): Vector[PhaseKind] =
      phaseOf(loop(generations = List(gen(1, "running")), stage = stage(1, 1, step)), 1)
        .stations.map(_.kind)
    assertEquals(stationsFor("working"), Vector(PhaseKind.Live, PhaseKind.Idle, PhaseKind.Idle))
    assertEquals(stationsFor("checking"), Vector(PhaseKind.Idle, PhaseKind.Live, PhaseKind.Idle))
    assertEquals(stationsFor("evaluating"), Vector(PhaseKind.Idle, PhaseKind.Idle, PhaseKind.Live))

  test("a parked loop's line never pulses, whatever its last activity said"):
    val l = loop(parked = Some("api failure"), generations = List(gen(1, "running")),
      stage = stage(1, 1, "working"))
    assert(phaseOf(l, 1).stations.forall(_.kind != PhaseKind.Live))

  test("another generation's stage does not pulse this one's line"):
    val l = loop(
      generations = List(gen(1, "accepted", attempts = List(attempt(1, check = Some(check(true)), verdict = Some(verdict(true))))),
        gen(2, "running", Some(1))),
      stage = stage(2, 1, "working")
    )
    assert(phaseOf(l, 1).stations.forall(_.kind != PhaseKind.Live))

  test("the stations are the window's tabs, and exactly one is selected"):
    val l = loop(generations = List(gen(1, "running")))
    assertEquals(phaseOf(l, 1, GenPane.Overview).stations.map(_.selected), Vector(false, true, false))
    assertEquals(phaseOf(l, 1, GenPane.Worker).stations.map(_.selected), Vector(true, false, false))
    assertEquals(phaseOf(l, 1, GenPane.Evaluator).stations.map(_.selected), Vector(false, false, true))
    assertEquals(phaseOf(l, 1).stations.map(_.pane), Vector(GenPane.Worker, GenPane.Overview, GenPane.Evaluator))

  // -- the overview pane -------------------------------------------------------

  private def overviewOf(l: LoopWire, gen: Int, attempt: Option[Int] = None): GenOverview =
    openGen(l, gen, GenPane.Overview, attempt).body match
      case GenBody.Overview(o) => o
      case other               => fail(s"expected an Overview body, got $other")

  test("the overview prefers the attempt's own account and falls back to the generation's"):
    val l = loop(generations = List(gen(1, "accepted", description = "what was accepted",
      attempts = List(attempt(1, description = "what attempt 1 tried")))))
    assertEquals(overviewOf(l, 1).description, "what attempt 1 tried")
    val bare = loop(generations = List(gen(1, "accepted", description = "what was accepted",
      attempts = List(attempt(1)))))
    assertEquals(overviewOf(bare, 1).description, "what was accepted")

  test("the commit is clipped to something a person can read back"):
    val l = loop(generations = List(gen(1, "accepted", commit = Some("a1f39c2e7b4d5061"), attempts = List(attempt(1)))))
    assertEquals(overviewOf(l, 1).commit, "a1f39c2e7")

  test("the checker's figures are dropped when they are the acceptance's own"):
    val same = loop(generations = List(gen(1, "accepted", metrics = List("m" -> 2.0),
      attempts = List(attempt(1, check = Some(check(true, "m" -> 2.0)))))))
    assertEquals(overviewOf(same, 1).check.map(_.metrics), Some(Vector.empty))
    val differs = loop(generations = List(gen(1, "abandoned",
      attempts = List(attempt(1, check = Some(check(true, "m" -> 2.0)))))))
    assertEquals(overviewOf(differs, 1).check.map(_.metrics), Some(Vector("m" -> "2")))

  test("a diff can only be asked for where the attempt snapshotted something"):
    val with_ = loop(generations = List(gen(1, "accepted", attempts = List(attempt(1)))))
    assert(overviewOf(with_, 1).diff.available)
    val without = loop(generations = List(gen(1, "accepted", attempts = List(attempt(1, hasSnapshot = false)))))
    assert(!overviewOf(without, 1).diff.available)

  test("a fetched diff reaches the pane, keyed by loop, generation and attempt"):
    val l = loop(generations = List(gen(1, "accepted", attempts = List(attempt(1)))))
    val s = stateWith(l, Focus.Generation(1, GenPane.Overview, None)).diffLoaded(l.id, 1, 1, "@@ a patch")
    genViewOf(s, l).body match
      case GenBody.Overview(o) => assertEquals(o.diff.state, Some(DiffState.Ready("@@ a patch")))
      case other               => fail(s"expected an Overview body, got $other")

  test("an attempt still in flight is marked unsubmitted"):
    val l = loop(generations = List(gen(1, "running", attempts = List(attempt(1)))),
      stage = stage(1, 2, "working"))
    assert(!overviewOf(l, 1).submitted)
    assert(overviewOf(l, 1, attempt = Some(1)).submitted)

  test("the window head carries the attempt count and the headline figure"):
    val l = loop(generations = List(
      gen(1, "accepted", metrics = List("p99_ms" -> 41.0)),
      gen(2, "accepted", Some(1), metrics = List("p99_ms" -> 33.0), attempts = List(attempt(1), attempt(2)))))
    assertEquals(openGen(l, 2).stats, Vector("attempts" -> "2", "p99_ms" -> "33"))

  test("a generation's title and state come from the ledger's own words"):
    val l = loop(generations = List(gen(1, "abandoned"), gen(2, "accepted", Some(1)), gen(3, "running", Some(2))))
    assertEquals(openGen(l, 2).title, "Generation 2")
    assertEquals((openGen(l, 1).state, openGen(l, 1).stateKind), ("abandoned", StatusKind.Interrupted))
    assertEquals(openGen(l, 2).stateKind, StatusKind.Done)
    assertEquals(openGen(l, 3).stateKind, StatusKind.Running)

  // -- the transcript panes ----------------------------------------------------

  private def paneOf(s: AppState, l: LoopWire, gen: Int, pane: GenPane): TranscriptPane =
    genViewOf(s, l).body match
      case GenBody.Transcript(p) => p
      case other                 => fail(s"expected a Transcript body, got $other")

  test("each pane addresses its own generation agent's label"):
    val l = loop(generations = List(gen(3, "running")))
    val s = stateWith(l, Focus.Generation(3, GenPane.Worker, None))
    assertEquals(paneOf(s, l, 3, GenPane.Worker).label, "gen-3-worker")
    val e = stateWith(l, Focus.Generation(3, GenPane.Evaluator, None))
    assertEquals(paneOf(e, l, 3, GenPane.Evaluator).label, "gen-3-eval")

  test("the label the loop is streaming into is Live, and streams"):
    val l = loop(generations = List(gen(3, "running")), liveLabel = Some("gen-3-worker"))
    val s = stateWith(l, Focus.Generation(3, GenPane.Worker, None))
      .reduce(WireMessage.Activity(TranscriptEvent.Said(l.id, "gen-3-worker", "hello")))
    val pane = paneOf(s, l, 3, GenPane.Worker)
    assertEquals(pane.status, PaneStatus.Live)
    assert(pane.streaming)
    assertEquals(pane.rows, Vector(TranscriptRow.Prose("hello")))

  test("a settled label with nothing in hand asks (Loading) and does not claim to stream"):
    val l = loop(generations = List(gen(1, "accepted")), parked = Some("x"))
    val pane = paneOf(stateWith(l, Focus.Generation(1, GenPane.Worker, None)), l, 1, GenPane.Worker)
    assertEquals(pane.status, PaneStatus.Loading)
    assert(!pane.streaming)

  test("a transcript already in hand needs no round trip"):
    val l = loop(generations = List(gen(1, "accepted")), parked = Some("x"))
    val s = stateWith(l, Focus.Generation(1, GenPane.Worker, None))
      .reduce(WireMessage.Activity(TranscriptEvent.Said(l.id, "gen-1-worker", "watched it live")))
    assertEquals(paneOf(s, l, 1, GenPane.Worker).status, PaneStatus.Settled)

  test("a transcript the host cannot serve says so instead of asking again"):
    val l = loop(generations = List(gen(1, "accepted")), parked = Some("x"))
    val s = stateWith(l, Focus.Generation(1, GenPane.Worker, None))
      .transcriptFailed(l.id, "gen-1-worker", "no transcript was recorded")
    assertEquals(paneOf(s, l, 1, GenPane.Worker).status, PaneStatus.Missing("no transcript was recorded"))

  test("a live label on a loop nobody is driving is not live"):
    val l = loop(generations = List(gen(1, "running")), orphaned = true, liveLabel = Some("gen-1-worker"))
    assert(!paneOf(stateWith(l, Focus.Generation(1, GenPane.Worker, None)), l, 1, GenPane.Worker).streaming)

  // -- the sentence is printed, never read -------------------------------------

  test("the activity line drives nothing: a stage with no sentence still pulses"):
    val l = loop(generations = List(gen(1, "running")), stage = stage(1, 1, "checking")).copy(activity = None)
    assertEquals(phaseOf(l, 1).stations.map(_.kind), Vector(PhaseKind.Idle, PhaseKind.Live, PhaseKind.Idle))
    assertEquals(LoopView.boardOf(l, None).activity, "")

  test("a sentence with no stage behind it pulses nothing, whatever it says"):
    val l = loop(generations = List(gen(1, "running")))
      .copy(activity = Some("gen 1, attempt 1 — working"))
    assert(phaseOf(l, 1).stations.forall(_.kind != PhaseKind.Live))
    assertEquals(LoopView.boardOf(l, None).activity, "gen 1, attempt 1 — working")

  test("a step this page has no station for costs a pulse, not a window"):
    val l = loop(generations = List(gen(1, "running")), stage = stage(1, 1, "dancing"))
    assert(phaseOf(l, 1).stations.forall(_.kind != PhaseKind.Live))
    // The attempt it names is still good: the pill for it is there either way.
    assertEquals(openGen(l, 1).attempts.map(_.attempt), Vector(1))

  // -- prose handling ----------------------------------------------------------

  /** Only the wrapping: spacing somebody typed inside a line is theirs to keep. */
  test("flow undoes hard wrapping and nothing else"):
    assertEquals(LoopView.flow("one\ntwo   three\n  four "), "one two   three four")
    assertEquals(LoopView.flow("already one line"), "already one line")
    assertEquals(LoopView.flow(""), "")

  test("splitGoal drops blank paragraphs and keeps the order of the rest"):
    assertEquals(LoopView.splitGoal("\n\nhead\n\n\nbody one\n\nbody two\n"), ("head", "body one\n\nbody two"))
    assertEquals(LoopView.splitGoal(""), ("", ""))
