package auk.runtime

import auk.agent.{LoopStage, LoopView}
import auk.loop.{AttemptHistory, GenerationHistory, GenerationOutcome, LoopHistory, LoopState, LoopStore}
import auk.workflow.{LoopAttemptWire, LoopBudgetsWire, LoopCheckWire, LoopGenerationWire, LoopStageWire, LoopVerdictWire, LoopWire}

/** How a loop becomes something a browser can draw.
  *
  * Two sources, because neither half can be derived from the other. The ledger says
  * what has HAPPENED — the definition, every generation, every attempt and what the
  * gates made of it — and it says that about every loop the project holds, including
  * ones this session has never touched. The [[LoopView]] the bridge computes says
  * what is happening RIGHT NOW: which of a generation's three agents is running,
  * which transcript it is streaming into, and whether this session is the one
  * driving. A ledger cannot know the second (it records the past) and the bridge does
  * not keep the first (it folds for the drive cycle, not for a reader), so the wire
  * is their merge.
  *
  * The view wins wherever they overlap, and there is one place they do: `phase`. A
  * loop being ADOPTED has a ledger that still says parked — the resume is only
  * written once its stored definition has re-checked — and reporting it as parked
  * would hide the one thing happening to it.
  *
  * Pure, so the whole projection tests without a bridge, a socket or a clock.
  */
object LoopWirer:

  def wire(history: LoopHistory, view: LoopView): LoopWire =
    // Tokens are the second place the two sources meet, and they meet the way the
    // panel's do: the ledger knows what every generation has already spent, and only
    // the driver knows about the run in flight, which it reports on the stage. The
    // live number is added to the generation the stage names HERE, once, so the page
    // never has to combine a settled total with a live one — and a loop off disk,
    // which has no stage, is simply the settled sum.
    val generations = history.generations
      .map(g => generation(g, view.stage.filter(_.gen == g.gen)))
      .toList
    LoopWire(
      id = view.id,
      phase = view.phase,
      // The full goal, not the view's first line: the browser has room for the
      // paragraph somebody wrote, and the TUI's one-line strip is why the view
      // clipped it.
      goal = history.goal,
      rubric = history.rubric,
      budgets = LoopBudgetsWire(
        history.budgets.maxGenerations,
        history.budgets.patience,
        history.budgets.maxAttemptsPerGeneration
      ),
      defSource = history.defSource,
      defVersion = history.defVersion,
      held = view.held,
      parked = view.parked,
      orphaned = view.orphaned,
      activity = view.activity,
      stage = view.stage.map(s => LoopStageWire(s.gen, s.attempt, s.step, s.inputTokens, s.outputTokens)),
      liveLabel = view.liveLabel,
      generations = generations,
      createdAt = history.createdAt,
      inputTokens = generations.map(_.inputTokens).sum,
      outputTokens = generations.map(_.outputTokens).sum
    )

  /** `view`'s loop, re-read and folded from its ledger. `None` when there is no
    * ledger to fold: a loop still being validated exists only in the session holding
    * it, and one whose ledger will not fold describes nothing worth drawing. */
  def fromStore(store: LoopStore, view: LoopView): Option[LoopWire] =
    store.readAll(view.id).flatMap(LoopHistory.fold).toOption.map(wire(_, view))

  /** A loop nobody here is driving, read entirely off disk.
    *
    * The phase and the lean lineage come from [[LoopBridge]]'s own derivation rather
    * than from a second opinion invented here — a foreign loop's ledger that says
    * "running" with no driver behind it is an orphan, and that judgement belongs in
    * one place. Such a view is never `held`: this session has no claim on it.
    */
  def fromDisk(store: LoopStore, loopId: String): Option[LoopWire] =
    for
      events  <- store.readAll(loopId).toOption
      history <- LoopHistory.fold(events).toOption
      state   <- LoopState.fold(events).toOption
    yield wire(history, LoopBridge.loopView(loopId, LoopBridge.diskPhase(state), None, state, held = false))

  /** One generation, plus the run in flight when this is the generation running it —
    * see [[wire]] for why the two are added together here rather than in the browser. */
  private def generation(g: GenerationHistory, live: Option[LoopStage]): LoopGenerationWire =
    LoopGenerationWire(
      gen = g.gen,
      parent = g.parent,
      state = stateName(g.outcome),
      description = g.description,
      metrics = g.metrics.toList,
      commit = g.commit,
      attempts = g.attempts.map(attempt).toList,
      startedAt = g.startedAt,
      settledAt = g.settledAt,
      inputTokens = g.inputTokens + live.map(_.inputTokens).getOrElse(0L),
      outputTokens = g.outputTokens + live.map(_.outputTokens).getOrElse(0L)
    )

  private def attempt(a: AttemptHistory): LoopAttemptWire =
    LoopAttemptWire(
      attempt = a.attempt,
      description = a.description,
      artifact = a.artifact.render,
      // Whether a diff can be asked for at all, so the browser knows before it asks.
      hasSnapshot = a.snapshotCommits.nonEmpty,
      check = a.check.map(c => LoopCheckWire(c.pass, c.reasons, c.metrics.toVector.sortBy(_._1).toList)),
      verdict = a.verdict.map(v => LoopVerdictWire(v.accepted, v.feedback, v.goalReached)),
      at = a.at
    )

  /** The wire's generation-state vocabulary. */
  private def stateName(outcome: GenerationOutcome): String = outcome match
    case GenerationOutcome.Running      => "running"
    case _: GenerationOutcome.Accepted  => "accepted"
    case _: GenerationOutcome.Abandoned => "abandoned"
