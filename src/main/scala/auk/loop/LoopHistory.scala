package auk.loop

import auk.llm.tools.Json

/** One attempt of one generation, with everything the ledger recorded about it.
  *
  * The counterpart of [[Submission]], which keeps only the latest attempt because
  * that is all a retry prompt needs. Here every attempt survives, each carrying the
  * two judgements passed on it: `check` is the mechanical gate and `verdict` the
  * evaluator's, and both are `None` until they have run — a verdict permanently so
  * for an attempt the checker refused, since the evaluator never saw it.
  */
final case class AttemptHistory(
    attempt: Int,
    artifact: Json,
    description: String,
    knowledge: Option[String],
    /** What was snapshotted for this attempt — raw commit SHAs, which is what a
      * per-attempt diff is computed from. */
    snapshotCommits: List[String],
    check: Option[CheckOutcome],
    verdict: Option[VerdictOutcome],
    at: String
)

/** How a generation settled, or that it has not.
  *
  * [[Abandoned]] deliberately does not carry the attempt count
  * [[LoopEvent.GenerationAbandoned]] claims: the attempts the ledger actually holds
  * submissions for are right there in [[GenerationHistory.attempts]], and that is
  * the truer number — the same judgement [[AbandonedDigest]] makes.
  */
enum GenerationOutcome:
  case Running
  case Accepted(snapshotId: String, commit: String, description: String, metrics: Map[String, Double], at: String)
  case Abandoned(rescueSnapshotId: Option[String], at: String)

/** One generation of a loop's lineage, whole: where it branched from, who worked
  * it, everything it tried, and how it ended. */
final case class GenerationHistory(
    gen: Int,
    /** The accepted generation this one builds on; `None` for one starting from the
      * loop's baseline. */
    parent: Option[Int],
    /** The worker's session — which names the directory holding this generation's
      * transcript tees, and is the only way to find them. */
    sessionId: String,
    startedAt: String,
    /** The commit this generation's tree stood on when it started — the loop's anchor
      * at that moment, frozen here because the anchor moves on. Usually the parent's
      * accepted commit, but a tree that was edited from outside the loop and adopted
      * ([[LoopEvent.ExternalEditsAdopted]]) anchors the generations after it at the
      * adopted commit, which belongs to no generation at all. Diffing against the
      * parent there would show the loop the human's edits as this generation's work. */
    base: String,
    attempts: Vector[AttemptHistory],
    outcome: GenerationOutcome
):
  def accepted: Boolean = outcome.isInstanceOf[GenerationOutcome.Accepted]

  def settledAt: Option[String] = outcome match
    case GenerationOutcome.Running            => None
    case a: GenerationOutcome.Accepted        => Some(a.at)
    case a: GenerationOutcome.Abandoned       => Some(a.at)

  /** The commit this generation's accepted work is stored at. */
  def commit: Option[String] = outcome match
    case a: GenerationOutcome.Accepted => Some(a.commit)
    case _                             => None

  def rescueSnapshotId: Option[String] = outcome match
    case a: GenerationOutcome.Abandoned => a.rescueSnapshotId
    case _                              => None

  /** What this generation says it did: the acceptance's own account, falling back to
    * the last attempt's, so a generation that failed still explains what it tried. */
  def description: String = outcome match
    case a: GenerationOutcome.Accepted => a.description
    case _                             => attempts.lastOption.map(_.description).getOrElse("")

  /** What was measured, in key order so a metric strip does not reshuffle between
    * frames. Only an acceptance has any: nothing else was taken into the lineage. */
  def metrics: Vector[(String, Double)] = outcome match
    case a: GenerationOutcome.Accepted => a.metrics.toVector.sortBy(_._1)
    case _                             => Vector.empty

  def attemptAt(attempt: Int): Option[AttemptHistory] = attempts.find(_.attempt == attempt)

/** A loop's whole ledger, folded for a reader rather than for the driver.
  *
  * [[LoopState]] answers "what should happen next", and keeps only what that needs:
  * the accepted lineage, the generation in flight, and a digest of the rest. A
  * dashboard asks the opposite question — "what happened" — and needs the parts the
  * engine can afford to forget: every attempt of every generation, what each of the
  * two gates said about it, which session held it (that names its transcript files),
  * and which commits it snapshotted (those make a diff possible). So this is a
  * second fold over the same events rather than an extension of the first: the
  * engine's fold stays lean, and neither can distort the other.
  *
  * The configuration fields are effective values, exactly as in [[LoopState]]: a
  * [[LoopEvent.DefAttached]] sets them wholesale and each later
  * [[LoopEvent.ConfigAmended]] overlays the fields it names.
  */
final case class LoopHistory(
    loopId: String,
    /** The tree the loop measures itself against, and where its anchor starts. */
    baselineCommit: String,
    createdAt: String,
    defVersion: Int,
    defSource: String,
    goal: String,
    rubric: String,
    budgets: Budgets,
    /** Every generation the loop has started, in order. */
    generations: Vector[GenerationHistory],
    /** Where the tree stands now: the commit the next generation would start from.
      * Moved by an acceptance and by an adoption of external edits, and by nothing
      * else — the same anchor [[LoopState.anchorCommit]] drives from, folded again
      * here so each generation can keep the one it actually started on. */
    anchorCommit: String,
    status: LoopStatus
):
  def generation(gen: Int): Option[GenerationHistory] = generations.find(_.gen == gen)

  /** The commit a generation's work should be read against: the anchor it started
    * from, which is its parent's accepted commit in the ordinary case and the loop's
    * baseline before anything has been accepted. A generation this ledger never
    * started reads against the baseline — a diff against it is still a true diff,
    * where no diff at all is nothing. */
  def baseFor(gen: Int): String =
    generation(gen).map(_.base).getOrElse(baselineCommit)

  def parkedReason: Option[ParkReason] = status match
    case LoopStatus.Parked(reason) => Some(reason)
    case LoopStatus.Running        => None

object LoopHistory:
  /** Fold a ledger into everything it says, or say why it cannot be a ledger.
    *
    * Pure and total, like [[LoopState.fold]], and deliberately more forgiving than
    * it. A reader is not a driver: it never acts on what it folds, so the ordering
    * rules that protect the drive cycle protect nothing here, and enforcing them
    * would only trade a loop drawn imperfectly for a loop not drawn at all. Version
    * numbers going backwards, attempts arriving out of order, work recorded after a
    * park — all absorbed, last word winning.
    *
    * `Left` is kept for the two shapes that leave nothing coherent to show: a ledger
    * that does not open with exactly one [[LoopEvent.LoopCreated]] (there is no loop
    * to describe, or two of them), and an event about a generation that was never
    * started (there is nowhere to put it). Every ledger [[LoopState.fold]] accepts is
    * therefore accepted here too, which is the property that matters: the dashboard
    * can draw whatever the engine could run.
    *
    * A check or verdict naming an attempt with no submission is dropped rather than
    * being an error or a placeholder — without the submission there is no artifact
    * and no description to attach the judgement to, and inventing one would put a
    * claim on screen the ledger never made.
    */
  def fold(events: Vector[LoopEvent]): Either[String, LoopHistory] =
    events.headOption match
      case None =>
        Left("empty ledger: a loop ledger must begin with a loop_created event")
      case Some(created: LoopEvent.LoopCreated) =>
        events.iterator.zipWithIndex.drop(1).foldLeft[Either[String, LoopHistory]](Right(initial(created))):
          case (acc, (event, index)) =>
            acc.flatMap(history => step(history, event).left.map(msg => s"event ${index + 1} (${event.kind}): $msg"))
      case Some(other) =>
        Left(s"event 1 (${other.kind}): a loop ledger must begin with a loop_created event")

  private def initial(e: LoopEvent.LoopCreated): LoopHistory =
    LoopHistory(
      loopId = e.loopId,
      baselineCommit = e.baselineCommit,
      createdAt = e.at,
      defVersion = 0,
      defSource = "",
      goal = "",
      rubric = "",
      budgets = Budgets(),
      generations = Vector.empty,
      anchorCommit = e.baselineCommit,
      status = LoopStatus.Running
    )

  /** Apply one event. Messages are unprefixed; [[fold]] adds the position. */
  private def step(history: LoopHistory, event: LoopEvent): Either[String, LoopHistory] =
    import LoopEvent.*
    event match
      case _: LoopCreated =>
        Left("a ledger holds exactly one loop_created event, at its head")

      case DefAttached(version, source, goal, rubric, budgets, _, _) =>
        Right(history.copy(
          defVersion = version,
          defSource = source,
          goal = goal,
          rubric = rubric,
          budgets = budgets
        ))

      case ConfigAmended(goal, rubric, budgets, _) =>
        Right(history.copy(
          goal = goal.getOrElse(history.goal),
          rubric = rubric.getOrElse(history.rubric),
          budgets = budgets.getOrElse(history.budgets)
        ))

      case GenerationStarted(gen, parent, sessionId, at) =>
        // A generation number seen twice keeps its first start: that one names the
        // session whose directory the transcripts of the work are actually in.
        if history.generation(gen).isDefined then Right(history)
        else
          val started =
            GenerationHistory(gen, parent, sessionId, at, history.anchorCommit, Vector.empty, GenerationOutcome.Running)
          Right(history.copy(generations = history.generations :+ started))

      case AttemptSubmitted(gen, attempt, artifact, description, knowledge, snapshotCommits, at) =>
        val record = AttemptHistory(attempt, artifact, description, knowledge, snapshotCommits, None, None, at)
        update(history, gen, s"attempt $attempt"): g =>
          g.copy(attempts = g.attempts.filterNot(_.attempt == attempt) :+ record)

      case CheckCompleted(gen, attempt, pass, reasons, metrics, at) =>
        val outcome = CheckOutcome(attempt, pass, reasons, metrics, at)
        update(history, gen, s"check of attempt $attempt"): g =>
          g.copy(attempts = g.attempts.map(a => if a.attempt == attempt then a.copy(check = Some(outcome)) else a))

      case VerdictIssued(gen, attempt, accepted, feedback, goalReached, at) =>
        val outcome = VerdictOutcome(attempt, accepted, feedback, goalReached, at)
        update(history, gen, s"verdict on attempt $attempt"): g =>
          g.copy(attempts = g.attempts.map(a => if a.attempt == attempt then a.copy(verdict = Some(outcome)) else a))

      // An acceptance is also where the tree moves to: what this generation left behind
      // is what the next one starts from.
      case GenerationAccepted(gen, snapshotId, commit, description, metrics, at) =>
        update(history, gen, "acceptance")(
          _.copy(outcome = GenerationOutcome.Accepted(snapshotId, commit, description, metrics, at))
        ).map(_.copy(anchorCommit = commit))

      case GenerationAbandoned(gen, _, rescueSnapshotId, at) =>
        update(history, gen, "abandonment"): g =>
          g.copy(outcome = GenerationOutcome.Abandoned(rescueSnapshotId, at))

      // The lineage is untouched — an adoption accepts nothing — but every generation
      // started after it stands on the adopted tree, so the anchor moves and theirs is
      // frozen from it. A reader never refuses one: an adoption naming no generation
      // has nothing to be out of order with.
      case ExternalEditsAdopted(_, commit, _, _) =>
        Right(history.copy(anchorCommit = commit))

      case Parked(reason, _) =>
        Right(history.copy(status = LoopStatus.Parked(reason)))

      case Resumed(_, _) =>
        Right(history.copy(status = LoopStatus.Running))

  /** Rewrite the generation `gen` names, or refuse: an event about a generation
    * nobody started has nowhere to go, and silently dropping it would hide work the
    * ledger says happened. */
  private def update(history: LoopHistory, gen: Int, what: String)(
      f: GenerationHistory => GenerationHistory
  ): Either[String, LoopHistory] =
    if !history.generations.exists(_.gen == gen) then
      Left(s"$what names generation $gen, which was never started")
    else Right(history.copy(generations = history.generations.map(g => if g.gen == gen then f(g) else g)))
