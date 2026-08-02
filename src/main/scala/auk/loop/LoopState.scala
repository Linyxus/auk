package auk.loop

import auk.llm.tools.Json

/** An accepted generation, as it stands in the lineage.
  *
  * `artifact` is carried over from the attempt that was accepted rather than from
  * the acceptance itself — the acceptance records how the work was *stored*
  * (`snapshotId`, `commit`) and how it is *described*, while the artifact is what
  * the worker actually produced.
  */
final case class GenerationRecord(
    gen: Int,
    parent: Option[Int],
    description: String,
    artifact: Json,
    snapshotId: String,
    commit: String,
    metrics: Map[String, Double],
    at: String
)

/** The latest attempt offered in the generation that is in flight. */
final case class Submission(
    attempt: Int,
    artifact: Json,
    description: String,
    knowledge: Option[String],
    snapshotCommits: List[String],
    at: String
)

/** What the checker said about an attempt. */
final case class CheckOutcome(attempt: Int, pass: Boolean, reasons: List[String], metrics: Map[String, Double], at: String)

/** What the evaluator said about an attempt. */
final case class VerdictOutcome(attempt: Int, accepted: Boolean, feedback: String, goalReached: Boolean, at: String)

/** The generation currently being worked on, and everything the driver needs to
  * decide what to say to the worker next.
  *
  * Only the *latest* of each kind is kept. A generation's history is already in
  * the ledger; what a retry prompt needs is the most recent rejection, which is
  * what [[latestRejection]] picks out.
  */
final case class InFlight(
    gen: Int,
    parent: Option[Int],
    sessionId: String,
    startedAt: String,
    /** How many attempts have been submitted so far — `0` between the start of a
      * generation and its first submission. */
    attempts: Int,
    lastSubmission: Option[Submission],
    lastCheck: Option[CheckOutcome],
    lastVerdict: Option[VerdictOutcome]
):
  /** The newest rejection in this generation, as text for a retry prompt.
    *
    * Within one attempt the checker runs before the evaluator, so when both have
    * rejected, the one belonging to the later attempt is the newer word — and on a
    * tie the verdict is, since it came second. `None` when nothing has been
    * rejected, which is also the case for a generation whose only attempt is still
    * being judged.
    */
  def latestRejection: Option[String] =
    val verdict = lastVerdict.filterNot(_.accepted)
    val check = lastCheck.filterNot(_.pass)
    (verdict, check) match
      case (Some(v), Some(c)) => Some(if v.attempt >= c.attempt then v.feedback else c.reasons.mkString("; "))
      case (Some(v), None)    => Some(v.feedback)
      case (None, Some(c))    => Some(c.reasons.mkString("; "))
      case (None, None)       => None

/** What an abandoned generation is remembered by, once it is gone.
  *
  * [[LoopEvent.GenerationAbandoned]] records that a generation failed but not what it
  * tried or why it was refused, and its schema is frozen — old ledgers must stay
  * readable. So the "why" is reconstructed here instead, at fold time, from the
  * [[InFlight]] the abandonment ended: `description` is the last attempt's own account
  * of itself and `rejection` the newest word against it ([[InFlight.latestRejection]],
  * which prefers the evaluator's prose over the checker's reasons when both belong to
  * the final attempt).
  *
  * Both are `None` for a generation that died with its session before submitting
  * anything — the rescue case — and that is an honest report rather than a gap:
  * `attempts` is 0 to say so. `attempts` counts the attempts the ledger actually holds
  * submissions for, not the number the abandoning driver claimed.
  *
  * This is what makes a failure survive its generation. An abandoned generation's
  * worker is gone and its tree is rolled back, so without this the next worker starts
  * with no idea that the road it is about to take is one somebody already walked down.
  */
final case class AbandonedDigest(
    gen: Int,
    attempts: Int,
    description: Option[String],
    rejection: Option[String],
    at: String
)

/** Whether a loop is running or stopped, and why it stopped. */
enum LoopStatus:
  case Running
  case Parked(reason: ParkReason)

/** Everything a ledger says about a loop, folded into one value.
  *
  * The configuration fields (`goal`, `rubric`, `budgets`, `artifactSchema`) are
  * *effective* values, not raw ones: a [[LoopEvent.DefAttached]] sets them wholesale
  * and each later [[LoopEvent.ConfigAmended]] overlays the fields it names, so
  * attaching a new definition version naturally resets the overlay base. Before any
  * definition is attached `defVersion` is 0 and they hold their defaults.
  *
  * `generations` is the accepted lineage in acceptance order, and `abandoned` is its
  * negative counterpart: one [[AbandonedDigest]] per generation that failed, in
  * abandonment order. Nothing ever appears in both — an accepted generation is in the
  * lineage and a failed one is only ever a digest. Each accepted record carries its
  * `parent`, so a lineage that branches (a fresh generation from the same parent after
  * an abandonment) is still readable, even though the vector itself is flat.
  */
final case class LoopState(
    loopId: String,
    baselineCommit: String,
    headAtCreation: String,
    /** The session that most recently owned the loop — created or resumed it. Not
      * the worker's session, which lives on [[InFlight.sessionId]]. */
    sessionId: String,
    createdAt: String,
    /** 0 until the first definition is attached, then the latest attached version. */
    defVersion: Int,
    defSource: String,
    goal: String,
    rubric: String,
    budgets: Budgets,
    artifactSchema: Json,
    generations: Vector[GenerationRecord],
    /** Every generation that was abandoned, in abandonment order — what the loop has
      * already tried and failed at, kept so a later generation is not sent down the
      * same road blind. */
    abandoned: Vector[AbandonedDigest],
    /** The number the next generation must use. Both acceptance and abandonment
      * consume a number, so this is one past the highest generation *started*. */
    nextGen: Int,
    /** Abandoned generations since the last accepted one. */
    consecutiveAbandoned: Int,
    /** The commit the working tree stands on between generations: what the next
      * generation starts from, what its diff is measured against, and what an
      * abandonment rolls back to.
      *
      * Usually the newest accepted generation's commit, and the baseline before
      * anything has been accepted — but not always, which is the whole reason it is a
      * field rather than a derivation. A tree that was edited from outside the loop is
      * adopted rather than absorbed or destroyed ([[LoopEvent.ExternalEditsAdopted]]),
      * and the adopted commit is on no branch of the lineage. So the anchor is the
      * loop's answer to "where is the tree", while [[latestAccepted]] stays the answer
      * to "what has this loop achieved"; the two agree until somebody else touches the
      * tree. */
    anchorCommit: String,
    inFlight: Option[InFlight],
    status: LoopStatus
):
  /** The newest accepted generation: the tip of the lineage, and the parent a new
    * generation names. NOT necessarily the tree it starts from — that is
    * [[anchorCommit]], which an adoption moves without accepting anything. */
  def latestAccepted: Option[GenerationRecord] = generations.lastOption

  /** Whether the tree a new generation starts from is one this loop's own lineage
    * produced. False once external edits have been adopted with nothing accepted
    * since — the anchor is then a commit no generation of this loop ever made, which
    * is worth telling the next worker. */
  def anchoredOnLineage: Boolean = anchorCommit == latestAccepted.map(_.commit).getOrElse(baselineCommit)

  /** Generations started, accepted and abandoned alike. */
  def generationsStarted: Int = nextGen - 1

  /** Whether a definition has been attached yet. */
  def defAttached: Boolean = defVersion > 0

  /** Whether the loop has started as many generations as it is allowed to. */
  def budgetExhausted: Boolean = generationsStarted >= budgets.maxGenerations

  /** Whether the abandoned streak has reached the patience budget. */
  def patienceExhausted: Boolean = consecutiveAbandoned >= budgets.patience

  /** Whether the in-flight generation has used up its retries. False when nothing
    * is in flight. */
  def attemptsExhausted: Boolean = inFlight.exists(_.attempts >= budgets.maxAttemptsPerGeneration)

  /** Whether the loop is stopped, and why. */
  def parkedReason: Option[ParkReason] = status match
    case LoopStatus.Parked(reason) => Some(reason)
    case LoopStatus.Running        => None

object LoopState:
  /** Fold a ledger into the state it describes, or say why it cannot be one.
    *
    * Total, pure and deterministic: no clock, no filesystem, and the same events
    * always give the same state. `Left` is reserved for ledgers that could not have
    * been produced by a correct driver — a missing or duplicated
    * [[LoopEvent.LoopCreated]], generation or attempt numbers out of order, an event
    * about a generation that is not in flight, an acceptance with nothing submitted,
    * a parent that was never accepted, a definition version that went backwards, new
    * work started while the loop is parked, or external edits adopted anywhere but at
    * a generation boundary of a running loop. Its message names the offending event by
    * position and wire tag.
    *
    * Deliberately *not* errors, because they are the driver's policy rather than the
    * ledger's structure: a verdict without a preceding check, an attempt count past
    * the budget, a park while a generation is in flight, and a generation settling
    * while the loop is parked (a park can land mid-generation, and that generation
    * is still allowed to finish — only *starting* new work needs a
    * [[LoopEvent.Resumed]] first).
    */
  def fold(events: Vector[LoopEvent]): Either[String, LoopState] =
    events.headOption match
      case None =>
        Left("empty ledger: a loop ledger must begin with a loop_created event")
      case Some(created: LoopEvent.LoopCreated) =>
        events.iterator.zipWithIndex.drop(1).foldLeft[Either[String, LoopState]](Right(initial(created))):
          case (acc, (event, index)) =>
            acc.flatMap(state => step(state, event).left.map(msg => s"event ${index + 1} (${event.kind}): $msg"))
      case Some(other) =>
        Left(s"event 1 (${other.kind}): a loop ledger must begin with a loop_created event")

  private def initial(e: LoopEvent.LoopCreated): LoopState =
    LoopState(
      loopId = e.loopId,
      baselineCommit = e.baselineCommit,
      headAtCreation = e.headAtCreation,
      sessionId = e.sessionId,
      createdAt = e.at,
      defVersion = 0,
      defSource = "",
      goal = "",
      rubric = "",
      budgets = Budgets(),
      artifactSchema = Json.Null,
      generations = Vector.empty,
      abandoned = Vector.empty,
      nextGen = 1,
      consecutiveAbandoned = 0,
      anchorCommit = e.baselineCommit,
      inFlight = None,
      status = LoopStatus.Running
    )

  /** Apply one event. Messages are unprefixed; [[fold]] adds the position. */
  private def step(state: LoopState, event: LoopEvent): Either[String, LoopState] =
    import LoopEvent.*
    event match
      case _: LoopCreated =>
        Left(s"a ledger holds exactly one loop_created event, at its head")

      case DefAttached(version, source, goal, rubric, budgets, artifactSchema, _) =>
        if version <= state.defVersion then
          Left(s"definition versions must increase but version $version follows version ${state.defVersion}")
        else
          Right(state.copy(
            defVersion = version,
            defSource = source,
            goal = goal,
            rubric = rubric,
            budgets = budgets,
            artifactSchema = artifactSchema
          ))

      case ConfigAmended(goal, rubric, budgets, _) =>
        Right(state.copy(
          goal = goal.getOrElse(state.goal),
          rubric = rubric.getOrElse(state.rubric),
          budgets = budgets.getOrElse(state.budgets)
        ))

      case GenerationStarted(gen, parent, sessionId, at) =>
        state.inFlight match
          case Some(flight) =>
            Left(s"generation $gen started while generation ${flight.gen} is still in flight")
          case None =>
            state.status match
              case LoopStatus.Parked(reason) =>
                Left(s"generation $gen started while the loop is parked ($reason); a resumed event must come first")
              case LoopStatus.Running =>
                if gen != state.nextGen then
                  Left(s"generations must run in order: expected generation ${state.nextGen} but saw $gen")
                else if parent.exists(p => !state.generations.exists(_.gen == p)) then
                  Left(s"generation $gen names parent ${parent.getOrElse(-1)}, which is not an accepted generation")
                else
                  val flight = InFlight(gen, parent, sessionId, at, 0, None, None, None)
                  Right(state.copy(inFlight = Some(flight), nextGen = gen + 1))

      case AttemptSubmitted(gen, attempt, artifact, description, knowledge, snapshotCommits, at) =>
        inFlightFor(state, gen, s"attempt $attempt").flatMap: flight =>
          if attempt != flight.attempts + 1 then
            Left(s"generation $gen: attempts must run in order: expected attempt ${flight.attempts + 1} but saw $attempt")
          else
            val submission = Submission(attempt, artifact, description, knowledge, snapshotCommits, at)
            Right(state.copy(inFlight = Some(flight.copy(attempts = attempt, lastSubmission = Some(submission)))))

      case CheckCompleted(gen, attempt, pass, reasons, metrics, at) =>
        for
          flight <- inFlightFor(state, gen, s"check of attempt $attempt")
          _      <- requireSubmitted(flight, gen, attempt, "checked")
        yield state.copy(inFlight = Some(flight.copy(lastCheck = Some(CheckOutcome(attempt, pass, reasons, metrics, at)))))

      case VerdictIssued(gen, attempt, accepted, feedback, goalReached, at) =>
        for
          flight <- inFlightFor(state, gen, s"verdict on attempt $attempt")
          _      <- requireSubmitted(flight, gen, attempt, "judged")
        yield state.copy(inFlight = Some(flight.copy(lastVerdict = Some(VerdictOutcome(attempt, accepted, feedback, goalReached, at)))))

      case GenerationAccepted(gen, snapshotId, commit, description, metrics, at) =>
        inFlightFor(state, gen, "acceptance").flatMap: flight =>
          flight.lastSubmission match
            case None =>
              Left(s"generation $gen was accepted without a submitted attempt")
            case Some(submission) =>
              val record = GenerationRecord(gen, flight.parent, description, submission.artifact, snapshotId, commit, metrics, at)
              Right(state.copy(
                generations = state.generations :+ record,
                consecutiveAbandoned = 0,
                anchorCommit = commit,
                inFlight = None
              ))

      case GenerationAbandoned(gen, _, _, at) =>
        inFlightFor(state, gen, "abandonment").map: flight =>
          state.copy(
            abandoned = state.abandoned :+ digestOf(flight, at),
            inFlight = None,
            consecutiveAbandoned = state.consecutiveAbandoned + 1
          )

      // Adoption is a boundary act of a RUNNING loop: the driver looked at a tree it
      // was about to hand to the next generation and found somebody else's work in it.
      // A generation in flight owns the tree, so nothing found there is foreign, and a
      // parked loop has no driver to be looking — either shape is a driver that has
      // lost track of where it is, not a tree that drifted.
      case ExternalEditsAdopted(_, commit, _, _) =>
        state.inFlight match
          case Some(flight) =>
            Left(s"external edits were adopted while generation ${flight.gen} is in flight")
          case None =>
            state.status match
              case LoopStatus.Parked(reason) =>
                Left(s"external edits were adopted while the loop is parked ($reason); a resumed event must come first")
              case LoopStatus.Running =>
                Right(state.copy(anchorCommit = commit))

      case Parked(reason, _) =>
        Right(state.copy(status = LoopStatus.Parked(reason)))

      case Resumed(sessionId, _) =>
        Right(state.copy(status = LoopStatus.Running, sessionId = sessionId))

  /** What a generation that is about to be abandoned leaves behind, read off the state
    * it had reached. Empty text is dropped rather than carried as an empty string: a
    * worker that submitted no account of itself is the same, to a later reader, as one
    * that submitted nothing at all. */
  private def digestOf(flight: InFlight, at: String): AbandonedDigest =
    AbandonedDigest(
      gen = flight.gen,
      attempts = flight.attempts,
      description = flight.lastSubmission.map(_.description).filter(_.trim.nonEmpty),
      rejection = flight.latestRejection.filter(_.trim.nonEmpty),
      at = at
    )

  /** The in-flight generation, provided it is the one `what` claims to be about. */
  private def inFlightFor(state: LoopState, gen: Int, what: String): Either[String, InFlight] =
    state.inFlight match
      case Some(flight) if flight.gen == gen => Right(flight)
      case Some(flight)                      => Left(s"$what names generation $gen but generation ${flight.gen} is in flight")
      case None                              => Left(s"$what names generation $gen, which is not in flight")

  /** An attempt can only be `checked`/`judged` once it has been submitted, and only
    * the latest submission is still open to either. */
  private def requireSubmitted(flight: InFlight, gen: Int, attempt: Int, verb: String): Either[String, Unit] =
    if flight.attempts == 0 then Left(s"generation $gen: attempt $attempt was $verb before it was submitted")
    else if attempt != flight.attempts then
      Left(s"generation $gen: attempt $attempt was $verb but attempt ${flight.attempts} is the latest submitted")
    else Right(())
