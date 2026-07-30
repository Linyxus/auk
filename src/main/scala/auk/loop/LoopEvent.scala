package auk.loop

import auk.llm.tools.Json

/** How much work a loop may spend before it has to stop and ask.
  *
  * `patience` is the one that is easy to misread: it is the number of
  * *consecutive abandoned generations* tolerated before the loop parks. A
  * generation that is accepted resets the streak to zero, so a loop that keeps
  * making progress never exhausts its patience however many failures it has seen
  * in total. `maxGenerations` counts generations *started*, not accepted, and
  * `maxAttemptsPerGeneration` caps the retries inside one generation.
  *
  * Nothing here is enforced by this layer: [[LoopState]] reports whether a budget
  * is spent (see [[LoopState.budgetExhausted]] and friends) and the driver decides
  * what to do about it, which it records as a [[LoopEvent.Parked]].
  */
final case class Budgets(maxGenerations: Int = 50, patience: Int = 2, maxAttemptsPerGeneration: Int = 3)

/** Why a loop stopped running. The first four are ordinary outcomes — the goal
  * was met, a budget ran out, or the user said stop — and the last two carry a
  * detail string because they describe something that went wrong: an API failure
  * the retry schedule could not absorb, or a state the driver could not make
  * sense of. A parked loop is resumable; see [[LoopEvent.Resumed]].
  */
enum ParkReason:
  case GoalReached, BudgetExhausted, PatienceExhausted, UserRequested
  case ApiFailure(detail: String)
  case Anomaly(detail: String)

/** A single durable step in a refinement loop's life.
  *
  * The ledger of these events is the loop's source of truth: everything the
  * driver knows about a loop — its goal, its budgets, which generations were
  * accepted, what the checker said about the attempt that is in flight right now
  * — is a fold over them ([[LoopState.fold]]). The events are appended to
  * `.auk/loops/<id>/ledger.jsonl` one per line and never rewritten, so a loop
  * survives the session that started it and can be picked up by another.
  *
  * Two conventions run through the whole vocabulary:
  *
  *   - `at` is an ISO-8601 timestamp stamped by the *caller*. This layer never
  *     reads a clock — it is pure, so a fold over the same ledger always gives the
  *     same state, and a test can write whatever timestamps it likes.
  *   - `artifact` and `artifactSchema` are opaque [[Json]] here. What shape a
  *     generation's result takes is the loop definition's business; the ledger
  *     carries it through untouched and compares schemas structurally (see
  *     [[LoopStore.schemasMatch]]) when checking for drift.
  */
enum LoopEvent:
  /** The loop exists. `baselineCommit` is the tree the loop measures itself
    * against and `headAtCreation` is where the branch stood when it was made —
    * kept apart because a loop may be started from a dirty tree, in which case the
    * baseline is a snapshot commit that HEAD knows nothing about. `sessionId` is
    * the session that created the loop; [[Resumed]] records later owners. */
  case LoopCreated(loopId: String, baselineCommit: String, headAtCreation: String, sessionId: String, at: String)

  /** A complete configuration snapshot, versioned. Version 1 is attached at
    * creation; a higher version lands whenever an amendment *replaces the checker*,
    * since new code means a new definition rather than an overlay on the old one.
    *
    * `source` is the definition's Scala source, which arrives post-hoc from the
    * eval capture rather than being known when the loop is created — it is recorded
    * here so the ledger explains, on its own, what was being checked at the time.
    * Attaching a version resets whatever [[ConfigAmended]] overlays had accumulated:
    * the snapshot is authoritative from that point on. Versions must increase. */
  case DefAttached(version: Int, source: String, goal: String, rubric: String, budgets: Budgets, artifactSchema: Json, at: String)

  /** A data-only amendment between definition versions: retune the goal, the
    * rubric, or the budgets without touching the checker. `None` leaves a field as
    * it stands, and `budgets` is replaced whole rather than field by field. */
  case ConfigAmended(goal: Option[String], rubric: Option[String], budgets: Option[Budgets], at: String)

  /** Work began on generation `gen`. `parent` names the accepted generation this
    * one builds on (`None` for the first, or for one starting from the baseline);
    * a generation that is abandoned can be followed by a fresh one from the same
    * parent. `sessionId` is the worker's session, not the driver's. */
  case GenerationStarted(gen: Int, parent: Option[Int], sessionId: String, at: String)

  /** The worker offered a result for `gen`. Attempts are numbered from 1 within a
    * generation and a generation may hold several, each a retry after the previous
    * one was rejected by the checker or the evaluator. `snapshotCommits` records
    * what was captured for this attempt; `knowledge` is whatever the worker learned
    * and wants carried forward. */
  case AttemptSubmitted(gen: Int, attempt: Int, artifact: Json, description: String, knowledge: Option[String], snapshotCommits: List[String], at: String)

  /** The Scala checker ran over an attempt. This is the mechanical gate — it either
    * passes or it comes back with `reasons`, which the driver turns into a retry
    * prompt. `metrics` are whatever the checker measured, passed through. */
  case CheckCompleted(gen: Int, attempt: Int, pass: Boolean, reasons: List[String], metrics: Map[String, Double], at: String)

  /** The evaluator agent judged an attempt. The judgement gate after the checker's
    * mechanical one: `feedback` is prose the worker can act on, and `goalReached`
    * is the evaluator's separate claim that the loop as a whole is done — an
    * attempt can be accepted without the goal being met. */
  case VerdictIssued(gen: Int, attempt: Int, accepted: Boolean, feedback: String, goalReached: Boolean, at: String)

  /** A generation settled as part of the lineage: its work is snapshotted at
    * `commit` and it becomes the parent the next generation builds on. Ends the
    * generation and resets the abandoned streak. */
  case GenerationAccepted(gen: Int, snapshotId: String, commit: String, description: String, metrics: Map[String, Double], at: String)

  /** A generation settled without producing anything worth keeping, after
    * `attempts` tries. `rescueSnapshotId` points at whatever was captured before
    * the tree was rolled back, so discarded work is still recoverable by hand.
    * Ends the generation and lengthens the abandoned streak. */
  case GenerationAbandoned(gen: Int, attempts: Int, rescueSnapshotId: Option[String], at: String)

  /** The loop stopped. Not a terminal state: a parked loop keeps its whole ledger
    * and can be picked up again by [[Resumed]]. Parking does not end an in-flight
    * generation — a driver that wants the generation dropped abandons it first. */
  case Parked(reason: ParkReason, at: String)

  /** A session took ownership of a parked loop and set it running again. */
  case Resumed(sessionId: String, at: String)

  /** The stable wire tag for this case, as written to the ledger by
    * [[LoopCodec]] and read back by it. It doubles as the event's name in
    * [[LoopState.fold]] error messages, so a complaint about a ledger names the
    * line's `type` field exactly as it appears on disk. */
  def kind: String = this match
    case _: LoopCreated         => "loop_created"
    case _: DefAttached         => "def_attached"
    case _: ConfigAmended       => "config_amended"
    case _: GenerationStarted   => "generation_started"
    case _: AttemptSubmitted    => "attempt_submitted"
    case _: CheckCompleted      => "check_completed"
    case _: VerdictIssued       => "verdict_issued"
    case _: GenerationAccepted  => "generation_accepted"
    case _: GenerationAbandoned => "generation_abandoned"
    case _: Parked              => "parked"
    case _: Resumed             => "resumed"
