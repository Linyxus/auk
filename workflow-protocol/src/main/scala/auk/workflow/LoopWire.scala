package auk.workflow

/** A refinement loop as the browser dashboard receives it.
  *
  * Where a workflow reaches the browser as a stream of deltas folded into a
  * [[Forest]], a loop arrives whole: the host holds the ledger, folds it, and sends
  * the result. A loop changes a handful of times per generation — a generation
  * starts, an attempt is submitted, a checker reports — so there is nothing for a
  * delta protocol to save, and a snapshot per change can never leave the browser
  * out of step with the disk.
  *
  * What the host does NOT send twice is the transcripts. A generation's worker and
  * evaluator stream as ordinary [[WireMessage.Activity]] frames keyed
  * `runId = loopId`, `nodeId = <label>` — the same shape a workflow node's
  * transcript has, and the same shape the tee files on disk hold — so the browser
  * folds all of them with one [[Transcript]] and the wire needs no loop-specific
  * transcript message.
  *
  * Every timestamp here is the ledger's own ISO-8601 string, passed through
  * untouched: the host never reinterprets a clock it did not read.
  */
final case class LoopBudgetsWire(maxGenerations: Int, patience: Int, maxAttemptsPerGeneration: Int)

/** What the mechanical checker said about one attempt. `metrics` are in key order,
  * as pairs rather than as a map, because a JSON object with numeric-looking keys
  * is silently reordered by every JS engine and the order is what keeps a metric
  * strip from reshuffling between frames. */
final case class LoopCheckWire(pass: Boolean, reasons: List[String], metrics: List[(String, Double)])

/** What the evaluator agent said about one attempt. `goalReached` is its separate
  * claim that the loop as a whole is finished — an attempt can be accepted without
  * it. */
final case class LoopVerdictWire(accepted: Boolean, feedback: String, goalReached: Boolean)

/** One attempt inside a generation: what the worker offered, and how the two gates
  * received it.
  *
  * `check` and `verdict` are `None` while the attempt is still being judged, and
  * the verdict stays `None` for an attempt the checker refused — the evaluator
  * never sees it. `hasSnapshot` says a diff can be asked for (the attempt recorded
  * a snapshot commit); the diff itself is fetched on demand from
  * `/api/loop/<id>/diff/<gen>/<attempt>` rather than pushed, since a patch is
  * larger than everything else on this wire put together.
  */
final case class LoopAttemptWire(
    attempt: Int,
    description: String,
    /** The submitted artifact as JSON text. */
    artifact: String,
    hasSnapshot: Boolean,
    check: Option[LoopCheckWire],
    verdict: Option[LoopVerdictWire],
    at: String
)

/** One generation of a loop's lineage, whole.
  *
  * `state` is `accepted`, `abandoned` or `running`, and at most one generation of a
  * loop is running. `description` prefers the acceptance's own account and falls
  * back to the last attempt's, so an abandoned generation still says what it tried;
  * `metrics` and `commit` belong to the acceptance alone, because only an accepted
  * generation was measured and stored.
  *
  * `parent` names the accepted generation this one builds on — `None` for one
  * starting from the loop's baseline — which is what lets a flat list be drawn as a
  * branching lineage: two generations naming the same parent are two attempts at
  * the same step.
  */
final case class LoopGenerationWire(
    gen: Int,
    parent: Option[Int],
    state: String,
    description: String,
    metrics: List[(String, Double)],
    commit: Option[String],
    attempts: List[LoopAttemptWire],
    startedAt: String,
    settledAt: Option[String]
)

/** A whole loop: its definition, its lineage, and what it is doing right now.
  *
  * The two halves come from different places and neither can be derived from the
  * other. The definition and the lineage are a fold of the ledger on disk, which is
  * true of every loop the project holds, including ones this session never touched.
  * `phase`, `activity`, `liveLabel` and `held` are the driver's, and only the
  * session actually driving a loop knows them: a ledger records what HAS happened,
  * which leaves "the worker is thinking" and "the evaluator is reading the diff"
  * indistinguishable. A loop read off disk therefore carries a phase derived from
  * its parked/orphaned status, no activity, and `held = false`.
  *
  * `goal` is the FULL goal text, not the first line the TUI's strip shows: the
  * browser has room for the paragraph somebody wrote.
  */
final case class LoopWire(
    id: String,
    /** The host's phase vocabulary, verbatim: `validating`, `adopting`,
      * `running (gen 3)`, `parked: <reason>`, `orphaned (dead session)`. */
    phase: String,
    goal: String,
    rubric: String,
    budgets: LoopBudgetsWire,
    /** The latest attached definition's Scala source — the checker the lineage was
      * measured by. */
    defSource: String,
    defVersion: Int,
    /** Whether the session that sent this is the one driving the loop. */
    held: Boolean,
    /** Why the loop stopped, without the phase's `parked: ` prefix. */
    parked: Option[String],
    /** Recorded as running with nobody driving it — what a session that ended
      * mid-generation left behind. */
    orphaned: Boolean,
    /** What the loop is doing right now — `gen 3, attempt 2 — evaluating`. */
    activity: Option[String],
    /** Where the live agent's transcript is filed (`gen-3-worker`), i.e. the
      * `nodeId` its [[WireMessage.Activity]] frames carry. */
    liveLabel: Option[String],
    generations: List[LoopGenerationWire],
    createdAt: String
)
