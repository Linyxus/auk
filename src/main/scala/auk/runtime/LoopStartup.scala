package auk.runtime

import auk.loop.{LoopState, LoopStore}

/** What a session finds waiting for it in `.auk/loops` when it opens, and how it says
  * so.
  *
  * A loop outlives the session that started it, which is only useful if the NEXT
  * session knows it is there. A lead that has to be told a loop exists before it can
  * think about it will never think about it, so a project with unfinished loops opens
  * by naming them in the system prompt, where they read as standing context rather
  * than as something that just happened ([[section]]).
  *
  * The USER's copy is almost all elsewhere: the bridge's own startup scan feeds the
  * `ctrl+c l` window, which shows the same loops live instead of a line that never goes
  * away, and the activity line in the live region counts only loops actually running —
  * so a project holding nothing but parked ones greets nobody with anything. The one
  * exception is [[orphanNote]]: a loop a dead session left RUNNING is not a considered
  * pause but an accident, and it earns one sentence in the transcript, once.
  *
  * Deliberately NOT an inbox item, in either direction. A system notice waiting in the
  * steering inbox fires a whole model turn before the user has typed anything, and
  * "you have a parked loop" is not worth a turn — it is worth a sentence in the prompt
  * for when it becomes relevant, and a sentence on screen when it was an accident.
  */
object LoopStartup:

  /** One loop this project holds, as much of it as a summary needs. */
  final case class Waiting(
      id: String,
      /** The phase vocabulary [[LoopBridge]] broadcasts: `parked: <reason>`, or
        * [[LoopBridge.Orphaned]] for one whose session ended while it ran. */
      phase: String,
      /** The newest accepted generation's number, if the loop has one. */
      latestGen: Option[Int],
      /** What the checker measured on that generation. */
      metrics: Map[String, Double],
      /** Generations started, accepted and abandoned alike. */
      started: Int,
      goal: String
  ):
    /** Whether this loop stopped because it was FINISHED rather than interrupted. A
      * loop that reached its goal is history; one that ran out of budget, or whose
      * session died, is work someone may want to pick up. */
    def settled: Boolean = phase == LoopBridge.phaseFor(auk.loop.ParkReason.GoalReached)

  /** Every loop in `store` whose ledger folds, newest lineage first is not worth the
    * trouble — they come in the store's own order, which is by id.
    *
    * A ledger that will not fold is left out rather than reported as broken: this runs
    * before anything has asked for a loop, and a startup that opens by complaining about
    * a file nobody mentioned is noise. The bridge says so properly if the loop is ever
    * named. */
  def scan(store: LoopStore): List[Waiting] =
    store.list().flatMap(id => store.state(id).toOption.map(state => waiting(id, state)))

  private def waiting(id: String, state: LoopState): Waiting =
    val phase = LoopBridge.diskPhase(state)
    Waiting(
      id = id,
      phase = phase,
      latestGen = state.latestAccepted.map(_.gen),
      metrics = state.latestAccepted.map(_.metrics).getOrElse(Map.empty),
      started = state.generationsStarted,
      goal = state.goal
    )

  /** The loops worth opening a session with: everything except the ones that finished.
    * A loop parked on `goal reached` is a completed piece of work, and greeting every
    * session with it forever would train the reader to skip the greeting. */
  def unfinished(found: List[Waiting]): List[Waiting] = found.filterNot(_.settled)

  /** The system-prompt section, or `None` when the project has no unfinished loops.
    *
    * One line per loop, carrying what a decision needs: where it stopped, how far it
    * got, and what it was for. The goal is quoted because it is the only part a lead
    * cannot reconstruct — the id is a name and the metrics are numbers, but the goal is
    * what says whether picking the loop back up is worth doing. */
  def section(found: List[Waiting]): Option[String] =
    val open = unfinished(found)
    if open.isEmpty then None
    else
      val lines = open.map(line)
      Some(
        "This project holds refinement loops that are not finished. They are not running: a loop " +
          "is only driven by the session that holds it, and picking one up is a deliberate act " +
          "(`lib.loop.get(\"<id>\").resume()`, which re-checks its definition first). " +
          "`lib.loop.list` shows them, `lib.loop.reconfigure` retunes one and `lib.loop.amend` " +
          "replaces its checker. Do not resume one because it is here — resume it when the user " +
          "asks for the work it was doing.\n\n" + lines.mkString("\n")
      )

  /** The one-off transcript note for loops an ended session left RUNNING, or `None`
    * when there are none.
    *
    * Only the orphans. A PARKED loop stopped because someone decided it should — the
    * window lists it and the prompt section names it, and saying so again at every
    * session open would be the sticky notice this replaced. An orphan is the opposite:
    * nobody decided anything, a session simply died holding it, and that is worth
    * being told once. Names the loops (an orphan you cannot name is one you cannot go
    * look at) and points at the window; it never says what to do, because whether to
    * resume is the user's call. */
  def orphanNote(found: List[Waiting]): Option[String] =
    val orphans = unfinished(found).filter(_.phase == LoopBridge.Orphaned)
    if orphans.isEmpty then None
    else
      val names = orphans.map(w => s"'${w.id}'").mkString(", ")
      val subject =
        if orphans.length == 1 then s"a loop ($names) was left running by a session that ended"
        else s"${orphans.length} loops ($names) were left running by sessions that ended"
      Some(s"$subject — ctrl+c l to view")

  private def line(w: Waiting): String =
    val progress =
      w.latestGen match
        case Some(gen) =>
          val measured = if w.metrics.isEmpty then "" else s", measured ${LoopBridge.metricsLine(w.metrics)}"
          s"accepted through generation $gen of ${w.started} started$measured"
        case None =>
          if w.started == 0 then "nothing started yet"
          else s"${w.started} generation(s) started, none accepted"
    val goal = if w.goal.trim.isEmpty then "" else s" — goal: ${w.goal.trim}"
    s"- `${w.id}` (${w.phase}): $progress$goal"
