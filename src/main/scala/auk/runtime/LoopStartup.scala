package auk.runtime

import auk.loop.{LoopState, LoopStore}

/** What a session finds waiting for it in `.auk/loops` when it opens, and how it says
  * so.
  *
  * A loop outlives the session that started it, which is only useful if the NEXT
  * session knows it is there. A lead that has to be told a loop exists before it can
  * think about it will never think about it, so a project with unfinished loops opens
  * by naming them: once in the user's notices, and once in the system prompt, where it
  * reads as standing context rather than as something that just happened.
  *
  * Deliberately NOT an inbox item. A system notice waiting in the steering inbox fires a
  * whole model turn before the user has typed anything, and "you have a parked loop" is
  * not worth a turn — it is worth a sentence in the prompt for when it becomes relevant.
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
    val phase = state.parkedReason.map(LoopBridge.phaseFor).getOrElse(LoopBridge.Orphaned)
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

  /** The one line the user sees, or `None` when there is nothing to say. It says how
    * many and how to look, and leaves the detail to the prompt section — the user is
    * opening a session, not reading a report. */
  def notice(found: List[Waiting]): Option[String] =
    val open = unfinished(found)
    if open.isEmpty then None
    else
      val orphaned = open.count(_.phase == LoopBridge.Orphaned)
      val what = if open.length == 1 then s"a loop ('${open.head.id}')" else s"${open.length} loops"
      val gap =
        if orphaned == 0 then ""
        else if orphaned == open.length && open.length == 1 then ", left running by a session that ended"
        else s" ($orphaned left running by a session that ended)"
      Some(s"[loop] this project has $what waiting$gap")

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
