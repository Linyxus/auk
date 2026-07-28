package auk.runtime.skills

import scala.util.Success
import gears.async.{Async, Future}

import auk.runtime.repl.{ReplProtocol, ScalaRepl}

/** Owns the lead agent's live REPL session and the last-good skill set, and
  * performs the transactional reload that keeps the two in step.
  *
  * The invariant: the live session always runs a VALIDATED skill set — one that
  * compiled as a whole and passed every test — and the store on disk always
  * holds that same set. A modification (save / remove / reload-from-disk) is
  * validated in a FRESH session first: the preamble loads, the whole candidate
  * set compiles as one unit, and every test runs (each wrapped in
  * `locally { … }` so no test binding leaks into scope). Only on success is the
  * set persisted and the fresh session swapped in as the live one — so the
  * model's scratch state is reset by a successful change, and untouched by a
  * failed one. The elder session serves evals until the very moment of the
  * swap.
  *
  * The one exception, by design, is the FIRST load: whatever is on disk at
  * session start is loaded into the initial session directly, and failures are
  * flagged in the report rather than blocking startup — there is no elder
  * session to fall back to.
  */
final class SkillManager(store: SkillStore, makeRepl: () => ScalaRepl):
  import SkillManager.*

  private var current: ScalaRepl = makeRepl()
  private var good: List[Skill] = Nil
  private var statuses: List[SkillStatus] = Nil
  private var loadNote: Option[String] = None
  private var queueTail: Future[Unit] | Null = null

  /** The live session — read through this indirection on every eval, since a
    * successful modification swaps it. */
  def repl: ScalaRepl = current

  /** The last-good skill set (what the live session has loaded). */
  def skills: List[Skill] = good

  /** Load whatever the store holds into the initial session. Failures are
    * recorded for the prompt report, never thrown: on a compile failure the
    * session still serves evals (with `lib` but without skills), per the
    * first-load rule. Does not touch the REPL when the store is empty, so a
    * skill-less project keeps its lazy worker spawn. */
  def initialLoad()(using Async): Unit = serialized:
    val (loaded, warnings) = store.loadAll()
    good = loaded
    if loaded.isEmpty then
      statuses = Nil
      loadNote = renderWarnings(warnings)
    else
      val (blob, spans) = SkillBlob.build(loaded)
      evalOn(current, blob, BlobTimeoutMs) match
        case Left(diag) =>
          val ids = SkillBlob.culprits(diag, spans)
          val blamed = if ids.isEmpty then "" else s" (likely in: ${ids.mkString(", ")})"
          loadNote = Some(
            s"NONE of the stored skills are loaded — the skill set failed to compile$blamed. " +
              s"Fix it with skill_save or skill_remove, or repair the files and run skill_reload.\n\n$diag"
          )
          statuses = loaded.map: s =>
            SkillStatus(s, SkillCode.interface(s.id, s.code), Some("not loaded (the set failed to compile)"))
        case Right(_) =>
          val failures = runTests(current, loaded)
          statuses = loaded.map(s => SkillStatus(s, SkillCode.interface(s.id, s.code), failures.get(s.id)))
          loadNote = renderWarnings(warnings)

  /** Add or update one skill: validate the whole candidate set in a fresh
    * session, persist and swap on success. Right is a message for the model
    * (including the reset note); Left a rejection with diagnostics, leaving
    * session, store, and skill set untouched. */
  def save(skill: Skill)(using Async): Either[String, String] = serialized:
    staticCheck(skill).flatMap: iface =>
      val existed = good.exists(_.id == skill.id)
      val candidate = (good.filterNot(_.id == skill.id) :+ skill).sortBy(_.id)
      validate(candidate).map: fresh =>
        store.persist(candidate)
        install(fresh, candidate)
        val verb = if existed then "updated" else "saved"
        s"Skill '${skill.id}' $verb. The whole set (${candidate.size} skill(s), " +
          s"${candidate.map(_.tests.size).sum} test(s)) compiled and passed.\n\n$SwapNote\n\nInterface:\n$iface"

  /** Remove one skill, validating that the remaining set still stands (a
    * dependent skill's compile failure rejects the removal, naming it). */
  def remove(id: String)(using Async): Either[String, String] = serialized:
    if !good.exists(_.id == id) then Left(s"no skill named '$id' is stored")
    else
      val candidate = good.filterNot(_.id == id)
      validate(candidate) match
        case Left(err) =>
          Left(s"removing '$id' breaks the remaining skill set, so nothing was changed:\n$err")
        case Right(fresh) =>
          store.persist(candidate)
          install(fresh, candidate)
          Right(s"Skill '$id' removed; ${candidate.size} skill(s) remain.\n\n$SwapNote")

  /** Re-read the store (e.g. after hand edits) and validate it as a candidate
    * set; swap on success, keep the elder session and set on failure. */
  def reloadFromDisk()(using Async): Either[String, String] = serialized:
    val (loaded, warnings) = store.loadAll()
    validate(loaded) match
      case Left(err) =>
        Left(
          s"the skill set on disk failed validation, so the live session keeps the " +
            s"previous good set (${good.size} skill(s)):\n$err"
        )
      case Right(fresh) =>
        install(fresh, loaded)
        val warn = renderWarnings(warnings).fold("")(w => s"\n\n$w")
        Right(s"Reloaded ${loaded.size} skill(s) from disk; all tests passed.\n\n$SwapNote$warn")

  /** The "Skills" system-prompt section: the contract, the load report, and the
    * per-skill index with redacted interfaces. */
  def promptSection: String =
    val listing =
      if statuses.isEmpty then "No skills are stored yet."
      else "Stored skills:\n\n" + statuses.map(renderStatus).mkString("\n\n")
    val note = loadNote.fold("")(n => s"$n\n\n")
    s"$Intro\n\n$note$listing"

  def close()(using Async): Unit = current.close()

  // -- validation --------------------------------------------------------------

  /** Basic shape checks plus interface extraction, before paying for a compile. */
  private def staticCheck(skill: Skill): Either[String, String] =
    if !Skill.validId(skill.id) then
      Left(s"'${skill.id}' is not a valid skill id: use a plain Scala identifier, e.g. 'MigrateSuite'")
    else if skill.description.trim.isEmpty then Left("the description must not be empty")
    else if skill.description.linesIterator.length > 1 then Left("the description must be a single line")
    else if skill.tests.exists(_.trim.isEmpty) then Left("a test snippet must not be empty")
    else SkillCode.interface(skill.id, skill.code)

  /** Compile the candidate set and run every test in a fresh session. Right is
    * that session — ready to become the live one; Left closes it and reports. */
  private def validate(candidate: List[Skill])(using Async): Either[String, ScalaRepl] =
    val fresh = makeRepl()
    val outcome: Either[String, Unit] =
      if candidate.isEmpty then Right(())
      else
        val (blob, spans) = SkillBlob.build(candidate)
        evalOn(fresh, blob, BlobTimeoutMs) match
          case Left(diag) =>
            val ids = SkillBlob.culprits(diag, spans)
            val blamed = if ids.isEmpty then "" else s" (likely in: ${ids.mkString(", ")})"
            Left(s"the skill set failed to compile$blamed:\n$diag")
          case Right(_) =>
            val failures = runTests(fresh, candidate)
            if failures.isEmpty then Right(())
            else
              Left(
                failures.toList
                  .map((id, msg) => s"[$id] $msg")
                  .mkString("tests failed:\n", "\n", "")
              )
    outcome match
      case Left(err) => fresh.close(); Left(err)
      case Right(_)  => Right(fresh)

  /** Run every skill's tests on `repl`, each wrapped in `locally { … }` so test
    * bindings never leak into the session. Returns failure messages per skill
    * id. A timed-out test kills the worker, so the run stops there. */
  private def runTests(repl: ScalaRepl, skills: List[Skill])(using Async): Map[String, String] =
    val failures = collection.mutable.LinkedHashMap.empty[String, List[String]]
    var sessionLost = false
    for
      skill <- skills
      (test, i) <- skill.tests.zipWithIndex
      if !sessionLost
    do
      evalOn(repl, s"locally {\n$test\n}", TestTimeoutMs) match
        case Right(_) => ()
        case Left(diag) =>
          val fatal = diag.contains("timed out") || diag.contains("worker exited")
          if fatal then sessionLost = true
          val label = s"test ${i + 1} of ${skill.tests.size} failed"
          val suffix = if fatal then " (this killed the REPL session; later tests were skipped)" else ""
          failures.updateWith(skill.id)(prev => Some(prev.getOrElse(Nil) :+ s"$label$suffix:\n$diag"))
    failures.view.mapValues(_.mkString("\n")).toMap

  private def evalOn(repl: ScalaRepl, code: String, timeoutMs: Int)(using Async): Either[String, Unit] =
    repl.eval(code, Some(timeoutMs)) match
      case ScalaRepl.EvalResult(ScalaRepl.Status.Completed(r), _) if r.ok => Right(())
      case ScalaRepl.EvalResult(ScalaRepl.Status.Completed(r), _) =>
        val detail = if r.output.nonEmpty then r.output else r.error.getOrElse("unknown error")
        val err = ReplProtocol.stripAnsi(detail)
        val stderr = if r.stderr.isEmpty then "" else s"\n[stderr]\n${ReplProtocol.stripAnsi(r.stderr)}"
        Left(err + stderr)
      case ScalaRepl.EvalResult(ScalaRepl.Status.TimedOut(ms), _) => Left(s"timed out after ${ms}ms")
      case ScalaRepl.EvalResult(ScalaRepl.Status.Failed(reason), _) => Left(reason)

  /** Make `fresh` the live session and `set` the good set; the elder session is
    * closed (waiting out any in-flight eval first — ScalaRepl serialises). */
  private def install(fresh: ScalaRepl, set: List[Skill])(using Async): Unit =
    val elder = current
    current = fresh
    good = set
    statuses = set.map(s => SkillStatus(s, SkillCode.interface(s.id, s.code), None))
    loadNote = None
    elder.close()

  private def renderWarnings(warnings: List[String]): Option[String] =
    Option.when(warnings.nonEmpty)(
      warnings.mkString("Some stored skills could not be read:\n- ", "\n- ", "")
    )

  private def renderStatus(st: SkillStatus): String =
    val iface = st.interface match
      case Right(sig) => s"```scala\n$sig\n```"
      case Left(err)  => s"(interface unavailable: $err)"
    val problem = st.problem.fold("")(p => s"\n⚠ $p")
    s"### ${st.skill.id} — ${st.skill.description}\n$iface$problem"

  /** FIFO-serialise `body` against other manager operations (the compound
    * validate→persist→swap sequences must not interleave). Same promise-chain
    * pattern as [[ScalaRepl.serialized]]; single-threaded JS makes the tail
    * swap race-free. */
  private def serialized[T](body: Async ?=> T)(using Async): T =
    val gate = Future.Promise[Unit]()
    val prev = queueTail
    queueTail = gate.asFuture
    if prev != null then { val _ = prev.awaitResult }
    try body
    finally gate.complete(Success(()))

object SkillManager:

  /** A skill's standing in the live session, for the prompt report. */
  final case class SkillStatus(skill: Skill, interface: Either[String, String], problem: Option[String])

  /** The whole set recompiles at session start (cold JS-hosted compiler), so
    * the budget is generous. */
  val BlobTimeoutMs: Int = 180_000

  /** One test snippet's eval budget. Hitting it kills the session, so tests
    * should stay fast. */
  val TestTimeoutMs: Int = 60_000

  private val SwapNote =
    "NOTE: the REPL session was replaced by the freshly validated one — scratch " +
      "definitions from earlier eval_scala calls are gone; `lib` and every skill " +
      "are loaded. (The Skills section of the system prompt refreshes next session; " +
      "within this one, rely on this message.)"

  private val Intro =
    """Skills are durable, tested Scala definitions stored in `.auk/skills/` and
      |preloaded into your REPL session — call them directly, like any other
      |definition in scope. Together they form ONE library compiled as a single
      |unit, so skills may reference each other freely.
      |
      |Manage them with the skill_save / skill_remove / skill_reload tools (NOT by
      |editing `.auk/skills/` through lib.fs — the tools validate and hot-swap the
      |session; raw edits take effect only after skill_reload). Every change is
      |validated in a fresh session first — the whole set must compile and every
      |test must pass — and is REJECTED with diagnostics otherwise, leaving the
      |live session untouched. A successful change swaps the fresh session in,
      |which RESETS your eval_scala scratch state, so save skills at natural
      |boundaries, not mid-computation.
      |
      |Shape: one top-level `object <Id>` per skill (its name IS the skill id),
      |optionally preceded by imports; public members need explicit result types.
      |Tests are standalone snippets containing `assert(...)` statements; they run
      |wrapped in `locally { … }` and see the whole skill set plus `lib`.
      |
      |When you find yourself re-deriving a procedure you have written before —
      |or building one likely to recur — crystallise it as a skill with a test,
      |so every future session gets it for free.""".stripMargin
