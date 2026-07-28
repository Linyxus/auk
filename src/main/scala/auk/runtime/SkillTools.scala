package auk.runtime

import gears.async.Async

import auk.llm.tools.{ApprovalRequest, RuntimeContext, Tool, ToolInput, ToolResult, desc}
import auk.runtime.skills.{Skill, SkillManager}

/** The skill-management tools: add/update, remove, and reload-from-disk.
  *
  * These are native tools rather than `lib.*` calls on purpose: a skill change
  * swaps the live REPL session itself, so it cannot run *inside* an eval on
  * that session — as a tool call it sits cleanly between evals, making the
  * swap moment well-defined. All three delegate to [[SkillManager]], which
  * validates every candidate set in a fresh session (whole-set compile + all
  * tests) and rejects with diagnostics, leaving the live session untouched, on
  * any failure.
  */
object SkillTools:
  def all(manager: SkillManager): List[Tool] =
    List(SkillSave(manager), SkillRemove(manager), SkillReload(manager))

case class SkillSaveParams(
    @desc(
      "The skill id: a plain Scala identifier that must match the name of the " +
        "code's single top-level object (e.g. 'MigrateSuite'). Reusing an " +
        "existing id updates that skill."
    )
    id: String,
    @desc("A one-line description of what the skill does, shown in the skill index.")
    description: String,
    @desc(
      "The skill's full source: optional leading `import` lines, then exactly " +
        "one top-level `object <id>` whose header line ends with `{` or `:`. " +
        "Public members need explicit result types (their signatures form the " +
        "advertised interface). Skills may call other stored skills directly."
    )
    code: String,
    @desc(
      "Test snippets: each a standalone block of statements ending in " +
        "`assert(...)` checks, run against the whole candidate skill set. At " +
        "least one meaningful test is strongly encouraged — tests are what " +
        "future changes are validated against. Each snippet runs wrapped in " +
        "`locally { ... }`, so its bindings stay local."
    )
    tests: List[String]
) derives ToolInput

final class SkillSave(manager: SkillManager) extends Tool:
  type Params = SkillSaveParams
  val name = "skill_save"
  val description: String =
    "Add or update a durable skill: a tested Scala object stored in `.auk/skills/` " +
      "and preloaded into every future REPL session (see the Skills section of the " +
      "system prompt for the shape rules). The change is validated first — the whole " +
      "skill set must compile as one unit and every test must pass — and rejected " +
      "with diagnostics otherwise. On success the live REPL session is REPLACED by " +
      "the validated one: your eval_scala scratch definitions are gone, so save at " +
      "a natural boundary, not mid-computation."
  val input: ToolInput[SkillSaveParams] = ToolInput[SkillSaveParams]

  def execute(params: SkillSaveParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    val summary = s"save skill '${params.id}'\n${params.code}"
    if !ctx.approvals.request(ApprovalRequest(name, summary)) then
      ToolResult.error("skill change not approved")
    else
      manager.save(Skill(params.id, params.description, params.code, params.tests)) match
        case Right(msg) => ToolResult.ok(msg)
        case Left(err)  => ToolResult.error(err)

case class SkillRemoveParams(
    @desc("The id of the stored skill to remove.")
    id: String
) derives ToolInput

final class SkillRemove(manager: SkillManager) extends Tool:
  type Params = SkillRemoveParams
  val name = "skill_remove"
  val description: String =
    "Remove a stored skill. The remaining set is re-validated first; if another " +
      "skill depends on the removed one, the removal is rejected with the compile " +
      "diagnostics naming it. On success the live REPL session is replaced " +
      "(eval_scala scratch state resets)."
  val input: ToolInput[SkillRemoveParams] = ToolInput[SkillRemoveParams]

  def execute(params: SkillRemoveParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    if !ctx.approvals.request(ApprovalRequest(name, s"remove skill '${params.id}'")) then
      ToolResult.error("skill change not approved")
    else
      manager.remove(params.id) match
        case Right(msg) => ToolResult.ok(msg)
        case Left(err)  => ToolResult.error(err)

case class SkillReloadParams() derives ToolInput

final class SkillReload(manager: SkillManager) extends Tool:
  type Params = SkillReloadParams
  val name = "skill_reload"
  val description: String =
    "Re-read the skill set from `.auk/skills/` (after files were edited outside " +
      "the skill tools) and validate it: whole-set compile plus every test. On " +
      "success the validated session replaces the live one (eval_scala scratch " +
      "state resets); on failure the live session keeps the previous good set."
  val input: ToolInput[SkillReloadParams] = ToolInput[SkillReloadParams]

  def execute(params: SkillReloadParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    if !ctx.approvals.request(ApprovalRequest(name, "reload skills from disk")) then
      ToolResult.error("skill reload not approved")
    else
      manager.reloadFromDisk() match
        case Right(msg) => ToolResult.ok(msg)
        case Left(err)  => ToolResult.error(err)
