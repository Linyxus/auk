package auk.runtime.skills

import java.io.IOException

import auk.platform.{FileSystem, PathOps, Platform}

/** Disk layout of the skill set, under `.auk/skills/`:
  *
  * {{{
  * .auk/skills/<id>/skill.scala     // "// description: …" header + the code
  * .auk/skills/<id>/tests/1.scala   // one test snippet per file, numbered
  * .auk/skills/<id>/tests/2.scala
  * }}}
  *
  * The store is dumb on purpose: it reads and writes files. Validation — does
  * the set compile, do the tests pass — belongs to [[SkillManager]], which only
  * calls [[persist]] with an already-validated set, so what is on disk is the
  * last GOOD set (modulo hand edits, which `skill_reload` re-validates).
  */
final class SkillStore(val root: String, fs: FileSystem = Platform.fs):
  import SkillStore.*

  /** Every well-formed skill on disk (sorted by id) plus a warning per
    * malformed entry. Never throws: an unreadable store is an empty one with
    * warnings. */
  def loadAll(): (List[Skill], List[String]) =
    if !fs.isDirectory(root) then (Nil, Nil)
    else
      val warnings = List.newBuilder[String]
      val skills = List.newBuilder[Skill]
      val dirs =
        try fs.listDir(root).filterNot(_.isFile).map(_.name).sorted
        catch
          case e: IOException =>
            warnings += s"could not list $root: ${e.getMessage}"
            Nil
      for id <- dirs do
        val dir = PathOps.join(root, id)
        val file = PathOps.join(dir, SkillFileName)
        if !Skill.validId(id) then warnings += s"$id: not a valid skill id, skipped"
        else if !fs.isRegularFile(file) then warnings += s"$id: no $SkillFileName, skipped"
        else
          try
            parseSkillFile(fs.readString(file)) match
              case Left(err) => warnings += s"$id: $err, skipped"
              case Right((description, code)) =>
                skills += Skill(id, description, code, readTests(dir))
          catch case e: IOException => warnings += s"$id: ${e.getMessage}, skipped"
      (skills.result(), warnings.result())

  /** Write `skills` as the store's complete content: each skill's directory is
    * rewritten and directories for absent ids are removed. */
  def persist(skills: List[Skill]): Unit =
    fs.createDirectories(root)
    val keep = skills.map(_.id).toSet
    fs.listDir(root)
      .filterNot(_.isFile)
      .filterNot(e => keep(e.name))
      .foreach(e => fs.removeAll(PathOps.join(root, e.name)))
    for skill <- skills do
      val dir = PathOps.join(root, skill.id)
      fs.removeAll(dir)
      fs.createDirectories(dir)
      fs.writeString(PathOps.join(dir, SkillFileName), renderSkillFile(skill))
      if skill.tests.nonEmpty then
        val testsDir = PathOps.join(dir, TestsDirName)
        fs.createDirectories(testsDir)
        for (test, i) <- skill.tests.zipWithIndex do
          fs.writeString(PathOps.join(testsDir, s"${i + 1}.scala"), test.stripTrailing.nn + "\n")

  private def readTests(dir: String): List[String] =
    val testsDir = PathOps.join(dir, TestsDirName)
    fs.listDir(testsDir)
      .filter(e => e.isFile && e.name.endsWith(".scala"))
      .sortBy(e => (e.name.stripSuffix(".scala").toIntOption.getOrElse(Int.MaxValue), e.name))
      .map(e => fs.readString(PathOps.join(testsDir, e.name)).stripTrailing.nn)

object SkillStore:
  val RelativePath = ".auk/skills"
  val SkillFileName = "skill.scala"
  val TestsDirName = "tests"
  val DescriptionPrefix = "// description: "

  def renderSkillFile(skill: Skill): String =
    DescriptionPrefix + skill.description + "\n" + skill.code.stripTrailing + "\n"

  /** Split a `skill.scala` into its description header and code. */
  def parseSkillFile(text: String): Either[String, (String, String)] =
    val lines = text.linesIterator.toList
    lines match
      case first :: rest if first.startsWith(DescriptionPrefix) =>
        val description = first.drop(DescriptionPrefix.length).trim
        if description.isEmpty then Left(s"empty description header")
        else Right((description, rest.dropWhile(_.trim.isEmpty).mkString("\n")))
      case _ =>
        Left(s"first line must be `${DescriptionPrefix}<one line>`")
