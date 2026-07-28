package auk.agent

import gears.async.Async
import auk.platform.PathOps

/** A system-prompt section whose body is gathered from the live environment at
  * session start, rather than baked into the source like the static sections.
  *
  * `render` returns `None` when the section has nothing to contribute — not a
  * git repository, no project-instructions file — so empty headings never reach
  * the prompt. New environmental concerns slot in as additional `DynamicSection`
  * values in [[SystemPrompt.dynamicSections]] without touching the assembler.
  */
trait DynamicSection:
  /** The `## ` heading this section renders under. */
  def title: String

  /** The section body for `env`, or `None` to omit the section entirely. */
  def render(env: PromptEnv)(using Async): Option[String]

object DynamicSection:

  /** Basic, always-present facts about the session: where it runs, which model
    * is driving it, the date, and — when the working directory is a git
    * checkout — a snapshot of the repository state. */
  object ProjectInfo extends DynamicSection:
    def title: String = "Environment"

    def render(env: PromptEnv)(using Async): Option[String] =
      val facts = List(
        s"Working directory: ${env.workingDirectory}",
        s"Model: ${env.modelName}",
        s"Today's date: ${env.today}"
      )
      val sections = facts.mkString("\n") :: Git.status(env).toList
      Some(sections.mkString("\n\n"))

  /** Project-wide instructions the user has committed alongside the code. We
    * prefer `CLAUDE.md` and fall back to `AGENTS.md`; the first one present
    * wins and the rest are ignored. Absent both, the section is omitted. */
  object ProjectInstructions extends DynamicSection:
    def title: String = "Project instructions"

    /** Candidate files, in descending priority. */
    val Candidates: List[String] = List("CLAUDE.md", "AGENTS.md")

    def render(env: PromptEnv)(using Async): Option[String] =
      Candidates.iterator
        .map(name => name -> PathOps.join(env.workingDirectory, name))
        .find((_, path) => env.fs.isRegularFile(path))
        .flatMap: (name, path) =>
          try
            val body = env.fs.readString(path).trim
            Option.when(body.nonEmpty):
              s"""The file `$name` in the working directory holds project-wide
                 |instructions. Treat them as authoritative for this project.
                 |
                 |<$name>
                 |$body
                 |</$name>""".stripMargin
          catch case _: Throwable => None

  /** The stored project memories, pushed as an index — one line per note — so
    * recall does not depend on the model remembering to call
    * `lib.memory.overview()`. Bodies stay pull (`lib.memory.read(id)`): push the
    * table of contents, never the book. Reads the same `.auk/memory` markdown
    * files `lib.memory` writes from the REPL worker; the directory name and the
    * `description:` frontmatter key must stay in step with
    * `auk.library.AukImpl`. */
  object MemoryIndex extends DynamicSection:
    def title: String = "Project Memory Index"

    /** Keep in step with the worker-side memory implementation. */
    val Dir: String = ".auk/memory"

    def render(env: PromptEnv)(using Async): Option[String] =
      val dir = PathOps.join(env.workingDirectory, Dir)
      val entries =
        try
          env.fs
            .listDir(dir)
            .filter(e => e.isFile && e.name.endsWith(".md"))
            .sortBy(_.name)
            .map: e =>
              val id = e.name.stripSuffix(".md")
              val description =
                try frontmatterDescription(env.fs.readString(PathOps.join(dir, e.name)))
                catch case _: Throwable => None
              s"- $id${description.fold("")(d => s" — $d")}"
        catch case _: Throwable => Nil
      Option.when(entries.nonEmpty):
        ("The project memories currently stored (see Project Memory; read one in " +
          "full with `lib.memory.read(id)`):\n\n") + entries.mkString("\n")

    /** The `description:` value of a leading `---`-fenced frontmatter block, or
      * of a plain `description:` line at the top of the file. */
    private def frontmatterDescription(content: String): Option[String] =
      val lines = content.linesIterator.toList
      val scope = lines match
        case first :: rest if first.trim == "---" => rest.takeWhile(_.trim != "---")
        case _                                    => lines.takeWhile(_.trim.nonEmpty)
      scope
        .find(_.trim.startsWith("description:"))
        .map(_.trim.stripPrefix("description:").trim)
        .filter(_.nonEmpty)
        .map(d => if d.length > 200 then d.take(200) + "…" else d)

  /** Reads a git repository's current state through the [[PromptEnv.process]]
    * seam. Every command is bounded in time and output; a non-zero exit, a
    * timeout, or simply not being in a repo yields `None`/empty so the prompt
    * degrades cleanly outside version control. */
  private object Git:
    private val TimeoutMs = 5000
    private val MaxBytes = 64 * 1024

    /** A human-readable snapshot, or `None` when the directory is not a repo. */
    def status(env: PromptEnv)(using Async): Option[String] =
      if run(env, List("rev-parse", "--is-inside-work-tree")).map(_.trim) != Some("true") then None
      else
        val parts = List(
          run(env, List("rev-parse", "--abbrev-ref", "HEAD")).map(_.trim).filter(_.nonEmpty)
            .map(b => s"Current branch: $b"),
          Some {
            // Keep the leading two-column XY prefix of each entry; only strip the
            // trailing newline. `(clean)` stands in for an empty working tree.
            val s = run(env, List("status", "--short")).map(_.stripTrailing.nn).getOrElse("")
            s"Status:\n${if s.isEmpty then "(clean)" else s}"
          },
          run(env, List("log", "--oneline", "-n", "5")).map(_.trim).filter(_.nonEmpty)
            .map(l => s"Recent commits:\n$l")
        ).flatten
        val header =
          "This is the git status at the start of the session. It is a snapshot " +
            "and will not update during the conversation."
        Some((header :: parts).mkString("\n\n"))

    /** Run `git args` in the project directory, returning its merged output on a
      * clean exit and `None` otherwise. */
    private def run(env: PromptEnv, args: List[String])(using Async): Option[String] =
      val r = env.process.runCaptured("git" :: args, env.workingDirectory, TimeoutMs, MaxBytes)
      if r.exitCode == 0 && !r.timedOut then Some(r.output) else None
