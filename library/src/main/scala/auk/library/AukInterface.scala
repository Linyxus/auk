package auk.library

/** A path in the file system. */
trait Path:
  /** Extends this path. For instance, `p / "src" / "main.py"`. */
  def / (sub: String): Path
  /** Opens the path as a [[FsEntry]]. */
  def openAsEntry: FsEntry
  /** Opens the path as a file entry. */
  def openAsFile: FsFile
  /** Opens the path as a directory entry. */
  def openAsDir: FsDir
  /** Gets the base name of this path. */
  def baseName: String
  /** Gets parent path. */
  def parent: Path

/** A generic file system entry. Can either be a file [[FsFile]] or a directory [[FsDir]]. */
sealed trait FsEntry:
  /** Gets the path of the entry. */
  def path: Path
  /** Whether this entry is a file? */
  def isFile: Boolean
  /** Whether this entry is a directory? */
  def isDir: Boolean
  /** This entry seen as a [[FsFile]] — for use after an [[isFile]] check, or
   *  when the kind is already known. Throws if this entry is a directory. */
  def asFile: FsFile = this match
    case f: FsFile => f
    case _: FsDir  => throw new RuntimeException(s"asFile: '$path' is a directory, not a file")
  /** This entry seen as a [[FsDir]] — for use after an [[isDir]] check, or when
   *  the kind is already known. Throws if this entry is a file. */
  def asDir: FsDir = this match
    case d: FsDir  => d
    case _: FsFile => throw new RuntimeException(s"asDir: '$path' is a file, not a directory")
  /** Whether this entry exists in the file system? */
  def exists: Boolean
  /** Gets the parent directory of this entry. */
  def parent: FsDir
  /** The final segment of this entry's path — the file or directory name,
   *  e.g. `main.scala` for `/src/main.scala`. */
  def name: String
  /** Last modification time as a human-readable datetime string
   *  (e.g. `2026-06-13 18:26:58`). See [[lastModifiedMs]] for the raw epoch. */
  def lastModified: String
  /** Last modification time, in milliseconds since the Unix epoch. */
  def lastModifiedMs: Long
  /** Deletes this entry. A directory is removed together with everything inside
   *  it (recursive). Does nothing if the entry does not exist. */
  def delete(): Unit
  /** Moves (renames) this entry to `dest`, returning a handle to the new
   *  location. The parent directory of `dest` must already exist. */
  def moveTo(dest: Path): FsEntry
  /** Copies this entry to `dest`, returning a handle to the copy. A directory is
   *  copied with all of its contents (recursive). Missing parent directories of
   *  `dest` are created as needed (unlike [[moveTo]], which requires `dest`'s
   *  parent to already exist). Copying an entry onto its own path is a no-op. */
  def copyTo(dest: Path): FsEntry

/** A file entry in the file system. */
abstract class FsFile extends FsEntry:
  /** Prints the file's content to standard output, one line per row, each
   *  prefixed with its 1-based line number in the format `<linenum>@ <line content>`.
   *  For example, a file holding
   *  {{{
   *  def greet =
   *    println("hi")
   *  }}}
   *  prints as:
   *  {{{
   *  1@ def greet =
   *  2@   println("hi")
   *  }}}
   *  By default the whole file is printed; pass `offset` (the 1-based line to
   *  start at) and `limit` (the maximum number of lines to print, or `-1` for
   *  "to the end") to read a window of a large file — e.g. `read(100, 40)`
   *  prints 40 lines starting at line 100. Line numbers stay absolute, so a
   *  window reports the file's true line numbers.
   *  To edit the file, copy the text *without* the `<linenum>@ ` prefix into
   *  [[replace]]. */
  def read(offset: Int = 1, limit: Int = -1): Unit
  /** Reads the full raw content of the file, with no line-number prefixes. */
  def rawContent: String
  /** The file's content split into raw lines (no line-number prefixes, no
   *  trailing newline character per line). */
  def lines: List[String]
  /** The number of lines in the file. */
  def lineCount: Int
  /** The size of the file in bytes. */
  def size: Long
  /** The file extension without the leading dot, e.g. `scala` for `main.scala`.
   *  Empty if the name has no extension. */
  def ext: String
  /** Finds every line whose text matches the regular expression `pattern`,
   *  returning one [[Match]] per matching line (with 1-based line numbers). */
  def grep(pattern: String): List[Match]
  /** Replaces a string with a new string in the file.
   *  There must be exactly one match of the `oldStr` for this to work. */
  def replace(oldStr: String, newStr: String): Unit
  /** Replaces every occurrence of `oldStr` with `newStr`, returning the number
   *  of replacements made (0 if there were none). */
  def replaceAll(oldStr: String, newStr: String): Int
  /** Writes content to this file. If the file exists, this *overwrites* the existing content.
   *  If the file does not exist yet, this creates the file and writes the content.
   *  However, if the parent directory does not exist yet, this will fail. */
  def write(content: String): Unit
  /** Appends `content` to the end of the file, creating the file first if it
   *  does not exist. The parent directory must already exist. */
  def append(content: String): Unit
  /** Creates the file empty if it does not exist; otherwise leaves its content
   *  unchanged. The parent directory must already exist. */
  def touch(): Unit


/** A directory entry in the file system. */
abstract class FsDir extends FsEntry:
  /** Creates this directory, including any missing parent directories
   *  (recursive, like `mkdir -p`). Does nothing if it already exists. */
  def makedir(): Unit
  /** The immediate children of this directory — both files and subdirectories,
   *  in no particular order. */
  def entries: List[FsEntry]
  /** The immediate child files of this directory (non-recursive). */
  def files: List[FsFile]
  /** The immediate child subdirectories of this directory (non-recursive). */
  def dirs: List[FsDir]
  /** Finds entries whose path matches the glob `pattern`, evaluated relative to
   *  this directory. Supports `*` (any run of characters within one path
   *  segment), `**` (spanning any number of segments, for recursive matches),
   *  and `?` (a single character). For example, `"*.scala"` matches Scala files
   *  directly in this directory, while a leading `**` segment reaches into
   *  subdirectories at any depth. */
  def glob(pattern: String): List[FsEntry]
  /** Recursively searches the text content of every file beneath this directory
   *  for lines matching the regular expression `pattern`, returning one
   *  [[Match]] per matching line. */
  def grep(pattern: String): List[Match]
  /** Like [[grep]], but searches only files whose path matches the glob
   *  `filePattern` (same syntax as [[glob]]), e.g. `dir.grep("TODO", "*.md")`
   *  to scan Markdown files. */
  def grep(pattern: String, filePattern: String): List[Match]
  /** A handle to the child file named `name` in this directory; the file need
   *  not exist. Shorthand for `(path / name).openAsFile`. */
  def file(name: String): FsFile
  /** A handle to the child subdirectory named `name`; it need not exist.
   *  Shorthand for `(path / name).openAsDir`. */
  def dir(name: String): FsDir
  /** Every descendant entry, recursively: the whole subtree beneath this
   *  directory, not including the directory itself.
   *  Avoid doing this unless the directory is a small, well-contained one,
   *  since the result of walking a folder can be huge. */
  def walk: List[FsEntry]

/** A single matching line produced by `grep` (see [[FsFile.grep]] and
 *  [[FsDir.grep]]). */
trait Match:
  /** The file the matching line was found in. */
  def file: Path
  /** The 1-based line number of the match within [[file]]. */
  def lineNumber: Int
  /** The full text of the matching line. */
  def line: String

/** Interface for file system. */
trait FileSystem:
  /** The current working directory. */
  def cwd: FsDir
  /** Access a path. */
  def access(p: Path): FsEntry
  /** Access a path that is a file. */
  def accessFile(p: Path): FsFile
  /** Access a path that is a directory. */
  def accessDir(p: Path): FsDir

/** The outcome of running an external command (see [[Shell]]).
 *
 *  Captured (not streamed): `stdout` and `stderr` are the program's full output
 *  as separate strings, `exitCode` is its exit status, and `timedOut` is true if
 *  the command was killed for exceeding the shell's timeout. A non-zero exit is
 *  *not* an error you have to catch — it is reported here for you to inspect.
 *  Renders readably in the REPL (the output followed by an `[exit N]` footer), so
 *  evaluating a result shows you what happened. For example:
 *  {{{
 *  val r = lib.shell.run("git", "rev-parse", "HEAD")
 *  if r.ok then r.stdout.trim else r.stderr
 *  }}}
 */
final case class CommandResult(stdout: String, stderr: String, exitCode: Int, timedOut: Boolean):
  /** True on a clean run: exited with code 0 and did not time out. */
  def ok: Boolean = exitCode == 0 && !timedOut
  /** The combined output — `stdout` followed by `stderr` — for when you just want
   *  to see everything. A newline is inserted between the two when `stdout` does
   *  not already end with one, so the streams never run together on a single line.
   *  Note this is concatenated, not interleaved in real time. */
  def output: String =
    if stderr.isEmpty then stdout
    else if stdout.isEmpty || stdout.endsWith("\n") then stdout + stderr
    else stdout + "\n" + stderr
  override def toString: String =
    val footer = if timedOut then "[timed out]" else s"[exit $exitCode]"
    val body = output
    if body.isEmpty then footer else s"$body\n$footer"

/** A validated handle to one external program, obtained from [[Shell.command]].
 *  Reuse it to run the same program repeatedly, and derive variants that run
 *  elsewhere ([[at]]) or with a different deadline ([[withTimeout]]):
 *  {{{
 *  val git = lib.shell.command("git")
 *  git.execute("add", "-A")
 *  git.execute("commit", "-m", "wip")
 *  git.at(otherRepo).execute("status", "--short") // same program, different dir
 *  }}}
 */
trait ShellCommand:
  /** Run the program with these literal arguments. Arguments are passed verbatim
   *  (no shell parsing/quoting/globbing) — `execute("a b")` is a single argument
   *  containing a space. Use [[Shell.sh]] when you need shell features. */
  def execute(args: String*): CommandResult
  /** The same program rooted at `dir` (a relative `dir` resolves against this
   *  command's working directory). Returns a new handle; this one is unchanged. */
  def at(dir: Path): ShellCommand
  /** The same program whose runs are killed after `ms` milliseconds. Returns a
   *  new handle; this one is unchanged. */
  def withTimeout(ms: Int): ShellCommand

/** Runs external programs. Reach it via `lib.shell`.
 *
 *  Run a single program with literal arguments via [[run]] (one-shot) or
 *  [[command]] (a reusable handle). Arguments are passed verbatim, with no shell
 *  parsing. Prefer the file-system interface ([[FileSystem]] / [[Path]]) for file
 *  operations — it gives you structured results.
 *
 *  As a nudge in that direction, [[command]] and [[run]] reject a few
 *  file-operation program *names* (`rm`, `ls`, `mv`, `mkdir`, `cat`, `touch`).
 *  This is a convenience guardrail, **not** a security boundary: it matches only
 *  those exact base names, so equivalents are not blocked (`cp`, `find -delete`,
 *  `python -c …`, `env rm …`), and [[sh]] runs an arbitrary shell line with no
 *  checks at all. Evaluated code can likewise mutate the file system directly via
 *  [[FileSystem]] / [[Path]]. Treat the shell as fully capable.
 *
 *  By default commands run in the current working directory ([[FileSystem.cwd]]);
 *  use [[at]] to root the shell elsewhere and [[withTimeout]] to change the kill
 *  deadline.
 */
trait Shell:
  /** A reusable handle for the program `name`. Throws if `name`'s base name is one
   *  of the denylisted file-operation programs (`rm`/`ls`/`mv`/`mkdir`/`cat`/`touch`)
   *  — use [[FileSystem]] / [[Path]] for those instead. See the trait doc for the
   *  denylist's (deliberate) limits. */
  def command(name: String): ShellCommand
  /** Run `name` with literal `args` and return the captured result. Shorthand for
   *  `command(name).execute(args*)`, with the same name check. For example:
   *  `lib.shell.run("git", "status", "--short")`. */
  def run(name: String, args: String*): CommandResult
  /** Run an arbitrary shell command line through `/bin/sh -c`, with full shell
   *  features (pipes, redirection, `&&`, globbing). Unlike [[command]] / [[run]],
   *  this applies **no** program denylist — it is the unrestricted escape hatch. */
  def sh(commandLine: String): CommandResult
  /** A shell rooted at `dir` (a relative `dir` resolves against [[FileSystem.cwd]]);
   *  carries the same policy and timeout. */
  def at(dir: Path): Shell
  /** A shell whose commands are killed after `ms` milliseconds (default two
   *  minutes). Returns a new shell; this one is unchanged. */
  def withTimeout(ms: Int): Shell

/** One stored memory: a durable, named piece of project knowledge. Obtained from
 *  [[Memory.get]] / [[Memory.all]]. */
trait MemoryEntry:
  /** The stable slug that names this memory — the handle for `read`/`write`/`delete`. */
  def id: String
  /** A one-line summary, shown in [[Memory.overview]]. */
  def description: String
  /** The full text of the memory. */
  def content: String

/** Durable, project-scoped memory you curate across sessions — facts worth keeping:
 *  conventions, build/test commands, where things live, hard-won gotchas. Reached as
 *  `lib.memory`. Each memory is an `id` (a stable slug), a one-line `description`, and
 *  the full `content`.
 *
 *  Start from [[overview]] to see what is already known, then [[read]] the ones that
 *  look relevant:
 *  {{{
 *  lib.memory.overview()              // build — how to build and test
 *                                     // workflow-arch — how workflows orchestrate
 *  lib.memory.read("build")           // prints that memory's full content
 *  lib.memory.write("build", "how to build and test", "Use `sbt test`; …")
 *  }}}
 *  Save durable knowledge, not transient task detail; reuse an `id` to update it, and
 *  [[delete]] one that goes stale. */
trait Memory:
  /** Print an at-a-glance index of every stored memory — one `id — description` per
   *  line — to stdout. Check this when picking up work on a project, before recalling
   *  specifics. */
  def overview(): Unit
  /** Print one memory's full content to stdout (or a clear 'not found' note if there
   *  is no memory with this `id`). */
  def read(id: String): Unit
  /** The memory with this `id`, or `None` if absent. */
  def get(id: String): Option[MemoryEntry]
  /** Every stored memory, ordered by `id` — for programmatic use (e.g. filtering). */
  def all: List[MemoryEntry]
  /** Create or replace the memory `id` with a one-line `description` and full
   *  `content`. Use it for durable facts, not transient task detail. Reusing an `id`
   *  overwrites it. `id` must be a filename-safe slug (letters, digits, `.`, `_`, `-`). */
  def write(id: String, description: String, content: String): Unit
  /** Delete the memory `id`. A no-op if there is no such memory. */
  def delete(id: String): Unit

/** A tool the assistant invoked during a past turn, paired with the result it
 *  produced. Obtained from [[HistoryMessage.toolCalls]]. */
trait HistoryToolCall:
  /** The tool's name, e.g. `"eval_scala"`. */
  def name: String
  /** The raw JSON arguments the model passed to the tool. */
  def arguments: String
  /** The tool's result text, or `""` if none was recorded. */
  def output: String
  /** Whether the tool reported an error. */
  def isError: Boolean

/** One message in a past conversation. Obtained from [[HistorySession.messages]]. */
trait HistoryMessage:
  /** Who produced it: `"user"`, `"assistant"`, or `"system"`. */
  def role: String
  /** The message text — the user's input, the assistant's answer, or a system
   *  note. May be `""` for an assistant turn that only called tools. */
  def text: String
  /** Reasoning the assistant recorded this turn (`""` when none, or for other
   *  roles). */
  def reasoning: String
  /** Tool calls the assistant made this turn, each paired with its result. Empty
   *  for non-assistant turns and assistant turns that called no tools. */
  def toolCalls: List[HistoryToolCall]

/** A past conversation in this project. Obtained from [[SessionHistory.get]] /
 *  [[SessionHistory.all]]. */
trait HistorySession:
  /** The session id (a UUID; its short prefix is shown in [[SessionHistory.overview]]). */
  def id: String
  /** When the conversation was last updated (epoch milliseconds), if known. */
  def modifiedAtMs: Option[Long]
  /** Number of user + assistant messages in the conversation. */
  def messageCount: Int
  /** A one-line preview of the latest message. */
  def preview: String
  /** The conversation's messages, in order. */
  def messages: List[HistoryMessage]

/** Read-only access to this project's past conversations — the session logs auk
 *  keeps under `.auk/sessions`. Reached as `lib.history`. Use it to recall what was
 *  tried, decided, or discussed in an earlier session.
 *
 *  Start from [[overview]] to see recent conversations, then [[read]] one by its
 *  short id, or [[search]] across all of them:
 *  {{{
 *  lib.history.overview()                 // 4a376536 · 2h ago · 6 msg — fix the build
 *  lib.history.read("4a376536")           // prints that conversation's transcript
 *  lib.history.search("workflow resume")  // conversations mentioning it, with snippets
 *  }}}
 *  For programmatic use, [[get]] / [[all]] return structured [[HistorySession]]s (each
 *  with its [[HistoryMessage]] list). Purely read-only — it never changes a session. */
trait SessionHistory:
  /** Print an index of recent conversations — newest first — one line each: short
   *  id, age, message count, and a preview. `limit` caps how many are shown. */
  def overview(limit: Int = 20): Unit
  /** Print one conversation's full transcript to stdout (or a clear not-found note).
   *  Accepts a full id or any unambiguous id prefix (as shown by [[overview]]). */
  def read(id: String): Unit
  /** Print the conversations whose transcript contains `query` (case-insensitive),
   *  newest first, each with a short snippet around the match. */
  def search(query: String): Unit
  /** One conversation by id — a full id or unambiguous prefix — or `None` if there
   *  is no such (single) conversation. */
  def get(id: String): Option[HistorySession]
  /** Every conversation, newest first — for programmatic use (e.g. filtering on
   *  [[HistorySession.messages]]). */
  def all: List[HistorySession]

/** The runtime interface for Auk agents. */
trait AukInterface:
  /** Constructor for path. */
  def path(p: String): Path
  /** File system API. */
  val fs: FileSystem
  /** Shell / external-process API. */
  val shell: Shell
  /** Project memory: durable, curated knowledge across sessions. Reached as
   *  `lib.memory`. Start with `lib.memory.overview()` to see what is stored. See
   *  [[Memory]]. */
  val memory: Memory
  /** Past conversations in this project, read-only. Reached as `lib.history`. Start
   *  with `lib.history.overview()` to see recent sessions, `read`/`search` to recall
   *  earlier decisions. See [[SessionHistory]]. */
  val history: SessionHistory
  /** Workflow orchestration: write code that spawns and composes many
   *  sub-agents, grouped for the live UI. Reached as `wf` in scope. Entry point
   *  is [[Workflow.start]]; build the graph with the top-level `group`,
   *  `inGroup`, `agent`, [[Agent.all]] and `Agent.pure` (recurse + `flatMap` +
   *  `Agent.pure` for loops). `wf.start[R]{…}` returns a [[WorkflowRun]] handle
   *  immediately (non-blocking); poll its [[WorkflowRun.status]] in a later eval or
   *  wait for the completion system notice, which carries the resolved `R`. The
   *  handle also drives the run: [[WorkflowRun.pause]] / [[WorkflowRun.resume]]
   *  (resume re-runs it, skipping the finished sub-agents). See [[Workflow]]. */
  def wf: Workflow
