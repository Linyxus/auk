package auk.library

/** A path in the file system. */
trait Path:
  /** Extends this path. For instance, `p / "src" / "main.py"`. */
  def / (sub: String): Path
  /** Opens the path as a [[FsEntry]]. */
  def asEntry: FsEntry
  /** Opens the path as a file entry. */
  def asFile: FsFile
  /** Opens the path as a directory entry. */
  def asDir: FsDir
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
   *  copied with all of its contents (recursive). The parent directory of
   *  `dest` must already exist. */
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
   *  not exist. Shorthand for `(path / name).asFile`. */
  def file(name: String): FsFile
  /** A handle to the child subdirectory named `name`; it need not exist.
   *  Shorthand for `(path / name).asDir`. */
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
  /** Get the current working directory path. */
  def cwd: Path
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
   *  to see everything. Note this is concatenated, not interleaved in real time. */
  def output: String = if stderr.isEmpty then stdout else stdout + stderr
  override def toString: String =
    val footer = if timedOut then "[timed out]" else s"[exit $exitCode]"
    val body = output
    if body.isEmpty then footer else s"$body\n$footer"

/** A validated handle to one external program, obtained from [[Shell.command]].
 *  Reuse it to run the same program repeatedly, e.g.
 *  {{{
 *  val git = lib.shell.command("git")
 *  git.execute("add", "-A")
 *  git.execute("commit", "-m", "wip")
 *  }}}
 */
trait ShellCommand:
  /** Run the program with these literal arguments. Arguments are passed verbatim
   *  (no shell parsing/quoting/globbing) — `execute("a b")` is a single argument
   *  containing a space. Use [[Shell.sh]] when you need shell features. */
  def execute(args: String*): CommandResult

/** Runs external programs. Reach it via `lib.shell`.
 *
 *  Run a single program with literal arguments via [[run]] (one-shot) or
 *  [[command]] (a reusable, validated handle). Arguments are passed verbatim,
 *  with no shell parsing. File operations are deliberately *not* available here —
 *  programs like `rm`, `ls`, `mv`, `mkdir`, `cat`, and `touch` are rejected; use
 *  the file-system interface ([[FileSystem]] / [[Path]]) instead, which is safer
 *  and gives you structured results.
 *
 *  By default commands run in the current working directory ([[FileSystem.cwd]]);
 *  use [[at]] to root the shell elsewhere and [[withTimeout]] to change the kill
 *  deadline.
 */
trait Shell:
  /** A reusable handle for the program `name`. Throws if the program is not
   *  permitted (a file-operation program — use [[FileSystem]] / [[Path]]). */
  def command(name: String): ShellCommand
  /** Run `name` with literal `args` and return the captured result. Shorthand for
   *  `command(name).execute(args*)`, with the same validation. For example:
   *  `lib.shell.run("git", "status", "--short")`. */
  def run(name: String, args: String*): CommandResult
  def sh(commandLine: String): CommandResult
  /** A shell rooted at `dir` (a relative `dir` resolves against [[FileSystem.cwd]]);
   *  carries the same policy and timeout. */
  def at(dir: Path): Shell
  /** A shell whose commands are killed after `ms` milliseconds (default two
   *  minutes). Returns a new shell; this one is unchanged. */
  def withTimeout(ms: Int): Shell

/** The runtime interface for Auk agents. */
trait AukInterface:
  /** Constructor for path. */
  def Path(p: String): Path
  /** File system API. */
  val fs: FileSystem
  /** Shell / external-process API. */
  val shell: Shell
