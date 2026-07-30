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
   *  prefixed with its address in the format `<linenum>#<hash>@ <line content>`.
   *  For example, a file holding
   *  {{{
   *  def greet =
   *    println("hi")
   *  }}}
   *  prints as:
   *  {{{
   *  1#ym@ def greet =
   *  2#n2@   println("hi")
   *  }}}
   *  By default the whole file is printed; pass `offset` (the 1-based line to
   *  start at) and `limit` (the maximum number of lines to print, or `-1` for
   *  "to the end") to read a window of a large file — e.g. `read(100, 40)`
   *  prints 40 lines starting at line 100. Line numbers stay absolute, so a
   *  window reports the file's true line numbers.
   *
   *  Reading is also how lines become addressable: `N#hh` is the token [[patch]]
   *  and [[insertAfter]] take, quoted back whole. The number is where the line sat
   *  when you saw it and the hash names what it held, so the token keeps working
   *  as the file moves under it — after your own patches, after a later read of
   *  some other window, after an edit from outside. Reads and edits interleave
   *  freely, and nothing has to be re-read to stay valid.
   *
   *  Tokens belong to the file, not to this handle: read it through one handle and
   *  patch it through any other. What you send back as content is the line text
   *  alone — the `N#hh@ ` prefix belongs to this display, never to the file.
   *
   *  A file holding a NUL byte is binary, and binary files are refused: the call
   *  fails and prints nothing, as does every operation that decodes the content
   *  as text — [[rawContent]], [[lines]], [[lineCount]], [[patch]] and
   *  [[insertAfter]]. [[size]], [[ext]] and [[write]] work regardless, and a
   *  directory grep skips binary files silently. */
  def read(offset: Int = 1, limit: Int = -1): Unit
  /** Reads the full raw content of the file, with no line-number prefixes.
   *  Refuses binary files (any NUL byte) rather than decoding them lossily —
   *  the guard every text-reading operation on this handle shares. */
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
   *  returning them as a [[GrepResult]] (line numbers are 1-based). Call
   *  `.display()` on the result to print the matching lines, `.matches` to work
   *  with them as a `List[Match]`. */
  def grep(pattern: String): GrepResult
  /** Replaces the lines from `fromRef` to `toRef` inclusive with `text` — the way
   *  to edit a file.
   *
   *  Line references are the `N#hh` tokens a [[read]] printed, quoted back whole:
   *  `patch("65#xy", "68#qp", "...")`. The number says where the line was; the
   *  hash says what it held. The hash is what counts — before anything is written
   *  the runtime looks for that exact text in the file as it is NOW, so a line
   *  that has drifted up or down since you saw it is still found, whether it moved
   *  because of your own earlier patch or because something else edited the file.
   *  A bare number is not an address and is refused.
   *  {{{
   *  val f = lib.path("Server.scala").openAsFile
   *  f.read()                                    // 40#7k@   def start(): Unit =
   *  f.patch("40#7k", "  def start(port: Int): Unit =")
   *  }}}
   *  Nothing is ever patched on a guess. If the text you addressed is gone, or the
   *  file now holds it in several places with nothing to tell them apart, the patch
   *  is refused and prints what the file holds there instead — with fresh tokens,
   *  so the refusal itself is the recovery. A range must be one span you saw in a
   *  single read, and it moves as a block: the whole span has to still be in the
   *  file, contiguous and entire.
   *
   *  `text` is the exact new content of the region, lines separated by newlines;
   *  `""` deletes the region outright. One trailing newline is ignored, so `"a\nb"`
   *  and `"a\nb\n"` are the same two lines. Only the addressed lines are rewritten
   *  — every other byte is left exactly as it was, the line endings it uses
   *  included — so a one-line patch stays a one-line `git diff`.
   *
   *  What lands is PRINTED with fresh `N#hh` tokens of its own, so you can patch it
   *  again immediately without re-reading. A long replacement is echoed head and
   *  tail with its middle elided, and an elided line was never shown — so it has no
   *  token and cannot be addressed until you [[read]] it. That is the rule
   *  throughout: what you have been shown is what you can address. The returned
   *  `String` is the one-line summary, so patching a list of files collects a
   *  readable log. */
  def patch(fromRef: String, toRef: String, text: String): String
  /** Replaces the single line `ref` names with `text` — `patch(ref, ref, text)`.
   *  See [[patch(fromRef:String, toRef:String, text:String)*]] for how references
   *  work. */
  def patch(ref: String, text: String): String
  /** Inserts `text` after the line `ref` names; `"0"` puts it at the top of the
   *  file. References, payload and the printed echo work exactly as in [[patch]] —
   *  the line you name is located by the content you were shown, so an insertion
   *  point survives edits elsewhere in the file. */
  def insertAfter(ref: String, text: String): String
  /** Writes content to this file. If the file exists, this *overwrites* the existing content.
   *  If the file does not exist yet, this creates the file and writes the content.
   *  However, if the parent directory does not exist yet, this will fail.
   *  Rewriting a file wholesale retires the tokens you were shown for it — nothing
   *  of the old content describes the new — so [[patch]] wants a fresh [[read]]
   *  afterwards. */
  def write(content: String): Unit
  /** Appends `content` to the end of the file, creating the file first if it
   *  does not exist. The parent directory must already exist. Tokens you were
   *  already shown keep working: appending leaves every line above it untouched. */
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
   *  subdirectories at any depth. Like [[walk]], this skips `.git` and entries
   *  excluded by `.gitignore` files found under this directory; use [[globAll]]
   *  to match those too. Returns a [[GlobResult]] — call `.display()` on it to
   *  print the paths, `.entries` for the `List[FsEntry]`. */
  def glob(pattern: String): GlobResult
  /** Recursively searches the text content of every file beneath this directory
   *  for lines matching the regular expression `pattern`, returning them as a
   *  [[GrepResult]] (call `.display()` to print the matching lines, `.matches`
   *  for the `List[Match]`). Skips `.git` and files excluded by `.gitignore`
   *  files found under this directory; this directory itself is always
   *  searched, even if some outer tree ignores it. Use [[grepAll]] to search
   *  everything. */
  def grep(pattern: String): GrepResult
  /** Like [[grep]], but searches only files whose path matches the glob
   *  `filePattern` (same syntax as [[glob]]), e.g. `dir.grep("TODO", "*.md")`
   *  to scan Markdown files. Prunes `.git` and `.gitignore`d paths like
   *  [[grep]]. */
  def grep(pattern: String, filePattern: String): GrepResult
  /** A handle to the child file named `name` in this directory; the file need
   *  not exist. Shorthand for `(path / name).openAsFile`. */
  def file(name: String): FsFile
  /** A handle to the child subdirectory named `name`; it need not exist.
   *  Shorthand for `(path / name).openAsDir`. */
  def dir(name: String): FsDir
  /** Every descendant entry, recursively: the whole subtree beneath this
   *  directory, not including the directory itself. Skips `.git` and entries
   *  excluded by `.gitignore` files found under this directory; use [[walkAll]]
   *  to include them.
   *  Avoid doing this unless the directory is a small, well-contained one,
   *  since the result of walking a folder can be huge. */
  def walk: List[FsEntry]
  /** Like [[walk]], but searches everything — no ignore rules, no `.git` skip. */
  def walkAll: List[FsEntry]
  /** Like [[glob]], but searches everything — no ignore rules, no `.git` skip. */
  def globAll(pattern: String): GlobResult
  /** Like [[grep]], but searches everything — no ignore rules, no `.git` skip. */
  def grepAll(pattern: String): GrepResult
  /** Like [[grep]] with a `filePattern`, but searches everything — no ignore
   *  rules, no `.git` skip. */
  def grepAll(pattern: String, filePattern: String): GrepResult

/** A single matching line produced by `grep` (see [[FsFile.grep]] and
 *  [[FsDir.grep]]). Renders as `<path>:<linenum>@ <line>`. */
trait Match:
  /** The file the matching line was found in — an [[FsFile]] handle you can read,
   *  grep, or edit directly (its [[FsFile.path]] gives the location). */
  def file: FsFile
  /** The 1-based line number of the match within [[file]]. */
  def lineNumber: Int
  /** The full text of the matching line. */
  def line: String

/** What a `grep` found — see [[FsFile.grep]] and [[FsDir.grep]].
 *
 *  A search over a large tree can match hundreds of thousands of lines, so the
 *  matches are not built up front and evaluating a result never dumps them:
 *  the REPL renders it as a one-line summary, e.g.
 *  `GrepResult(2566 matches — use .display() or .matches)`. From there:
 *
 *   - to LOOK at the matches, call [[display]] — it prints a window of them and
 *     says how many it left out;
 *   - to WORK with them (map, filter, edit the files), call [[matches]] for the
 *     full `List[Match]`. That call is the one that pays to build the list, so
 *     reach for [[length]] or [[display]] when you only need to look.
 *
 *  {{{
 *  val hits = lib.fs.cwd.grep("TODO", "*.scala")
 *  hits.length                             // how many lines matched
 *  hits.display()                          // print the first 200
 *  hits.display(200)                       // print the next 200
 *  hits.matches.map(_.file.name).distinct  // which files they are in
 *  }}}
 */
trait GrepResult:
  /** Prints a window of the matches, one per line as `<path>:<linenum>@ <line>`
   *  (the same rendering as a [[Match]] itself).
   *
   *  By default the first 200 matches are printed; pass `offset` (how many to
   *  skip) and `limit` (how many to print at most, or a negative number for "to
   *  the end") to move the window — e.g. `display(200)` prints the matches
   *  201-400. Whatever the window leaves out is reported rather than silently
   *  dropped: `(... 200 matches skipped ...)` above and
   *  `(... 1841 more matches ...)` below. A result with nothing in it prints
   *  `(no matches)`. */
  def display(offset: Int = 0, limit: Int = 200): Unit
  /** How many lines matched. Cheap: reading it does not build the matches. */
  def length: Int
  /** True when nothing matched. */
  def isEmpty: Boolean
  /** True when at least one line matched. */
  def nonEmpty: Boolean
  /** Every match as a `List[Match]`, in search order — files in walk order, a
   *  file's own matches in line order. Building this list is the expensive part
   *  of a big search; it happens on the first call and is then cached, so asking
   *  twice costs nothing extra, but skip it entirely when [[length]] or
   *  [[display]] already answers the question. */
  def matches: List[Match]

/** What a `glob` found — see [[FsDir.glob]] and [[FsDir.globAll]].
 *
 *  Behaves like [[GrepResult]], over matching entries instead of matching
 *  lines: evaluating it renders a one-line summary
 *  (`GlobResult(37 entries — use .display() or .entries)`), [[display]] prints
 *  the paths, and [[entries]] builds the `List[FsEntry]` when you need the
 *  handles themselves. */
trait GlobResult:
  /** Prints a window of the matched paths, one per line, directories with a
   *  trailing `/`.
   *
   *  Windowing works exactly as in [[GrepResult.display]]: `offset` entries are
   *  skipped, at most `limit` are printed (negative means "to the end"), and
   *  what is left out is reported as `(... 200 entries skipped ...)` /
   *  `(... 37 more entries ...)`. An empty result prints `(no entries)`. */
  def display(offset: Int = 0, limit: Int = 200): Unit
  /** How many entries matched. Cheap: reading it does not build the handles. */
  def length: Int
  /** True when nothing matched. */
  def isEmpty: Boolean
  /** True when at least one entry matched. */
  def nonEmpty: Boolean
  /** Every matched entry as a `List[FsEntry]`, in walk order. Built on the first
   *  call and cached afterwards; prefer [[length]] / [[display]] when you do not
   *  need the handles. */
  def entries: List[FsEntry]

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

/** A team member's status, as observed through the roster mirror.
 *
 *   - [[MemberStatus.Idle]]: the member has finished its last turn and is waiting
 *     for the next message.
 *   - [[MemberStatus.Working]]: the member is currently running a turn.
 *   - [[MemberStatus.Retired]]: the lead has retired the member ([[Member.retire]]);
 *     it runs no further turns and rejects messages, but its record — description
 *     and [[Member.lastResponse]] — stays readable.
 *   - [[MemberStatus.Lead]]: not tracked — this is the lead handle, which has no
 *     working/idle state of its own.
 */
enum MemberStatus:
  case Idle, Working, Retired, Lead

/** A handle to one participant in the agent team — either a member (one the lead
 *  created, or one you were told about) or the lead. Handles are thin: they hold
 *  only the id and read the rest through the shared roster mirror at call time, so a
 *  handle stashed in a `val` reflects the member's *current* [[status]] and
 *  [[lastResponse]] in a later eval (the mirror refreshes between evals — see
 *  [[Team]]). Prints as `Member(<id>: <description>, <status>)`.
 *
 *  A handle outlives the member: after [[retire]] it still reports the member's
 *  [[description]] and [[lastResponse]], its [[status]] reads
 *  [[MemberStatus.Retired]], and only [[sendMessage]] fails. */
trait Member:
  /** This member's id — the stable name it was created with (or `"lead"`). */
  def id: String
  /** This member's one-line description (what it is responsible for). For the lead
   *  handle, a fixed label. */
  def description: String
  /** This member's current [[MemberStatus]] as of the last mirror refresh: `Working`
   *  while it runs a turn, `Idle` when waiting, `Retired` once the lead has retired
   *  it, `Lead` for the lead handle. Reflects the state observed *between* evals, so
   *  do not spin on it inside one eval. */
  def status: MemberStatus
  /** This member's final message from its most recently completed turn. The idle
   *  system notice announces that a turn ended but does NOT carry the response —
   *  this accessor is how the response is read, in a later eval once the notice
   *  has arrived. Survives [[retire]]: a retired member's last answer stays
   *  readable. Throws [[IllegalStateException]] if this is the lead handle, or
   *  if the member has not completed a turn yet. */
  def lastResponse: String
  /** Send this member a message, asynchronously. Returns immediately — the member
   *  runs the message on its own; you do NOT await a reply here. When it finishes the
   *  turn it goes idle and the lead receives a short system notice; the response
   *  itself is read from [[lastResponse]]. Throws [[IllegalArgumentException]] if
   *  the target is yourself or the text is empty, and [[IllegalStateException]] if
   *  this member has been retired. */
  def sendMessage(text: String): Unit
  /** Retire this member (LEAD ONLY): shut it down for good. The turn it is running
   *  is cancelled, messages still queued for it are dropped, and its worker is
   *  closed; it runs nothing further and [[sendMessage]] to it fails from here on.
   *  Its record survives — [[description]] and [[lastResponse]] stay readable,
   *  [[status]] becomes [[MemberStatus.Retired]], and the id stays reserved for the
   *  rest of the session, so [[Team.newMember]] can never reuse it.
   *
   *  Retire a member once its job is done: it takes the member out of play and frees
   *  its worker. Throws [[IllegalStateException]] if this is the lead handle (the
   *  lead cannot be retired), if you are not the lead, or if this member has already
   *  been retired. */
  def retire(): Unit

/** The agent-team entry point, reached as `team` in scope. See [[AukInterface.team]]
 *  for the overview.
 *
 *  The team is a set of long-lived agents. The lead (the main agent) creates members
 *  with [[newMember]]; everyone exchanges asynchronous messages via
 *  [[Member.sendMessage]]. Nothing here blocks: sends are fire-and-forget, and an
 *  idle notice reaches the lead on its own when a member finishes its turn — the
 *  reply itself is then read from [[Member.lastResponse]]. When a member's job is
 *  done the lead retires it with [[Member.retire]]: it stops running and rejects
 *  messages, but stays in the roster with its [[Member.lastResponse]] readable and
 *  its id reserved.
 *
 *  Reads go through a local roster mirror the host pushes to this worker. The mirror
 *  advances only *between* evals (the worker services the socket while idle), so a
 *  member's [[Member.status]]/[[Member.lastResponse]] reflect the last refresh —
 *  observe changes in a *later* eval, never by looping inside one.
 *
 *  In a context with no team (e.g. a workflow-node REPL), any operation
 *  throws a clear "team unavailable" error. */
trait Team:
  /** Create a new team member (LEAD ONLY) and return a handle to it. The member is a
   *  fresh, long-lived agent identified by `id` with the given `description`; it
   *  starts idle and runs whenever it is sent a message. Send it its first task with
   *  `handle.sendMessage(...)`.
   *
   *  `id` must be non-empty and use only letters, digits, `-` and `_`; it is
   *  permanent for the session, so choose a short, stable name. Throws:
   *    - [[IllegalStateException]] if you are not the lead;
   *    - [[IllegalArgumentException]] for an invalid id, the reserved id `"lead"`, a
   *      duplicate id, or an empty description. */
  def newMember(id: String, description: String): Member
  /** The handle for `id`. `"lead"` resolves to the lead handle. Throws
   *  [[IllegalArgumentException]] for an unknown member id. */
  def getMember(id: String): Member
  /** Every participant: the lead first, then the members in creation order. */
  def listMembers: List[Member]
  /** The lead handle, for a member to message the lead
   *  (`team.lead.sendMessage(...)`). Throws [[IllegalStateException]] if you ARE the
   *  lead (you would be messaging yourself). */
  def lead: Member

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
   *  sub-agents, grouped for the live UI. Reached as `wf` in scope. The tool for
   *  work whose STRUCTURE is the point — a typed fan-out over many items, dependency
   *  pipelines and joins, a final synthesis — built from anonymous sub-agents that
   *  are disposable once their results land; for a single delegate, or a collaborator
   *  whose context is worth returning to, see [[team]]. Entry point
   *  is [[Workflow.start]]; build the graph with the top-level `group`,
   *  `inGroup`, `agent`, [[Agent.all]] and `Agent.pure` (recurse + `flatMap` +
   *  `Agent.pure` for loops). `wf.start[R]{…}` returns a [[WorkflowRun]] handle
   *  immediately (non-blocking); poll its [[WorkflowRun.status]] in a later eval or
   *  wait for the completion system notice, which carries the resolved `R`. The
   *  handle also drives the run: [[WorkflowRun.pause]] / [[WorkflowRun.resume]]
   *  (resume re-runs it, skipping the finished sub-agents). See [[Workflow]]. */
  def wf: Workflow
  /** Agent team: named collaborator agents, reached as `team` in scope. The tool for
   *  delegation that is talked about rather than composed: one task handed to a fresh
   *  member, an adjacent task handed to the member whose context already covers that
   *  ground, or an exchange whose next message depends on a reply not yet read — where
   *  a graph of many anonymous sub-agents is the point instead, see [[wf]]. The lead
   *  creates members, and everyone exchanges asynchronous messages; an idle notice
   *  reaches the lead AUTOMATICALLY when a member's turn ends (read
   *  [[Member.lastResponse]] for the reply), so never poll for a reply — do other work
   *  (or end the turn) and act when the notice lands. A member whose job is done is
   *  retired with [[Member.retire]], which is what also makes a member the light tool
   *  for a one-off: it then runs nothing further and rejects messages, but its
   *  `lastResponse` stays readable. The full contract, including each method's failure
   *  cases, is on [[Team]] and [[Member]]. */
  def team: Team
