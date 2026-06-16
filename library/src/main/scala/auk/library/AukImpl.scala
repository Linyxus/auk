package auk.library

import scala.scalajs.js
import scala.util.matching.Regex

/** Node `fs`/`path`/`child_process`/`os` modules, reached through the
 *  REPL-injected global require. */
private object Node:
  def fs: js.Dynamic = js.Dynamic.global.require("node:fs")
  def path: js.Dynamic = js.Dynamic.global.require("node:path")
  def childProcess: js.Dynamic = js.Dynamic.global.require("node:child_process")
  def os: js.Dynamic = js.Dynamic.global.require("node:os")

private def jsArrToList[A](arr: js.Array[A]): List[A] =
  (0 until arr.length).toList.map(i => arr(i))

/** The Node error `code` (e.g. "ENOENT") carried by a caught exception, or ""
 *  when it is not a JS error with a `code`. */
private def errorCode(t: Throwable): String =
  t match
    case js.JavaScriptException(e) =>
      val code = e.asInstanceOf[js.Dynamic].code
      if code == null || js.isUndefined(code) then "" else code.asInstanceOf[String]
    case _ => ""

/** A short, human-readable detail for an error — its Node `code` if it has one,
 *  otherwise its message. Used to frame raw Node failures in the library's own
 *  vocabulary instead of leaking a bare JS error. */
private def errorDetail(t: Throwable): String =
  val code = errorCode(t)
  if code.nonEmpty then code else Option(t.getMessage).getOrElse("error")

/** Compile `pattern` as a regex, raising a clear error (instead of leaking a raw
 *  regex-engine exception) when it is malformed. */
private def compileRegex(pattern: String): Regex =
  try pattern.r
  catch
    case t: Throwable =>
      throw new RuntimeException(s"grep: invalid regular expression '$pattern': ${errorDetail(t)}")

/** Split file content into lines, treating CRLF, a lone CR, and LF all as line
 *  separators, and dropping the single empty segment a trailing newline creates. */
private def splitLines(c: String): List[String] =
  if c.isEmpty then Nil
  else
    val ls = c.split("\r\n|\r|\n", -1).toList
    if ls.nonEmpty && ls.last == "" then ls.init else ls

/** Translates a glob (`*`, `**`, `?`) into an anchored regex over `/`-separated
 *  relative paths. `**` spans segments (and `**` followed by `/` also matches
 *  zero directories); `*` and `?` stay within a single segment. */
private def globToRegex(glob: String): Regex =
  val sb = new StringBuilder("^")
  val n = glob.length
  var i = 0
  while i < n do
    val c = glob.charAt(i)
    if c == '*' && i + 1 < n && glob.charAt(i + 1) == '*' then
      if i + 2 < n && glob.charAt(i + 2) == '/' then
        sb.append("(?:.*/)?"); i += 3
      else
        sb.append(".*"); i += 2
    else if c == '*' then
      sb.append("[^/]*"); i += 1
    else if c == '?' then
      sb.append("[^/]"); i += 1
    else
      if "\\.+()[]{}^$|".indexOf(c.toInt) >= 0 then sb.append('\\')
      sb.append(c); i += 1
  sb.append('$')
  sb.toString.r

/** A file-system path, backed by Node's `path` module.
  *
  * Pure value: constructing one never touches the file system, so a `PathImpl`
  * may name something that does not exist. `/` joins (and normalizes via
  * `path.join`); equality and `toString` are by the path string, so paths
  * render readably in the REPL and compare by value.
  */
final class PathImpl(val raw: String) extends Path:
  def / (sub: String): Path = PathImpl(Node.path.join(raw, sub).asInstanceOf[String])
  def baseName: String = Node.path.basename(raw).asInstanceOf[String]
  def parent: Path = PathImpl(Node.path.dirname(raw).asInstanceOf[String])
  def openAsFile: FsFile = FsFileImpl(raw)
  def openAsDir: FsDir = FsDirImpl(raw)
  def openAsEntry: FsEntry =
    try
      if Node.fs.statSync(raw).isDirectory().asInstanceOf[Boolean] then FsDirImpl(raw)
      else FsFileImpl(raw)
    catch
      // A missing path is the normal case (the caller wants a handle to a path
      // that need not exist) — hand back a file handle. But a real failure
      // (EACCES/ELOOP/ENOTDIR/…) must not be masked as "it's a file": surface it.
      case t: Throwable if errorCode(t) == "ENOENT" => FsFileImpl(raw)
      case t: Throwable => throw new RuntimeException(s"cannot open (${errorDetail(t)}): $raw")

  override def toString: String = raw
  override def equals(other: Any): Boolean = other match
    case that: PathImpl => that.raw == raw
    case _              => false
  override def hashCode: Int = raw.hashCode

/** Shared [[FsEntry]] behaviour for the file and directory handles. Deliberately
  * not an `FsEntry` subtype (that trait is sealed and lives in another file);
  * mixed into the impls below, whose abstract `FsEntry` members it satisfies.
  */
private trait EntryOps:
  protected def raw: String

  protected def statOpt: Option[js.Dynamic] =
    try Some(Node.fs.statSync(raw)) catch case _: Throwable => None

  def path: Path = PathImpl(raw)
  def name: String = Node.path.basename(raw).asInstanceOf[String]
  def parent: FsDir = FsDirImpl(Node.path.dirname(raw).asInstanceOf[String])
  def exists: Boolean = Node.fs.existsSync(raw).asInstanceOf[Boolean]
  def isFile: Boolean = statOpt.exists(_.isFile().asInstanceOf[Boolean])
  def isDir: Boolean = statOpt.exists(_.isDirectory().asInstanceOf[Boolean])

  def lastModifiedMs: Long =
    statOpt
      .map(_.mtimeMs.asInstanceOf[Double].toLong)
      .getOrElse(throw new RuntimeException(s"cannot stat (does it exist?): $raw"))

  def lastModified: String =
    val d = new js.Date(lastModifiedMs.toDouble)
    def p2(x: Int): String = if x < 10 then s"0$x" else x.toString
    s"${d.getFullYear().toInt}-${p2(d.getMonth().toInt + 1)}-${p2(d.getDate().toInt)} " +
      s"${p2(d.getHours().toInt)}:${p2(d.getMinutes().toInt)}:${p2(d.getSeconds().toInt)}"

  def delete(): Unit =
    Node.fs.rmSync(raw, js.Dynamic.literal(recursive = true, force = true))

  def moveTo(dest: Path): FsEntry =
    Node.fs.renameSync(raw, dest.toString)
    dest.openAsEntry

  def copyTo(dest: Path): FsEntry =
    val src = Node.path.resolve(raw).asInstanceOf[String]
    val dst = Node.path.resolve(dest.toString).asInstanceOf[String]
    if src == dst then dest.openAsEntry // copy onto self: a no-op, like moveTo/delete
    else
      try Node.fs.cpSync(raw, dest.toString, js.Dynamic.literal(recursive = true))
      catch
        case t: Throwable =>
          throw new RuntimeException(s"copyTo failed ($raw -> ${dest.toString}): ${errorDetail(t)}")
      dest.openAsEntry

  override def toString: String = raw

/** A file handle. */
private final class FsFileImpl(val raw: String) extends FsFile with EntryOps:
  def rawContent: String = Node.fs.readFileSync(raw, "utf8").asInstanceOf[String]

  def lines: List[String] = splitLines(rawContent)

  def lineCount: Int = lines.length

  def read(offset: Int = 1, limit: Int = -1): Unit =
    val ls = lines.zipWithIndex.map((l, i) => (l, i + 1)) // 1-based line numbers
    val from = math.max(offset, 1)
    val windowed = ls.dropWhile(_._2 < from)
    val selected = if limit < 0 then windowed else windowed.take(limit)
    if selected.nonEmpty then println(selected.map((l, n) => s"$n@ $l").mkString("\n"))

  def size: Long =
    statOpt
      .map(_.size.asInstanceOf[Double].toLong)
      .getOrElse(throw new RuntimeException(s"cannot stat (does it exist?): $raw"))

  def ext: String = Node.path.extname(raw).asInstanceOf[String].stripPrefix(".")

  def grep(pattern: String): List[Match] = grepIn(lines, compileRegex(pattern))

  /** Grep already-split lines with an already-compiled regex — shared by the
   *  public `grep` and by directory-wide grep (which compiles the pattern once
   *  and reads each file's content a single time). */
  private[library] def grepIn(ls: List[String], re: Regex): List[Match] =
    ls.zipWithIndex.collect {
      case (line, i) if re.findFirstIn(line).isDefined => MatchImpl(PathImpl(raw), i + 1, line)
    }

  def replace(oldStr: String, newStr: String): Unit =
    val c = rawContent
    occurrences(c, oldStr) match
      case 1 => write(c.replace(oldStr, newStr))
      case 0 => throw new RuntimeException(s"replace: no occurrence of the target string in $raw")
      case k =>
        throw new RuntimeException(
          s"replace: expected exactly one occurrence but found $k in $raw; " +
            "make the target string more specific"
        )

  def replaceAll(oldStr: String, newStr: String): Int =
    val c = rawContent
    val k = occurrences(c, oldStr)
    if k > 0 then write(c.replace(oldStr, newStr))
    k

  def write(content: String): Unit =
    Node.fs.writeFileSync(raw, content, "utf8")

  def append(content: String): Unit =
    Node.fs.appendFileSync(raw, content, "utf8")

  def touch(): Unit =
    if !exists then write("")

  private def occurrences(haystack: String, needle: String): Int =
    if needle.isEmpty then 0
    else
      var count = 0
      var idx = haystack.indexOf(needle)
      while idx >= 0 do
        count += 1
        idx = haystack.indexOf(needle, idx + needle.length)
      count

/** A directory handle. */
private final class FsDirImpl(val raw: String) extends FsDir with EntryOps:
  def makedir(): Unit =
    Node.fs.mkdirSync(raw, js.Dynamic.literal(recursive = true))

  private def childHandles: List[FsEntry] =
    val arr = Node.fs
      .readdirSync(raw, js.Dynamic.literal(withFileTypes = true))
      .asInstanceOf[js.Array[js.Dynamic]]
    jsArrToList(arr).map { d =>
      val childPath = Node.path.join(raw, d.name).asInstanceOf[String]
      if isDirEntry(d, childPath) then FsDirImpl(childPath) else FsFileImpl(childPath)
    }

  /** Whether a `readdirSync` Dirent denotes a directory, *following* symlinks: a
   *  regular dir entry is a directory; a symlink is resolved with `statSync` (a
   *  broken or cyclic link resolves to "not a directory" → a file handle). Plain
   *  files and dirs cost no extra syscall — only symlinks are re-stat'd. */
  private def isDirEntry(d: js.Dynamic, childPath: String): Boolean =
    if d.isDirectory().asInstanceOf[Boolean] then true
    else if d.isSymbolicLink().asInstanceOf[Boolean] then
      try Node.fs.statSync(childPath).isDirectory().asInstanceOf[Boolean]
      catch case _: Throwable => false
    else false

  def entries: List[FsEntry] = childHandles
  def files: List[FsFile] = childHandles.collect { case f: FsFile => f }
  def dirs: List[FsDir] = childHandles.collect { case d: FsDir => d }

  def walk: List[FsEntry] =
    // Follow directory symlinks, but guard against cycles: refuse to descend a
    // directory whose canonical path already appears among the ancestors on the
    // current branch. A symlink to a *non-ancestor* directory is still traversed
    // (its contents listed under the link); only a true loop is cut, so `walk`
    // always terminates.
    def realOf(p: String): String =
      try Node.fs.realpathSync(p).asInstanceOf[String]
      catch case _: Throwable => p
    def go(d: FsDirImpl, ancestors: Set[String]): List[FsEntry] =
      val real = realOf(d.raw)
      if ancestors.contains(real) then Nil
      else
        val next = ancestors + real
        d.entries.flatMap {
          case sub: FsDirImpl => sub +: go(sub, next)
          case e              => List(e)
        }
    go(this, Set.empty)

  def glob(pattern: String): List[FsEntry] =
    val re = globToRegex(pattern)
    walk.filter(e => re.matches(relativePath(e.path)))

  def grep(pattern: String): List[Match] =
    val re = compileRegex(pattern)
    walk.collect { case f: FsFileImpl => f }.flatMap(safeGrep(_, re))

  def grep(pattern: String, filePattern: String): List[Match] =
    val re = compileRegex(pattern)
    glob(filePattern).collect { case f: FsFileImpl => f }.flatMap(safeGrep(_, re))

  def file(name: String): FsFile = FsFileImpl(Node.path.join(raw, name).asInstanceOf[String])
  def dir(name: String): FsDir = FsDirImpl(Node.path.join(raw, name).asInstanceOf[String])

  private def relativePath(p: Path): String =
    Node.path.relative(raw, p.toString).asInstanceOf[String]

  /** Grep one file for an already-compiled regex, reading its content once.
   *  Binary files (containing a NUL byte) are skipped rather than searched as
   *  mojibake, and any per-file I/O error is swallowed so one unreadable file
   *  does not abort a directory-wide grep. (An invalid pattern is rejected up
   *  front by the caller, so a bad regex is never silently swallowed here.) */
  private def safeGrep(f: FsFileImpl, re: Regex): List[Match] =
    try
      val c = f.rawContent
      if c.contains('\u0000') then Nil
      else f.grepIn(splitLines(c), re)
    catch case _: Throwable => Nil

/** A single grep match; renders as `<path>:<linenum>@ <line>`. */
private final class MatchImpl(val file: Path, val lineNumber: Int, val line: String) extends Match:
  override def toString: String = s"$file:$lineNumber@ $line"

/** The file-system API — a thin facade over [[Path]]'s open methods. */
private final class FileSystemImpl extends FileSystem:
  def cwd: Path = PathImpl(js.Dynamic.global.process.cwd().asInstanceOf[String])
  def access(p: Path): FsEntry = p.openAsEntry
  def accessFile(p: Path): FsFile = p.openAsFile
  def accessDir(p: Path): FsDir = p.openAsDir

/** Shared shell runner over Node's synchronous `child_process.spawnSync`, plus
 *  the static program policy. Synchronous to match the `*Sync` fs calls — the
 *  REPL worker can block while the child runs. */
private object Sh:
  /** Timeout applied when the caller does not change it (two minutes). */
  val DefaultTimeoutMs = 120_000
  /** Cap on captured output, mirroring the old bash tool's bound (~10 MB). */
  val MaxBuffer = 10 * 1024 * 1024
  /** Programs that must go through the file-system interface instead. */
  val Forbidden = Set("rm", "ls", "mkdir", "mv", "cat", "touch")

  /** The final path segment, so `/bin/rm` and `./rm` are caught like `rm`. */
  def baseName(program: String): String =
    Node.path.basename(program).asInstanceOf[String]

  /** Resolve a working directory the way `at` does: a relative `dir` against
   *  `base`, an absolute `dir` standing on its own (Node `path.resolve`). */
  def resolveCwd(base: String, dir: Path): String =
    Node.path.resolve(base, dir.toString).asInstanceOf[String]

  /** Return `ms` if it is a usable kill deadline, else raise a clear error —
   *  shared by every `withTimeout` so a non-positive deadline is rejected up
   *  front rather than silently disabling the timeout. */
  def checkTimeout(ms: Int): Int =
    if ms <= 0 then throw new RuntimeException(s"withTimeout: timeout must be positive; got $ms")
    ms

  /** Run `program` with literal `args` in `cwd`, killed after `timeoutMs`.
   *  Never throws on a non-zero exit; encodes everything into a CommandResult. */
  def exec(program: String, args: Seq[String], cwd: String, timeoutMs: Int): CommandResult =
    val opts = js.Dynamic.literal(
      cwd = cwd,
      timeout = timeoutMs,
      encoding = "utf8",
      input = "", // empty stdin: readers see EOF instead of hanging
      maxBuffer = MaxBuffer
    )
    val res = Node.childProcess.spawnSync(program, js.Array(args*), opts)
    val stdout = strOr(res.stdout)
    val stderr = strOr(res.stderr)
    val err = res.error
    val errCode = if isNullish(err) then "" else strOr(err.code)
    val timedOut = errCode == "ETIMEDOUT"
    val status = res.status
    val signal = res.signal
    val exitCode =
      if !isNullish(status) then status.asInstanceOf[Int]
      else if timedOut then 124 // conventional "killed by timeout" code
      else if errCode == "ENOENT" then 127 // command not found
      else if !isNullish(signal) then 128 + signalNumber(strOr(signal)) // killed by a signal
      else 1
    CommandResult(stdout, stderr, exitCode, timedOut)

  private def isNullish(v: js.Dynamic): Boolean = v == null || js.isUndefined(v)
  private def strOr(v: js.Dynamic): String = if isNullish(v) then "" else v.asInstanceOf[String]

  /** A signal name (e.g. "SIGKILL") mapped to its number via node:os, so a
   *  signal-killed child reports the conventional `128 + N` exit code (137 for
   *  SIGKILL). An unknown signal falls back to 0 (→ exit 128). */
  private def signalNumber(name: String): Int =
    try
      val n = Node.os.constants.signals.selectDynamic(name)
      if isNullish(n) then 0 else n.asInstanceOf[Int]
    catch case _: Throwable => 0

/** A shell rooted at `cwd` with a kill deadline of `timeoutMs`. */
private final class ShellImpl(cwd: String, timeoutMs: Int) extends Shell:
  def command(name: String): ShellCommand =
    if Sh.Forbidden.contains(Sh.baseName(name)) then
      throw new RuntimeException(
        s"shell: '$name' is not permitted — use the file-system interface " +
          "(lib.fs / lib.path) for file operations instead"
      )
    ShellCommandImpl(name, cwd, timeoutMs)

  def run(name: String, args: String*): CommandResult = command(name).execute(args*)

  def sh(commandLine: String): CommandResult =
    Sh.exec("/bin/sh", List("-c", commandLine), cwd, timeoutMs)

  def at(dir: Path): Shell = ShellImpl(Sh.resolveCwd(cwd, dir), timeoutMs)

  def withTimeout(ms: Int): Shell = ShellImpl(cwd, Sh.checkTimeout(ms))

/** A validated handle to a single program, rooted at `cwd` with a kill deadline
 *  of `timeoutMs`. Immutable: [[at]] and [[withTimeout]] derive fresh handles,
 *  reusing the shell runner's resolve/validation so they behave exactly like
 *  their [[ShellImpl]] counterparts. The program was already checked against the
 *  policy by [[ShellImpl.command]], so re-rooting it never needs re-validation. */
private final class ShellCommandImpl(program: String, cwd: String, timeoutMs: Int) extends ShellCommand:
  def execute(args: String*): CommandResult = Sh.exec(program, args, cwd, timeoutMs)
  def at(dir: Path): ShellCommand = ShellCommandImpl(program, Sh.resolveCwd(cwd, dir), timeoutMs)
  def withTimeout(ms: Int): ShellCommand = ShellCommandImpl(program, cwd, Sh.checkTimeout(ms))

/** The [[AukInterface]] implementation preloaded into REPL sessions.
  *
  * A class, not an object: the session preamble (see
  * `auk.runtime.repl.ReplPreamble`) creates the instance, so construction can
  * later carry session-specific state — working directory, policy, handles —
  * without changing evaluated code.
  */
final class AukImpl extends AukInterface:
  def path(p: String): Path = PathImpl(p)

  val fs: FileSystem = new FileSystemImpl

  val shell: Shell =
    new ShellImpl(js.Dynamic.global.process.cwd().asInstanceOf[String], Sh.DefaultTimeoutMs)

  val wf: Workflow = new Workflow()
