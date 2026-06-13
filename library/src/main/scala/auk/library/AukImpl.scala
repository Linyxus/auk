package auk.library

import scala.scalajs.js
import scala.util.matching.Regex

/** Node `fs`/`path` modules, reached through the REPL-injected global require. */
private object Node:
  def fs: js.Dynamic = js.Dynamic.global.require("node:fs")
  def path: js.Dynamic = js.Dynamic.global.require("node:path")

private def jsArrToList[A](arr: js.Array[A]): List[A] =
  (0 until arr.length).toList.map(i => arr(i))

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
  def asFile: FsFile = FsFileImpl(raw)
  def asDir: FsDir = FsDirImpl(raw)
  def asEntry: FsEntry =
    try
      if Node.fs.statSync(raw).isDirectory().asInstanceOf[Boolean] then FsDirImpl(raw)
      else FsFileImpl(raw)
    catch case _: Throwable => FsFileImpl(raw)

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
    dest.asEntry

  def copyTo(dest: Path): FsEntry =
    Node.fs.cpSync(raw, dest.toString, js.Dynamic.literal(recursive = true))
    dest.asEntry

  override def toString: String = raw

/** A file handle. */
private final class FsFileImpl(val raw: String) extends FsFile with EntryOps:
  def rawContent: String = Node.fs.readFileSync(raw, "utf8").asInstanceOf[String]

  def lines: List[String] =
    val c = rawContent
    if c.isEmpty then Nil
    else
      val ls = c.split("\n", -1).toList.map(s => if s.endsWith("\r") then s.dropRight(1) else s)
      if ls.nonEmpty && ls.last == "" then ls.init else ls

  def lineCount: Int = lines.length

  def content: String = number(lines.zipWithIndex)
  def slice(from: Int, until: Int): String = number(lines.zipWithIndex.slice(from, until))
  private def number(ls: List[(String, Int)]): String =
    ls.map((l, i) => s"$i@ $l").mkString("\n")

  def size: Long =
    statOpt
      .map(_.size.asInstanceOf[Double].toLong)
      .getOrElse(throw new RuntimeException(s"cannot stat (does it exist?): $raw"))

  def ext: String = Node.path.extname(raw).asInstanceOf[String].stripPrefix(".")

  def grep(pattern: String): List[Match] =
    val re = pattern.r
    lines.zipWithIndex.collect {
      case (line, i) if re.findFirstIn(line).isDefined => MatchImpl(PathImpl(raw), i, line)
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
      if d.isDirectory().asInstanceOf[Boolean] then FsDirImpl(childPath) else FsFileImpl(childPath)
    }

  def entries: List[FsEntry] = childHandles
  def files: List[FsFile] = childHandles.collect { case f: FsFile => f }
  def dirs: List[FsDir] = childHandles.collect { case d: FsDir => d }

  def walk: List[FsEntry] =
    entries.flatMap {
      case d: FsDir => d +: d.walk
      case e        => List(e)
    }

  def glob(pattern: String): List[FsEntry] =
    val re = globToRegex(pattern)
    walk.filter(e => re.matches(relativePath(e.path)))

  def grep(pattern: String): List[Match] =
    walk.collect { case f: FsFile => f }.flatMap(safeGrep(_, pattern))

  def grep(pattern: String, filePattern: String): List[Match] =
    glob(filePattern).collect { case f: FsFile => f }.flatMap(safeGrep(_, pattern))

  def file(name: String): FsFile = FsFileImpl(Node.path.join(raw, name).asInstanceOf[String])
  def dir(name: String): FsDir = FsDirImpl(Node.path.join(raw, name).asInstanceOf[String])

  private def relativePath(p: Path): String =
    Node.path.relative(raw, p.toString).asInstanceOf[String]

  private def safeGrep(f: FsFile, pattern: String): List[Match] =
    try f.grep(pattern)
    catch case _: Throwable => Nil

/** A single grep match; renders as `<path>:<linenum>@ <line>`. */
private final class MatchImpl(val file: Path, val lineNumber: Int, val line: String) extends Match:
  override def toString: String = s"$file:$lineNumber@ $line"

/** The file-system API — a thin facade over [[Path]]'s open methods. */
private final class FileSystemImpl extends FileSystem:
  def cwd: Path = PathImpl(js.Dynamic.global.process.cwd().asInstanceOf[String])
  def access(p: Path): FsEntry = p.asEntry
  def accessFile(p: Path): FsFile = p.asFile
  def accessDir(p: Path): FsDir = p.asDir

/** The [[AukInterface]] implementation preloaded into REPL sessions.
  *
  * A class, not an object: the session preamble (see
  * `auk.runtime.repl.ReplPreamble`) creates the instance, so construction can
  * later carry session-specific state — working directory, policy, handles —
  * without changing evaluated code.
  */
final class AukImpl extends AukInterface:
  def Path(p: String): Path = PathImpl(p)

  val fs: FileSystem = new FileSystemImpl
