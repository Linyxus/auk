package auk.grep

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.util.matching.Regex

/** auk-grep: the recursive search engine behind the library's `grep`, `glob`,
 *  and `walk`.
 *
 *  Deliberately naive for now — a faithful port of the library's original
 *  tree-walk + per-line regex, so routing through it changed no behavior. It
 *  exists as a seam: `sbt grepBench` races it against ripgrep, and successive
 *  optimizations (ignore-aware pruning, byte-level literal prefiltering,
 *  match-time line indexing) land here without touching the library's API.
 */

/** Node built-ins, reached through the globally-published `require`: injected
 *  by the REPL worker bootstrap in production, and by the build's jsEnvInput
 *  prelude for tests and the benchmark runner. */
private object Node:
  def fs: js.Dynamic = js.Dynamic.global.require("node:fs")
  def path: js.Dynamic = js.Dynamic.global.require("node:path")

/** One matching line: the file's `path` as walked (root-joined), the 1-based
 *  `line` number, and the line's `text` without its terminator. */
final case class Match(path: String, line: Int, text: String)

/** One walked entry: its `path` as walked; `dir` distinguishes directories. */
final case class Entry(path: String, dir: Boolean)

/** Line splitting shared by the engine and the library's `FsFile.lines`. */
object Lines:
  /** Split content into lines, treating CRLF, a lone CR, and LF all as line
   *  separators, and dropping the single empty segment a trailing newline
   *  creates. */
  def split(c: String): List[String] =
    if c.isEmpty then Nil
    else
      val ls = c.split("\r\n|\r|\n", -1).toList
      if ls.nonEmpty && ls.last == "" then ls.init else ls

object Glob:
  /** Translates a glob (`*`, `**`, `?`) into an anchored regex over
   *  `/`-separated relative paths. `**` spans segments (and `**` followed by
   *  `/` also matches zero directories); `*` and `?` stay within a single
   *  segment. */
  def toRegex(glob: String): Regex =
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

object Walker:
  /** Every entry under `root`, pruned: children named `.git` are skipped
   *  unconditionally, and entries matched by `.gitignore` files found from
   *  `root` downward are skipped (an ignored directory is not descended). The
   *  root itself is never tested — an explicitly-named directory is always
   *  walked, even one an outer tree would ignore — and ignore files above the
   *  root are never consulted. Depth-first in readdir order, each surviving
   *  directory before its descendants. Directory symlinks are followed, but a
   *  cycle (an identity already among the branch's ancestors) is not descended,
   *  so `walk` always terminates. */
  def walk(root: String): List[Entry] = collect(root, honorIgnores = true)

  /** Like [[walk]], but with no pruning at all: every entry, including `.git`
   *  and anything a `.gitignore` would exclude. */
  def walkAll(root: String): List[Entry] = collect(root, honorIgnores = false)

  /** The entries under `root` whose `/`-separated path relative to `root`
   *  matches the glob `pattern`, pruned like [[walk]]. */
  def glob(root: String, pattern: String): List[Entry] =
    globCollect(root, pattern, honorIgnores = true)

  /** Like [[glob]], but with no pruning (see [[walkAll]]). */
  def globAll(root: String, pattern: String): List[Entry] =
    globCollect(root, pattern, honorIgnores = false)

  private def collect(root: String, honorIgnores: Boolean): List[Entry] =
    val out = ListBuffer.empty[Entry]
    visit(root, honorIgnores)(out += _)
    out.toList

  private def globCollect(root: String, pattern: String, honorIgnores: Boolean): List[Entry] =
    val re = Glob.toRegex(pattern)
    val out = ListBuffer.empty[Entry]
    visit(root, honorIgnores)(e => if re.matches(relative(root, e.path)) then out += e)
    out.toList

  /** The traversal core shared by [[walk]]/[[glob]] and the search engine:
   *  applies `f` to every surviving entry under `root` in depth-first readdir
   *  order (each directory before its descendants), so a caller can match
   *  during traversal instead of after materializing the whole tree. */
  private[grep] def visit(root: String, honorIgnores: Boolean)(f: Entry => Unit): Unit =
    def go(dir: String, dirId: String, ancestors: Set[String], scopes: List[Scope]): Unit =
      val here =
        if honorIgnores then
          val rules = readIgnore(dir)
          if rules.nonEmpty then scopes :+ Scope(dir, rules) else scopes
        else scopes
      children(dir).foreach { c =>
        val pruned =
          honorIgnores && (c.name == ".git" || ignored(c.path, c.name, c.dir, here))
        if !pruned then
          f(Entry(c.path, c.dir))
          if c.dir then
            val childId = if c.viaSymlink then realOf(c.path) else dirId + "/" + c.name
            if !ancestors.contains(childId) then
              go(c.path, childId, ancestors + childId, here)
      }
    val rootId = realOf(root)
    go(root, rootId, Set(rootId), Nil)

  private final case class Scope(dir: String, rules: List[IgnoreRule])

  private def readIgnore(dir: String): List[IgnoreRule] =
    try Ignore.parse(Node.fs.readFileSync(Node.path.join(dir, ".gitignore"), "utf8").asInstanceOf[String])
    catch case _: Throwable => Nil

  /** Whether the `.gitignore` scopes from the root down ignore this entry:
   *  scopes outermost-first, rules in file order, last match wins, `!` negates. */
  private def ignored(path: String, name: String, isDir: Boolean, scopes: List[Scope]): Boolean =
    var result = false
    scopes.foreach { s =>
      val rel = relative(s.dir, path)
      s.rules.foreach(r => if r.matches(rel, name, isDir) then result = !r.negated)
    }
    result

  private def realOf(p: String): String =
    try Node.fs.realpathSync(p).asInstanceOf[String]
    catch case _: Throwable => p

  private def relative(root: String, p: String): String =
    Node.path.relative(root, p).asInstanceOf[String]

  /** One walked child: its `path`, base `name`, whether it resolves to a
   *  directory, and whether the entry itself is a symlink — so the cycle guard
   *  canonicalizes it (a syscall) rather than extending the parent's identity. */
  private final case class Child(path: String, name: String, dir: Boolean, viaSymlink: Boolean)

  /** One directory level, in readdir order. A regular dir entry is a directory;
   *  a symlink is resolved with `statSync`, *following* it (a broken or cyclic
   *  link resolves to "not a directory" → a file). Plain files and dirs cost no
   *  extra syscall — only symlinks are re-stat'd. */
  private def children(dir: String): List[Child] =
    val arr = Node.fs
      .readdirSync(dir, js.Dynamic.literal(withFileTypes = true))
      .asInstanceOf[js.Array[js.Dynamic]]
    (0 until arr.length).toList.map { i =>
      val d = arr(i)
      val name = d.name.asInstanceOf[String]
      val childPath = Node.path.join(dir, name).asInstanceOf[String]
      val isLink = d.isSymbolicLink().asInstanceOf[Boolean]
      val isDirectory =
        if d.isDirectory().asInstanceOf[Boolean] then true
        else if isLink then
          try Node.fs.statSync(childPath).isDirectory().asInstanceOf[Boolean]
          catch case _: Throwable => false
        else false
      Child(childPath, name, isDirectory, isLink)
    }

object Grep:
  /** Compile `pattern` as a regex, raising a clear error (instead of leaking a
   *  raw regex-engine exception) when it is malformed. */
  def compile(pattern: String): Regex =
    try pattern.r
    catch
      case t: Throwable =>
        throw new RuntimeException(s"grep: invalid regular expression '$pattern': ${errorDetail(t)}")

  /** Every matching line in every file under `root`, in walk order — pruned:
   *  `.git` and paths excluded by `.gitignore` files found from `root` down are
   *  skipped (see [[Walker.walk]]); the root itself is always searched. Binary
   *  files (a NUL byte) are skipped rather than searched as mojibake, and any
   *  per-file I/O error is swallowed so one unreadable file does not abort the
   *  search. (An invalid `pattern` is rejected up front.) Files are matched
   *  during traversal, so no full entry list is materialized first. */
  def search(root: String, pattern: String): List[Match] =
    val re = compile(pattern)
    val out = ListBuffer.empty[Match]
    Walker.visit(root, honorIgnores = true)(e => if !e.dir then out ++= searchSafely(e.path, re))
    out.toList

  /** Like [[search]], but restricted to files whose path relative to `root`
   *  matches the glob `filePattern`. Pruned like [[search]]. */
  def search(root: String, pattern: String, filePattern: String): List[Match] =
    val re = compile(pattern)
    Walker.glob(root, filePattern).filter(!_.dir).flatMap(e => searchSafely(e.path, re))

  /** Like [[search]], but with no pruning: `.git` and `.gitignore`d paths are
   *  searched too. */
  def searchAll(root: String, pattern: String): List[Match] =
    val re = compile(pattern)
    val out = ListBuffer.empty[Match]
    Walker.visit(root, honorIgnores = false)(e => if !e.dir then out ++= searchSafely(e.path, re))
    out.toList

  /** Like [[search]] with a `filePattern`, but with no pruning (see
   *  [[searchAll]]). */
  def searchAll(root: String, pattern: String, filePattern: String): List[Match] =
    val re = compile(pattern)
    Walker.globAll(root, filePattern).filter(!_.dir).flatMap(e => searchSafely(e.path, re))

  /** Every matching line of the single file at `path`. Strict, unlike the
   *  directory-wide [[search]]: read errors propagate and binary content is
   *  searched as-is — greping a file the caller named is never silently
   *  skipped. */
  def searchFile(path: String, pattern: String): List[Match] =
    val re = compile(pattern)
    matchLines(path, Lines.split(readContent(path)), re)

  /** One file of a directory-wide search: read fd-first with an early binary
   *  sniff (see [[readTextContent]]), NUL-marked binaries skipped, I/O errors
   *  swallowed. */
  private def searchSafely(path: String, re: Regex): List[Match] =
    try
      readTextContent(path) match
        case Some(c) => matchLines(path, Lines.split(c), re)
        case None    => Nil
    catch case _: Throwable => Nil

  /** Read a file for a directory-wide search: its decoded UTF-8 text, or `None`
   *  when it is a NUL-marked binary. The file is opened once and an 8 KB head is
   *  sniffed for a NUL first, so a large binary is closed and skipped without
   *  reading or decoding its bulk; a text file reads the remainder into a single
   *  Buffer and decodes once. The NUL check spans the whole content, not just
   *  the head: a 0x00 byte in UTF-8 is only ever the encoding of U+0000, so this
   *  reproduces the old "decoded content contains a NUL" rule exactly — the head
   *  sniff is a pure early exit, never a change in which files are skipped. */
  private def readTextContent(path: String): Option[String] =
    val Buffer = js.Dynamic.global.Buffer
    val HeadLen = 8192
    val fd = Node.fs.openSync(path, "r")
    try
      val head = Buffer.allocUnsafe(HeadLen)
      val n = Node.fs.readSync(fd, head, 0, HeadLen, 0).asInstanceOf[Double].toInt
      val hi = head.indexOf(0).asInstanceOf[Double].toInt
      if hi >= 0 && hi < n then None // a NUL within the head: binary, skip early
      else if n < HeadLen then
        // The whole file fit in the head read; its content is [0, n).
        Some(head.applyDynamic("toString")("utf8", 0, n).asInstanceOf[String])
      else
        // More to read: size the buffer, carry the head over, read the rest.
        val size = Node.fs.fstatSync(fd).size.asInstanceOf[Double].toInt
        val full = Buffer.allocUnsafe(size)
        head.copy(full, 0, 0, n)
        var m = n
        var reading = true
        while reading && m < size do
          val r = Node.fs.readSync(fd, full, m, size - m, m).asInstanceOf[Double].toInt
          if r <= 0 then reading = false else m += r
        // Full-fidelity NUL check across everything read, before decoding.
        val zi = full.indexOf(0).asInstanceOf[Double].toInt
        if zi >= 0 && zi < m then None
        else Some(full.applyDynamic("toString")("utf8", 0, m).asInstanceOf[String])
    finally Node.fs.closeSync(fd)

  private def matchLines(path: String, ls: List[String], re: Regex): List[Match] =
    ls.zipWithIndex.collect {
      case (line, i) if re.findFirstIn(line).isDefined => Match(path, i + 1, line)
    }

  private def readContent(path: String): String =
    Node.fs.readFileSync(path, "utf8").asInstanceOf[String]

  /** A short, human-readable detail for a regex-compile error — its Node
   *  `code` if it is a JS error carrying one, otherwise its message. */
  private def errorDetail(t: Throwable): String =
    val code = t match
      case js.JavaScriptException(e) =>
        val c = e.asInstanceOf[js.Dynamic].code
        if c == null || js.isUndefined(c) then "" else c.asInstanceOf[String]
      case _ => ""
    if code.nonEmpty then code else Option(t.getMessage).getOrElse("error")
