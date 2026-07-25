package auk.grep

import java.util.regex.Pattern
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

  private final val Lf = 10 // the LF byte, this engine's only line separator on the fast path
  private final val Cr = 13 // the CR byte; its presence routes a file to the reference path

  /** How many files a search's prefilter is judged on before it may be given up:
   *  16 — a few hundred KB of scanning, far too little to matter, and enough of a
   *  sample that one unrepresentative directory does not decide the question (the
   *  walk is depth-first, so the first files examined are neighbours and their
   *  contents correlate). */
  private final val PrefilterProbe = 16

  /** A required literal, encoded once: the UTF-8 `bytes` a file must contain for
   *  the pattern to have any chance of matching it, and their byte `length`.
   *  A plain class, not a `case` class: this engine's IR ships inside
   *  `library.bin`, where a case class's generated `Product` surface costs
   *  several KB it would never use.
   *
   *  It also carries this search's tally, which is what makes the give-up
   *  adaptive PER SEARCH: one `Needle` is built per [[kitFor]] call, so the
   *  counters cannot leak from one search into the next.
   *
   *  [[worthwhile]] is the rule, and it is the measured break-even: scanning a
   *  megabyte costs ~0.13 ms, and rejecting one saves ~0.25 ms of NUL scan,
   *  decode and regex, so the prefilter pays exactly when it rejects more than
   *  half of what it sees. Below that it is charging the search for nothing, so
   *  after a [[PrefilterProbe]]-file probe a prefilter rejecting less than half
   *  the files is dropped for the remainder of THAT search. The counters then
   *  stop moving (only a scan updates them), so the decision is final and this
   *  cannot oscillate.
   *
   *  A rate is what works: requiring ZERO rejections instead would almost never
   *  fire, because one unrelated file — a manifest, a LICENSE — is enough to
   *  make a needle that is in 99.9% of a tree look useful forever.
   *
   *  Note that giving up can only stop the engine from SKIPPING a file, never
   *  from searching one, so no give-up rule can change a result. */
  private final class Needle(val bytes: js.Dynamic, val length: Int):
    var examined: Int = 0 // files this search has scanned for the needle
    var rejected: Int = 0 // ... of which it dropped, unread
    def worthwhile: Boolean = examined < PrefilterProbe || rejected * 2 >= examined

  /** Test hooks, never touched by production code. `prefilterEnabled` runs this
   *  exact engine with the literal prefilter switched off, so a test can assert
   *  that prefiltered and unprefiltered searches return identical results;
   *  `prefilterSkips` and `prefilterScans` count the files the prefilter dropped
   *  before decoding and the files it scanned at all, so a test can prove both
   *  that the gate fires and that it gives up when it stops paying (nothing else
   *  can observe either — a skipped file's result is by construction the one it
   *  would have had, and so is a file the prefilter has stopped scanning). */
  private[grep] var prefilterEnabled: Boolean = true
  private[grep] var prefilterSkips: Int = 0
  private[grep] var prefilterScans: Int = 0

  /** A pattern compiled for both matching paths: `re`, the per-line reference
   *  (which confirms every emitted match and is the sole semantics of record),
   *  and `fast`, the same source compiled for the whole-content fast path —
   *  present only when the pattern is free of constructs that would make
   *  whole-content scanning diverge from per-line matching (see [[hazardous]]);
   *  plus `needle`, the pre-encoded required literal (see [[requiredLiteral]])
   *  used to rule files out before they are decoded.
   *
   *  The fast-path pattern is compiled with NO flags — in particular not
   *  MULTILINE. MULTILINE would be the natural way to let `^`/`$` anchor per
   *  line during a whole-content scan, but Scala.js only emulates it under an
   *  ES2018 linker target, and the REPL worker that runs this engine links at a
   *  lower target. So instead, any pattern carrying a bare `^`/`$` anchor is
   *  treated as [[hazardous]] and routed to the per-line reference (which
   *  anchors per line correctly); the fast path handles only anchor-free
   *  patterns, for which a plain unanchored scan is position-independent and
   *  a per-line match always has a whole-content occurrence at the same offset. */
  private final case class Kit(re: Regex, fast: Option[Pattern], needle: Option[Needle])

  /** Compile `pattern` into a [[Kit]]: always the reference regex, plus the
   *  no-flags fast-path pattern unless the source is [[hazardous]], plus the
   *  encoded required literal when one can be extracted. An invalid pattern is
   *  rejected by `compile` before the hazard scan runs, so the fast-path compile
   *  of a scanned pattern always succeeds. */
  private def kitFor(pattern: String): Kit =
    val re = compile(pattern)
    val fast = if hazardous(pattern) then None else Some(Pattern.compile(pattern))
    val needle = if prefilterEnabled then requiredLiteral(pattern).flatMap(encodeNeedle) else None
    Kit(re, fast, needle)

  /** Whether `pattern` holds any construct whose whole-content meaning could
   *  differ from its per-line meaning, forcing the reference path. A cheap,
   *  conservative single scan that errs toward hazardous when unsure:
   *   - a bare `^` or `$` anchor (one not inside a `[...]` class and not
   *     escaped): per line it anchors to each line's edge, but an unanchored
   *     whole-content scan cannot reproduce that without MULTILINE (see [[Kit]]);
   *   - the input-anchor escapes (backslash A, z, Z): they anchor to the whole
   *     file, not to a line;
   *   - the literal escapes (backslash n, backslash r): they can match across
   *     line boundaries;
   *   - any actual control character (a raw newline, CR, or tab in the source);
   *   - any `(?...` group other than `(?:`: lookaround `(?=` `(?!` `(?<`, and
   *     every inline-flag group like `(?i)` or `(?m:...)` — flags and lookaround
   *     around line edges are exactly where the two paths part.
   *  A backslash consumes the following character as one escape, so an escaped
   *  backslash then a literal A is not mistaken for the input anchor. Character
   *  classes are tracked so that `[^a]` (negation) and `[$]` (a literal `$`) are
   *  not mistaken for anchors; the tracking errs toward closing a class early,
   *  which only over-reports hazards (safe) and never hides a real anchor. */
  private def hazardous(pattern: String): Boolean =
    val n = pattern.length
    var i = 0
    var inClass = false // inside a [...] character class
    while i < n do
      val c = pattern.charAt(i)
      if c.toInt == 92 then // a backslash: inspect the escaped character
        if i + 1 < n then
          val d = pattern.charAt(i + 1)
          if d == 'n' || d == 'r' then return true // line-terminator literal, in or out of a class
          if !inClass && (d == 'A' || d == 'z' || d == 'Z') then return true // whole-file anchor
          i += 2
        else i += 1
      else if inClass then
        if c == ']' then inClass = false
        i += 1
      else if c == '[' then
        inClass = true; i += 1
      else if c == '^' || c == '$' then return true // a bare per-line anchor
      else if c == '(' && i + 1 < n && pattern.charAt(i + 1) == '?' then
        if i + 2 >= n || pattern.charAt(i + 2) != ':' then return true
        i += 3
      else if c < ' ' then return true // a raw control char (embedded newline/CR/tab)
      else i += 1
    false

  private final val MinLiteral = 3 // shorter needles are too common to pay for

  // What a quantifier does to the character it follows.
  private final val QNone = 0 // no quantifier: the character stands, the run continues
  private final val QDrop = 1 // `?` `*` `{0,...}`: a match need not contain the character
  private final val QKeep = 2 // `+` `{n,...}`, n >= 1: the character occurs at least once
  private final val QBad  = 3 // a `{` that is not a quantifier — the dialects disagree, give up

  /** A quantifier's `kind` and the index just past it (and past any lazy or
   *  possessive modifier). Plain, for the reason [[Needle]] is. */
  private final class Quant(val kind: Int, val next: Int)

  /** A literal string that EVERY match of `pattern` must contain, if one of at
   *  least [[MinLiteral]] characters can be proven — the prefilter's whole
   *  premise, so this is deliberately timid: anything it cannot read with
   *  certainty yields `None` (no prefilter), never a guess.
   *
   *  One linear scan over the top level of the pattern accumulates literal
   *  *runs*; the longest run wins, ties going to the earliest. A run is built
   *  from plain characters and escaped metacharacters, and is broken (not
   *  abandoned) by anything that matches text this scan cannot name: `.`, a
   *  predefined class (backslash d/w/s and their negations), a boundary
   *  (backslash b/B), an anchor, a line-terminator escape, a character class, or
   *  a group — a group contributes nothing at all, since its content may be
   *  optional. A quantifier ends the run either way: `x?`/`x*` also drop `x`
   *  (it need not occur), while `x+`/`x{2}` keep one `x` but cannot continue —
   *  `a{2}b` matches "aab", which does not contain "ab".
   *
   *  Extraction is ABANDONED (yielding `None`) by anything whose reading is
   *  uncertain: a top-level `|` (each branch may be taken, so nothing is
   *  required), any `(?...` group other than `(?:` at any depth — inline flags
   *  like `(?i)` change how the rest of the pattern matches, and lookaround
   *  matches text a match need not contain — an unrecognized escape, and the
   *  handful of class and brace shapes that Java and JS regex syntax read
   *  differently (see [[skipClass]], [[braceQuant]]).
   *
   *  The result is required under BOTH matching paths: it is a substring of the
   *  pattern's own source, independent of where matching starts, so anchoring
   *  and the per-line/whole-content split do not affect it. */
  private[grep] def requiredLiteral(pattern: String): Option[String] =
    val n = pattern.length
    val run = new StringBuilder
    var best = ""
    var i = 0
    while i < n do
      val c = pattern.charAt(i)
      val escaped = c == '\\'
      // For an escape, `d` is the escaped character; otherwise `d` is `c` itself,
      // so the literal case below handles both with one branch.
      val d = if escaped && i + 1 < n then pattern.charAt(i + 1) else c
      if escaped && i + 1 >= n then return None // a trailing backslash: not a pattern we understand
      else if escaped && breaksRun(d) then
        best = closeRun(run, best)
        val q = quantAt(pattern, i + 2)
        if q.kind == QBad then return None
        i = q.next
      else if escaped && !literalEscape(d) then return None // an escape with no plain meaning here
      else if !escaped && c == '|' then return None // an alternation: neither branch is required
      else if !escaped && (c == '(' || c == '[') then
        val j = if c == '(' then skipGroup(pattern, i) else skipClass(pattern, i)
        if j < 0 then return None
        best = closeRun(run, best)
        val q = quantAt(pattern, j)
        if q.kind == QBad then return None
        i = q.next
      else if !escaped && (c == '.' || c == '^' || c == '$') then
        best = closeRun(run, best)
        val q = quantAt(pattern, i + 1)
        if q.kind == QBad then return None
        i = q.next
      else if !escaped && (c == ')' || c == '*' || c == '+' || c == '?' || c == '{') then
        return None // a dangling quantifier or an unbalanced group: unreadable here
      else
        // A literal character: `d`, ending at `k`. Its quantifier decides whether
        // it survives, and always ends the run.
        val k = if escaped then i + 2 else i + 1
        val q = quantAt(pattern, k)
        if q.kind == QBad then return None
        if q.kind != QDrop then run.append(d)
        if q.kind != QNone then best = closeRun(run, best)
        i = q.next
    best = closeRun(run, best)
    if best.length >= MinLiteral then Some(best) else None

  /** End the current literal run, returning the better of it and the incumbent
   *  `best` — strictly longer wins, so a tie keeps the EARLIER run. */
  private def closeRun(run: StringBuilder, best: String): String =
    val out = if run.length > best.length then run.toString else best
    run.setLength(0)
    out

  /** Whether an escaped `d` stands for the character itself. Only punctuation:
   *  Java rejects escapes of alphabetic characters that name no construct, so an
   *  unlisted letter is a construct this scan does not model. */
  private def literalEscape(d: Char): Boolean =
    "\\.^$|?*+()[]{}/-".indexOf(d.toInt) >= 0

  /** Whether an escaped `d` matches text, but text this scan cannot name — so it
   *  breaks the literal run without abandoning extraction. The predefined
   *  classes, the boundaries, and the line-terminator escapes. */
  private def breaksRun(d: Char): Boolean =
    d == 'd' || d == 'D' || d == 'w' || d == 'W' || d == 's' || d == 'S' ||
      d == 'b' || d == 'B' || d == 'n' || d == 'r' || d == 't' || d == 'f'

  /** The quantifier at `i`, if any (see [[Quant]] and the `Q*` constants). */
  private def quantAt(pattern: String, i: Int): Quant =
    if i >= pattern.length then new Quant(QNone, i)
    else
      pattern.charAt(i) match
        case '?' | '*' => new Quant(QDrop, pastModifier(pattern, i + 1))
        case '+'       => new Quant(QKeep, pastModifier(pattern, i + 1))
        case '{'       => braceQuant(pattern, i)
        case _         => new Quant(QNone, i)

  /** Past a quantifier's optional lazy (`?`) or possessive (`+`) modifier. */
  private def pastModifier(pattern: String, i: Int): Int =
    if i < pattern.length && (pattern.charAt(i) == '?' || pattern.charAt(i) == '+') then i + 1 else i

  /** A `{n}` / `{n,}` / `{n,m}` quantifier at `i` — [[QDrop]] when its minimum is
   *  zero, [[QKeep]] otherwise. Any other `{` is [[QBad]]: JS reads a brace that
   *  is not a well-formed quantifier as a literal, Java rejects it, and guessing
   *  wrong would put a `{` into a needle that no match contains. */
  private def braceQuant(pattern: String, i: Int): Quant =
    val n = pattern.length
    var j = i + 1
    var digits = 0
    var min = false // whether the minimum is non-zero
    while j < n && pattern.charAt(j) >= '0' && pattern.charAt(j) <= '9' do
      if pattern.charAt(j) != '0' then min = true
      digits += 1
      j += 1
    if digits == 0 then new Quant(QBad, -1)
    else
      if j < n && pattern.charAt(j) == ',' then
        j += 1
        while j < n && pattern.charAt(j) >= '0' && pattern.charAt(j) <= '9' do j += 1
      if j < n && pattern.charAt(j) == '}' then
        new Quant(if min then QKeep else QDrop, pastModifier(pattern, j + 1))
      else new Quant(QBad, -1)

  /** Past the group opening at `i`, or -1 to abandon extraction: any `(?...`
   *  other than `(?:` — at ANY nesting depth, so a `(?i)` buried inside a plain
   *  group is caught too — a `\Q`/`\E` quoted section (whose contents would
   *  derail this scan's bracket counting), or an unbalanced group. */
  private def skipGroup(pattern: String, i: Int): Int =
    val n = pattern.length
    var j = i
    var depth = 0
    while j < n do
      val c = pattern.charAt(j)
      if c == '\\' then
        if j + 1 >= n then return -1
        val d = pattern.charAt(j + 1)
        if d == 'Q' || d == 'E' then return -1
        j += 2
      else if c == '[' then
        val k = skipClass(pattern, j)
        if k < 0 then return -1
        j = k
      else if c == '(' then
        if j + 1 < n && pattern.charAt(j + 1) == '?' then
          if j + 2 >= n || pattern.charAt(j + 2) != ':' then return -1
          j += 3
        else j += 1
        depth += 1
      else if c == ')' then
        depth -= 1
        j += 1
        if depth == 0 then return j
      else j += 1
    -1

  /** Past the character class opening at `i`, or -1 to abandon extraction.
   *  Getting a class's END wrong is the one way this scan could hand back a
   *  literal that is not required (the class's own contents would be read as
   *  literal text), so it gives up on every shape the two regex dialects read
   *  differently: a leading `]` (a class containing `]` in Java, an empty class
   *  in JS), a nested `[` or an `&` intersection (Java syntax, plain characters
   *  in JS), and `\Q`/`\E` quoting. */
  private def skipClass(pattern: String, i: Int): Int =
    val n = pattern.length
    var j = i + 1
    if j < n && pattern.charAt(j) == '^' then j += 1
    if j < n && pattern.charAt(j) == ']' then return -1
    while j < n do
      val c = pattern.charAt(j)
      if c == '\\' then
        if j + 1 >= n then return -1
        val d = pattern.charAt(j + 1)
        if d == 'Q' || d == 'E' then return -1
        j += 2
      else if c == '[' || c == '&' then return -1
      else if c == ']' then return j + 1
      else j += 1
    -1

  /** UTF-8-encode a required literal into a [[Needle]], or `None` when its bytes
   *  could not honestly be compared against a file's raw bytes:
   *
   *   - the literal contains U+FFFD. Invalid UTF-8 decodes to U+FFFD, so such a
   *     pattern can match decoded text whose raw bytes look nothing like the
   *     needle's — the one way a byte-level prefilter could drop a real match.
   *   - the literal does not survive an encode/decode round trip. The same
   *     hazard from the other side: an unpaired surrogate (a quantifier can drop
   *     half of a surrogate pair) encodes to U+FFFD's bytes, which the raw
   *     content would not contain either.
   *
   *  Everything else is safe by construction: for text holding no U+FFFD, every
   *  character decoded from exactly the bytes UTF-8 gives it, so a substring of
   *  the decoded content is a byte substring of the file. */
  private def encodeNeedle(lit: String): Option[Needle] =
    if lit.indexOf(0xFFFD) >= 0 then None
    else
      val bytes = js.Dynamic.global.Buffer.from(lit, "utf8")
      if bytes.applyDynamic("toString")("utf8").asInstanceOf[String] != lit then None
      else Some(new Needle(bytes, bytes.length.asInstanceOf[Double].toInt))

  /** Whether the prefilter rules this file out: the pattern requires a literal
   *  and the file's first `n` bytes do not contain it, so no line of it can
   *  match and it need not be decoded (or even NUL-scanned) at all.
   *
   *  `indexOf` searches the whole buffer, which the read path over-allocates, so
   *  a hit counts only when it ENDS within the content — and since `indexOf`
   *  returns the EARLIEST occurrence, no real hit can hide behind one that
   *  straddles that boundary.
   *
   *  A needle this search has stopped believing in (see [[Needle.worthwhile]])
   *  is not scanned for at all: the file takes the ordinary path, exactly as if
   *  the pattern had yielded no literal. */
  private def missesNeedle(buf: js.Dynamic, n: Int, needle: Option[Needle]): Boolean =
    needle match
      case Some(nd) if nd.worthwhile =>
        prefilterScans += 1
        val hit = buf.indexOf(nd.bytes).asInstanceOf[Double].toInt
        val missed = hit < 0 || hit + nd.length > n
        nd.examined += 1
        if missed then
          nd.rejected += 1
          prefilterSkips += 1
        missed
      case _ => false

  /** Every matching line in every file under `root`, in walk order — pruned:
   *  `.git` and paths excluded by `.gitignore` files found from `root` down are
   *  skipped (see [[Walker.walk]]); the root itself is always searched. Binary
   *  files (a NUL byte) are skipped rather than searched as mojibake, and any
   *  per-file I/O error is swallowed so one unreadable file does not abort the
   *  search. (An invalid `pattern` is rejected up front.) Files are matched
   *  during traversal, so no full entry list is materialized first. */
  def search(root: String, pattern: String): List[Match] =
    val kit = kitFor(pattern)
    val out = ListBuffer.empty[Match]
    Walker.visit(root, honorIgnores = true)(e => if !e.dir then out ++= searchSafely(e.path, kit))
    out.toList

  /** Like [[search]], but restricted to files whose path relative to `root`
   *  matches the glob `filePattern`. Pruned like [[search]]. */
  def search(root: String, pattern: String, filePattern: String): List[Match] =
    val kit = kitFor(pattern)
    Walker.glob(root, filePattern).filter(!_.dir).flatMap(e => searchSafely(e.path, kit))

  /** Like [[search]], but with no pruning: `.git` and `.gitignore`d paths are
   *  searched too. */
  def searchAll(root: String, pattern: String): List[Match] =
    val kit = kitFor(pattern)
    val out = ListBuffer.empty[Match]
    Walker.visit(root, honorIgnores = false)(e => if !e.dir then out ++= searchSafely(e.path, kit))
    out.toList

  /** Like [[search]] with a `filePattern`, but with no pruning (see
   *  [[searchAll]]). */
  def searchAll(root: String, pattern: String, filePattern: String): List[Match] =
    val kit = kitFor(pattern)
    Walker.globAll(root, filePattern).filter(!_.dir).flatMap(e => searchSafely(e.path, kit))

  /** Every matching line of the single file at `path`. Strict, unlike the
   *  directory-wide [[search]]: read errors propagate and binary content is
   *  searched as-is — greping a file the caller named is never silently skipped.
   *  It reads the file whole rather than fd-first, so the literal prefilter (a
   *  way to avoid reads the caller has already committed to) does not apply. */
  def searchFile(path: String, pattern: String): List[Match] =
    matchContent(path, readContent(path), kitFor(pattern))

  /** One file of a directory-wide search: read fd-first with an early binary
   *  sniff (see [[readTextContent]]), NUL-marked binaries skipped, I/O errors
   *  swallowed. */
  private def searchSafely(path: String, kit: Kit): List[Match] =
    try
      readTextContent(path, kit.needle) match
        case Some(c) => matchContent(path, c, kit)
        case None    => Nil
    catch case _: Throwable => Nil

  /** Match `content` from `path` against the compiled `kit`, returning exactly
   *  what the per-line reference would. The whole-content fast path is taken
   *  only when the kit is non-hazardous AND the content is non-empty and
   *  CR-free — so LF-only line slicing reproduces [[Lines.split]]; every other
   *  case (a hazardous pattern, any CR/CRLF content, or empty content) falls
   *  back to the reference [[matchLines]], which stays the semantics of record.
   *  Used by both the directory-wide search (via [[searchSafely]]) and
   *  [[searchFile]]; the latter may pass NUL-bearing content, which does not
   *  interact with LF slicing. */
  private def matchContent(path: String, content: String, kit: Kit): List[Match] =
    kit.fast match
      case Some(fast) if content.nonEmpty && content.indexOf(Cr) < 0 =>
        fastMatch(path, content, fast, kit.re)
      case _ =>
        matchLines(path, Lines.split(content), kit.re)

  /** The whole-content fast path: one native scan proposes at most one candidate
   *  per line, and the per-line reference `re` confirms each before it is emitted
   *  — the scan only nominates lines, `re` decides, so a cross-line candidate
   *  (e.g. a negated class spanning a newline) is rejected and no false positive
   *  escapes. The pattern is anchor-free (bare `^`/`$` are [[hazardous]]), so an
   *  unanchored whole-content scan finds every per-line match at the same offset
   *  — no candidate line is skipped. Preconditions from [[matchContent]]:
   *  `content` is non-empty and CR-free, so line boundaries are LF-only and
   *  match [[Lines.split]]. The line cursor only moves forward across candidates
   *  (match offsets never decrease), so locating each match's line is O(content)
   *  over the whole scan, never O(candidates x content). */
  private def fastMatch(path: String, content: String, fast: Pattern, re: Regex): List[Match] =
    val n = content.length
    val matcher = fast.matcher(content)
    val out = ListBuffer.empty[Match]
    var from = 0
    var lineStart = 0 // offset of the current line's first char
    var lineNo = 1    // 1-based number of the line starting at lineStart
    var searching = true
    while searching && matcher.find(from) do
      val o = matcher.start()
      // Advance the cursor forward to the line containing offset o.
      var nl = content.indexOf(Lf, lineStart)
      var lineEnd = if nl < 0 then n else nl // exclusive end of the line's text
      while o > lineEnd do
        lineStart = nl + 1 // nl >= 0 here: the last line has lineEnd == n >= o
        lineNo += 1
        nl = content.indexOf(Lf, lineStart)
        lineEnd = if nl < 0 then n else nl
      if lineStart >= n then
        // A zero-width candidate past the final newline (e.g. x* matches empty at
        // end of content); Lines.split has no such trailing line, so nothing real
        // remains.
        searching = false
      else
        val text = content.substring(lineStart, lineEnd)
        if re.findFirstIn(text).isDefined then out += Match(path, lineNo, text)
        // One candidate per line: resume from the start of the next line.
        if nl < 0 then searching = false // the candidate was on the last line
        else
          from = nl + 1
          lineStart = nl + 1
          lineNo += 1
    out.toList

  /** Read a file for a directory-wide search: its decoded UTF-8 text, `None` when
   *  it is a NUL-marked binary or when `needle` rules it out. The file is opened
   *  once and an 8 KB head is sniffed for a NUL first, so a large binary is
   *  closed and skipped without reading or decoding its bulk; a text file reads
   *  the remainder into a single Buffer and decodes once. The NUL check spans the
   *  whole content, not just the head: a 0x00 byte in UTF-8 is only ever the
   *  encoding of U+0000, so this reproduces the old "decoded content contains a
   *  NUL" rule exactly — the head sniff is a pure early exit, never a change in
   *  which files are skipped.
   *
   *  The prefilter runs on the raw bytes, before decoding: a file missing the
   *  pattern's required literal (see [[missesNeedle]]) cannot match, so it is
   *  dropped without the NUL scan, the UTF-16 decode or the regex. Dropping it
   *  earlier than the NUL check changes nothing observable — both routes return
   *  `None` — and it is the point: neither scan runs. */
  private def readTextContent(path: String, needle: Option[Needle]): Option[String] =
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
        if missesNeedle(head, n, needle) then None
        else Some(head.applyDynamic("toString")("utf8", 0, n).asInstanceOf[String])
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
        if missesNeedle(full, m, needle) then None
        else
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
