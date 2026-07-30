package auk.library

import scala.scalajs.js

import auk.grep.Lines

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

// -- the native grep engine: calling it, and converting what it returns --------
//
// [[GrepEngine]] is linked JS, so results arrive as JS arrays of plain objects
// and failures as JS errors. These helpers are the whole boundary: one call
// wrapper that restores the library's error vocabulary, two converters that
// rebuild `List` results in engine order, and the result objects
// ([[GrepResultImpl]] / [[GlobResultImpl]]) that hold the raw rows and run a
// converter only if the caller asks for the handles.

/** Run a native-engine call, re-raising its JS error as a [[RuntimeException]]
 *  carrying the same message. The engine's `grep: invalid regular expression
 *  '…': …` contract is pinned by the suites on both sides of the boundary, so
 *  it must read identically here and in a direct `auk.grep` call. */
private def engineCall[A](body: => A): A =
  try body
  catch
    case js.JavaScriptException(e) =>
      val message = e.asInstanceOf[js.Dynamic].message
      val text =
        if message == null || js.isUndefined(message) then e.toString
        else message.asInstanceOf[String]
      throw new RuntimeException(text)

/** `{path, dir}` rows as library handles, in engine (walk) order. */
private def toEntries(rows: js.Array[js.Dynamic]): List[FsEntry] =
  var i = rows.length - 1
  var out: List[FsEntry] = Nil
  while i >= 0 do
    val r = rows(i)
    val p = r.path.asInstanceOf[String]
    out = (if r.dir.asInstanceOf[Boolean] then FsDirImpl(p) else FsFileImpl(p)) :: out
    i -= 1
  out

/** `{path, line, text}` rows as [[Match]]es, in engine order. A file's matches
 *  are adjacent, so one file handle is shared across a run of them — the engine
 *  hands back the same path string for each, making the check a reference
 *  comparison, and handles are immutable values anyway. */
private def toMatches(rows: js.Array[js.Dynamic]): List[Match] =
  var i = rows.length - 1
  var out: List[Match] = Nil
  var path = ""
  var file: FsFile = FsFileImpl(path)
  while i >= 0 do
    val r = rows(i)
    val p = r.path.asInstanceOf[String]
    if !(p eq path) then
      path = p
      file = FsFileImpl(p)
    out = MatchImpl(file, r.line.asInstanceOf[Int], r.text.asInstanceOf[String]) :: out
    i -= 1
  out

/** The text [[GrepResult.display]] and [[GlobResult.display]] print: rows
 *  `offset` until `offset + limit`, rendered by `row`, framed by a marker line
 *  for whatever the window leaves out on either side.
 *
 *  `one`/`many` name the rows ("match"/"matches", "entry"/"entries") so the
 *  markers read truly for both results. A negative `limit` means "to the end",
 *  as in [[FsFile.read]]; a negative `offset` starts at the beginning; an
 *  `offset` past the end reports only what there actually was to skip, and an
 *  empty window says so rather than printing nothing at all.
 *
 *  One string, printed by the caller in one `println`: a full window can be
 *  hundreds of thousands of lines, and per-line printing to a captured stdout
 *  is the slow way to do that.
 */
private def windowText(total: Int, offset: Int, limit: Int, one: String, many: String)(
    row: Int => String
): String =
  def noun(n: Int): String = if n == 1 then one else many
  if total == 0 then s"(no $many)"
  else
    val from = math.min(math.max(offset, 0), total)
    val until = if limit < 0 then total else math.min(total, from + math.max(limit, 0))
    val sb = new StringBuilder
    if from > 0 then sb.append(s"(... $from ${noun(from)} skipped ...)\n")
    if until <= from then sb.append(s"(no $many in this window)\n")
    else
      var i = from
      while i < until do
        sb.append(row(i)).append('\n')
        i += 1
    val more = total - until
    if more > 0 then sb.append(s"(... $more more ${noun(more)} ...)\n")
    sb.result().stripSuffix("\n")

/** `{path, line, text}` rows as a [[GrepResult]].
 *
 *  The rows are the engine's own JS array, held as they arrived: `length` reads
 *  it directly and `display` renders a window of it field by field, so neither
 *  builds a single [[Match]]. Only [[matches]] does — once, cached by the `lazy
 *  val` — because on a big search that allocation IS the cost of the search: in
 *  the REPL worker, 110186 matches take 67 ms to find and 448 ms to find AND
 *  materialize.
 */
private final class GrepResultImpl(rows: js.Array[js.Dynamic]) extends GrepResult:
  def length: Int = rows.length
  def isEmpty: Boolean = rows.length == 0
  def nonEmpty: Boolean = rows.length > 0

  lazy val matches: List[Match] = toMatches(rows)

  def display(offset: Int, limit: Int): Unit =
    println(windowText(rows.length, offset, limit, "match", "matches") { i =>
      val r = rows(i)
      s"${r.path.asInstanceOf[String]}:${r.line.asInstanceOf[Int]}@ ${r.text.asInstanceOf[String]}"
    })

  override def toString: String =
    if rows.length == 0 then "GrepResult(no matches)"
    else
      val noun = if rows.length == 1 then "match" else "matches"
      s"GrepResult(${rows.length} $noun — use .display() or .matches)"

/** `{path, dir}` rows as a [[GlobResult]] — [[GrepResultImpl]]'s shape, over
 *  entries instead of matching lines. */
private final class GlobResultImpl(rows: js.Array[js.Dynamic]) extends GlobResult:
  def length: Int = rows.length
  def isEmpty: Boolean = rows.length == 0
  def nonEmpty: Boolean = rows.length > 0

  lazy val entries: List[FsEntry] = toEntries(rows)

  def display(offset: Int, limit: Int): Unit =
    println(windowText(rows.length, offset, limit, "entry", "entries") { i =>
      val r = rows(i)
      val p = r.path.asInstanceOf[String]
      if r.dir.asInstanceOf[Boolean] then p + "/" else p
    })

  override def toString: String =
    if rows.length == 0 then "GlobResult(no entries)"
    else
      val noun = if rows.length == 1 then "entry" else "entries"
      s"GlobResult(${rows.length} $noun — use .display() or .entries)"

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

  // Value equality (like PathImpl): two handles are the same entry when they have
  // the same concrete kind — file vs dir — and the same path. So `lib.fs.cwd` is
  // equal across calls, and entries behave in sets / as map keys.
  override def equals(other: Any): Boolean = other match
    case that: EntryOps => getClass == that.getClass && raw == that.raw
    case _              => false
  override def hashCode: Int = raw.hashCode

// -- addressing lines by the token you were shown -----------------------------
//
// `read` prints every line as `N#hh@ text`: `N` is where the line sat when it was
// shown and `hh` names its content, and together they are a token the caller
// hands back to `patch`. The number is only a hint. At patch time the runtime
// looks for the REMEMBERED CONTENT in the live file, so a line that has since
// moved is still found, and a line whose content is gone — or that the file now
// holds twice over with nothing to tell the copies apart — is refused with the
// current state printed, never patched by position.
//
// What that buys: reads and edits interleave freely (a token from an old window
// keeps working after later reads and later edits), an edit elsewhere in the file
// is simply irrelevant, and nothing has to be re-read to stay valid. What it
// costs: a line the caller has never been shown cannot be addressed at all, which
// is the point — every address is one the caller can see it earned.

/** One line as it was shown: the number it was shown at, its text (the needle a
 *  later patch searches for), and where it sat in the run that showed it — the
 *  run supplies the neighbours that break ties when the file holds that text more
 *  than once. */
private final case class Sighting(line: Int, text: String, run: Int, index: Int, uniqueAtShow: Boolean)

/** Where a reference resolved to in the live file, and the tokens a patch of it
 *  retires: the ones the caller was SHOWN for those lines, which is not the same
 *  as the tokens their current line numbers would make. */
private final case class Resolved(from: Int, to: Int, shown: Vector[String])

/** A line this session has since replaced. Its token is dead, so a caller still
 *  holding it is told so rather than sent hunting. `run` is the echo that reported
 *  the replacement: its lines are the anchor for finding the region again, which
 *  is why nothing is copied here — for a replacement they are the new text, and
 *  for a deletion they are the context either side, and either locates it. */
private final case class Tombstone(hint: Int, run: Int)

/** How many displayed lines one file remembers. Generous: the point of a sighting
 *  is that it stays good for as long as the caller might plausibly still act on
 *  it, and a line costs one map entry. Eviction is lazy and oldest-first, and an
 *  evicted token fails like one that was never shown — asking for a fresh read. */
private val SightingLimit = 5000

/** Everything one file's tokens are worth: the sightings by token, the runs they
 *  came from (a run is a contiguous block of lines as displayed once, so
 *  neighbours cost nothing), and the tokens this session has killed. */
private final class Sightings:
  private val byToken = scala.collection.mutable.LinkedHashMap.empty[String, Sighting]
  private val runs = scala.collection.mutable.Map.empty[Int, Vector[String]]
  private val runRefs = scala.collection.mutable.Map.empty[Int, Int]
  private val dead = scala.collection.mutable.LinkedHashMap.empty[String, Tombstone]
  private var nextRun = 0

  def get(token: String): Option[Sighting] = byToken.get(token)
  def tombstone(token: String): Option[Tombstone] = dead.get(token)
  /** The lines this file's `neighbours` come from, for the run `s` was shown in. */
  def runLines(s: Sighting): Vector[String] = runs.getOrElse(s.run, Vector.empty)
  /** One run's lines, empty once it has been evicted. */
  def runLines(id: Int): Vector[String] = runs.getOrElse(id, Vector.empty)

  /** Every token remembered for line number `n`, for a "did you mean" list. */
  def at(n: Int): List[(String, Sighting)] =
    byToken.toList.filter((_, s) => s.line == n)

  /** Remember `texts` as one contiguous run starting at line `from`, and return
   *  their tokens. A token shown again refreshes: the newest sighting for a token
   *  wins, since it is the one the caller just saw. */
  def record(from: Int, texts: Vector[String], unique: Vector[Boolean]): Int =
    if texts.isEmpty then -1
    else
      val run = nextRun
      nextRun += 1
      runs(run) = texts
      runRefs(run) = 0
      texts.zipWithIndex.foreach: (t, i) =>
        val line = from + i
        val token = s"$line#${lineHash(t)}"
        forget(token)
        byToken(token) = Sighting(line, t, run, i, unique.lift(i).getOrElse(false))
        runRefs(run) = runRefs(run) + 1
        dead.remove(token) // shown again, so it is alive again
      evict()
      run

  /** Kill `token`: the line it named is gone, replaced by `replacement`. Only a
   *  token that was actually shown is worth a tombstone — one nobody was ever
   *  given cannot come back, and claiming "you replaced it" of a token the caller
   *  never held would be a lie in an error message. */
  def kill(token: String, hint: Int, run: Int): Unit =
    byToken.get(token) match
      // The echo just showed this token for live content: it names a line that is
      // there now, so retiring it would kill an address we have already handed out.
      case Some(s) if s.run == run => ()
      case Some(_) =>
        forget(token)
        dead(token) = Tombstone(hint, run)
        while dead.size > SightingLimit do dead.remove(dead.head._1)
      case None => ()

  /** Drop everything: the file was rewritten wholesale, so nothing shown of the
   *  old content describes the new. */
  def clear(): Unit =
    byToken.clear(); runs.clear(); runRefs.clear(); dead.clear()

  private def forget(token: String): Unit =
    byToken.remove(token).foreach: s =>
      val left = runRefs.getOrElse(s.run, 0) - 1
      if left <= 0 then
        runs.remove(s.run); runRefs.remove(s.run)
      else runRefs(s.run) = left

  private def evict(): Unit =
    while byToken.size > SightingLimit do forget(byToken.head._1)

/** Every file's sightings, by absolute path — like the tokens themselves, a
 *  property of the FILE, so any handle to it can patch what any other displayed. */
private val sightings = scala.collection.mutable.Map.empty[String, Sightings]

/** Two lowercase base36 characters naming `text`'s content: FNV-1a over its
 *  characters, folded into 1296 values. Cheap, stable across processes, and with
 *  no clock or randomness in it, so the same line always shows the same token.
 *  Collisions (1 in 1296 for a given line number) cost nothing on their own — the
 *  content is checked in full when the token is used; the hash only has to make a
 *  token worth quoting and hard to invent. */
private def lineHash(text: String): String =
  var h = 0x811c9dc5
  var i = 0
  while i < text.length do
    h = (h ^ text.charAt(i).toInt) * 0x01000193
    i += 1
  val v = math.floorMod(h, 1296)
  val digits = "0123456789abcdefghijklmnopqrstuvwxyz"
  s"${digits.charAt(v / 36)}${digits.charAt(v % 36)}"

/** A file's live lines, indexed once per operation: the content plus where each
 *  1-based line starts and where its text ends (its terminator running from there
 *  to the next line's start). */
private final class LiveLines(val content: String, val starts: Array[Int], val ends: Array[Int]):
  def count: Int = starts.length
  def text(i: Int): String = content.substring(starts(i - 1), ends(i - 1))
  /** Where line `i` ends, its terminator included. */
  def fullEnd(i: Int): Int = if i < count then starts(i) else content.length
  /** The terminator line `i` carries — empty for an unterminated last line. */
  def terminator(i: Int): String = content.substring(ends(i - 1), fullEnd(i))
  /** Where text inserted after line `n` goes; `0` is the top of the file. */
  def insertAt(n: Int): Int = if n < count then starts(n) else content.length
  def texts: Vector[String] = (1 to count).toVector.map(text)
  /** The 1-based line beginning at offset `off` (one past the last line when
   *  `off` is the end of the content). */
  def lineAtOffset(off: Int): Int =
    var i = 0
    while i < count && starts(i) < off do i += 1
    i + 1

private def isTerminator(c: Char): Boolean = c == '\n' || c == '\r'

/** The line ending to give lines a patch adds: whichever of CRLF and lone LF the
 *  file already uses more, LF when it has neither or uses both equally. */
private def dominantEol(c: String): String =
  var crlf = 0
  var lf = 0
  var i = 0
  while i < c.length do
    if c.charAt(i) == '\n' then
      if i > 0 && c.charAt(i - 1) == '\r' then crlf += 1 else lf += 1
    i += 1
  if crlf > lf then "\r\n" else "\n"

/** The 1-based lines of `c` as `(starts, ends)` offsets: split on CRLF, a lone CR
 *  and LF alike, dropping the empty segment a trailing terminator would leave.
 *  Deliberately the same segmentation as [[Lines.split]] — the display, the
 *  needles and the splices all have to agree on what a line is — but keeping the
 *  offsets that byte-exact splicing needs. */
private def indexLines(c: String): (Array[Int], Array[Int]) =
  val starts = scala.collection.mutable.ArrayBuffer.empty[Int]
  val ends = scala.collection.mutable.ArrayBuffer.empty[Int]
  var i = 0
  var lineStart = 0
  while i < c.length do
    val ch = c.charAt(i)
    if ch == '\n' then
      starts += lineStart; ends += i; i += 1; lineStart = i
    else if ch == '\r' then
      starts += lineStart; ends += i
      i += (if i + 1 < c.length && c.charAt(i + 1) == '\n' then 2 else 1)
      lineStart = i
    else i += 1
  if lineStart < c.length then // a last line with no terminator
    starts += lineStart; ends += c.length
  (starts.toArray, ends.toArray)

// How much of a region is echoed before its middle is elided, and how much
// context a refusal prints around the place it is talking about.
private val EchoLimit = 12
private val EchoHead = 6
private val EchoTail = 5
private val ErrorContext = 3

/** A file handle. */
private final class FsFileImpl(val raw: String) extends FsFile with EntryOps:
  def rawContent: String = Node.fs.readFileSync(raw, "utf8").asInstanceOf[String]

  def lines: List[String] = Lines.split(rawContent)

  def lineCount: Int = lines.length

  def read(offset: Int = 1, limit: Int = -1): Unit =
    val live = liveLines
    val from = math.max(offset, 1)
    val until = if limit < 0 then live.count else math.min(live.count, from + math.max(limit, 0) - 1)
    if from <= until then println(show(live, from, until))

  def size: Long =
    statOpt
      .map(_.size.asInstanceOf[Double].toLong)
      .getOrElse(throw new RuntimeException(s"cannot stat (does it exist?): $raw"))

  def ext: String = Node.path.extname(raw).asInstanceOf[String].stripPrefix(".")

  def grep(pattern: String): GrepResult =
    GrepResultImpl(engineCall(GrepEngine.grepFile(raw, pattern)))

  def patch(fromRef: String, toRef: String, text: String): String =
    val live = liveLines
    val resolved = resolveBlock("patch", fromRef, toRef, live)
    val (from, to) = (resolved.from, resolved.to)
    val newLines = Lines.split(text).toVector
    // The region runs from the start of the first line to the end of the last,
    // terminator included — a line owns the bytes that end it. That terminator is
    // handed back to the replacement verbatim, so replacing a CRLF line keeps its
    // CRLF and replacing an unterminated last line leaves it unterminated.
    val payload =
      if newLines.isEmpty then "" else newLines.mkString(dominantEol(live.content)) + live.terminator(to)
    val killed = (from to to).toVector.map(live.text)
    splice(live, live.starts(from - 1), live.fullEnd(to), payload)
    // Echo first: the run it records is what the dead tokens anchor on.
    val (head, run) = reportPatch(from, to, killed.length, newLines.length, live.starts(from - 1))
    retire(Set(fromRef, toRef) ++ resolved.shown, from, killed, run)
    head

  def patch(ref: String, text: String): String = patch(ref, ref, text)

  def insertAfter(ref: String, text: String): String =
    val live = liveLines
    val after = if ref.trim == "0" then 0 else resolveBlock("insertAfter", ref, ref, live).to
    val newLines = Lines.split(text).toVector
    val at = live.insertAt(after)
    // Terminators are the one place an insertion writes outside the lines it
    // addresses: text appended after an unterminated last line has to close that
    // line first, or it would run onto the end of it.
    val eol = dominantEol(live.content)
    val opensLine = at > 0 && !isTerminator(live.content.charAt(at - 1))
    val endsFile = at >= live.content.length
    val terminated = live.content.nonEmpty && isTerminator(live.content.charAt(live.content.length - 1))
    val payload =
      if newLines.isEmpty then ""
      else
        (if opensLine then eol else "") + newLines.mkString(eol) +
          (if endsFile && !terminated then "" else eol)
    splice(live, at, at, payload)
    reportInsert(after, newLines.length, at + (if opensLine then eol.length else 0))

  def write(content: String): Unit =
    writeRaw(content)
    // A wholesale rewrite: nothing shown of the old content describes the new.
    store.clear()

  def append(content: String): Unit =
    // Deliberately NOT clearing: appending leaves every line above it untouched,
    // so every token already shown still names the line it named.
    Node.fs.appendFileSync(raw, content, "utf8")

  def touch(): Unit =
    if !exists then write("")

  // -- addressing: showing lines, and finding them again ----------------------

  private def writeRaw(content: String): Unit =
    Node.fs.writeFileSync(raw, content, "utf8")

  /** This file's entry in [[sightings]]: its absolute path, normalized but not
   *  resolved through symlinks — two names for one file cost at worst a refusal,
   *  since a token is only ever believed after its content is found. */
  private def store: Sightings =
    val key = Node.path.resolve(raw).asInstanceOf[String]
    sightings.getOrElseUpdate(key, new Sightings)

  private def liveLines: LiveLines =
    val content = rawContent
    val (starts, ends) = indexLines(content)
    LiveLines(content, starts, ends)

  /** Render live lines `from`..`until` as `N#hh@ text`, remembering them so every
   *  line printed becomes addressable. This is the ONLY way an address is minted:
   *  reads, patch echoes and refusals all come through here, which is why a token
   *  the caller can see is always a token it can use. */
  private def show(live: LiveLines, from: Int, until: Int): String = showBlock(live, from, until)._1

  /** [[show]], also handing back the run it recorded — what a tombstone anchors
   *  on. THE invariant of this whole scheme lives here: an identifier is never
   *  printed without being recorded, so anything the caller can read off its
   *  screen is something it can hand straight back. */
  private def showBlock(live: LiveLines, from: Int, until: Int): (String, Int) =
    val texts = (from to until).toVector.map(live.text)
    // Whether each line's text was unique in the WHOLE file at the moment it was
    // shown. A line that had look-alikes then can never be resolved on a lone
    // content match later — the survivor might be one of its siblings.
    val counts = scala.collection.mutable.Map.empty[String, Int]
    live.texts.foreach(t => counts(t) = counts.getOrElse(t, 0) + 1)
    val run = store.record(from, texts, texts.map(t => counts.getOrElse(t, 0) == 1))
    (texts.zipWithIndex.map((t, i) => s"${from + i}#${lineHash(t)}@ $t").mkString("\n"), run)

  /** Like [[show]], but eliding the middle of a long block. The head and tail are
   *  remembered as SEPARATE runs: they are not adjacent in the file, and a run
   *  that claimed they were would hand a later tie-break a neighbour that does not
   *  exist. */
  private def showElided(live: LiveLines, from: Int, until: Int): (String, Int) =
    val count = until - from + 1
    if count <= EchoLimit then showBlock(live, from, until)
    else
      val (head, run) = showBlock(live, from, from + EchoHead - 1)
      val (tail, _) = showBlock(live, until - EchoTail + 1, until)
      (s"$head\n…\n$tail", run)

  /** The lines around `line`, for showing a caller what is there now. */
  private def showAround(live: LiveLines, line: Int): String =
    val from = math.max(1, line - ErrorContext)
    val until = math.min(live.count, line + ErrorContext)
    if from > until then "(the file is empty)" else showElided(live, from, until)._1

  /** Read `ref` as the `N#hh` token a read printed. */
  private def parseRef(op: String, ref: String): (Int, String) =
    val t = ref.trim
    val hash = t.indexOf('#')
    val number = if hash < 0 then "" else t.substring(0, hash)
    val tag = if hash < 0 then "" else t.substring(hash + 1)
    val n = number.toIntOption.getOrElse(-1)
    if n < 1 || tag.length != 2 then
      throw new RuntimeException(
        s"$op: '$ref' is not a line reference — use the whole `N#hh` token from a read, " +
          "e.g. patch(\"65#xy\", \"...\"), not the number on its own"
      )
    (n, tag.toLowerCase)

  /** The live lines `fromRef`..`toRef` name, or a refusal that shows what the file
   *  holds now.
   *
   *  The remembered lines are the needle: the block the caller was shown has to be
   *  in the file, contiguous and entire. Found once, that is the answer however far
   *  it has drifted — content it is, not position. Found several times (blank
   *  lines, a lone brace), the run's neighbours have to pick one out, and if they
   *  cannot, nothing is patched: an address that might mean two places is worth
   *  nothing, and guessing is the one failure this design exists to rule out. */
  private def resolveBlock(op: String, fromRef: String, toRef: String, live: LiveLines): Resolved =
    val (fromLine, fromHash) = parseRef(op, fromRef)
    val (toLine, toHash) = parseRef(op, toRef)
    val a = sighting(op, s"$fromLine#$fromHash", fromLine, live)
    val b = if toRef == fromRef then a else sighting(op, s"$toLine#$toHash", toLine, live)
    if a.run != b.run then
      throw new RuntimeException(
        s"$op: '$fromRef' and '$toRef' come from different reads; a range has to be one span you " +
          "saw at once — read the whole span, then patch it"
      )
    if b.index < a.index then
      throw new RuntimeException(s"$op: '$fromRef' comes after '$toRef'; give the range in file order")
    val run = store.runLines(a)
    val needle = run.slice(a.index, b.index + 1)
    val hits = matches(live, needle)
    val chosen =
      if hits.isEmpty then throw notFound(op, live, needle, a.line)
      else if hits.length == 1 then
        // A lone match proves itself only when the text was unique in the file at
        // the moment it was shown. If it had look-alikes then and exactly one is
        // left now, the survivor may well be a sibling rather than the line the
        // caller saw — so the neighbours have to say so.
        if a.uniqueAtShow && b.uniqueAtShow then hits
        else if edgesConfirm(live, run, a.index, b.index, hits.head, needle.length) then hits
        else throw unconfirmed(op, live, hits.head, needle.length)
      else hits.filter(p => edgesAgree(live, run, a.index, b.index, p, needle.length))
    if chosen.length != 1 then throw ambiguous(op, live, needle, hits, a.line)
    // The tokens the caller was SHOWN for this span — the ones a patch retires.
    // Deriving them from live line numbers instead would miss every token whose
    // line has drifted since, leaving it alive to match a look-alike elsewhere.
    val shown = (a.index to b.index).toVector
      .map(j => s"${a.line - a.index + j}#${lineHash(run(j))}")
    Resolved(chosen.head, chosen.head + needle.length - 1, shown)

  /** The sighting `token` names, or the refusal that explains why there is none. */
  private def sighting(op: String, token: String, line: Int, live: LiveLines): Sighting =
    store.get(token) match
      case Some(s) => s
      case None =>
        store.tombstone(token) match
          case Some(t) =>
            // The echo that reported the replacement is the anchor: find its block
            // and the region is exact, however far the file has moved since.
            val anchor = store.runLines(t.run)
            val at = matches(live, anchor) match
              case one :: Nil => one
              case _          => t.hint
            throw new RuntimeException(
              s"$op: you replaced $token earlier this session; that region now holds:\n" +
                showAround(live, at)
            )
          case None =>
            val others = store.at(line)
            val hint =
              if others.isEmpty then
                s"read the file to see line $line, then patch the token it prints"
              else
                "did you mean " + others
                  .map((tk, s) => s"$tk ${preview(s.text)}")
                  .mkString(", ") + "?"
            throw new RuntimeException(s"$op: no line was shown as $token — $hint")

  /** Every 1-based position where `needle` sits in the live file, in file order. */
  private def matches(live: LiveLines, needle: Vector[String]): List[Int] =
    if needle.isEmpty then Nil
    else
      val texts = live.texts
      var out = List.empty[Int]
      var i = texts.length - needle.length
      while i >= 0 do
        if texts.slice(i, i + needle.length) == needle then out = (i + 1) :: out
        i -= 1
      out

  /** The one line holding `text`, if the file holds it exactly once. */
  private def uniqueLine(live: LiveLines, text: String): Option[Int] =
    matches(live, Vector(text)) match
      case one :: Nil => Some(one)
      case _          => None

  /** Do the lines just outside a candidate match the ones just outside the block
   *  when it was shown? The tie-break for a block the file holds more than once. */
  private def edgesAgree(live: LiveLines, run: Vector[String], a: Int, b: Int, at: Int, len: Int): Boolean =
    val before = if a > 0 then Some(run(a - 1)) else None
    val after = if b + 1 < run.length then Some(run(b + 1)) else None
    before.forall(p => at > 1 && live.text(at - 1) == p) &&
      after.forall(n => at + len <= live.count && live.text(at + len) == n)

  /** Like [[edgesAgree]], but demanding POSITIVE evidence: a line shown with no
   *  neighbours at all confirms nothing, and nothing is exactly what a lone match
   *  is worth for a line that had look-alikes when it was shown. */
  private def edgesConfirm(live: LiveLines, run: Vector[String], a: Int, b: Int, at: Int, len: Int): Boolean =
    val hasNeighbour = a > 0 || b + 1 < run.length
    hasNeighbour && edgesAgree(live, run, a, b, at, len)

  private def unconfirmed(op: String, live: LiveLines, at: Int, len: Int): RuntimeException =
    new RuntimeException(
      s"$op: that text is still in the file, but not where you saw it and with different lines " +
        "around it — and the file held other copies of it when you were shown it, so this one " +
        "cannot be confirmed as yours. Nothing was changed. What is there now:\n" +
        showElided(live, math.max(1, at - 1), math.min(live.count, at + len))._1
    )

  private def notFound(op: String, live: LiveLines, needle: Vector[String], hint: Int): RuntimeException =
    val what =
      if needle.length == 1 then "the line you addressed is no longer in the file"
      else s"the ${needle.length} lines you addressed are no longer together in the file"
    new RuntimeException(
      s"$op: $what — something changed it after you saw it (this is not a line you " +
        s"replaced yourself). Around line $hint the file now holds:\n" +
        showAround(live, hint)
    )

  private def ambiguous(
      op: String,
      live: LiveLines,
      needle: Vector[String],
      hits: List[Int],
      hint: Int
  ): RuntimeException =
    val ranked = hits.sortBy(p => (math.abs(p - hint), p))
    // Each candidate is shown WITH its neighbours, which is exactly the evidence
    // that tells the copies apart — so the tokens this refusal prints resolve
    // where the one the caller held could not. The refusal is the recovery.
    val shown = ranked
      .take(4)
      .map(p => showElided(live, math.max(1, p - 1), math.min(live.count, p + needle.length))._1)
    val more = if ranked.length > shown.length then s"\n(... ${ranked.length - shown.length} more ...)" else ""
    new RuntimeException(
      s"$op: that text is in the file ${ranked.length} times and nothing tells the copies apart, " +
        s"so nothing was changed. Address the one you mean by its own token:\n" +
        shown.mkString("\n--\n") + more
    )

  // -- applying, and reporting what landed ------------------------------------

  /** Replace the bytes `[start, end)` with `payload`, byte for byte: everything
   *  outside them survives exactly, mixed line endings included, so a one-line
   *  patch stays a one-line diff. */
  private def splice(live: LiveLines, start: Int, end: Int, payload: String): Unit =
    writeRaw(live.content.substring(0, start) + payload + live.content.substring(end))

  /** Kill the tokens this patch destroyed: the ones it was given, and the ones
   *  the replaced lines would have been shown as. A token that survives this and
   *  no longer matches anything is refused by search, which says the same thing
   *  with less certainty; killing what we know keeps the better message. */
  private def retire(refs: Set[String], from: Int, killed: Vector[String], run: Int): Unit =
    refs.foreach(r => store.kill(r.trim.toLowerCase, from, run))
    killed.zipWithIndex.foreach: (t, i) =>
      store.kill(s"${from + i}#${lineHash(t)}", from, run)

  private def reportPatch(from: Int, to: Int, oldCount: Int, newCount: Int, offset: Int): (String, Int) =
    val live = liveLines
    val head =
      s"patched lines $from-$to (${lineWord(oldCount)}) with ${lineWord(newCount)}; " +
        s"file now has ${lineWord(live.count)}"
    echo(head, live, live.lineAtOffset(offset), newCount)

  private def reportInsert(after: Int, count: Int, offset: Int): String =
    val live = liveLines
    val head = s"inserted ${lineWord(count)} after line $after; file now has ${lineWord(live.count)}"
    echo(head, live, live.lineAtOffset(offset), count)._1

  /** What a patch tells the caller: one line saying what moved, then the region as
   *  it now stands with a line of context either side — and every line of it
   *  numbered and hashed, so what was just written can be patched again straight
   *  away without re-reading it.
   *
   *  The block goes to stdout, which the tool result carries whole; the head line
   *  alone is returned, both because a rendered value is clipped at ~79 characters
   *  and because a one-line summary is what a caller collecting many patches
   *  wants. */
  private def echo(head: String, live: LiveLines, anchor: Int, count: Int): (String, Int) =
    val from = math.max(1, anchor - 1)
    val until = math.min(live.count, anchor + count)
    val (body, run) = if from > until then ("", -1) else showElided(live, from, until)
    println(if body.isEmpty then head else s"$head\n$body")
    (head, run)

  private def preview(text: String): String =
    val t = text.trim
    val short = if t.length > 40 then t.take(40) + "…" else t
    s"'$short'"

  private def lineWord(n: Int): String = if n == 1 then s"$n line" else s"$n lines"

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

  // walk/glob/grep all route through the auk-grep engine as linked JS (see
  // [[GrepEngine]]; walk semantics — symlink following, cycle guard, readdir
  // order — are documented on the engine itself). One interop call per
  // operation, then the rows come back as library handles.

  def walk: List[FsEntry] = toEntries(engineCall(GrepEngine.walk(raw)))

  def glob(pattern: String): GlobResult = GlobResultImpl(engineCall(GrepEngine.glob(raw, pattern)))

  def grep(pattern: String): GrepResult =
    GrepResultImpl(engineCall(GrepEngine.grep(raw, pattern)))

  def grep(pattern: String, filePattern: String): GrepResult =
    GrepResultImpl(engineCall(GrepEngine.grepGlob(raw, pattern, filePattern)))

  def walkAll: List[FsEntry] = toEntries(engineCall(GrepEngine.walkAll(raw)))

  def globAll(pattern: String): GlobResult = GlobResultImpl(engineCall(GrepEngine.globAll(raw, pattern)))

  def grepAll(pattern: String): GrepResult =
    GrepResultImpl(engineCall(GrepEngine.grepAll(raw, pattern)))

  def grepAll(pattern: String, filePattern: String): GrepResult =
    GrepResultImpl(engineCall(GrepEngine.grepAllGlob(raw, pattern, filePattern)))

  def file(name: String): FsFile = FsFileImpl(Node.path.join(raw, name).asInstanceOf[String])
  def dir(name: String): FsDir = FsDirImpl(Node.path.join(raw, name).asInstanceOf[String])

/** A single grep match; renders as `<path>:<linenum>@ <line>`. */
private final class MatchImpl(val file: FsFile, val lineNumber: Int, val line: String) extends Match:
  override def toString: String = s"${file.path}:$lineNumber@ $line"

/** The file-system API — a thin facade over [[Path]]'s open methods. */
private final class FileSystemImpl extends FileSystem:
  def cwd: FsDir = FsDirImpl(js.Dynamic.global.process.cwd().asInstanceOf[String])
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

private[library] final case class MemoryEntryImpl(id: String, description: String, content: String) extends MemoryEntry

/** [[Memory]] over one Markdown file per memory under `baseDir` (in production
 *  `<cwd>/.auk/memory`), each carrying a one-line `description` frontmatter. `baseDir`
 *  is a constructor arg so tests can point it at a scratch directory. */
private[library] final class MemoryImpl(baseDir: String) extends Memory:
  import MemoryImpl.*

  private def filePath(id: String): String =
    Node.path.join(baseDir, id + MdExt).asInstanceOf[String]

  /** The ids of every `<id>.md` under `baseDir`, sorted; empty if the dir is absent. */
  private def listIds: List[String] =
    if !Node.fs.existsSync(baseDir).asInstanceOf[Boolean] then Nil
    else
      val arr = Node.fs.readdirSync(baseDir).asInstanceOf[js.Array[String]]
      jsArrToList(arr).filter(_.endsWith(MdExt)).map(_.dropRight(MdExt.length)).sorted

  def get(id: String): Option[MemoryEntry] =
    val p = filePath(id)
    if !Node.fs.existsSync(p).asInstanceOf[Boolean] then None
    else
      val (desc, content) = parse(Node.fs.readFileSync(p, "utf8").asInstanceOf[String])
      Some(MemoryEntryImpl(id, desc, content))

  def all: List[MemoryEntry] = listIds.flatMap(get)

  def write(id: String, description: String, content: String): Unit =
    val cleanId = validId(id)
    val desc = oneLine(description)
    if desc.isEmpty then
      throw new RuntimeException("memory: description must be a non-empty one-line summary")
    Node.fs.mkdirSync(baseDir, js.Dynamic.literal(recursive = true))
    Node.fs.writeFileSync(filePath(cleanId), serialize(desc, content), "utf8")

  def delete(id: String): Unit =
    val p = filePath(id)
    if Node.fs.existsSync(p).asInstanceOf[Boolean] then
      Node.fs.rmSync(p, js.Dynamic.literal(force = true))

  def overview(): Unit = println(renderOverview(all))

  def read(id: String): Unit =
    get(id) match
      case Some(m) => println(s"# ${m.id} — ${m.description}\n\n${m.content}")
      case None    => println(s"no memory with id '$id'")

private[library] object MemoryImpl:
  private val MdExt = ".md"
  private val Fence = "---"

  /** Validate `id` as a filename-safe slug (letters, digits, `.`, `_`, `-`) so it can
   *  never escape the memory directory (no `/`, no bare `.`/`..`). */
  def validId(id: String): String =
    val t = id.trim
    if t.matches("[A-Za-z0-9._-]+") && t != "." && t != ".." then t
    else throw new RuntimeException(
      s"memory: invalid id '$id' — use a slug of letters, digits, '.', '_', '-' (no '/' or '..')")

  /** Collapse a description to a single trimmed line (frontmatter is one line/field). */
  def oneLine(s: String): String =
    s.split("\r\n|\r|\n", -1).map(_.trim).filter(_.nonEmpty).mkString(" ")

  /** A memory file: a `description` frontmatter block then the content, verbatim. */
  def serialize(description: String, content: String): String =
    s"$Fence\ndescription: $description\n$Fence\n$content"

  /** Parse a memory file into `(description, content)`. Only a *leading* `---`…`---`
   *  block is frontmatter, so the content may itself contain `---`; a file without the
   *  leading fence loads as all-content with an empty description. */
  def parse(raw: String): (String, String) =
    raw.split("\n", -1).toList match
      case first :: rest if first.trim == Fence =>
        val (frontmatter, afterOpen) = rest.span(_.trim != Fence)
        afterOpen match
          case _ :: contentLines => // drop the closing fence; the rest is content
            val desc = frontmatter
              .collectFirst { case l if l.startsWith("description:") => l.stripPrefix("description:").trim }
              .getOrElse("")
            (desc, contentLines.mkString("\n"))
          case Nil => ("", raw) // no closing fence → not real frontmatter
      case _ => ("", raw)

  /** The overview text: a header then one `  id — description` line per memory. */
  def renderOverview(entries: List[MemoryEntry]): String =
    if entries.isEmpty then "(no memories stored)"
    else s"Memories (${entries.size}):\n" + entries.map(m => s"  ${m.id} — ${m.description}").mkString("\n")

private[library] final case class HistoryToolCallImpl(name: String, arguments: String, output: String, isError: Boolean)
    extends HistoryToolCall
private[library] final case class HistoryMessageImpl(role: String, text: String, reasoning: String, toolCalls: List[HistoryToolCall])
    extends HistoryMessage
private[library] final case class HistorySessionImpl(
    id: String,
    modifiedAtMs: Option[Long],
    messageCount: Int,
    preview: String,
    messages: List[HistoryMessage]
) extends HistorySession

/** [[SessionHistory]] over the append-only JSONL session logs under `baseDir` (in
 *  production `<cwd>/.auk/sessions`), one `<uuid>.jsonl` per conversation. `baseDir`
 *  and `now` are constructor args so tests can point at a scratch dir with a fixed
 *  clock. Purely read-only: it never writes. */
private[library] final class SessionHistoryImpl(
    baseDir: String,
    now: () => Long = () => System.currentTimeMillis()
) extends SessionHistory:
  import SessionHistoryImpl.*

  /** Every top-level `<id>.jsonl` log as `(id, mtimeMs)`, newest first; empty if the
   *  sessions dir is absent. Nested `<uuid>/subagents/…` dirs are skipped — files only. */
  private def logs: List[(String, Long)] =
    if !Node.fs.existsSync(baseDir).asInstanceOf[Boolean] then Nil
    else
      val names = jsArrToList(Node.fs.readdirSync(baseDir).asInstanceOf[js.Array[String]])
      names.filter(_.endsWith(JsonlExt)).flatMap { name =>
        val p = Node.path.join(baseDir, name).asInstanceOf[String]
        val st = Node.fs.statSync(p)
        if st.isFile().asInstanceOf[Boolean] then
          Some(name.dropRight(JsonlExt.length) -> st.mtimeMs.asInstanceOf[Double].toLong)
        else None
      }.sortBy(-_._2)

  private def loadSession(id: String, mtimeMs: Option[Long]): HistorySession =
    val p = Node.path.join(baseDir, id + JsonlExt).asInstanceOf[String]
    val raw =
      if Node.fs.existsSync(p).asInstanceOf[Boolean] then Node.fs.readFileSync(p, "utf8").asInstanceOf[String]
      else ""
    val events = raw.split("\n", -1).toList.filter(_.trim.nonEmpty).flatMap(parseLine)
    val messages = messagesFrom(events)
    HistorySessionImpl(id, mtimeMs, messageCountOf(messages), previewOf(messages), messages)

  /** Resolve a full id or unambiguous prefix to `(id, mtimeMs)`, or a message
   *  explaining why it could not be resolved (absent / ambiguous). */
  private def resolveId(idOrPrefix: String): Either[String, (String, Long)] =
    val available = logs
    available.find(_._1 == idOrPrefix) match
      case Some(hit) => Right(hit)
      case None =>
        available.filter(_._1.startsWith(idOrPrefix)) match
          case Nil           => Left(s"no conversation with id '$idOrPrefix'")
          case single :: Nil => Right(single)
          case many          => Left(s"ambiguous id '$idOrPrefix' — matches ${many.map(m => shortId(m._1)).mkString(", ")}")

  def all: List[HistorySession] = logs.map((id, mt) => loadSession(id, Some(mt)))

  def get(id: String): Option[HistorySession] =
    resolveId(id).toOption.map((sid, mt) => loadSession(sid, Some(mt)))

  def overview(limit: Int = 20): Unit =
    val sessions = all
    println(renderOverview(now(), sessions.take(math.max(0, limit)), sessions.size))

  def read(id: String): Unit =
    resolveId(id) match
      case Right((sid, mt)) => println(renderTranscript(now(), loadSession(sid, Some(mt))))
      case Left(message)    => println(message)

  def search(query: String): Unit =
    val q = query.trim.toLowerCase
    val matches =
      if q.isEmpty then Nil
      else
        all.flatMap { s =>
          val text = transcriptText(s)
          Option.when(text.toLowerCase.contains(q))(s -> snippetAround(text, q))
        }
    println(renderSearch(now(), query, matches))

private[library] object SessionHistoryImpl:
  private val JsonlExt = ".jsonl"
  private val PreviewLen = 48
  private val ReasoningLen = 120
  private val ArgsLen = 100
  private val OutputLen = 200
  private val SnippetPad = 32

  /** The parsed forms of the session-log events we surface. A deliberate subset of
   *  `auk.session.SessionEvent` — enough to reconstruct a readable transcript. */
  sealed trait Ev
  final case class UserEv(text: String) extends Ev
  final case class AssistantEv(text: String, reasoning: String, calls: List[RawCall]) extends Ev
  final case class ResultsEv(rows: List[ResultRow]) extends Ev
  case object InterruptedEv extends Ev
  final case class NoticeEv(text: String) extends Ev
  final case class RawCall(id: String, name: String, arguments: String)
  final case class ResultRow(toolUseId: String, content: String, isError: Boolean)

  // -- JSON parsing (the on-disk schema is fixed by auk.session.SessionEvent) ----

  private def jsStr(d: js.Dynamic): String =
    if d == null || js.isUndefined(d) then "" else d.asInstanceOf[String]

  private def jsBool(d: js.Dynamic): Boolean =
    d != null && !js.isUndefined(d) && d.asInstanceOf[Boolean]

  private def jsArr(d: js.Dynamic): List[js.Dynamic] =
    if d == null || js.isUndefined(d) then Nil else jsArrToList(d.asInstanceOf[js.Array[js.Dynamic]])

  /** Parse one JSONL line into an [[Ev]], or `None` if it is blank, malformed, or an
   *  event type we don't surface. Robust to a partial trailing line after a crash. */
  def parseLine(line: String): Option[Ev] =
    try
      val o = js.JSON.parse(line)
      jsStr(o.`type`) match
        case "user_submitted"        => Some(UserEv(jsStr(o.text)))
        case "system_notice"         => Some(NoticeEv(jsStr(o.text)))
        case "interrupted"           => Some(InterruptedEv)
        case "assistant_responded"   => Some(parseAssistant(o))
        case "tool_results_received" => Some(ResultsEv(parseResults(o)))
        case _                       => None
    catch case _: Throwable => None

  private def parseAssistant(o: js.Dynamic): AssistantEv =
    val texts = List.newBuilder[String]
    val reasonings = List.newBuilder[String]
    val calls = List.newBuilder[RawCall]
    jsArr(o.message.content).foreach { c =>
      jsStr(c.kind) match
        case "text"     => val t = jsStr(c.text); if t.nonEmpty then texts += t
        case "thinking" => val t = jsStr(c.text); if t.nonEmpty then reasonings += t
        case "reasoning" =>
          val t = jsArr(c.blocks).map(b => firstNonEmpty(jsStr(b.text), jsStr(b.summary))).filter(_.nonEmpty).mkString("\n")
          if t.nonEmpty then reasonings += t
        case "tool_use" => calls += RawCall(jsStr(c.id), jsStr(c.name), jsStr(c.input))
        case _          => () // redacted_thinking — no human-readable text to show
    }
    AssistantEv(texts.result().mkString("\n"), reasonings.result().mkString("\n"), calls.result())

  private def parseResults(o: js.Dynamic): List[ResultRow] =
    jsArr(o.results).map(r => ResultRow(jsStr(r.toolUseId), jsStr(r.content), jsBool(r.isError)))

  // -- transcript reconstruction -------------------------------------------------

  /** Build the ordered message list, joining each `tool_use` to its result — which
   *  arrives in a later `tool_results_received` event — by id, exactly as the host's
   *  `Model.historyFrom` does. */
  def messagesFrom(events: List[Ev]): List[HistoryMessage] =
    val results: Map[String, ResultRow] =
      events.collect { case ResultsEv(rows) => rows }.flatten.map(r => r.toolUseId -> r).toMap
    events.flatMap {
      case UserEv(text)   => Some(HistoryMessageImpl("user", text, "", Nil))
      case NoticeEv(text) => Some(HistoryMessageImpl("system", text, "", Nil))
      case InterruptedEv  => Some(HistoryMessageImpl("system", "(interrupted)", "", Nil))
      case ResultsEv(_)   => None
      case AssistantEv(text, reasoning, calls) =>
        val toolCalls = calls.map { c =>
          val r = results.get(c.id)
          HistoryToolCallImpl(c.name, c.arguments, r.map(_.content).getOrElse(""), r.exists(_.isError))
        }
        Option.when(text.nonEmpty || reasoning.nonEmpty || toolCalls.nonEmpty)(
          HistoryMessageImpl("assistant", text, reasoning, toolCalls)
        )
    }

  def messageCountOf(messages: List[HistoryMessage]): Int =
    messages.count(m => m.role == "user" || m.role == "assistant")

  /** A one-line preview from the latest user/assistant message with text. */
  def previewOf(messages: List[HistoryMessage]): String =
    messages.reverseIterator
      .collectFirst {
        case m if (m.role == "user" || m.role == "assistant") && oneLine(m.text).nonEmpty => oneLine(m.text)
      }
      .map(truncate(_, PreviewLen))
      .getOrElse("(no messages)")

  // -- formatting helpers --------------------------------------------------------

  def oneLine(s: String): String =
    s.split("\r\n|\r|\n", -1).map(_.trim).filter(_.nonEmpty).mkString(" ")

  def truncate(s: String, max: Int): String =
    if s.length <= max then s else s.take(math.max(0, max - 1)).trim + "…"

  private def firstNonEmpty(a: String, b: String): String = if a.nonEmpty then a else b

  def shortId(id: String): String = if id.length <= 8 then id else id.take(8)

  /** A compact age like "just now", "5m ago", "3h ago", "2d ago", "4mo ago". */
  def relativeTime(deltaMs: Long): String =
    val secs = math.max(0L, deltaMs) / 1000
    if secs < 45 then "just now"
    else
      val mins = secs / 60
      if mins < 60 then s"${mins}m ago"
      else
        val hours = mins / 60
        if hours < 24 then s"${hours}h ago"
        else
          val days = hours / 24
          if days < 30 then s"${days}d ago" else s"${days / 30}mo ago"

  private def age(nowMs: Long, modifiedAtMs: Option[Long]): String =
    modifiedAtMs.map(mt => relativeTime(nowMs - mt)).getOrElse("unknown")

  /** All the text in a session, flattened — what [[SessionHistory.search]] scans. */
  def transcriptText(s: HistorySession): String =
    s.messages.map { m =>
      val tools = m.toolCalls.map(c => s"${c.name} ${c.arguments} ${c.output}").mkString(" ")
      s"${m.text} ${m.reasoning} $tools"
    }.mkString(" ")

  /** A short window of `text` around the first occurrence of `queryLower`. */
  def snippetAround(text: String, queryLower: String): String =
    val flat = oneLine(text)
    val idx = flat.toLowerCase.indexOf(queryLower)
    if idx < 0 then truncate(flat, SnippetPad * 2)
    else
      val start = math.max(0, idx - SnippetPad)
      val end = math.min(flat.length, idx + queryLower.length + SnippetPad)
      flat.substring(start, end)

  // -- rendering -----------------------------------------------------------------

  def renderOverview(nowMs: Long, shown: List[HistorySession], total: Int): String =
    if total == 0 then "(no conversations yet)"
    else
      val header = s"Conversations ($total):"
      val rows = shown.map(s => s"  ${shortId(s.id)} · ${age(nowMs, s.modifiedAtMs)} · ${s.messageCount} msg — ${s.preview}")
      val more =
        if shown.size < total then List(s"  … and ${total - shown.size} older (lib.history.overview($total) or .all)")
        else Nil
      (header :: rows ::: more).mkString("\n")

  def renderTranscript(nowMs: Long, s: HistorySession): String =
    val head = s"Conversation ${shortId(s.id)} — ${age(nowMs, s.modifiedAtMs)} · ${s.messageCount} messages"
    (head :: s.messages.flatMap(renderMessage)).mkString("\n")

  private def renderMessage(m: HistoryMessage): List[String] = m.role match
    case "user" =>
      "" :: "▌ user" :: indent(m.text)
    case "assistant" =>
      val reasoning = if m.reasoning.nonEmpty then List(s"  ✻ thought: ${truncate(oneLine(m.reasoning), ReasoningLen)}") else Nil
      val tools = m.toolCalls.flatMap { c =>
        val call = s"  ⚙ ${c.name}  ${truncate(oneLine(c.arguments), ArgsLen)}"
        val out =
          if c.isError then List(s"    → [error] ${truncate(oneLine(c.output), OutputLen)}")
          else if c.output.nonEmpty then List(s"    → ${truncate(oneLine(c.output), OutputLen)}")
          else Nil
        call :: out
      }
      val answer = if m.text.nonEmpty then indent(m.text) else Nil
      "" :: "▌ assistant" :: (reasoning ::: tools ::: answer)
    case _ =>
      List("", s"◆ ${oneLine(m.text)}")

  private def indent(text: String): List[String] =
    text.split("\n", -1).toList.map(l => s"  $l")

  def renderSearch(nowMs: Long, query: String, matches: List[(HistorySession, String)]): String =
    if matches.isEmpty then s"(no conversations match '$query')"
    else
      val header = s"${matches.size} match${if matches.size == 1 then "" else "es"} for '$query':"
      val rows = matches.map((s, snippet) => s"  ${shortId(s.id)} · ${age(nowMs, s.modifiedAtMs)} — …$snippet…")
      (header :: rows).mkString("\n")

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

  val team: Team = new TeamImpl()

  val loop: LoopApi = new LoopImpl(shell)

  val memory: Memory =
    new MemoryImpl(Node.path.join(js.Dynamic.global.process.cwd().asInstanceOf[String], ".auk", "memory").asInstanceOf[String])

  val history: SessionHistory =
    new SessionHistoryImpl(Node.path.join(js.Dynamic.global.process.cwd().asInstanceOf[String], ".auk", "sessions").asInstanceOf[String])
