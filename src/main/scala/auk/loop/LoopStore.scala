package auk.loop

import java.io.IOException

import auk.llm.tools.{Json, RuntimeContext}
import auk.platform.{FileSystem, PathOps, Platform}
import auk.session.JsonlLog

/** Where loops live on disk, under a project's `.auk` directory:
  *
  * {{{
  * .auk/.gitignore                     // "*", so none of this reaches a commit
  * .auk/loops/<loopId>/ledger.jsonl    // the append-only event log
  * .auk/loops/<loopId>/knowledge.md    // what the loop has learned, free-form
  * }}}
  *
  * The store is deliberately dumb: it appends lines, reads them back, and knows
  * nothing about what a well-formed loop looks like — that is [[LoopState.fold]]'s
  * job, reachable here as [[state]] for callers who just want the answer.
  *
  * The `.gitignore` matters more than it looks. A loop drives generations of work
  * on the live git tree and snapshots the result, and the ledger sits inside that
  * tree; without the ignore, every append would show up as a working-tree change in
  * the very diff the loop is trying to evaluate. auk's own repository happens to
  * list `.auk/` in its top-level `.gitignore`, but a project that merely *uses* auk
  * has no such line, so the directory ignores itself. Writing it is a one-time,
  * never-overwriting act: a project that has put its own rules there keeps them.
  */
final class LoopStore(val root: String, fs: FileSystem = Platform.fs):
  import LoopStore.*

  /** The directory holding every loop. */
  def loopsDir: String = PathOps.join(root, LoopsDirName)

  def dirFor(loopId: String): String = PathOps.join(loopsDir, loopId)
  def ledgerPath(loopId: String): String = PathOps.join(dirFor(loopId), LedgerFileName)
  def knowledgePath(loopId: String): String = PathOps.join(dirFor(loopId), KnowledgeFileName)

  /** Create `loopId`'s directory, and the `.auk/.gitignore` if it is missing.
    * Idempotent, and called by every write, so no caller has to remember it. */
  def ensure(loopId: String): Either[String, Unit] =
    validateId(loopId).flatMap: _ =>
      try
        fs.createDirectories(dirFor(loopId))
        val ignore = PathOps.join(root, GitignoreFileName)
        if !fs.exists(ignore) then fs.writeString(ignore, IgnoreEverything)
        Right(())
      catch case e: IOException => Left(s"failed to prepare loop '$loopId': ${e.getMessage}")

  /** Append one event to `loopId`'s ledger, flushed before returning. */
  def append(loopId: String, event: LoopEvent): Either[String, Unit] =
    for
      _ <- ensure(loopId)
      _ <- JsonlLog.append(ledgerPath(loopId), LoopCodec.encode(event), fs)
    yield ()

  /** Every event in `loopId`'s ledger, in order. A loop with no ledger yet reads as
    * empty rather than as an error, matching the append-on-demand layout.
    *
    * Strict: one undecodable line fails the whole read, because a ledger with a hole
    * in it is not a ledger. The message carries the line's 1-based position, counting
    * the non-blank lines that [[JsonlLog]] hands back — which is the file line number
    * for any ledger this store wrote, since [[append]] never writes a blank one. */
  def readAll(loopId: String): Either[String, Vector[LoopEvent]] =
    for
      _      <- validateId(loopId)
      lines  <- JsonlLog.readLines(ledgerPath(loopId), fs)
      events <- decodeAll(loopId, lines)
    yield events

  /** `loopId`'s ledger folded into the state it describes. */
  def state(loopId: String): Either[String, LoopState] =
    readAll(loopId).flatMap(LoopState.fold)

  /** Every loop with a ledger, sorted by id. Directories without one — a loop
    * whose creation was interrupted between `ensure` and its first `append` — are
    * not loops yet, and neither are entries whose names could not be loop ids. */
  def list(): List[String] =
    fs.listDir(loopsDir)
      .filterNot(_.isFile)
      .map(_.name)
      .filter(id => validateId(id).isRight && fs.isRegularFile(ledgerPath(id)))
      .sorted

  /** `loopId`'s accumulated knowledge, or `""` when it has recorded none. */
  def readKnowledge(loopId: String): Either[String, String] =
    validateId(loopId).flatMap: _ =>
      val path = knowledgePath(loopId)
      try Right(if fs.isRegularFile(path) then fs.readString(path) else "")
      catch case e: IOException => Left(s"failed to read knowledge for loop '$loopId': ${e.getMessage}")

  /** Replace `loopId`'s knowledge with `text`. */
  def writeKnowledge(loopId: String, text: String): Either[String, Unit] =
    ensure(loopId).flatMap: _ =>
      try
        fs.writeString(knowledgePath(loopId), text)
        Right(())
      catch case e: IOException => Left(s"failed to write knowledge for loop '$loopId': ${e.getMessage}")

  private def decodeAll(loopId: String, lines: List[String]): Either[String, Vector[LoopEvent]] =
    lines.zipWithIndex.foldLeft[Either[String, Vector[LoopEvent]]](Right(Vector.empty)):
      case (acc, (line, index)) =>
        for
          events <- acc
          event  <- LoopCodec.decode(line).left.map(msg => s"loop '$loopId' ledger line ${index + 1}: $msg")
        yield events :+ event

object LoopStore:
  /** The `.auk` directory, relative to a project root — what a store is rooted at. */
  val AukRelativePath = ".auk"

  val LoopsDirName = "loops"
  val LedgerFileName = "ledger.jsonl"
  val KnowledgeFileName = "knowledge.md"
  val GitignoreFileName = ".gitignore"

  /** The contents written to a missing `.auk/.gitignore`: everything under `.auk`,
    * the file itself included. Unlike a shell glob, a gitignore `*` matches dotfiles,
    * so the directory disappears from git entirely. */
  val IgnoreEverything = "*\n"

  /** A store rooted at `aukDir`, which should be a project's `.auk` directory. */
  def directory(aukDir: String): LoopStore = LoopStore(aukDir)

  /** The store for the project `ctx` is working in. */
  def in(ctx: RuntimeContext): LoopStore = LoopStore(ctx.resolve(AukRelativePath), ctx.fs)

  /** Loop ids are letters, digits, `_` and `-`, and at least one character.
    *
    * Tighter than a filename needs to be, because the id becomes a component of a
    * git ref (`refs/auk/snapshots/<loopId>/...`) as well as a directory name. The
    * pattern excludes `.` and `/`, so it also rules out `..` and any other way of
    * addressing something outside the loop's own directory.
    */
  def validateId(loopId: String): Either[String, Unit] =
    if loopId.matches("[A-Za-z0-9_-]+") then Right(())
    else Left(s"invalid loop id '$loopId': expected one or more of [A-Za-z0-9_-]")

  /** Whether two JSON values are the same document, for spotting a loop definition
    * whose artifact schema has drifted from the one its ledger recorded.
    *
    * Not `==`, which is what the [[Json]] ADT gives for free but answers a narrower
    * question: [[Json.Obj]] keeps its fields in a list, so `==` calls `{"a":1,"b":2}`
    * and `{"b":2,"a":1}` different documents, and a schema rendered from a map has no
    * stable field order to rely on. Here objects are compared as key/value sets
    * (last occurrence wins for a duplicated key, matching [[Json.Obj.get]]) while
    * arrays stay order-sensitive, since a JSON array's order is part of its meaning.
    *
    * Numbers compare by value — `1` and `1.0` are the same number — which is what
    * `BigDecimal` already does and is worth knowing is intended rather than incidental.
    */
  def schemasMatch(a: Json, b: Json): Boolean = (a, b) match
    case (Json.Obj(fieldsA), Json.Obj(fieldsB)) =>
      val mapA = lastWins(fieldsA)
      val mapB = lastWins(fieldsB)
      mapA.keySet == mapB.keySet && mapA.forall((key, value) => mapB.get(key).exists(schemasMatch(value, _)))
    case (Json.Arr(elemsA), Json.Arr(elemsB)) =>
      elemsA.sizeCompare(elemsB) == 0 && elemsA.lazyZip(elemsB).forall(schemasMatch)
    case (Json.Num(x), Json.Num(y)) => x == y
    case _                          => a == b

  private def lastWins(fields: List[(String, Json)]): Map[String, Json] =
    fields.foldLeft(Map.empty[String, Json])((acc, kv) => acc + kv)
