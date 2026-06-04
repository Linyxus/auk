package auk.session

import java.io.IOException

import auk.platform.{FileSystem, DirEntry, PathOps, Platform}

/** A registry of [[Session]]s: create new ones, reopen old ones, enumerate them.
  *
  * This is the boundary the agent loop talks to when it wants persistence. It
  * deliberately says nothing about *where* sessions live — the default
  * implementation keeps a JSONL log per session on disk (see
  * [[SessionProvider.directory]]), but a test or an alternate backend can supply
  * its own.
  */
trait SessionProvider:
  /** Begin a fresh session with a newly minted id. The session's log file is
    * created lazily, on its first [[Session.append]]. */
  def create(): Either[String, Session]

  /** Reopen the session with `id`, or `None` if no such session exists. */
  def open(id: String): Either[String, Option[Session]]

  /** All known session ids, most-recently-modified first. */
  def list(): Either[String, List[String]]

  /** Summaries for all known sessions, most-recently-modified first. */
  def summaries(): Either[String, List[SessionSummary]] =
    list().flatMap: ids =>
      ids.foldRight[Either[String, List[SessionSummary]]](Right(Nil)):
        case (id, acc) =>
          for
            tail <- acc
            maybeSession <- open(id)
            summary <- maybeSession match
              case Some(session) =>
                session.events.map(events => SessionSummary.from(id, None, events))
              case None =>
                Right(SessionSummary.from(id, None, Nil))
          yield summary :: tail

  /** The most recently modified session, if any — the natural one to resume. */
  def latest(): Either[String, Option[Session]] =
    list().flatMap:
      case Nil          => Right(None)
      case newest :: _  => open(newest)

object SessionProvider:
  /** Standard location of the session logs, relative to a project root. */
  val RelativePath = ".auk/sessions"

  /** A file-backed provider storing one `<id>.jsonl` log per session under
    * `dir`. Pass e.g. `ctx.resolve(SessionProvider.RelativePath)`. */
  def directory(dir: String): SessionProvider = FileSessionProvider(dir)

/** File-backed [[SessionProvider]]: each session is a `<dir>/<id>.jsonl` file. */
private final class FileSessionProvider(dir: String, fs: FileSystem = Platform.fs) extends SessionProvider:
  private val Suffix = ".jsonl"

  private def fileFor(id: String): String = PathOps.join(dir, id + Suffix)

  /** Session log files in `dir`, newest first. Empty if `dir` is absent. */
  private def logFiles(): List[DirEntry] =
    fs.listDir(dir)
      .filter(e => e.isFile && e.name.endsWith(Suffix))
      .sortBy(e => -e.mtimeMs) // newest first

  def create(): Either[String, Session] =
    val id = Platform.uuid.random()
    try
      fs.createDirectories(dir)
      Right(Session(id, fileFor(id), fs))
    catch
      case e: IOException =>
        Left(s"failed to create session directory: ${e.getMessage}")

  def open(id: String): Either[String, Option[Session]] =
    val path = fileFor(id)
    if fs.isRegularFile(path) then Right(Some(Session(id, path, fs)))
    else Right(None)

  def list(): Either[String, List[String]] =
    Right(logFiles().map(_.name.stripSuffix(Suffix)))

  override def summaries(): Either[String, List[SessionSummary]] =
    logFiles().foldRight[Either[String, List[SessionSummary]]](Right(Nil)):
      case (entry, acc) =>
        val id = entry.name.stripSuffix(Suffix)
        for
          tail <- acc
          events <- Session(id, PathOps.join(dir, entry.name), fs).events
        yield SessionSummary.from(id, Some(entry.mtimeMs), events) :: tail
