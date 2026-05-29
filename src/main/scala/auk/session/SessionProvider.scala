package auk.session

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.UUID

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
  def directory(dir: Path): SessionProvider = FileSessionProvider(dir)

/** File-backed [[SessionProvider]]: each session is a `<dir>/<id>.jsonl` file. */
private final class FileSessionProvider(dir: Path) extends SessionProvider:
  private val Suffix = ".jsonl"

  private def fileFor(id: String): Path = dir.resolve(id + Suffix).nn

  def create(): Either[String, Session] =
    val id = UUID.randomUUID().toString
    try
      Files.createDirectories(dir)
      Right(Session(id, fileFor(id)))
    catch
      case e: IOException =>
        Left(s"failed to create session directory: ${e.getMessage}")

  def open(id: String): Either[String, Option[Session]] =
    val path = fileFor(id)
    if Files.isRegularFile(path) then Right(Some(Session(id, path)))
    else Right(None)

  def list(): Either[String, List[String]] =
    // `listFiles` returns null when `dir` is absent or not a directory; either
    // way there are no sessions yet.
    val files = dir.toFile.nn.listFiles()
    if files == null then Right(Nil)
    else
      val ids = files.nn.toList
        .filter(f => f.nn.isFile && f.nn.getName.nn.endsWith(Suffix))
        .sortBy(f => -f.nn.lastModified()) // newest first
        .map(f => f.nn.getName.nn.stripSuffix(Suffix))
      Right(ids)

  override def summaries(): Either[String, List[SessionSummary]] =
    val files = dir.toFile.nn.listFiles()
    if files == null then Right(Nil)
    else
      val sessionFiles = files.nn.toList
        .filter(f => f.nn.isFile && f.nn.getName.nn.endsWith(Suffix))
        .sortBy(f => -f.nn.lastModified())
      sessionFiles.foldRight[Either[String, List[SessionSummary]]](Right(Nil)):
        case (file, acc) =>
          val f = file.nn
          val id = f.getName.nn.stripSuffix(Suffix)
          for
            tail <- acc
            events <- Session(id, f.toPath.nn).events
          yield SessionSummary.from(id, Some(f.lastModified()), events) :: tail
