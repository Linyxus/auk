package auk.session

import auk.llm.tools.Json
import auk.llm.tools.Json.get
import auk.platform.{FileSystem, Platform}

/** One recorded prompt: the text, the session it was submitted in, and when. */
final case class InputEntry(text: String, sessionId: String, atMs: Long)

/** A durable, cross-session record of every prompt the user submits, appended to
  * `.auk/history.jsonl` (one JSON object per line).
  *
  * Unlike the per-session conversation log — where inputs live only as that
  * session's `UserSubmitted` events — this spans *all* sessions, so it survives
  * `/new` and `/resume` and gives one place to see everything the user has ever
  * typed. Writes are best-effort: a failure is reported but never blocks a turn.
  * The clock is the caller's (`atMs`), keeping this pure and unit-testable.
  */
final class InputHistory(path: String, fs: FileSystem = Platform.fs):
  /** Append one submitted prompt. */
  def record(text: String, sessionId: String, atMs: Long): Either[String, Unit] =
    JsonlLog.append(path, encode(InputEntry(text, sessionId, atMs)), fs)

  /** Every recorded entry, oldest first; a corrupt line is reported, not dropped. */
  def entries: Either[String, List[InputEntry]] =
    JsonlLog.readLines(path, fs).flatMap: lines =>
      lines.foldRight[Either[String, List[InputEntry]]](Right(Nil)):
        case (line, acc) => for e <- decode(line); rest <- acc yield e :: rest

  private def encode(e: InputEntry): String =
    Json.Obj(List(
      "ts"      -> Json.num(e.atMs),
      "session" -> Json.Str(e.sessionId),
      "text"    -> Json.Str(e.text)
    )).render

  private def decode(line: String): Either[String, InputEntry] =
    Json.parse(line).flatMap:
      case obj: Json.Obj =>
        obj.get("text") match
          case Some(Json.Str(text)) =>
            Right(InputEntry(
              text = text,
              sessionId = obj.get("session").collect { case Json.Str(s) => s }.getOrElse(""),
              atMs = obj.get("ts").collect { case Json.Num(n) => n.toLong }.getOrElse(0L)
            ))
          case _ => Left("input entry is missing a string 'text'")
      case other => Left(s"input entry should be an object but was ${other.typeName}")

object InputHistory:
  /** Standard location of the input-history log, relative to a project root. */
  val RelativePath = ".auk/history.jsonl"
