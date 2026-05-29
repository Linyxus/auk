package auk.session

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

/** One persistent conversation, backed by an append-only log of [[SessionEvent]]s.
  *
  * A session is the durable spine of an agent run: the model-facing history is a
  * fold over its events, so the agent loop can crash and a fresh loop can pick up
  * exactly where the last one stopped by [[replay]]ing the log. Persistence is
  * automatic — each [[append]] writes one JSON line to disk before returning, so
  * a committed event survives a crash without the caller managing files at all.
  *
  * The log is one JSON object per line (JSONL), which makes appends a pure
  * tail-write (no rewrite of prior events) and keeps a partially written final
  * line isolated to the step that was in flight.
  *
  * You normally obtain a `Session` from a [[SessionProvider]] rather than
  * constructing one directly.
  *
  * Appends and reads are serialized on a per-session lock. The agent loop drives
  * appends from a single thread, but the lock keeps a concurrent reader (e.g. a
  * UI listing the transcript) from observing a half-written tail.
  */
final class Session(val id: String, private val path: Path):
  private val lock = new AnyRef

  /** Append `event` to the log, durably, before returning. */
  def append(event: SessionEvent): Either[String, Unit] =
    lock.synchronized:
      try
        val parent = path.getParent
        if parent != null then Files.createDirectories(parent)
        Files.writeString(
          path,
          SessionEvent.encode(event) + "\n",
          UTF_8,
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
        Right(())
      catch
        case e: IOException =>
          Left(s"failed to append to session '$id': ${e.getMessage}")

  /** Every event in the log, in order. A missing file is an empty session; a
    * line that fails to parse is reported (with its position) rather than
    * silently dropped, so a corrupt log is not mistaken for a shorter history. */
  def events: Either[String, List[SessionEvent]] =
    lock.synchronized:
      if !Files.exists(path) then Right(Nil)
      else
        try
          val content = Files.readString(path, UTF_8).nn
          // `-1` keeps trailing empties so a torn final line is visible; blank
          // lines (including that trailing one) are skipped below.
          val lines = content.split("\n", -1).nn.toList.map(_.nn)
          lines.zipWithIndex
            .filter((line, _) => line.trim.nn.nonEmpty)
            .foldRight[Either[String, List[SessionEvent]]](Right(Nil)):
              case ((line, idx), acc) =>
                for
                  ev <- SessionEvent
                    .decode(line)
                    .left
                    .map(err => s"session '$id' is corrupt at line ${idx + 1}: $err")
                  rest <- acc
                yield ev :: rest
        catch
          case e: IOException =>
            Left(s"failed to read session '$id': ${e.getMessage}")
