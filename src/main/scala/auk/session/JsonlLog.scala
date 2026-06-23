package auk.session

import java.io.IOException

import auk.platform.{FileSystem, PathOps, Platform}

/** Append-only JSONL helpers shared by the session, input-history, and sub-agent
  * logs: one JSON object per line, with parent directories created lazily.
  *
  * Each [[append]] flushes to disk before returning (the same durability the
  * [[Session]] log relies on), so a committed line survives a crash. This is the
  * one place the "create parent dir, append a line" pattern lives, so the several
  * logs that share it stay consistent.
  */
object JsonlLog:
  /** Append `line` (without a trailing newline) as one JSONL record, creating
    * parent directories on demand. */
  def append(path: String, line: String, fs: FileSystem = Platform.fs): Either[String, Unit] =
    try
      PathOps.parent(path).foreach(fs.createDirectories)
      fs.appendString(path, line + "\n")
      Right(())
    catch case e: IOException => Left(s"failed to append to '$path': ${e.getMessage}")

  /** Truncate `path` to empty, creating parent directories on demand — used when a
    * log is about to be rewritten from scratch (e.g. a resumed sub-agent re-running
    * an interrupted attempt), so the fresh records don't splice onto the old ones. */
  def reset(path: String, fs: FileSystem = Platform.fs): Either[String, Unit] =
    try
      PathOps.parent(path).foreach(fs.createDirectories)
      fs.writeString(path, "")
      Right(())
    catch case e: IOException => Left(s"failed to reset '$path': ${e.getMessage}")

  /** Every non-blank line of `path`, in order; a missing file reads as empty. */
  def readLines(path: String, fs: FileSystem = Platform.fs): Either[String, List[String]] =
    try
      if !fs.exists(path) then Right(Nil)
      else
        val lines = fs.readString(path).split("\n", -1).nn.toList.map(_.nn)
        Right(lines.filter(_.trim.nn.nonEmpty))
    catch case e: IOException => Left(s"failed to read '$path': ${e.getMessage}")
