package auk.snapshot

/** Failures produced by the snapshot primitive.
  *
  * Everything in this package returns `Either[GitError, ?]`; nothing throws for an expected
  * failure. The first four cases name a condition the caller can act on. [[CommandFailed]] is the
  * fallback for a `git` invocation that exited nonzero without a more specific meaning: it carries
  * the argument vector as handed to the binary — leading `-C <dir>` included — so the failing
  * command can be reproduced by hand, plus the exit code (`-1` when the process was killed by a
  * signal instead of exiting) and whatever git wrote to stderr.
  */
enum GitError:
  case NotARepository(dir: String)
  case InvalidId(id: String, reason: String)
  case SnapshotExists(id: String)
  case SnapshotNotFound(id: String)
  case CommandFailed(args: List[String], exitCode: Int, stderr: String)
