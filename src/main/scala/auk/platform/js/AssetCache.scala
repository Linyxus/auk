package auk.platform.js

import scala.scalajs.js

/** Shared publish step for the content-addressed SEA asset caches
  * ([[WebUiAssets]] and [[ReplArtifacts]]): atomically move a fully-written
  * staging directory into its final location under the system temp dir.
  *
  * Both caches key the final dir on a content tag, so any two copies for the
  * same tag are byte-identical. Extraction writes to a process-private `tmp`
  * and renames it into place, so a concurrent auk never observes a half-written
  * cache; losing the rename race just means using the winner's identical copy.
  *
  * The subtlety this centralizes: `rename(2)` refuses to replace a NON-EMPTY
  * directory (`ENOTEMPTY`). A `dir` left incomplete by an interrupted earlier
  * run (or an older build) would otherwise wedge every future run forever — the
  * rename keeps failing and the failure was previously swallowed as "lost the
  * race". [[publish]] detects that case, clears the stale dir, and retries.
  */
private[platform] object AssetCache:
  private def rmOpts = js.Dynamic.literal(recursive = true, force = true).asInstanceOf[js.Object]

  /** Move the fully-written staging dir `tmp` into `dir`. `complete` reports
    * whether `dir` currently holds a usable cache (re-evaluated as the
    * filesystem changes). Returns true once `dir` is present and complete.
    *
    * On a clean run the rename onto a missing `dir` just succeeds. If `dir`
    * already exists it is either a complete copy from a concurrent winner (used
    * as-is) or a stale/partial cache from an interrupted run — in the latter
    * case the rename hits `ENOTEMPTY`, so we drop the stale dir and retry.
    * Dropping it is safe because every copy for this tag is byte-identical.
    */
  def publish(tmp: String, dir: String, complete: => Boolean): Boolean =
    def install(): Boolean =
      try { NodeFs.renameSync(tmp, dir); true }
      catch case _: Throwable => false
    if install() || complete then true
    else
      try NodeFs.rmSync(dir, rmOpts)
      catch case _: Throwable => ()
      install() || complete
