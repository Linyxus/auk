package auk.snapshot

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Node builtins [[Worktree]] needs beyond the ones [[GitCmd]] already facades: reset is the one
  * operation in this package that edits the working tree itself, so it is the only one that removes
  * files it did not create.
  */
@js.native
@JSImport("node:fs", JSImport.Namespace)
private[snapshot] object WorktreeFs extends js.Object:
  def rmSync(path: String, options: js.Any): Unit = js.native
  def rmdirSync(path: String): Unit = js.native
  def readdirSync(path: String): js.Array[String] = js.native
  def existsSync(path: String): Boolean = js.native
  def lstatSync(path: String): WorktreeStats = js.native

@js.native
private[snapshot] trait WorktreeStats extends js.Object:
  def isDirectory(): Boolean = js.native

@js.native
@JSImport("node:path", JSImport.Namespace)
private[snapshot] object WorktreePath extends js.Object:
  def join(parts: String*): String = js.native
  def dirname(path: String): String = js.native

/** What a [[Worktree.reset]] did.
  *
  * `changed` counts the paths written out of the target tree and `deleted` the paths removed from
  * disk; a type change is counted in both, because it is both. Neither counts a path the reset found
  * already correct — those are not touched at all.
  *
  * `skippedGitlinks` is the part a caller has to act on rather than log. It names the nested
  * repositories the reset deliberately left alone, and it is non-empty exactly when the restoration
  * is incomplete: the working tree matches the target everywhere else, and differs from it by these
  * entries. A caller that cares whether it really got the recorded state back — rather than most of
  * it — checks this list rather than inferring the gap from a tree comparison.
  */
final case class ResetOutcome(changed: Int, deleted: Int, skippedGitlinks: List[String])

object ResetOutcome:
  /** The reset that had nothing to do. */
  val Unchanged: ResetOutcome = ResetOutcome(0, 0, Nil)

/** Why a [[Worktree.reset]] did not happen.
  *
  * [[Git]] wraps the package's own [[GitError]] unchanged — a repository that is not one, a revision
  * that does not resolve, a git command that failed. [[WouldClobberIgnored]] is the refusal that has
  * no [[GitError]] to wrap, because nothing went wrong: the reset was possible and was declined.
  */
enum ResetError:
  case Git(error: GitError)

  /** The reset would have had to write a file over a directory holding ignored files, and stopped
    * instead. The paths are the ignored files themselves — the things that would have been lost —
    * rather than the directories standing in the way, since those are what a human needs to see to
    * decide what to do. Nothing was touched: this is decided before the first removal.
    */
  case WouldClobberIgnored(paths: List[String])

/** Putting a recorded state back on disk, in place.
  *
  * [[Snapshot]] only ever reads the working tree. This is the other direction: after [[reset]] the
  * repository's *capturable* content — exactly what [[Snapshot.create]] would record, so tracked
  * files plus untracked ones `.gitignore` does not exclude — is the content of the given commit's
  * tree. It is meant for a caller that runs work directly in the live checkout and needs to abandon
  * it: snapshot the dirty state first, then reset back to the last state worth keeping.
  *
  * What it does not touch is the point. The real `.git/index` is never opened — not even for its
  * mtime — because the whole operation runs against a throwaway `GIT_INDEX_FILE` in the temp
  * directory, the same dance [[Snapshot.create]] uses. HEAD does not move, no branch moves, and
  * nothing is written to a reflog: this is not `git reset --hard`, which would do all three. Ignored
  * files are invisible to it, so a `.auk/`, a `node_modules/` or a `target/` survives a reset
  * untouched, with one exception spelled out below.
  *
  * The mechanism is a tree-to-tree diff rather than a checkout: what the working tree hashes to
  * right now is compared against the target tree, and only the paths that differ are touched. A path
  * that is already correct is not rewritten, so its mtime is left alone and a watcher does not see it
  * change. Removals happen before writes, so a path that changed type — a file that became a
  * directory, or the reverse — is never asked to be two things at once, and a directory emptied by
  * the removals is pruned, since a capture has no way to record an empty directory and leaving one
  * behind would be visible cruft the target state never had.
  *
  * Two places the operation refuses to be simple, both observed against git rather than assumed:
  *
  *   - A gitlink is left exactly as it is, in both directions. Removing one would mean deleting a
  *     nested repository's own `.git`, and no snapshot holds that — a snapshot records the gitlink's
  *     object id and nothing else, so the deletion would be unrecoverable. Restoring one is not
  *     possible either: `checkout-index` for a gitlink path succeeds and leaves an empty directory.
  *     So a reset across a submodule boundary does not make the trees equal, and the difference is
  *     exactly the gitlink entries — which is why [[ResetOutcome.skippedGitlinks]] reports them by
  *     name instead of leaving a caller to notice. Submodules are out of scope for v1 here as they
  *     are in [[Snapshot]].
  *   - A path the target tree records as a file, standing in the working tree as a *directory*
  *     holding ignored files, is not reset: the whole operation refuses with
  *     [[ResetError.WouldClobberIgnored]] before touching anything. Left to itself `git
  *     checkout-index -f` removes that directory, ignored files and all, to write the file — and
  *     ignored files are precisely the ones no snapshot holds, so nothing could put them back. A
  *     directory holding only capturable content is not affected by this: everything in it is a
  *     removal of its own and is gone, and the directory pruned, before the write.
  *
  * Failures come back as `Left`, as everywhere else in this package. Success is not all-or-nothing,
  * though, and a caller should not read it that way: the removals and the writes are separate git
  * and filesystem operations, so a command that fails midway can leave the working tree partly
  * reset. There is no rollback here, and there does not need to be — the caller's snapshot-first
  * policy is the recovery story, since the state this was called on was recorded before it ran. What
  * *is* guaranteed is that the one destructive case that could not be recovered from, above, is
  * decided before the first removal.
  *
  * A filesystem error while removing a file — a directory that cannot be written, say — throws
  * rather than returning a `Left`, the same way [[Snapshot]]'s own temp-index cleanup does: the
  * `Either` channel is for failures a caller can expect, and being unable to unlink a file that was
  * on disk a moment ago is not one of them.
  */
object Worktree:

  /** Makes the repository containing `dir` hold the capturable content of `commit`'s tree.
    *
    * `dir` may be any directory inside the repository; like [[Snapshot.create]], the operation
    * covers the whole repository rather than that subtree. `commit` is anything git can peel to a
    * tree — a commit id, `HEAD`, or a snapshot's `refs/auk/snapshots/<id>` — exactly as
    * [[Snapshot.treeOf]] accepts it; anything else comes back as [[GitError.CommandFailed]] carrying
    * git's own complaint, wrapped in [[ResetError.Git]]. Resetting to the state already on disk is a
    * no-op that writes nothing and reports [[ResetOutcome.Unchanged]].
    */
  def reset(dir: String, commit: String): Either[ResetError, ResetOutcome] =
    for
      root    <- git(toplevelOf(dir))
      target  <- git(Snapshot.treeOf(root, commit))
      current <- git(Snapshot.currentTree(root))
      outcome <- if current == target then Right(ResetOutcome.Unchanged)
                 else applyDiff(root, current, target)
    yield outcome

  /** Everything below reports the package's own [[GitError]]; this is where it becomes a reason a
    * reset did not happen.
    */
  private def git[A](result: Either[GitError, A]): Either[ResetError, A] =
    result.left.map(ResetError.Git.apply)

  // -----------------------------------------------------------------------------------------
  // Steps
  // -----------------------------------------------------------------------------------------

  /** Every worktree-touching command runs here rather than in the caller's directory:
    * `checkout-index` resolves the paths it is fed against the current directory, and the paths in a
    * tree diff are relative to the top of the repository. Resolving it up front is also what makes
    * `dir` free to be any directory inside the repository.
    */
  private def toplevelOf(dir: String): Either[GitError, String] =
    GitCmd.run(dir, Map.empty, "rev-parse", "--show-toplevel") match
      case Right(root) if root.nonEmpty => Right(root)
      case _                            => Left(GitError.NotARepository(dir))

  /** The order is the contract: nothing is removed until the whole plan is known to be safe to
    * carry out, and nothing is written until the removals have made room for it.
    */
  private def applyDiff(
    root: String,
    current: String,
    target: String
  ): Either[ResetError, ResetOutcome] =
    for
      changes <- git(changesBetween(root, current, target))
      wanted   = changes.filter(_.materialized).map(_.path)
      _       <- refuseIfIgnoredWouldGo(root, wanted)
      removals = changes.filter(_.removed)
      _        = removals.foreach(change => remove(root, change.path))
      _       <- if wanted.isEmpty then Right(()) else git(materialize(root, target, wanted))
    yield ResetOutcome(wanted.size, removals.size, changes.filter(_.gitlink).map(_.path))

  /** One path the reset has to act on, in the direction it has to act.
    *
    * A type change is both a removal and a write — the old file goes, the new one is written in its
    * place — and a modification is neither, just a write over what is there. A gitlink is a third
    * thing: a path in the diff that the reset will not act on at all, kept so it can be reported.
    */
  private final case class Change(
    path: String,
    removed: Boolean,
    materialized: Boolean,
    gitlink: Boolean
  )

  /** The mode git gives a nested repository, and the whole basis for telling one apart. */
  private val Gitlink = "160000"

  /** The separator `-z` writes with, and the terminator `--stdin` expects after every path. */
  private val Nul = '\u0000'

  /** `--raw -z` rather than `--name-status`: `-z` because it is the only output format git does not
    * quote — `core.quotePath` escaping is a documented gap in [[Snapshot.diffTrees]] and there is no
    * reason to inherit it here — and `--raw` because the modes it carries are what identifies a
    * gitlink, which this operation has to refuse to touch. Rename detection is off in plumbing
    * already; `--no-renames` says so out loud, since a rename reported as one would hide a path that
    * has to be written.
    */
  private def changesBetween(
    root: String,
    current: String,
    target: String
  ): Either[GitError, List[Change]] =
    GitCmd
      .raw(root, Map.empty, "diff-tree", "-r", "-z", "--no-renames", current, target)
      .map(parseChanges)

  /** git writes one record per changed path: a NUL-terminated
    * `:<oldmode> <newmode> <oldsha> <newsha> <status>` field, then a NUL-terminated path. So
    * splitting on the separator yields metadata and path alternately. A record naming a gitlink on
    * either side is neutered whole rather than half — skipping only the removal would still let the
    * write destroy the nested repository the removal was spared — but it is kept in the plan, since
    * being able to name what was skipped is the whole point of reporting it.
    */
  private def parseChanges(output: String): List[Change] =
    output
      .split(Nul)
      .grouped(2)
      .flatMap:
        case Array(meta, path) if meta.startsWith(":") =>
          meta.drop(1).split(' ') match
            case Array(oldMode, newMode, _, _, status) =>
              if oldMode == Gitlink || newMode == Gitlink then
                Some(Change(path, removed = false, materialized = false, gitlink = true))
              else
                Some(Change(path, status == "D" || status == "T", status != "D", gitlink = false))
            case _ => None
        case _ => None
      .toList

  /** The pre-flight: the reset declines rather than destroying ignored files.
    *
    * `checkout-index -f` will remove a directory standing where it has to write a file — ignored
    * contents and all — and ignored files are exactly the ones no snapshot holds, so nothing could
    * put them back. Snapshot-first is not a recovery story for content that was never captured.
    *
    * Only paths the reset is about to write can collide, and only where a directory really stands in
    * the way, so those are the ones asked about. A symlink pointing at a directory is not one of
    * them: unlinking it writes the file without disturbing what it pointed at, which is why this
    * looks at the path itself rather than following it. Everything capturable in such a directory is
    * a removal of its own and will be gone before the write, so the question is narrower than "is
    * this directory empty" — it is "does git consider anything in here ignored", which is what
    * `ls-files --others --ignored` answers, descending into wholly-ignored directories as well.
    */
  private def refuseIfIgnoredWouldGo(root: String, wanted: List[String]): Either[ResetError, Unit] =
    val blocked = wanted.filter(path => isDirectory(WorktreePath.join(root, path)))
    if blocked.isEmpty then Right(())
    else
      val args =
        List("ls-files", "-z", "--others", "--ignored", "--exclude-standard", "--") ++ blocked
      git(GitCmd.raw(root, Map.empty, args*)).flatMap: listing =>
        val doomed = listing.split(Nul).filter(_.nonEmpty).toList
        if doomed.isEmpty then Right(()) else Left(ResetError.WouldClobberIgnored(doomed))

  /** `lstat`, so a symlink is a symlink rather than whatever it points at, and guarded by an
    * existence check because most paths a reset writes are not there yet.
    */
  private def isDirectory(path: String): Boolean =
    WorktreeFs.existsSync(path) && WorktreeFs.lstatSync(path).isDirectory()

  /** Deliberately not a recursive removal: every path git records outside a gitlink is a file or a
    * symlink, gitlinks never reach here, and a plain unlink cannot take a directory tree with it if
    * that assumption is ever wrong. `force` covers the path having gone already.
    */
  private def remove(root: String, path: String): Unit =
    val options = js.Dictionary.empty[js.Any]
    options("force") = true
    WorktreeFs.rmSync(WorktreePath.join(root, path), options)
    pruneEmptyParents(root, path)

  /** Walks up from a removed path, dropping directories the removal emptied and stopping at the
    * first one that still holds something — including anything ignored, which is what keeps this
    * from reaching into a directory the target state has no opinion about. The repository root is
    * the fence, and it is never a candidate itself.
    */
  private def pruneEmptyParents(root: String, path: String): Unit =
    val fence = root + "/"
    var directory = WorktreePath.dirname(WorktreePath.join(root, path))
    while prunable(fence, directory) do
      WorktreeFs.rmdirSync(directory)
      directory = WorktreePath.dirname(directory)

  private def prunable(fence: String, directory: String): Boolean =
    directory.startsWith(fence)
      && WorktreeFs.existsSync(directory)
      && WorktreeFs.readdirSync(directory).length == 0

  /** Writes the wanted paths out of the target tree with `checkout-index`, which is git's own
    * blob-to-file step: it creates leading directories, restores the executable bit, and writes a
    * symlink as a symlink rather than as a file holding its target. The index it reads is a private
    * one holding nothing but the target tree, so the user's own index is neither read nor written —
    * and `-u` is deliberately absent, which is the flag that would have written one.
    */
  private def materialize(
    root: String,
    target: String,
    paths: List[String]
  ): Either[GitError, Unit] =
    withTempIndex: index =>
      val env = Map("GIT_INDEX_FILE" -> index)
      for
        _ <- GitCmd.run(root, env, (PlainIndex ++ List("read-tree", target))*)
        _ <- checkoutIndex(root, env, paths)
      yield ()

  /** The paths go in on stdin rather than in the argument vector: a generation's worth of changes
    * can be more paths than the platform's argument limit allows, and `-z` means a name holding a
    * newline is still one name.
    */
  private def checkoutIndex(
    root: String,
    env: Map[String, String],
    paths: List[String]
  ): Either[GitError, Unit] =
    val args = PlainIndex ++ List("checkout-index", "-f", "-z", "--stdin")
    gitWithInput(root, env, paths.mkString("", Nul.toString, Nul.toString), args*).map(_ => ())

  // -----------------------------------------------------------------------------------------
  // The git binary, with something on stdin
  // -----------------------------------------------------------------------------------------

  /** [[GitCmd]] closes stdin so git can never block on a prompt, which is right for every command
    * in this package but one. This is that one, and it stays here rather than widening [[GitCmd]]:
    * feeding a command stdin is this operation's need, not the package's.
    *
    * Node's `input` option supersedes `stdio[0]`, and the environment is copied in wholesale for the
    * same reason [[GitCmd]] does it — `execFileSync` replaces it rather than extending it.
    */
  private def gitWithInput(
    dir: String,
    extraEnv: Map[String, String],
    input: String,
    args: String*
  ): Either[GitError.CommandFailed, String] =
    val argv = "-C" :: dir :: args.toList
    val options = js.Dictionary.empty[js.Any]
    val env = js.Dictionary.empty[String]
    NodeProcess.env.foreach((name, value) => env(name) = value)
    extraEnv.foreach((name, value) => env(name) = value)
    options("cwd") = dir
    options("env") = env
    options("encoding") = "utf8"
    options("input") = input
    options("stdio") = js.Array[js.Any]("pipe", "pipe", "pipe")
    options("maxBuffer") = MaxBuffer
    try Right(text(NodeChildProcess.execFileSync("git", js.Array(argv*), options)))
    catch
      case js.JavaScriptException(error) =>
        val thrown = error.asInstanceOf[js.Dynamic]
        Left(GitError.CommandFailed(argv, status(thrown), text(thrown.stderr).trim))

  /** 64 MiB, matching [[GitCmd]]: `checkout-index` says nothing on success, but a failure reports a
    * line per path.
    */
  private val MaxBuffer = 64 * 1024 * 1024

  private def status(error: js.Dynamic): Int =
    val code = error.status
    if code == null || js.isUndefined(code) then -1 else code.asInstanceOf[Int]

  private def text(value: js.Any): String =
    if value == null || js.isUndefined(value) then "" else value.toString

  // -----------------------------------------------------------------------------------------
  // Temp index
  // -----------------------------------------------------------------------------------------

  /** The same `core.splitIndex=false` [[Snapshot]] pins, for the same reason: with it on, writing
    * the index drops a `sharedindex.*` file next to it, inside the user's `.git`.
    */
  private val PlainIndex = List("-c", "core.splitIndex=false")

  /** The same prefix [[Snapshot]] allocates under, so a leftover from either is caught by the same
    * check — the name says whose it is, and a leftover is a bug.
    */
  private val IndexPrefix = "auk-index-"

  private var indexCounter = 0

  private def withTempIndex[A](body: String => Either[GitError, A]): Either[GitError, A] =
    val index = allocateIndex()
    try body(index)
    finally removeIndex(index)

  private def allocateIndex(): String =
    indexCounter += 1
    val random = (js.Math.random() * 1e9).toInt
    NodePath.join(NodeOs.tmpdir(), s"$IndexPrefix${NodeProcess.pid}-reset-$indexCounter-$random")

  private def removeIndex(index: String): Unit =
    val options = js.Dictionary.empty[js.Any]
    options("force") = true
    NodeFs.rmSync(index, options)
    NodeFs.rmSync(index + ".lock", options)
