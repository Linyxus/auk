package auk.snapshot

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Everything auk adds on top of the suites ported from athame.
  *
  * [[SnapshotSuite]] and [[TreeOfSuite]] arrived verbatim and stay that way, so a future re-sync
  * against athame diffs cleanly; nothing in this file touches them or their fixture. What they cover
  * is the primitive's contract. What this file covers is the rest of the world it meets — the file
  * kinds git records specially, the shapes a working directory can take, what a snapshot is worth
  * once it is put back on disk, and the isolation the implementation promises.
  *
  * Every test here pins behavior that was observed first and asserted second: the armored diff
  * headers, the octal escaping of non-ASCII paths and the retention contract around `git gc` are
  * git's, not this codebase's, and they are written down so a change in either would be noticed.
  */

/** Node builtins these tests need beyond the ones [[TestFs]] already facades. */
@js.native
@JSImport("node:fs", JSImport.Namespace)
private[snapshot] object ExtFs extends js.Object:
  def symlinkSync(target: String, path: String): Unit = js.native
  def readlinkSync(path: String): String = js.native
  def lstatSync(path: String): ExtStats = js.native
  def writeFileSync(path: String, data: String): Unit = js.native

@js.native
private[snapshot] trait ExtStats extends js.Object:
  def mode: Int = js.native
  def size: Double = js.native
  def mtimeMs: Double = js.native

/** One line of `git ls-tree -r`. The mode is the point: a symlink, a gitlink and an executable file
  * are all just "a path with content" until it is read.
  */
private[snapshot] final case class TreeEntry(mode: String, kind: String, oid: String, path: String)

/** Extra coverage for [[Snapshot]], in one suite so the private `TMPDIR` below is claimed and
  * released exactly once.
  */
class SnapshotExtSuite extends munit.FunSuite:

  // -------------------------------------------------------------------------------------------
  // Fixtures
  // -------------------------------------------------------------------------------------------

  /** These tests never inspect the shared temp directory, but they do fill it: every snapshot
    * writes an index file while it works, and [[SnapshotSuite]]'s hygiene tests count what appears
    * there. So this suite takes its temp files somewhere private, the way [[TreeOfSuite]] does —
    * `os.tmpdir()` reads `TMPDIR` on every call, so redirecting it moves this process's temp files
    * and nobody else's.
    */
  private var releaseTmpdir: () => Unit = () => ()

  override def beforeAll(): Unit =
    val previous = NodeProcess.env.get("TMPDIR")
    val own = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "auk-ext-"))
    NodeProcess.env("TMPDIR") = own
    releaseTmpdir = () =>
      previous match
        case Some(value) => NodeProcess.env("TMPDIR") = value
        case None        => NodeProcess.env -= "TMPDIR"
      TestFs.rmSync(own, recursiveForce)

  override def afterAll(): Unit = releaseTmpdir()

  private def withRepo(body: TempRepo => Unit): Unit =
    val repo = TempRepo.create()
    try body(repo)
    finally repo.dispose()

  private def created(result: Either[GitError, SnapshotCreated])(using
    munit.Location
  ): SnapshotCreated =
    result match
      case Right(snapshot) => snapshot
      case Left(error)     => fail(s"expected a snapshot, got $error")

  private def snapshot(repo: TempRepo, id: String, force: Boolean = false)(using
    munit.Location
  ): SnapshotCreated =
    created(Snapshot.create(repo.dir, id, force))

  private def seed(repo: TempRepo): String =
    repo.write("a.txt", "hello")
    repo.write("sub/deep/c.txt", "deep")
    repo.commit("init")

  /** Runs git in any directory, failing loudly: a broken fixture should not look like a failed
    * assertion. [[TempRepo.git]] is bound to its own repository, and some of these tests need a
    * second one.
    */
  private def gitIn(dir: String, args: String*)(using munit.Location): String =
    GitCmd.run(dir, Map.empty, args*) match
      case Right(output) => output
      case Left(error)   => fail(s"fixture command 'git ${args.mkString(" ")}' failed: $error")

  /** Every entry a commit records, mode and object id included.
    *
    * `core.quotePath=false` so a non-ASCII path arrives as itself rather than as octal escapes; a
    * path holding a quote or a backslash is still quoted by git, which is its own rule and one of
    * the things pinned below.
    */
  private def entriesIn(dir: String, commit: String)(using munit.Location): List[TreeEntry] =
    gitIn(dir, "-c", "core.quotePath=false", "ls-tree", "-r", commit).linesIterator
      .filter(_.nonEmpty)
      .map { line =>
        val (meta, tabbedPath) = line.span(_ != '\t')
        meta.split(' ') match
          case Array(mode, kind, oid) => TreeEntry(mode, kind, oid, tabbedPath.drop(1))
          case _                      => fail(s"unparsable ls-tree line '$line'")
      }
      .toList

  private def entries(repo: TempRepo, commit: String)(using munit.Location): List[TreeEntry] =
    entriesIn(repo.dir, commit)

  private def entry(repo: TempRepo, commit: String, path: String)(using
    munit.Location
  ): TreeEntry =
    entries(repo, commit)
      .find(_.path == path)
      .getOrElse(fail(s"'$path' is not in the tree of $commit"))

  /** The object id git would give a file as it sits on disk. Equality against a tree entry's id is
    * the byte-for-byte check: same id, same bytes, whatever those bytes are.
    */
  private def hashOnDisk(repo: TempRepo, relative: String): String =
    repo.git("hash-object", "--", relative)

  private def patchOf(repo: TempRepo, from: String, to: String)(using munit.Location): String =
    Snapshot.diffTrees(repo.dir, from, to) match
      case Right(patch) => patch
      case other        => fail(s"expected a patch, got $other")

  /** Leftover temp indexes belonging to this process, counted the way [[SnapshotSuite]] counts
    * them — inside this suite's own temp directory.
    */
  private def tempIndexes(): Set[String] =
    TestFs.readdirSync(TestOs.tmpdir()).toSet.filter(_.startsWith(s"auk-index-${NodeProcess.pid}-"))

  private def scratchPath(prefix: String): String =
    TestPath.join(TestOs.tmpdir(), s"$prefix${(js.Math.random() * 1e9).toInt}")

  /** Checks `commit` out into a throwaway worktree and hands `body` its path. A detached worktree is
    * the only way to see a snapshot as files without writing over the checkout it came from.
    */
  private def restored(repo: TempRepo, commit: String)(body: String => Unit): Unit =
    val at = scratchPath("auk-restore-")
    repo.git("worktree", "add", "--detach", "-q", at, commit)
    try body(at)
    finally
      TestFs.rmSync(at, recursiveForce)
      repo.git("worktree", "prune")

  /** A linked worktree of `repo` on a branch of its own. */
  private def withLinkedWorktree(repo: TempRepo, branch: String)(body: String => Unit): Unit =
    val at = scratchPath("auk-linked-")
    repo.git("worktree", "add", "-q", "-b", branch, at)
    try body(at)
    finally
      TestFs.rmSync(at, recursiveForce)
      repo.git("worktree", "prune")

  private def read(where: String, relative: String): String =
    TestFs.readFileSync(TestPath.join(where, relative), "utf8")

  private def exists(where: String, relative: String): Boolean =
    TestFs.existsSync(TestPath.join(where, relative))

  /** A name outside ASCII, written as escapes so this source file stays ASCII. Han ideographs have
    * no canonical decomposition, so no filesystem can hand the name back in a different normal form
    * than it went in as — which would be a flaky test rather than a real one.
    */
  private val UnicodeName = "\u6587\u4ef6.txt"

  /** git quotes a path holding a `"` whatever `core.quotePath` says, and escapes the quote inside
    * it. This is that name as git writes it back.
    */
  private val QuotedName = "quo\"te.txt"
  private val QuotedNameAsGitWritesIt = "\"quo\\\"te.txt\""

  /** Content git calls binary: it looks for a NUL in the opening bytes and finds one first thing. */
  private val BinaryPayload = "\u0000\u0001binary\u0000 payload\n"
  private val OtherBinaryPayload = "\u0000\u0001binary\u0000 payload, revised\n"

  // -------------------------------------------------------------------------------------------
  // Symlinks
  // -------------------------------------------------------------------------------------------

  test("symlinks: a symlink is recorded as a link, not as a copy of what it points at"):
    assume(TestOs.platform() != "win32", "symlinks are a POSIX fixture here")
    withRepo: repo =>
      seed(repo)
      ExtFs.symlinkSync("a.txt", repo.path("link.txt"))
      val result = snapshot(repo, "s1")
      val recorded = entry(repo, result.commit, "link.txt")
      assertEquals(recorded.mode, "120000")
      assertEquals(recorded.kind, "blob")
      // The blob holds the target path. A copy would have held "hello", the target's content.
      assertEquals(repo.git("cat-file", "blob", recorded.oid), "a.txt")
      assertNotEquals(recorded.oid, entry(repo, result.commit, "a.txt").oid)

  test("symlinks: a symlink with nothing at the other end neither breaks create nor changes shape"):
    assume(TestOs.platform() != "win32", "symlinks are a POSIX fixture here")
    withRepo: repo =>
      seed(repo)
      ExtFs.symlinkSync("nowhere.txt", repo.path("dangling.txt"))
      val result = snapshot(repo, "s1")
      val recorded = entry(repo, result.commit, "dangling.txt")
      assertEquals(recorded.mode, "120000")
      assertEquals(repo.git("cat-file", "blob", recorded.oid), "nowhere.txt")

  test("symlinks: a restored symlink is still a symlink to the same target"):
    assume(TestOs.platform() != "win32", "symlinks are a POSIX fixture here")
    withRepo: repo =>
      seed(repo)
      ExtFs.symlinkSync("a.txt", repo.path("link.txt"))
      val result = snapshot(repo, "s1")
      restored(repo, result.commit): where =>
        assertEquals(ExtFs.readlinkSync(TestPath.join(where, "link.txt")), "a.txt")
        assertEquals(read(where, "link.txt"), "hello")

  // -------------------------------------------------------------------------------------------
  // The executable bit
  // -------------------------------------------------------------------------------------------

  test("modes: an executable file is recorded 100755 and a plain one 100644"):
    assume(TestOs.platform() != "win32", "the executable bit is a POSIX permission bit")
    withRepo: repo =>
      seed(repo)
      repo.write("script.sh", "#!/bin/sh\necho hi\n")
      repo.makeExecutable("script.sh")
      val result = snapshot(repo, "s1")
      assertEquals(entry(repo, result.commit, "script.sh").mode, "100755")
      assertEquals(entry(repo, result.commit, "a.txt").mode, "100644")

  test("modes: clearing the executable bit is recorded too"):
    assume(TestOs.platform() != "win32", "the executable bit is a POSIX permission bit")
    withRepo: repo =>
      repo.write("script.sh", "#!/bin/sh\n")
      repo.makeExecutable("script.sh")
      val head = repo.commit("init")
      assertEquals(entry(repo, head, "script.sh").mode, "100755")
      repo.chmod("script.sh", Integer.parseInt("644", 8))
      val result = snapshot(repo, "s1")
      assertEquals(entry(repo, result.commit, "script.sh").mode, "100644")

  test("modes: the executable bit survives a round trip through a snapshot"):
    assume(TestOs.platform() != "win32", "the executable bit is a POSIX permission bit")
    withRepo: repo =>
      seed(repo)
      repo.write("script.sh", "#!/bin/sh\n")
      repo.makeExecutable("script.sh")
      val result = snapshot(repo, "s1")
      val executable = Integer.parseInt("111", 8)
      restored(repo, result.commit): where =>
        assertNotEquals(ExtFs.lstatSync(TestPath.join(where, "script.sh")).mode & executable, 0)
        assertEquals(ExtFs.lstatSync(TestPath.join(where, "a.txt")).mode & executable, 0)

  // -------------------------------------------------------------------------------------------
  // Exotic filenames
  // -------------------------------------------------------------------------------------------

  test("names: spaces and non-ASCII are captured, and come back as themselves"):
    withRepo: repo =>
      seed(repo)
      repo.write("a file with spaces.txt", "spaced")
      repo.write(UnicodeName, "outside ascii")
      val result = snapshot(repo, "s1")
      val paths = entries(repo, result.commit).map(_.path)
      assert(paths.contains("a file with spaces.txt"), clue(paths))
      assert(paths.contains(UnicodeName), clue(paths))
      assertEquals(repo.show(result.commit, "a file with spaces.txt"), "spaced")
      assertEquals(repo.show(result.commit, UnicodeName), "outside ascii")

  test("names: a quote in a name is captured, and git writes the name back quoted"):
    withRepo: repo =>
      seed(repo)
      repo.write(QuotedName, "quoted")
      val result = snapshot(repo, "s1")
      val paths = entries(repo, result.commit).map(_.path)
      // Even with core.quotePath=false: quoting a `"` is not the non-ASCII rule, it is git's
      // unambiguity rule, and it applies regardless.
      assert(paths.contains(QuotedNameAsGitWritesIt), clue(paths))
      assertEquals(repo.show(result.commit, QuotedName), "quoted")

  test("names: a diff over a non-ASCII name carries git's own octal escaping"):
    withRepo: repo =>
      seed(repo)
      repo.write(UnicodeName, "before\n")
      val first = snapshot(repo, "s1")
      repo.write(UnicodeName, "after\n")
      val second = snapshot(repo, "s2")
      val patch = patchOf(repo, first.tree, second.tree)
      // diffTrees pins the prefixes and turns off color, textconv and renames, but not
      // core.quotePath — so a non-ASCII path arrives escaped byte by byte, in octal, inside quotes.
      // That is the armored output as it stands, and a caller reconstructing paths has to expect it.
      assert(
        patch.contains("diff --git \"a/\\346\\226\\207\\344\\273\\266.txt\" \"b/\\346\\226\\207\\344\\273\\266.txt\""),
        clue(patch)
      )
      assert(patch.contains("-before"), clue(patch))
      assert(patch.contains("+after"), clue(patch))

  test("names: a diff quotes a name with a quote and leaves a name with spaces alone"):
    withRepo: repo =>
      seed(repo)
      repo.write(QuotedName, "before\n")
      repo.write("a file with spaces.txt", "before\n")
      val first = snapshot(repo, "s1")
      repo.write(QuotedName, "after\n")
      repo.write("a file with spaces.txt", "after\n")
      val second = snapshot(repo, "s2")
      val patch = patchOf(repo, first.tree, second.tree)
      assert(patch.contains("diff --git \"a/quo\\\"te.txt\" \"b/quo\\\"te.txt\""), clue(patch))
      // A space needs no escaping, so the header keeps it raw — the two-argument shape of the line
      // is what makes it parseable, not any quoting.
      assert(
        patch.contains("diff --git a/a file with spaces.txt b/a file with spaces.txt"),
        clue(patch)
      )

  // -------------------------------------------------------------------------------------------
  // Binary files
  // -------------------------------------------------------------------------------------------

  test("binary: a file with NUL bytes is captured byte for byte"):
    withRepo: repo =>
      seed(repo)
      repo.write("blob.bin", BinaryPayload)
      val result = snapshot(repo, "s1")
      // Content addressing is the assertion: same object id, same bytes, and no decoding as text
      // anywhere in the comparison.
      assertEquals(entry(repo, result.commit, "blob.bin").oid, hashOnDisk(repo, "blob.bin"))
      // And nothing was dropped on the way in. Every code point in the payload is below 0x80, so
      // the blob's size in bytes is the payload's length in characters.
      assertEquals(
        repo.git("cat-file", "-s", entry(repo, result.commit, "blob.bin").oid),
        BinaryPayload.length.toString
      )

  test("binary: a change to a binary file is reported as differing, not as lines"):
    withRepo: repo =>
      seed(repo)
      repo.write("blob.bin", BinaryPayload)
      val first = snapshot(repo, "s1")
      repo.write("blob.bin", OtherBinaryPayload)
      val second = snapshot(repo, "s2")
      val patch = patchOf(repo, first.tree, second.tree)
      assert(patch.contains("Binary files a/blob.bin and b/blob.bin differ"), clue(patch))
      // No textconv driver could have turned it into text, because the diff refuses to run one.
      assert(!patch.contains("@@ -"), clue(patch))

  test("binary: a binary file survives a round trip byte for byte"):
    withRepo: repo =>
      seed(repo)
      repo.write("blob.bin", BinaryPayload)
      val result = snapshot(repo, "s1")
      val recorded = entry(repo, result.commit, "blob.bin").oid
      restored(repo, result.commit): where =>
        assertEquals(GitCmd.run(where, Map.empty, "hash-object", "--", "blob.bin"), Right(recorded))

  // -------------------------------------------------------------------------------------------
  // Linked worktrees
  // -------------------------------------------------------------------------------------------

  test("linked worktree: a snapshot records the worktree it was taken in, not the main checkout"):
    withRepo: repo =>
      repo.write("tracked.txt", "shared")
      val mainHead = repo.commit("init")
      repo.write("main-only.txt", "only in the main checkout")
      withLinkedWorktree(repo, "feature"): linked =>
        ExtFs.writeFileSync(TestPath.join(linked, "wt-only.txt"), "only in the worktree")
        val result = created(Snapshot.create(linked, "w1"))
        val paths = entriesIn(linked, result.commit).map(_.path)
        assertEquals(paths, List("tracked.txt", "wt-only.txt"))
        assert(!paths.contains("main-only.txt"), clue(paths))
        // The main checkout keeps its own untracked file, and its own status.
        assertEquals(repo.status, "? main-only.txt")
        assertEquals(repo.head, mainHead)

  test("linked worktree: the snapshot parents on the worktree's own HEAD, not the main one"):
    withRepo: repo =>
      repo.write("tracked.txt", "shared")
      val mainHead = repo.commit("init")
      withLinkedWorktree(repo, "feature"): linked =>
        // Give the worktree a commit of its own, so its HEAD is somewhere the main checkout's is
        // not: without this the branch would still be sitting on the commit it was cut from, and
        // "parented on the worktree's HEAD" and "parented on main's" would be the same assertion.
        ExtFs.writeFileSync(TestPath.join(linked, "tracked.txt"), "moved on")
        gitIn(linked, "add", "-A")
        gitIn(linked, "-c", "user.name=Test", "-c", "user.email=t@e", "commit", "-m", "on feature")
        val worktreeHead = gitIn(linked, "rev-parse", "HEAD")
        assertNotEquals(worktreeHead, mainHead)

        ExtFs.writeFileSync(TestPath.join(linked, "wt-only.txt"), "only in the worktree")
        val result = created(Snapshot.create(linked, "w1"))
        assertEquals(result.parent, Some(worktreeHead))
        assertEquals(gitIn(linked, "rev-list", "--parents", "-1", result.commit),
          s"${result.commit} $worktreeHead")
        // HEAD is per-worktree, so the main checkout neither moved nor noticed.
        assertEquals(repo.head, mainHead)
        assertEquals(repo.git("symbolic-ref", "HEAD"), "refs/heads/main")

  test("linked worktree: the ref lands in the shared repository, so both directories agree"):
    withRepo: repo =>
      repo.write("tracked.txt", "shared")
      repo.commit("init")
      withLinkedWorktree(repo, "feature"): linked =>
        ExtFs.writeFileSync(TestPath.join(linked, "wt-only.txt"), "only in the worktree")
        val result = created(Snapshot.create(linked, "w1"))
        // refs/auk/ is not one of git's per-worktree namespaces, so the ref is written once and
        // read the same from either side.
        assertEquals(Snapshot.resolve(linked, "w1"), Right(result.commit))
        assertEquals(Snapshot.resolve(repo.dir, "w1"), Right(result.commit))
        assertEquals(Snapshot.list(repo.dir), Right(List(SnapshotEntry("w1", result.commit))))
        assertEquals(Snapshot.list(linked), Snapshot.list(repo.dir))
        assertEquals(Snapshot.treeOf(repo.dir, result.ref), Right(result.tree))

  // -------------------------------------------------------------------------------------------
  // Embedded repositories
  // -------------------------------------------------------------------------------------------

  test("embedded repository: a nested checkout is recorded as a gitlink, not recursed into"):
    withRepo: repo =>
      seed(repo)
      val nested = repo.path("nested")
      TestFs.mkdirSync(nested, recursiveForce)
      gitIn(nested, "init", "-b", "main")
      repo.write("nested/inner.txt", "inner")
      gitIn(nested, "add", "-A")
      gitIn(nested, "-c", "user.name=Test", "-c", "user.email=t@e", "commit", "-m", "inner")
      val innerHead = gitIn(nested, "rev-parse", "HEAD")

      val result = snapshot(repo, "s1")
      val recorded = entry(repo, result.commit, "nested")
      assertEquals(recorded.kind, "commit")
      assertEquals(recorded.mode, "160000")
      assertEquals(recorded.oid, innerHead)
      // The gitlink is the whole record: nothing from inside the nested repository comes along.
      val paths = entries(repo, result.commit).map(_.path)
      assert(!paths.contains("nested/inner.txt"), clue(paths))

  test("embedded repository: the gitlink names a commit the outer repository does not have"):
    withRepo: repo =>
      seed(repo)
      val nested = repo.path("nested")
      TestFs.mkdirSync(nested, recursiveForce)
      gitIn(nested, "init", "-b", "main")
      repo.write("nested/inner.txt", "inner")
      gitIn(nested, "add", "-A")
      gitIn(nested, "-c", "user.name=Test", "-c", "user.email=t@e", "commit", "-m", "inner")
      val innerHead = gitIn(nested, "rev-parse", "HEAD")

      val result = snapshot(repo, "s1")
      assertEquals(entry(repo, result.commit, "nested").oid, innerHead)
      // A gitlink is a recorded object id and nothing more: the object itself lives in the nested
      // repository, so the snapshot is not self-contained across a submodule boundary.
      assert(
        repo.tryGit("cat-file", "-e", innerHead).isLeft,
        "the outer repository was not supposed to hold the nested commit"
      )

  // -------------------------------------------------------------------------------------------
  // Garbage collection: the retention contract
  // -------------------------------------------------------------------------------------------

  test("gc: a snapshot survives an aggressive gc, because its ref keeps it reachable"):
    withRepo: repo =>
      seed(repo)
      repo.write("untracked.txt", "loose")
      val result = snapshot(repo, "s1")
      repo.git("gc", "--prune=now", "--quiet")
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Right(result.commit))
      assertEquals(Snapshot.treeOf(repo.dir, result.ref), Right(result.tree))
      assertEquals(Snapshot.treeOf(repo.dir, result.commit), Right(result.tree))
      assertEquals(repo.show(result.commit, "untracked.txt"), "loose")

  test("gc: a deleted snapshot's commit is really gone after the next gc"):
    withRepo: repo =>
      seed(repo)
      repo.write("untracked.txt", "loose")
      val result = snapshot(repo, "s1")
      assert(repo.tryGit("cat-file", "-e", result.commit).isRight, "the commit should exist first")
      // Nothing else is holding the commit: git logs ref updates for refs/heads, refs/remotes,
      // refs/notes and HEAD, and refs/auk/ is none of them, so the ref has no reflog to outlive it.
      assertEquals(repo.tryGit("reflog", "show", result.ref), Right(""))

      assertEquals(Snapshot.delete(repo.dir, "s1"), Right(()))
      repo.git("gc", "--prune=now", "--quiet")

      // Dropping the ref is all delete does; gc is what actually reclaims the commit. Until then a
      // deleted snapshot is still resolvable by its raw commit id, which is the retention contract
      // a layer above has to plan around: keep the ref, or lose the state at the next gc.
      assert(repo.tryGit("cat-file", "-e", result.commit).isLeft, "the commit outlived its gc")
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Left(GitError.SnapshotNotFound("s1")))
      assert(Snapshot.treeOf(repo.dir, result.commit).isLeft, "its tree outlived it")
      assertEquals(Snapshot.list(repo.dir), Right(Nil))

  // -------------------------------------------------------------------------------------------
  // The rename contract
  // -------------------------------------------------------------------------------------------

  test("renames: a renamed file reads as a delete and an add, never as a rename"):
    withRepo: repo =>
      seed(repo)
      repo.write("old.txt", "content that would survive rename detection intact\n")
      val first = snapshot(repo, "s1")
      repo.remove("old.txt")
      repo.write("new.txt", "content that would survive rename detection intact\n")
      val second = snapshot(repo, "s2")

      val patch = patchOf(repo, first.tree, second.tree)
      // --no-renames on purpose: a rename hunk hides the file's content behind a similarity score,
      // while a delete plus an add spells both sides out, which is what a reader reconstructing the
      // change needs.
      assert(patch.contains("deleted file mode"), clue(patch))
      assert(patch.contains("new file mode"), clue(patch))
      assert(patch.contains("-content that would survive rename detection intact"), clue(patch))
      assert(patch.contains("+content that would survive rename detection intact"), clue(patch))
      assert(!patch.contains("rename from"), clue(patch))
      assert(!patch.contains("rename to"), clue(patch))
      assert(!patch.contains("similarity index"), clue(patch))

  test("renames: the armoring holds even when the repository asks for rename detection"):
    withRepo: repo =>
      seed(repo)
      // The flag has to beat the config, or the output would depend on whose repository it ran in.
      repo.git("config", "diff.renames", "true")
      repo.git("config", "diff.renameLimit", "1000")
      repo.write("old.txt", "content that would survive rename detection intact\n")
      val first = snapshot(repo, "s1")
      repo.remove("old.txt")
      repo.write("new.txt", "content that would survive rename detection intact\n")
      val second = snapshot(repo, "s2")

      val patch = patchOf(repo, first.tree, second.tree)
      assert(!patch.contains("rename from"), clue(patch))
      assert(!patch.contains("similarity index"), clue(patch))
      assert(patch.contains("deleted file mode"), clue(patch))
      assert(patch.contains("new file mode"), clue(patch))

  // -------------------------------------------------------------------------------------------
  // Round trips: what comes back out
  // -------------------------------------------------------------------------------------------

  test("round trip: a dirty working tree comes back exactly as it was"):
    withRepo: repo =>
      seed(repo)
      repo.write("a.txt", "edited")
      repo.write("staged.txt", "staged")
      repo.git("add", "staged.txt")
      repo.write("untracked.txt", "loose")
      val result = snapshot(repo, "s1")
      restored(repo, result.commit): where =>
        assertEquals(read(where, "a.txt"), "edited")
        assertEquals(read(where, "sub/deep/c.txt"), "deep")
        assertEquals(read(where, "staged.txt"), "staged")
        assertEquals(read(where, "untracked.txt"), "loose")

  test("round trip: the restored checkout has nothing left to report"):
    withRepo: repo =>
      seed(repo)
      repo.write("a.txt", "edited")
      repo.write("untracked.txt", "loose")
      val result = snapshot(repo, "s1")
      restored(repo, result.commit): where =>
        // Clean status plus the same tree oid: the checkout is the snapshot, not an approximation
        // of it. The untracked file came back tracked, which is what recording it means.
        assertEquals(GitCmd.run(where, Map.empty, "status", "--porcelain"), Right(""))
        assertEquals(GitCmd.run(where, Map.empty, "rev-parse", "HEAD^{tree}"), Right(result.tree))

  test("round trip: a file deleted before the snapshot stays deleted"):
    withRepo: repo =>
      seed(repo)
      repo.remove("sub/deep/c.txt")
      val result = snapshot(repo, "s1")
      restored(repo, result.commit): where =>
        assert(!exists(where, "sub/deep/c.txt"), "the deleted file was resurrected")
        assert(exists(where, "a.txt"), "the surviving file went missing")

  test("round trip: a snapshot taken with an unborn HEAD restores its files"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.write("sub/b.txt", "nested")
      val result = snapshot(repo, "s1")
      assertEquals(result.parent, None)
      restored(repo, result.commit): where =>
        assertEquals(read(where, "a.txt"), "hello")
        assertEquals(read(where, "sub/b.txt"), "nested")

  test("round trip: the older of two snapshots restores the older content"):
    withRepo: repo =>
      seed(repo)
      repo.write("a.txt", "first")
      val first = snapshot(repo, "s1")
      repo.write("a.txt", "second")
      repo.write("late.txt", "arrived later")
      val second = snapshot(repo, "s2")
      restored(repo, first.commit): where =>
        assertEquals(read(where, "a.txt"), "first")
        assert(!exists(where, "late.txt"), "a later file leaked into the earlier snapshot")
      restored(repo, second.commit): where =>
        assertEquals(read(where, "a.txt"), "second")
        assertEquals(read(where, "late.txt"), "arrived later")

  test("round trip: an ignored file is not resurrected"):
    withRepo: repo =>
      seed(repo)
      repo.write(".gitignore", "ignored.txt\n")
      repo.write("ignored.txt", "noise")
      val result = snapshot(repo, "s1")
      restored(repo, result.commit): where =>
        assert(!exists(where, "ignored.txt"), "an ignored file came back")
        assertEquals(read(where, ".gitignore"), "ignored.txt\n")

  test("round trip: restoring leaves the repository the snapshot came from alone"):
    withRepo: repo =>
      val head = seed(repo)
      repo.write("staged.txt", "staged")
      repo.git("add", "staged.txt")
      repo.write("a.txt", "unstaged edit")
      val result = snapshot(repo, "s1")
      val status = repo.status
      val staged = repo.git("diff", "--cached")
      restored(repo, result.commit)(_ => ())
      assertEquals(repo.status, status)
      assertEquals(repo.git("diff", "--cached"), staged)
      assertEquals(repo.head, head)
      assertEquals(repo.read("a.txt"), "unstaged edit")

  // -------------------------------------------------------------------------------------------
  // Shapes a working directory can take
  // -------------------------------------------------------------------------------------------

  test("empty repository: a snapshot of nothing records git's empty tree"):
    withRepo: repo =>
      val result = snapshot(repo, "s1")
      assertEquals(result.parent, None)
      assertEquals(entries(repo, result.commit), Nil)
      assertEquals(repo.git("cat-file", "-t", result.tree), "tree")
      assertEquals(repo.git("cat-file", "-s", result.tree), "0")
      assertEquals(Snapshot.currentTree(repo.dir), Right(result.tree))

  test("empty directory: git has no name for one, so nothing is recorded"):
    withRepo: repo =>
      seed(repo)
      TestFs.mkdirSync(repo.path("hollow/deeper"), recursiveForce)
      val result = snapshot(repo, "s1")
      assertEquals(entries(repo, result.commit).map(_.path), List("a.txt", "sub/deep/c.txt"))

  test("empty file: recorded, as an empty blob"):
    withRepo: repo =>
      seed(repo)
      repo.write("hollow.txt", "")
      val result = snapshot(repo, "s1")
      val recorded = entry(repo, result.commit, "hollow.txt")
      assertEquals(recorded.mode, "100644")
      assertEquals(repo.git("cat-file", "-s", recorded.oid), "0")

  test("bytes: CRLF line endings and trailing whitespace are recorded as they are"):
    withRepo: repo =>
      seed(repo)
      // Pinned locally: with autocrlf on, git would rewrite the bytes on the way in, and the point
      // is that a snapshot records what is on disk.
      repo.git("config", "core.autocrlf", "false")
      repo.write("crlf.txt", "one\r\ntwo   \r\n")
      val result = snapshot(repo, "s1")
      assertEquals(entry(repo, result.commit, "crlf.txt").oid, hashOnDisk(repo, "crlf.txt"))

  test("bytes: a file with no trailing newline keeps its last line"):
    withRepo: repo =>
      seed(repo)
      repo.write("bare.txt", "no newline at the end")
      val result = snapshot(repo, "s1")
      assertEquals(entry(repo, result.commit, "bare.txt").oid, hashOnDisk(repo, "bare.txt"))
      assertEquals(repo.show(result.commit, "bare.txt"), "no newline at the end")

  test("paths: a deeply nested path is recorded whole"):
    withRepo: repo =>
      seed(repo)
      val deep = "one/two/three/four/five/six/seven/eight.txt"
      repo.write(deep, "at the bottom")
      val result = snapshot(repo, "s1")
      assertEquals(repo.show(result.commit, deep), "at the bottom")

  test("paths: the repository's own .git is never captured"):
    withRepo: repo =>
      seed(repo)
      repo.write("untracked.txt", "loose")
      val paths = entries(repo, snapshot(repo, "s1").commit).map(_.path)
      assert(!paths.exists(path => path == ".git" || path.startsWith(".git/")), clue(paths))

  test("exclusions: a .gitignore in a subdirectory only excludes inside that subdirectory"):
    withRepo: repo =>
      seed(repo)
      repo.write("sub/.gitignore", "noise.txt\n")
      repo.write("sub/noise.txt", "excluded")
      repo.write("noise.txt", "kept")
      val paths = entries(repo, snapshot(repo, "s1").commit).map(_.path)
      assert(!paths.contains("sub/noise.txt"), clue(paths))
      assert(paths.contains("noise.txt"), clue(paths))

  test("exclusions: .git/info/exclude is honored like a .gitignore"):
    withRepo: repo =>
      seed(repo)
      repo.write(".git/info/exclude", "secret.txt\n")
      repo.write("secret.txt", "not for the snapshot")
      repo.write("public.txt", "fine")
      val paths = entries(repo, snapshot(repo, "s1").commit).map(_.path)
      assert(!paths.contains("secret.txt"), clue(paths))
      assert(paths.contains("public.txt"), clue(paths))

  test("size: a patch larger than Node's default buffer comes back whole"):
    withRepo: repo =>
      val head = seed(repo)
      // Node's execFileSync defaults to a 1 MiB buffer and throws ENOBUFS past it; GitCmd raises
      // that to 64 MiB, and this is the row that notices if it ever goes back.
      val big = (1 to 30000).map(line => s"line $line: ${"x" * 40}").mkString("", "\n", "\n")
      assert(big.length > 1024 * 1024, clue(big.length))
      repo.write("big.txt", big)
      val result = snapshot(repo, "s1")
      val patch = patchOf(repo, repo.git("rev-parse", s"$head^{tree}"), result.tree)
      assert(patch.length > 1024 * 1024, clue(patch.length))
      assert(patch.contains("line 30000: "), clue(patch.length))

  // -------------------------------------------------------------------------------------------
  // Isolation from the repository it snapshots
  // -------------------------------------------------------------------------------------------

  test("isolation: a stale index.lock in the user's .git does not stop a snapshot"):
    withRepo: repo =>
      seed(repo)
      // What another git process mid-operation leaves lying about. A snapshot never opens the real
      // index, so it has no reason to care — and must not remove someone else's lock either.
      repo.write(".git/index.lock", "held by someone else\n")
      val result = snapshot(repo, "s1")
      assert(repo.exists(".git/index.lock"), "the snapshot removed a lock it does not own")
      assertEquals(repo.read(".git/index.lock"), "held by someone else\n")
      repo.remove(".git/index.lock")
      assertEquals(repo.show(result.commit, "a.txt"), "hello")

  test("isolation: an ambient GIT_INDEX_FILE is overridden, and never written"):
    withRepo: repo =>
      seed(repo)
      val ambient = scratchPath("auk-ambient-index-")
      repo.write("untracked.txt", "loose")
      NodeProcess.env("GIT_INDEX_FILE") = ambient
      val result =
        try snapshot(repo, "s1")
        finally NodeProcess.env -= "GIT_INDEX_FILE"
      assert(!TestFs.existsSync(ambient), s"the snapshot wrote the ambient index at $ambient")
      assertEquals(repo.show(result.commit, "untracked.txt"), "loose")

  test("isolation: the user's index file is not written, not even its timestamp"):
    withRepo: repo =>
      seed(repo)
      repo.write("staged.txt", "staged")
      repo.git("add", "staged.txt")
      repo.write("untracked.txt", "loose")
      val index = repo.path(".git/index")
      val before = ExtFs.lstatSync(index)
      val (size, mtime) = (before.size, before.mtimeMs)
      snapshot(repo, "s1")
      val after = ExtFs.lstatSync(index)
      assertEquals(after.size, size)
      assertEquals(after.mtimeMs, mtime)

  test("isolation: a failed snapshot leaves the repository as it found it"):
    assume(TestOs.platform() != "win32", "the fixture relies on POSIX permission bits")
    withRepo: repo =>
      seed(repo)
      repo.write("staged.txt", "staged")
      repo.git("add", "staged.txt")
      val status = repo.status
      val staged = repo.git("diff", "--cached")
      val head = repo.head

      repo.write("unreadable.txt", "secret")
      repo.chmod("unreadable.txt", 0)
      assert(Snapshot.create(repo.dir, "s1").isLeft, "the fixture was supposed to fail")
      repo.chmod("unreadable.txt", Integer.parseInt("644", 8))
      repo.remove("unreadable.txt")

      assertEquals(repo.status, status)
      assertEquals(repo.git("diff", "--cached"), staged)
      assertEquals(repo.head, head)
      assertEquals(Snapshot.list(repo.dir), Right(Nil))

  test("isolation: HEAD's reflog does not grow"):
    withRepo: repo =>
      seed(repo)
      val before = repo.tryGit("reflog", "show", "main")
      snapshot(repo, "s1")
      snapshot(repo, "s2", force = true)
      assertEquals(repo.tryGit("reflog", "show", "main"), before)

  test("isolation: twenty snapshots in a row all land, and leave nothing behind"):
    withRepo: repo =>
      seed(repo)
      val before = tempIndexes()
      val ids = (1 to 20).map(n => if n < 10 then s"s0$n" else s"s$n").toList
      val commits = ids.map(id => snapshot(repo, id).commit)
      assertEquals(commits.distinct.size, 20, clue(commits))
      assertEquals(Snapshot.list(repo.dir).map(_.map(_.id)), Right(ids))
      assertEquals(tempIndexes().diff(before), Set.empty[String])

  test("isolation: snapshots taken from several directories of one repository agree"):
    withRepo: repo =>
      seed(repo)
      val before = tempIndexes()
      val fromRoot = created(Snapshot.create(repo.dir, "from/root"))
      val fromSub = created(Snapshot.create(repo.path("sub"), "from/sub"))
      val fromDeep = created(Snapshot.create(repo.path("sub/deep"), "from/deep"))
      // One repository, so one tree: where the caller stood does not change what is recorded.
      assertEquals(fromSub.tree, fromRoot.tree)
      assertEquals(fromDeep.tree, fromRoot.tree)
      assertEquals(
        Snapshot.list(repo.dir).map(_.map(_.id)),
        Right(List("from/deep", "from/root", "from/sub"))
      )
      assertEquals(tempIndexes().diff(before), Set.empty[String])

  test("isolation: deleting a snapshot frees its id for reuse"):
    withRepo: repo =>
      seed(repo)
      val first = snapshot(repo, "s1")
      assertEquals(Snapshot.delete(repo.dir, "s1"), Right(()))
      repo.write("a.txt", "changed")
      val second = snapshot(repo, "s1")
      assertNotEquals(second.commit, first.commit)
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Right(second.commit))
      assertEquals(repo.show(second.commit, "a.txt"), "changed")

  test("isolation: two snapshots keep their own trees while the working tree moves under them"):
    withRepo: repo =>
      seed(repo)
      repo.write("a.txt", "first")
      val first = snapshot(repo, "s1")
      repo.write("a.txt", "second")
      val second = snapshot(repo, "s2")
      repo.write("a.txt", "third")
      assertNotEquals(second.tree, first.tree)
      assertEquals(Snapshot.treeOf(repo.dir, first.ref), Right(first.tree))
      assertEquals(Snapshot.treeOf(repo.dir, second.ref), Right(second.tree))
      assertEquals(repo.show(first.commit, "a.txt"), "first")
      assertEquals(repo.show(second.commit, "a.txt"), "second")
      // The third edit is only on disk: currentTree sees it, and neither snapshot does.
      val current = Snapshot.currentTree(repo.dir)
      assertNotEquals(current, Right(first.tree))
      assertNotEquals(current, Right(second.tree))

  test("isolation: diffTrees reads both ways between two snapshots"):
    withRepo: repo =>
      seed(repo)
      repo.write("a.txt", "before\n")
      val first = snapshot(repo, "s1")
      repo.write("a.txt", "after\n")
      val second = snapshot(repo, "s2")
      val forward = patchOf(repo, first.tree, second.tree)
      assert(forward.contains("-before"), clue(forward))
      assert(forward.contains("+after"), clue(forward))
      val backward = patchOf(repo, second.tree, first.tree)
      assert(backward.contains("-after"), clue(backward))
      assert(backward.contains("+before"), clue(backward))

/** The `{ recursive: true, force: true }` every removal in this file wants. */
private[snapshot] def recursiveForce: js.Any =
  val options = js.Dictionary.empty[js.Any]
  options("recursive") = true
  options("force") = true
  options
