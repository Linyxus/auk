package auk.snapshot

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Node builtins these tests need beyond what [[TestFs]] and [[ExtFs]] already facade.
  *
  * Timestamps are set by hand rather than waited out: "this file was not rewritten" is a claim about
  * an exact mtime, and a test that writes, resets and compares within the same clock tick would
  * pass whether or not the file was touched. Backdating first makes the answer unambiguous.
  */
@js.native
@JSImport("node:fs", JSImport.Namespace)
private[snapshot] object ResetFs extends js.Object:
  def utimesSync(path: String, atime: Double, mtime: Double): Unit = js.native

/** Tests for [[Worktree.reset]]: putting a recorded state back into the live checkout.
  *
  * Every test here is the same shape as the operation it covers — record a state, mutate the working
  * tree, reset, then ask two questions. Did the capturable content come back (the tree oid, plus
  * what is actually on disk, since two trees being equal says nothing about modes and symlinks)?
  * And did everything the reset promised not to touch survive: the user's index file, HEAD, the
  * branch, the reflog, and the ignored files that no snapshot holds and that nothing could restore.
  *
  * The two carve-outs — nested repositories, and a directory of ignored files standing where the
  * target tree holds a file — are pinned here as well. Both were observed against the git binary
  * first and asserted second, and both are written down precisely because they are the places the
  * headline promise does not hold.
  */
class WorktreeSuite extends munit.FunSuite:

  // -------------------------------------------------------------------------------------------
  // Fixtures
  // -------------------------------------------------------------------------------------------

  /** A private `TMPDIR`, as in [[TreeOfSuite]] and [[SnapshotExtSuite]]: suites run concurrently in
    * separate processes, every capture leaves a temp index in the OS temp directory while it works,
    * and [[SnapshotSuite]]'s hygiene tests count what turns up there. `os.tmpdir()` reads `TMPDIR`
    * on every call, so redirecting it moves this process's temp files and nobody else's — which is
    * also what lets the hygiene test below assert a flatly empty result instead of a difference.
    */
  private var releaseTmpdir: () => Unit = () => ()

  override def beforeAll(): Unit =
    val previous = NodeProcess.env.get("TMPDIR")
    val own = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "auk-reset-"))
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

  private def snapshot(repo: TempRepo, id: String)(using munit.Location): SnapshotCreated =
    created(Snapshot.create(repo.dir, id))

  /** Resets and insists it worked: a reset that failed would make every assertion after it a lie
    * about the wrong thing.
    */
  private def resetTo(repo: TempRepo, commit: String)(using munit.Location): ResetOutcome =
    resetIn(repo.dir, commit)

  private def resetIn(dir: String, commit: String)(using munit.Location): ResetOutcome =
    Worktree.reset(dir, commit) match
      case Right(outcome) => outcome
      case Left(error)    => fail(s"reset to $commit failed: $error")

  private def currentTree(repo: TempRepo)(using munit.Location): String =
    Snapshot.currentTree(repo.dir) match
      case Right(tree) => tree
      case Left(error) => fail(s"expected a tree, got $error")

  private def seed(repo: TempRepo): String =
    repo.write("a.txt", "hello")
    repo.write("sub/deep/c.txt", "deep")
    repo.commit("init")

  /** Every entry a tree records, mode included: a tree oid proves the content matches, and the mode
    * is the part a reader still has to check by hand.
    */
  private def entries(repo: TempRepo, treeish: String)(using munit.Location): List[TreeEntry] =
    repo.git("-c", "core.quotePath=false", "ls-tree", "-r", treeish).linesIterator
      .filter(_.nonEmpty)
      .map { line =>
        val (meta, tabbedPath) = line.span(_ != '\t')
        meta.split(' ') match
          case Array(mode, kind, oid) => TreeEntry(mode, kind, oid, tabbedPath.drop(1))
          case _                      => fail(s"unparsable ls-tree line '$line'")
      }
      .toList

  /** The object id git would give a file as it sits on disk: same id, same bytes. */
  private def hashOnDisk(repo: TempRepo, relative: String): String =
    repo.git("hash-object", "--", relative)

  /** Leftover temp indexes. This suite owns its `TMPDIR`, so anything matching is its own. */
  private def tempIndexes(): Set[String] =
    TestFs.readdirSync(TestOs.tmpdir()).toSet.filter(_.startsWith("auk-index-"))

  private def scratchPath(prefix: String): String =
    TestPath.join(TestOs.tmpdir(), s"$prefix${(js.Math.random() * 1e9).toInt}")

  private val Executable = Integer.parseInt("111", 8)

  private def isExecutable(repo: TempRepo, relative: String): Boolean =
    (ExtFs.lstatSync(repo.path(relative)).mode & Executable) != 0

  /** A timestamp far enough in the past that nothing could have written it by accident: 2001-09-09,
    * the second the Unix clock reached 1e9.
    */
  private val LongAgo = 1000000000.0

  private def backdate(repo: TempRepo, relatives: String*): Unit =
    relatives.foreach(relative => ResetFs.utimesSync(repo.path(relative), LongAgo, LongAgo))

  private def mtime(repo: TempRepo, relative: String): Double =
    ExtFs.lstatSync(repo.path(relative)).mtimeMs

  /** Content git calls binary: it looks for a NUL in the opening bytes and finds one first thing.
    * Assembled from code points rather than written as escapes, so this source file holds no control
    * characters of its own.
    */
  private val Nul = 0.toChar.toString
  private val BinaryPayload = Nul + 1.toChar.toString + "binary" + Nul + " payload\n"
  private val OtherBinaryPayload = Nul + 2.toChar.toString + "revised" + Nul + " payload\n"

  // -------------------------------------------------------------------------------------------
  // Round trips
  // -------------------------------------------------------------------------------------------

  test("round trip: a hard-mutated working tree comes back to the tree the snapshot recorded"):
    assume(TestOs.platform() != "win32", "symlinks and the executable bit are POSIX fixtures here")
    withRepo: repo =>
      seed(repo)
      repo.write("script.sh", "#!/bin/sh\necho one\n")
      repo.makeExecutable("script.sh")
      repo.write("blob.bin", BinaryPayload)
      repo.write("one/two/three.txt", "at the bottom")
      ExtFs.symlinkSync("a.txt", repo.path("link.txt"))
      val recorded = snapshot(repo, "s1")

      // Every kind of drift at once: content, existence, depth, mode, link target and bytes.
      repo.write("a.txt", "edited after the snapshot")
      repo.remove("sub/deep/c.txt")
      repo.write("late/arrival.txt", "not in the snapshot")
      repo.write("script.sh", "#!/bin/sh\necho two\n")
      repo.chmod("script.sh", Integer.parseInt("644", 8))
      repo.write("blob.bin", OtherBinaryPayload)
      repo.remove("one/two/three.txt")
      repo.remove("link.txt")
      ExtFs.symlinkSync("blob.bin", repo.path("link.txt"))
      assertNotEquals(currentTree(repo), recorded.tree)

      val outcome = resetTo(repo, recorded.commit)

      assertEquals(currentTree(repo), recorded.tree)
      assertEquals(entries(repo, currentTree(repo)), entries(repo, recorded.tree))
      // Six paths written: a.txt and blob.bin edited, script.sh changed in content and mode,
      // link.txt retargeted (a symlink on both sides, so a modification rather than a type change),
      // and sub/deep/c.txt and one/two/three.txt brought back. One removed: late/arrival.txt, whose
      // directory was pruned behind it and is not a path of its own.
      assertEquals(outcome, ResetOutcome(changed = 6, deleted = 1, skippedGitlinks = Nil))

  test("round trip: the restored files are right on disk, not just in the tree"):
    withRepo: repo =>
      seed(repo)
      repo.write("one/two/three.txt", "at the bottom")
      val recorded = snapshot(repo, "s1")
      repo.write("a.txt", "edited")
      repo.remove("one/two/three.txt")
      repo.write("late.txt", "arrived later")

      resetTo(repo, recorded.commit)

      assertEquals(repo.read("a.txt"), "hello")
      assertEquals(repo.read("sub/deep/c.txt"), "deep")
      assertEquals(repo.read("one/two/three.txt"), "at the bottom")
      assert(!repo.exists("late.txt"), "a file the snapshot never held survived the reset")

  test("round trip: the executable bit comes back, in both directions"):
    assume(TestOs.platform() != "win32", "the executable bit is a POSIX permission bit")
    withRepo: repo =>
      seed(repo)
      repo.write("runnable.sh", "#!/bin/sh\n")
      repo.makeExecutable("runnable.sh")
      repo.write("plain.sh", "#!/bin/sh\n")
      val recorded = snapshot(repo, "s1")
      repo.chmod("runnable.sh", Integer.parseInt("644", 8))
      repo.makeExecutable("plain.sh")

      resetTo(repo, recorded.commit)

      assert(isExecutable(repo, "runnable.sh"), "the executable bit did not come back")
      assert(!isExecutable(repo, "plain.sh"), "an executable bit the snapshot never held survived")
      // A mode-only change carries the same blob on both sides, so the diff reports it as a
      // modification with an unchanged object id — the file has to be rewritten to fix the mode.
      assertEquals(currentTree(repo), recorded.tree)

  test("round trip: a symlink comes back as a symlink, pointing where it did"):
    assume(TestOs.platform() != "win32", "symlinks are a POSIX fixture here")
    withRepo: repo =>
      seed(repo)
      repo.write("other.txt", "elsewhere")
      ExtFs.symlinkSync("a.txt", repo.path("link.txt"))
      val recorded = snapshot(repo, "s1")
      repo.remove("link.txt")
      ExtFs.symlinkSync("other.txt", repo.path("link.txt"))

      resetTo(repo, recorded.commit)

      // A restored link is a link, not a copy of what it pointed at: readlink answers, and the
      // content read through it is the target's.
      assertEquals(ExtFs.readlinkSync(repo.path("link.txt")), "a.txt")
      assertEquals(repo.read("link.txt"), "hello")
      assertEquals(entries(repo, currentTree(repo)).find(_.path == "link.txt").map(_.mode),
        Some("120000"))

  test("round trip: binary bytes come back exactly"):
    withRepo: repo =>
      seed(repo)
      repo.write("blob.bin", BinaryPayload)
      val recorded = snapshot(repo, "s1")
      val original = hashOnDisk(repo, "blob.bin")
      repo.write("blob.bin", OtherBinaryPayload)
      assertNotEquals(hashOnDisk(repo, "blob.bin"), original)

      resetTo(repo, recorded.commit)

      // Content addressing is the assertion: same object id, same bytes, nothing decoded as text
      // anywhere in the comparison.
      assertEquals(hashOnDisk(repo, "blob.bin"), original)
      assertEquals(repo.git("cat-file", "-s", original), BinaryPayload.length.toString)

  test("round trip: a reset run from a subdirectory covers the whole repository"):
    withRepo: repo =>
      seed(repo)
      val recorded = snapshot(repo, "s1")
      repo.write("a.txt", "edited at the root")
      repo.write("sub/deep/c.txt", "edited deep down")
      repo.write("top-level-litter.txt", "not in the snapshot")

      // The caller stands three directories down; the reset reaches everything above it too.
      resetIn(repo.path("sub/deep"), recorded.commit)

      assertEquals(currentTree(repo), recorded.tree)
      assertEquals(repo.read("a.txt"), "hello")
      assert(!repo.exists("top-level-litter.txt"), "a root-level file escaped a reset from a subdir")

  test("round trip: a snapshot taken with an unborn HEAD resets like any other"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.write("sub/b.txt", "nested")
      val recorded = snapshot(repo, "s1")
      assertEquals(recorded.parent, None)
      repo.write("a.txt", "edited")
      repo.remove("sub/b.txt")
      repo.write("c.txt", "later")

      resetTo(repo, recorded.commit)

      assertEquals(currentTree(repo), recorded.tree)
      assertEquals(repo.read("a.txt"), "hello")
      assertEquals(repo.read("sub/b.txt"), "nested")
      assert(!repo.exists("c.txt"), "a file the snapshot never held survived")

  test("round trip: resetting twice to the same snapshot changes nothing the second time"):
    withRepo: repo =>
      seed(repo)
      val recorded = snapshot(repo, "s1")
      repo.write("a.txt", "edited")
      resetTo(repo, recorded.commit)
      backdate(repo, "a.txt", "sub/deep/c.txt")

      resetTo(repo, recorded.commit)

      assertEquals(currentTree(repo), recorded.tree)
      // The second reset had nothing to do, so it wrote nothing — the backdated timestamps stand.
      assertEquals(mtime(repo, "a.txt"), LongAgo * 1000)
      assertEquals(mtime(repo, "sub/deep/c.txt"), LongAgo * 1000)

  // -------------------------------------------------------------------------------------------
  // What a reset promises not to touch
  // -------------------------------------------------------------------------------------------

  test("isolation: the user's index file is not written, not even its timestamp"):
    withRepo: repo =>
      seed(repo)
      repo.write("staged.txt", "staged")
      repo.git("add", "staged.txt")
      val recorded = snapshot(repo, "s1")
      repo.write("a.txt", "edited")
      repo.write("late.txt", "arrived later")

      val before = ExtFs.lstatSync(repo.path(".git/index"))
      val (size, stamp) = (before.size, before.mtimeMs)
      resetTo(repo, recorded.commit)

      val after = ExtFs.lstatSync(repo.path(".git/index"))
      assertEquals(after.size, size)
      assertEquals(after.mtimeMs, stamp)

  test("isolation: HEAD and the branch do not move, and HEAD's reflog does not grow"):
    withRepo: repo =>
      val head = seed(repo)
      val recorded = snapshot(repo, "s1")
      repo.write("a.txt", "edited")
      repo.write("late.txt", "arrived later")
      val reflog = repo.tryGit("reflog", "show", "main")

      resetTo(repo, recorded.commit)

      // This is the difference from `git reset --hard`, which would move all three.
      assertEquals(repo.head, head)
      assertEquals(repo.git("symbolic-ref", "HEAD"), "refs/heads/main")
      assertEquals(repo.tryGit("reflog", "show", "main"), reflog)
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Right(recorded.commit))

  test("isolation: the staged state survives, because the index is never touched"):
    withRepo: repo =>
      seed(repo)
      repo.write("staged.txt", "staged")
      repo.git("add", "staged.txt")
      val recorded = snapshot(repo, "s1")
      val staged = repo.git("diff", "--cached")
      repo.write("a.txt", "edited")
      repo.write("staged.txt", "edited after staging")

      resetTo(repo, recorded.commit)

      // `diff --cached` reads HEAD against the index, and the reset moved neither — so the staged
      // change is still staged, and the file on disk is back to what was recorded.
      assertEquals(repo.git("diff", "--cached"), staged)
      assertEquals(repo.read("staged.txt"), "staged")

  test("isolation: ignored files survive a reset byte for byte, timestamps included"):
    withRepo: repo =>
      repo.write(".gitignore", ".auk/\nnode_modules/\ntarget/\n")
      repo.write("a.txt", "hello")
      repo.commit("init")
      repo.write(".auk/state.json", "{\"generation\": 7}")
      repo.write("node_modules/pkg/index.js", "module.exports = 1\n")
      repo.write("target/out.o", BinaryPayload)
      val recorded = snapshot(repo, "s1")
      // None of it was captured, so none of it could be restored — surviving is the only option.
      assertEquals(entries(repo, recorded.tree).map(_.path), List(".gitignore", "a.txt"))

      repo.write("a.txt", "edited")
      repo.write("late.txt", "arrived later")
      backdate(repo, ".auk/state.json", "node_modules/pkg/index.js", "target/out.o")

      resetTo(repo, recorded.commit)

      assertEquals(repo.read(".auk/state.json"), "{\"generation\": 7}")
      assertEquals(repo.read("node_modules/pkg/index.js"), "module.exports = 1\n")
      assertEquals(repo.read("target/out.o"), BinaryPayload)
      assertEquals(mtime(repo, ".auk/state.json"), LongAgo * 1000)
      assertEquals(mtime(repo, "node_modules/pkg/index.js"), LongAgo * 1000)
      assertEquals(mtime(repo, "target/out.o"), LongAgo * 1000)
      assertEquals(currentTree(repo), recorded.tree)

  test("isolation: a reset leaves no temp index behind"):
    withRepo: repo =>
      seed(repo)
      val recorded = snapshot(repo, "s1")
      repo.write("a.txt", "edited")
      repo.write("late.txt", "arrived later")

      resetTo(repo, recorded.commit)
      // Once more with nothing to do, so both the working path and the no-op path are covered.
      resetTo(repo, recorded.commit)

      assertEquals(tempIndexes(), Set.empty[String])

  // -------------------------------------------------------------------------------------------
  // What gets rewritten, and what does not
  // -------------------------------------------------------------------------------------------

  test("mtimes: an untouched path keeps its timestamp and a rewritten one does not"):
    withRepo: repo =>
      seed(repo)
      repo.write("stable.txt", "never changes")
      val recorded = snapshot(repo, "s1")
      repo.write("a.txt", "edited")
      // Both files start in 2001; only the one the reset has to rewrite should leave it.
      backdate(repo, "a.txt", "stable.txt", "sub/deep/c.txt")

      resetTo(repo, recorded.commit)

      assertEquals(mtime(repo, "stable.txt"), LongAgo * 1000)
      assertEquals(mtime(repo, "sub/deep/c.txt"), LongAgo * 1000)
      assertNotEquals(mtime(repo, "a.txt"), LongAgo * 1000)
      assertEquals(repo.read("a.txt"), "hello")

  test("no-op: resetting to the state already on disk writes nothing at all"):
    withRepo: repo =>
      seed(repo)
      val recorded = snapshot(repo, "s1")
      val status = repo.status
      backdate(repo, "a.txt", "sub/deep/c.txt")

      val outcome = resetTo(repo, recorded.commit)

      assertEquals(outcome, ResetOutcome.Unchanged)
      assertEquals(mtime(repo, "a.txt"), LongAgo * 1000)
      assertEquals(mtime(repo, "sub/deep/c.txt"), LongAgo * 1000)
      assertEquals(repo.status, status)
      assertEquals(tempIndexes(), Set.empty[String])

  test("pruning: a directory the removals emptied is pruned, and one still holding something is not"):
    withRepo: repo =>
      repo.write(".gitignore", "keep.log\n")
      repo.write("a.txt", "hello")
      repo.commit("init")
      val recorded = snapshot(repo, "s1")

      repo.write("gone/deeper/x.txt", "not in the snapshot")
      repo.write("kept/y.txt", "not in the snapshot either")
      repo.write("kept/keep.log", "ignored, and therefore not the reset's business")

      resetTo(repo, recorded.commit)

      // Nothing in `gone/` survived the removal, and a capture cannot record an empty directory —
      // so leaving one behind would be cruft the recorded state never had.
      assert(!repo.exists("gone"), "an emptied directory was left behind")
      assert(!repo.exists("kept/y.txt"), "a file the snapshot never held survived")
      // `kept/` stops the walk: it still holds an ignored file, which is not the reset's to remove.
      assert(repo.exists("kept"), "a directory holding an ignored file was pruned")
      assertEquals(repo.read("kept/keep.log"), "ignored, and therefore not the reset's business")
      assertEquals(currentTree(repo), recorded.tree)

  test("type change: a file that became a symlink is a file again, and the reverse"):
    assume(TestOs.platform() != "win32", "symlinks are a POSIX fixture here")
    withRepo: repo =>
      seed(repo)
      repo.write("was-file.txt", "plain content")
      ExtFs.symlinkSync("a.txt", repo.path("was-link.txt"))
      val recorded = snapshot(repo, "s1")

      repo.remove("was-file.txt")
      ExtFs.symlinkSync("a.txt", repo.path("was-file.txt"))
      repo.remove("was-link.txt")
      repo.write("was-link.txt", "no longer a link")

      resetTo(repo, recorded.commit)

      assertEquals(repo.read("was-file.txt"), "plain content")
      assertEquals(ExtFs.readlinkSync(repo.path("was-link.txt")), "a.txt")
      assertEquals(currentTree(repo), recorded.tree)

  test("type change: a file that became a directory, and a directory that became a file"):
    withRepo: repo =>
      seed(repo)
      repo.write("was-file", "a file at this path")
      repo.write("was-dir/inside.txt", "a directory at this path")
      val recorded = snapshot(repo, "s1")

      repo.remove("was-file")
      repo.write("was-file/inside.txt", "now a directory")
      repo.remove("was-dir")
      repo.write("was-dir", "now a file")

      resetTo(repo, recorded.commit)

      // Removals run before writes, so neither path is ever asked to be two things at once.
      assertEquals(repo.read("was-file"), "a file at this path")
      assertEquals(repo.read("was-dir/inside.txt"), "a directory at this path")
      assertEquals(currentTree(repo), recorded.tree)

  // -------------------------------------------------------------------------------------------
  // Ignore rules gate the add side, not the write side
  // -------------------------------------------------------------------------------------------

  test("ignored: a tracked path that gitignore also names is restored like any other"):
    withRepo: repo =>
      repo.write(".gitignore", "secret.txt\n")
      repo.write("a.txt", "hello")
      repo.write("secret.txt", "recorded")
      // Force-added, so it is tracked despite the rule — and `add -A` keeps recording it, because
      // an ignore rule only ever decides whether an *untracked* path joins the index.
      repo.git("add", "-f", "secret.txt")
      repo.commit("init")
      val recorded = snapshot(repo, "s1")
      assert(entries(repo, recorded.tree).map(_.path).contains("secret.txt"))

      repo.write("secret.txt", "edited after the snapshot")
      assertNotEquals(currentTree(repo), recorded.tree)

      resetTo(repo, recorded.commit)

      assertEquals(repo.read("secret.txt"), "recorded")
      assertEquals(currentTree(repo), recorded.tree)

  test("ignored: a path the current rules hide is still written back, and capture still drops it"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      repo.write("noise.log", "captured while nothing excluded it")
      val recorded = snapshot(repo, "s1")
      assert(entries(repo, recorded.tree).map(_.path).contains("noise.log"))

      // The rule arrives afterwards, and lives in .git/info/exclude — outside the capture, so the
      // reset cannot restore the state of the rule itself the way it would a .gitignore.
      repo.write(".git/info/exclude", "noise.log\n")
      repo.remove("noise.log")

      resetTo(repo, recorded.commit)

      // checkout-index writes what the tree says, whatever the ignore rules say: they gate the add
      // side only. So the file is back on disk with its recorded bytes...
      assertEquals(repo.read("noise.log"), "captured while nothing excluded it")
      // ...and yet the trees do not match, because the next capture excludes it again. The
      // invariant "capturable content equals the target tree" holds only while the ignore rules
      // outside the capture — .git/info/exclude, core.excludesFile — are the ones the snapshot was
      // taken under.
      assertNotEquals(currentTree(repo), recorded.tree)
      assertEquals(
        entries(repo, recorded.tree).map(_.path).diff(entries(repo, currentTree(repo)).map(_.path)),
        List("noise.log")
      )

  test("clobber: a reset that would destroy ignored files refuses, and touches nothing"):
    withRepo: repo =>
      repo.write(".gitignore", "*.log\nsecrets/\n")
      repo.write("a.txt", "hello")
      repo.write("build", "a file, at a path a build directory would want")
      repo.commit("init")
      val recorded = snapshot(repo, "s1")

      // The worker replaced the file with a directory and filled it with things no snapshot holds:
      // an ignored file by extension, and one inside a wholly ignored directory.
      repo.remove("build")
      repo.write("build/output.log", "ignored, and not the reset's to destroy")
      repo.write("build/nested/deep.log", "ignored, further down")
      repo.write("secrets/.env", "API_KEY=the sort of thing that must not evaporate")
      repo.write("a.txt", "edited too, and not restored either")
      val before = repo.status
      val doomedBytes = hashOnDisk(repo, "build/output.log")

      Worktree.reset(repo.dir, recorded.commit) match
        case Left(ResetError.WouldClobberIgnored(paths)) =>
          // The ignored files themselves, not the directory standing in the way: these are what a
          // human needs to see. `secrets/.env` is not among them — nothing is being written at
          // `secrets`, so that directory is in no danger.
          assertEquals(paths.sorted, List("build/nested/deep.log", "build/output.log"))
        case other => fail(s"expected WouldClobberIgnored, got $other")

      // Refused before the first removal, so the working tree is exactly as it was — including the
      // edit to a.txt, which a reset that had got as far as writing would have reverted.
      assertEquals(hashOnDisk(repo, "build/output.log"), doomedBytes)
      assertEquals(repo.read("build/nested/deep.log"), "ignored, further down")
      assertEquals(repo.read("secrets/.env"), "API_KEY=the sort of thing that must not evaporate")
      assertEquals(repo.read("a.txt"), "edited too, and not restored either")
      assertEquals(repo.status, before)
      assertEquals(tempIndexes(), Set.empty[String])

  test("clobber: a directory holding only capturable content is reset, not refused"):
    withRepo: repo =>
      repo.write(".gitignore", "*.log\n")
      repo.write("a.txt", "hello")
      repo.write("build", "a file, at a path a build directory would want")
      repo.commit("init")
      val recorded = snapshot(repo, "s1")

      // Same shape as the refusal above, except everything in the directory is capturable — so it
      // is all in the snapshot that was taken before this ran, and all of it is a removal of its
      // own. Nothing here is unrecoverable, and the reset goes through.
      repo.remove("build")
      repo.write("build/main.c", "int main(void) { return 0; }")
      repo.write("build/nested/aux.c", "static int aux;")

      val outcome = resetTo(repo, recorded.commit)

      assertEquals(repo.read("build"), "a file, at a path a build directory would want")
      assertEquals(currentTree(repo), recorded.tree)
      assertEquals(outcome, ResetOutcome(changed = 1, deleted = 2, skippedGitlinks = Nil))

  test("clobber: a directory mixing captured and ignored content refuses too"):
    withRepo: repo =>
      repo.write(".gitignore", "*.log\n")
      repo.write("a.txt", "hello")
      repo.write("build", "a file, at a path a build directory would want")
      repo.commit("init")
      val recorded = snapshot(repo, "s1")

      repo.remove("build")
      repo.write("build/main.c", "int main(void) { return 0; }")
      repo.write("build/output.log", "ignored, and mixed in with content that is not")

      // The capturable half would be removed first, leaving the ignored half alone in the
      // directory for the write to destroy — so a mixed directory is exactly as dangerous as a
      // wholly ignored one, and refused on the same terms.
      assertEquals(
        Worktree.reset(repo.dir, recorded.commit),
        Left(ResetError.WouldClobberIgnored(List("build/output.log")))
      )
      assertEquals(repo.read("build/main.c"), "int main(void) { return 0; }")
      assertEquals(repo.read("build/output.log"), "ignored, and mixed in with content that is not")

  test("clobber: a symlink pointing at a directory is not a directory in the way"):
    assume(TestOs.platform() != "win32", "symlinks are a POSIX fixture here")
    withRepo: repo =>
      repo.write(".gitignore", "*.log\n")
      repo.write("a.txt", "hello")
      repo.write("perch", "a file, at a path a symlink would later take")
      repo.write("elsewhere/keep.log", "ignored, and pointed at from perch")
      repo.commit("init")
      val recorded = snapshot(repo, "s1")

      repo.remove("perch")
      ExtFs.symlinkSync("elsewhere", repo.path("perch"))

      val outcome = resetTo(repo, recorded.commit)

      // Unlinking the symlink writes the file without disturbing what it pointed at, so the
      // pre-flight looks at the path itself rather than following it — otherwise this would refuse
      // for a directory that was never in danger.
      assertEquals(repo.read("perch"), "a file, at a path a symlink would later take")
      assertEquals(repo.read("elsewhere/keep.log"), "ignored, and pointed at from perch")
      assertEquals(outcome.changed, 1)
      assertEquals(currentTree(repo), recorded.tree)

  // -------------------------------------------------------------------------------------------
  // Nested repositories
  // -------------------------------------------------------------------------------------------

  /** A repository inside the repository, with a commit of its own. */
  private def nestedRepo(repo: TempRepo, at: String): String =
    val nested = repo.path(at)
    TestFs.mkdirSync(nested, recursiveForce)
    GitCmd.run(nested, Map.empty, "init", "-b", "main")
    repo.write(s"$at/inner.txt", "inner content")
    GitCmd.run(nested, Map.empty, "add", "-A")
    GitCmd.run(nested, Map.empty,
      "-c", "user.name=Test", "-c", "user.email=t@e", "commit", "-m", "inner")
    GitCmd.run(nested, Map.empty, "rev-parse", "HEAD").getOrElse("")

  test("nested repository: a gitlink the target tree lacks is left alone, not deleted"):
    withRepo: repo =>
      seed(repo)
      val recorded = snapshot(repo, "s1")
      val innerHead = nestedRepo(repo, "nested")
      assertEquals(entries(repo, currentTree(repo)).find(_.path == "nested").map(_.mode),
        Some("160000"))

      val outcome = resetTo(repo, recorded.commit)

      // Removing the gitlink would mean deleting the nested repository's own .git, and no snapshot
      // holds that — the outer repository does not even have the inner commit. So the reset leaves
      // it standing, and says so rather than leaving a caller to notice the gap.
      assertEquals(outcome.skippedGitlinks, List("nested"))
      assert(repo.exists("nested/.git"), "the reset deleted a nested repository")
      assertEquals(repo.read("nested/inner.txt"), "inner content")
      assertEquals(GitCmd.run(repo.path("nested"), Map.empty, "rev-parse", "HEAD"), Right(innerHead))
      assertNotEquals(currentTree(repo), recorded.tree)
      // And the difference is exactly the gitlink: everything else came back.
      assertEquals(
        entries(repo, currentTree(repo)).filter(_.mode != "160000"),
        entries(repo, recorded.tree)
      )

  test("nested repository: a gitlink the target tree holds is not recreated"):
    withRepo: repo =>
      seed(repo)
      nestedRepo(repo, "nested")
      val recorded = snapshot(repo, "s1")
      assert(entries(repo, recorded.tree).exists(_.path == "nested"))
      repo.remove("nested")
      repo.write("a.txt", "edited too")

      val outcome = resetTo(repo, recorded.commit)

      // `checkout-index` for a gitlink path succeeds and leaves an empty directory — it cannot
      // restore a nested repository's content, so the reset does not pretend to. Everything that
      // is not a gitlink still comes back, and the one that did not is named.
      assertEquals(outcome.skippedGitlinks, List("nested"))
      assertEquals(outcome.changed, 1)
      assert(!repo.exists("nested/inner.txt"), "a nested repository was half-recreated")
      assertEquals(repo.read("a.txt"), "hello")
      assertNotEquals(currentTree(repo), recorded.tree)
      assertEquals(
        entries(repo, recorded.tree).filter(_.mode != "160000"),
        entries(repo, currentTree(repo))
      )

  // -------------------------------------------------------------------------------------------
  // Failures
  // -------------------------------------------------------------------------------------------

  test("failure: an unresolvable commit comes back as the failed git command"):
    withRepo: repo =>
      seed(repo)
      repo.write("a.txt", "edited")
      Worktree.reset(repo.dir, "no-such-revision") match
        case Left(ResetError.Git(GitError.CommandFailed(args, exitCode, stderr))) =>
          assert(args.contains("rev-parse"), clue(args))
          assertNotEquals(exitCode, 0)
          assert(stderr.nonEmpty, "git was supposed to explain itself")
        case other => fail(s"expected a wrapped CommandFailed, got $other")
      // Nothing was touched on the way to failing: the target is resolved before anything moves.
      assertEquals(repo.read("a.txt"), "edited")

  test("failure: a directory outside a repository is not a repository"):
    val dir = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "auk-plain-"))
    try
      assertEquals(Worktree.reset(dir, "HEAD"), Left(ResetError.Git(GitError.NotARepository(dir))))
    finally TestFs.rmSync(dir, recursiveForce)

  test("failure: a path that is not a directory at all is not a repository"):
    val file = scratchPath("auk-not-a-dir-")
    ExtFs.writeFileSync(file, "not a directory\n")
    try
      assertEquals(Worktree.reset(file, "HEAD"), Left(ResetError.Git(GitError.NotARepository(file))))
    finally TestFs.rmSync(file, recursiveForce)
