package auk.snapshot

import scala.scalajs.js

/** Behavior tests for the snapshot primitive, organized by the rules they pin (S1-S10).
  *
  * Every repository here is a fresh [[TempRepo]] under the OS temp directory: no test may name a
  * repository any other way, so nothing in this suite can reach auk's own git state.
  */
class SnapshotSuite extends munit.FunSuite:

  // -------------------------------------------------------------------------------------------
  // Fixtures
  // -------------------------------------------------------------------------------------------

  private def withRepo(body: TempRepo => Unit): Unit =
    val repo = TempRepo.create()
    try body(repo)
    finally repo.dispose()

  /** A directory under the OS temp dir that is deliberately not a repository. */
  private def withPlainDir(body: String => Unit): Unit =
    val dir = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "auk-plain-"))
    val options = js.Dictionary.empty[js.Any]
    options("recursive") = true
    options("force") = true
    try body(dir)
    finally TestFs.rmSync(dir, options)

  /** One commit holding a root file and a nested one; returns the commit. */
  private def seed(repo: TempRepo): String =
    repo.write("a.txt", "hello")
    repo.write("sub/b.txt", "nested")
    repo.commit("init")

  /** Staged changes, unstaged edits and untracked files, all at once. */
  private def dirty(repo: TempRepo): Unit =
    repo.write("staged.txt", "staged")
    repo.git("add", "staged.txt")
    repo.write("sub/b.txt", "staged edit")
    repo.git("add", "sub/b.txt")
    repo.write("sub/b.txt", "further edit")
    repo.write("a.txt", "unstaged edit")
    repo.write("untracked.txt", "loose")

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

  private def assertInvalidId(result: Either[GitError, ?], id: String)(using
    munit.Location
  ): Unit =
    result match
      case Left(GitError.InvalidId(rejected, reason)) =>
        assertEquals(rejected, id)
        assert(reason.nonEmpty, "InvalidId should explain itself")
      case other => fail(s"expected InvalidId('$id'), got $other")

  /** Names of leftover temp indexes in the OS temp directory that THIS process created. The name
    * embeds the creating pid, and filtering on it matters: the build runs suites in concurrent Node
    * processes, so another suite's mid-`create` index is visible here and is not a leak.
    */
  private def tempIndexes(): Set[String] =
    TestFs.readdirSync(TestOs.tmpdir()).toSet.filter(_.startsWith(s"auk-index-${NodeProcess.pid}-"))

  /** Runs `body` with git's global and system config pointed at nothing, so the only identity git
    * can possibly see is the one the snapshot code hands it.
    */
  private def withoutAmbientGitConfig(body: => Unit): Unit =
    val env = NodeProcess.env
    val keys = List("GIT_CONFIG_GLOBAL", "GIT_CONFIG_SYSTEM")
    val saved = keys.map(key => key -> env.get(key))
    keys.foreach(key => env(key) = "/dev/null")
    try body
    finally
      saved.foreach { (key, previous) =>
        previous match
          case Some(value) => env(key) = value
          case None        => env -= key
      }

  // -------------------------------------------------------------------------------------------
  // Creation (S1)
  // -------------------------------------------------------------------------------------------

  test("create: the ref lands in the hidden namespace at the returned commit"):
    withRepo: repo =>
      seed(repo)
      val result = snapshot(repo, "s1")
      assertEquals(result.id, "s1")
      assertEquals(result.ref, "refs/auk/snapshots/s1")
      assertEquals(repo.git("rev-parse", "--verify", result.ref), result.commit)

  test("create: snapshots stay out of git branch"):
    withRepo: repo =>
      seed(repo)
      snapshot(repo, "s1")
      val branches = repo.git("branch", "--list", "--all")
      assert(!branches.contains("s1"), clue(branches))
      assert(!branches.contains("snapshots"), clue(branches))
      assertEquals(branches.linesIterator.map(_.trim).toList, List("* main"))

  test("create: the commit is parented on HEAD, and on nothing else"):
    withRepo: repo =>
      val head = seed(repo)
      val result = snapshot(repo, "s1")
      assertEquals(result.parent, Some(head))
      assertEquals(
        repo.git("rev-list", "--parents", "-1", result.commit),
        s"${result.commit} $head"
      )

  test("create: the commit message names the snapshot"):
    withRepo: repo =>
      seed(repo)
      val result = snapshot(repo, "s1")
      assertEquals(repo.git("log", "-1", "--format=%s", result.commit), "auk snapshot s1")

  test("create: the reported tree is the commit's own tree"):
    withRepo: repo =>
      seed(repo)
      val result = snapshot(repo, "s1")
      assertEquals(repo.git("rev-parse", s"${result.commit}^{tree}"), result.tree)

  // -------------------------------------------------------------------------------------------
  // What ends up in the snapshot (S1)
  // -------------------------------------------------------------------------------------------

  test("content: a modified tracked file is recorded as it is on disk"):
    withRepo: repo =>
      seed(repo)
      repo.write("a.txt", "modified")
      val result = snapshot(repo, "s1")
      assertEquals(repo.show(result.commit, "a.txt"), "modified")
      assertEquals(repo.git("show", "HEAD:a.txt"), "hello")

  test("content: an untracked file is recorded"):
    withRepo: repo =>
      seed(repo)
      repo.write("fresh.txt", "brand new")
      val result = snapshot(repo, "s1")
      assertEquals(repo.show(result.commit, "fresh.txt"), "brand new")

  test("content: the working tree wins over the index for a staged-then-edited file"):
    withRepo: repo =>
      seed(repo)
      repo.write("c.txt", "first")
      repo.git("add", "c.txt")
      repo.write("c.txt", "second")
      val result = snapshot(repo, "s1")
      assertEquals(repo.show(result.commit, "c.txt"), "second")

  test("content: a staged deletion leaves the path out of the tree"):
    withRepo: repo =>
      seed(repo)
      repo.git("rm", "-q", "sub/b.txt")
      val result = snapshot(repo, "s1")
      assertEquals(repo.treePaths(result.commit), List("a.txt"))

  test("content: a file deleted from the working tree only is dropped as well"):
    withRepo: repo =>
      seed(repo)
      repo.remove("sub/b.txt")
      val result = snapshot(repo, "s1")
      assertEquals(repo.treePaths(result.commit), List("a.txt"))

  // -------------------------------------------------------------------------------------------
  // .gitignore (S1)
  // -------------------------------------------------------------------------------------------

  test("gitignore: an ignored file never reaches the snapshot"):
    withRepo: repo =>
      seed(repo)
      repo.write(".gitignore", "ignored.txt\nignored-dir/\n")
      repo.write("ignored.txt", "noise")
      repo.write("kept.txt", "signal")
      val paths = repo.treePaths(snapshot(repo, "s1").commit)
      assert(!paths.contains("ignored.txt"), clue(paths))
      assert(paths.contains("kept.txt"), clue(paths))

  test("gitignore: an ignored directory never reaches the snapshot"):
    withRepo: repo =>
      seed(repo)
      repo.write(".gitignore", "ignored-dir/\n")
      repo.write("ignored-dir/deep/noise.txt", "noise")
      val paths = repo.treePaths(snapshot(repo, "s1").commit)
      assert(!paths.exists(_.startsWith("ignored-dir/")), clue(paths))

  test("gitignore: .gitignore itself is captured"):
    withRepo: repo =>
      seed(repo)
      repo.write(".gitignore", "ignored.txt\n")
      val result = snapshot(repo, "s1")
      assertEquals(repo.show(result.commit, ".gitignore"), "ignored.txt")

  // -------------------------------------------------------------------------------------------
  // Zero disturbance (S2)
  // -------------------------------------------------------------------------------------------

  test("zero disturbance: git status reports exactly what it did before"):
    withRepo: repo =>
      seed(repo)
      dirty(repo)
      val before = repo.status
      snapshot(repo, "s1")
      assertEquals(repo.status, before)

  test("zero disturbance: the staged diff survives untouched"):
    withRepo: repo =>
      seed(repo)
      dirty(repo)
      val before = repo.git("diff", "--cached")
      assert(before.nonEmpty, "the fixture was supposed to stage something")
      snapshot(repo, "s1")
      assertEquals(repo.git("diff", "--cached"), before)

  test("zero disturbance: HEAD and the checked-out branch do not move"):
    withRepo: repo =>
      val head = seed(repo)
      dirty(repo)
      snapshot(repo, "s1")
      assertEquals(repo.head, head)
      assertEquals(repo.git("symbolic-ref", "HEAD"), "refs/heads/main")
      assertEquals(repo.git("rev-parse", "refs/heads/main"), head)

  test("zero disturbance: working tree contents and mtimes are unchanged"):
    withRepo: repo =>
      seed(repo)
      dirty(repo)
      val files = List("a.txt", "sub/b.txt", "staged.txt", "untracked.txt")
      val before = files.map(file => (repo.read(file), repo.mtime(file)))
      snapshot(repo, "s1")
      assertEquals(files.map(file => (repo.read(file), repo.mtime(file))), before)

  test("zero disturbance: an in-progress merge is left in progress"):
    withRepo: repo =>
      repo.write("c.txt", "base")
      repo.commit("base")
      repo.git("checkout", "-q", "-b", "other")
      repo.write("c.txt", "other side")
      repo.commit("other")
      repo.git("checkout", "-q", "main")
      repo.write("c.txt", "main side")
      repo.commit("main")
      val merge = repo.tryGit("-c", "user.name=Test", "-c", "user.email=t@e", "merge", "other")
      assert(merge.isLeft, "the fixture merge was supposed to conflict")
      val mergeHead = repo.read(".git/MERGE_HEAD")
      val before = repo.status
      assert(before.linesIterator.exists(_.startsWith("u ")), clue(before))
      snapshot(repo, "s1")
      assertEquals(repo.read(".git/MERGE_HEAD"), mergeHead)
      assertEquals(repo.status, before)

  test("zero disturbance: a split-index repository gets nothing new in .git"):
    withRepo: repo =>
      seed(repo)
      repo.git("config", "core.splitIndex", "true")
      repo.write("untracked.txt", "loose")
      val before = TestFs.readdirSync(repo.path(".git")).toSet
      val result = snapshot(repo, "s1")
      val added = TestFs.readdirSync(repo.path(".git")).toSet.diff(before)
      assert(!added.exists(_.startsWith("sharedindex")), clue(added))
      assertEquals(repo.show(result.commit, "untracked.txt"), "loose")

  // -------------------------------------------------------------------------------------------
  // Unborn and detached HEAD (S3)
  // -------------------------------------------------------------------------------------------

  test("unborn HEAD: the snapshot has no parent"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      val result = snapshot(repo, "s1")
      assertEquals(result.parent, None)
      assertEquals(repo.git("rev-list", "--parents", "-1", result.commit), result.commit)

  test("unborn HEAD: content is captured all the same"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.write("sub/b.txt", "nested")
      val result = snapshot(repo, "s1")
      assertEquals(repo.treePaths(result.commit), List("a.txt", "sub/b.txt"))
      assertEquals(repo.show(result.commit, "sub/b.txt"), "nested")

  test("unborn HEAD: the repository still has no commits afterwards"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      snapshot(repo, "s1")
      assertEquals(repo.git("symbolic-ref", "HEAD"), "refs/heads/main")
      assert(
        repo.tryGit("rev-parse", "--verify", "--quiet", "HEAD").isLeft,
        "HEAD was supposed to stay unborn"
      )

  test("detached HEAD: the parent is the detached commit"):
    withRepo: repo =>
      val first = seed(repo)
      repo.write("a.txt", "second")
      repo.commit("second")
      repo.git("checkout", "-q", first)
      val result = snapshot(repo, "s1")
      assertEquals(result.parent, Some(first))
      assertEquals(repo.git("rev-parse", s"${result.commit}^"), first)

  // -------------------------------------------------------------------------------------------
  // Identity (S4)
  // -------------------------------------------------------------------------------------------

  test("identity: the author is the tool"):
    withRepo: repo =>
      seed(repo)
      val result = snapshot(repo, "s1")
      assertEquals(repo.git("log", "-1", "--format=%an <%ae>", result.commit), "auk <auk@snapshot>")

  test("identity: the committer is the tool"):
    withRepo: repo =>
      seed(repo)
      val result = snapshot(repo, "s1")
      assertEquals(repo.git("log", "-1", "--format=%cn <%ce>", result.commit), "auk <auk@snapshot>")

  test("identity: a snapshot works with no identity configured anywhere"):
    withoutAmbientGitConfig:
      withRepo: repo =>
        seed(repo)
        assert(
          repo.tryGit("config", "--get", "user.email").isLeft,
          "the fixture repository was supposed to have no identity at all"
        )
        val result = snapshot(repo, "s1")
        assertEquals(
          repo.git("log", "-1", "--format=%an <%ae> %cn <%ce>", result.commit),
          "auk <auk@snapshot> auk <auk@snapshot>"
        )

  // -------------------------------------------------------------------------------------------
  // Uniqueness (S6)
  // -------------------------------------------------------------------------------------------

  test("uniqueness: a second snapshot under the same id is refused"):
    withRepo: repo =>
      seed(repo)
      val first = snapshot(repo, "s1")
      repo.write("a.txt", "changed")
      assertEquals(Snapshot.create(repo.dir, "s1"), Left(GitError.SnapshotExists("s1")))
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Right(first.commit))

  test("uniqueness: force repoints the ref at the new commit"):
    withRepo: repo =>
      seed(repo)
      val first = snapshot(repo, "s1")
      repo.write("a.txt", "changed")
      val second = snapshot(repo, "s1", force = true)
      assertNotEquals(second.commit, first.commit)
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Right(second.commit))
      assertEquals(repo.show(second.commit, "a.txt"), "changed")

  test("uniqueness: force leaves the old commit unreferenced"):
    withRepo: repo =>
      seed(repo)
      val first = snapshot(repo, "s1")
      repo.write("a.txt", "changed")
      val second = snapshot(repo, "s1", force = true)
      val reachable = repo.git("rev-list", "--all").linesIterator.toSet
      assert(reachable.contains(second.commit), clue(reachable))
      assert(!reachable.contains(first.commit), clue(reachable))
      assertEquals(Snapshot.list(repo.dir), Right(List(SnapshotEntry("s1", second.commit))))

  // -------------------------------------------------------------------------------------------
  // Ids (S5)
  // -------------------------------------------------------------------------------------------

  test("ids: an empty id is rejected"):
    withRepo: repo =>
      seed(repo)
      assertInvalidId(Snapshot.create(repo.dir, ""), "")

  test("ids: an id with a space is rejected"):
    withRepo: repo =>
      seed(repo)
      assertInvalidId(Snapshot.create(repo.dir, "has space"), "has space")

  test("ids: an id containing '..' is rejected"):
    withRepo: repo =>
      seed(repo)
      assertInvalidId(Snapshot.create(repo.dir, "a..b"), "a..b")

  test("ids: an id ending in a slash is rejected"):
    withRepo: repo =>
      seed(repo)
      assertInvalidId(Snapshot.create(repo.dir, "ends/"), "ends/")

  test("ids: an id ending in .lock is rejected"):
    withRepo: repo =>
      seed(repo)
      assertInvalidId(Snapshot.create(repo.dir, "x.lock"), "x.lock")

  test("ids: an id with an empty path component is rejected"):
    withRepo: repo =>
      seed(repo)
      assertInvalidId(Snapshot.create(repo.dir, "a//b"), "a//b")

  test("ids: a rejected id creates nothing"):
    withRepo: repo =>
      seed(repo)
      assertInvalidId(Snapshot.create(repo.dir, "has space"), "has space")
      assertEquals(Snapshot.list(repo.dir), Right(Nil))

  test("ids: slashes are legal, so an id can carry structure"):
    withRepo: repo =>
      seed(repo)
      val result = snapshot(repo, "sync/1/before")
      assertEquals(result.ref, "refs/auk/snapshots/sync/1/before")
      assertEquals(Snapshot.resolve(repo.dir, "sync/1/before"), Right(result.commit))

  // -------------------------------------------------------------------------------------------
  // sameAsHeadTree (S7)
  // -------------------------------------------------------------------------------------------

  test("sameAsHeadTree: true for a clean checkout of HEAD"):
    withRepo: repo =>
      seed(repo)
      val result = snapshot(repo, "s1")
      assert(result.sameAsHeadTree)
      assertEquals(result.tree, repo.git("rev-parse", "HEAD^{tree}"))

  test("sameAsHeadTree: false once anything on disk differs"):
    withRepo: repo =>
      seed(repo)
      repo.write("untracked.txt", "loose")
      val result = snapshot(repo, "s1")
      assert(!result.sameAsHeadTree)
      assertNotEquals(result.tree, repo.git("rev-parse", "HEAD^{tree}"))

  test("sameAsHeadTree: false when HEAD is unborn"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      assert(!snapshot(repo, "s1").sameAsHeadTree)

  test("sameAsHeadTree: two snapshots of one dirty state share a tree"):
    withRepo: repo =>
      seed(repo)
      dirty(repo)
      val first = snapshot(repo, "s1")
      val second = snapshot(repo, "s2")
      assertEquals(second.tree, first.tree)
      assertNotEquals(second.commit, first.commit)

  // -------------------------------------------------------------------------------------------
  // list / resolve / delete (S8)
  // -------------------------------------------------------------------------------------------

  test("list: a repository with no snapshots lists nothing"):
    withRepo: repo =>
      seed(repo)
      assertEquals(Snapshot.list(repo.dir), Right(Nil))

  test("list: entries come back sorted by id, each with its own commit"):
    withRepo: repo =>
      seed(repo)
      val b = snapshot(repo, "b")
      repo.write("a.txt", "one")
      val a = snapshot(repo, "a")
      repo.write("a.txt", "two")
      val nested = snapshot(repo, "sync/1/before")
      assertEquals(
        Snapshot.list(repo.dir),
        Right(
          List(
            SnapshotEntry("a", a.commit),
            SnapshotEntry("b", b.commit),
            SnapshotEntry("sync/1/before", nested.commit)
          )
        )
      )

  test("resolve: an existing snapshot resolves to its commit"):
    withRepo: repo =>
      seed(repo)
      val result = snapshot(repo, "s1")
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Right(result.commit))

  test("resolve: an unknown snapshot is not found"):
    withRepo: repo =>
      seed(repo)
      snapshot(repo, "s1")
      assertEquals(Snapshot.resolve(repo.dir, "s2"), Left(GitError.SnapshotNotFound("s2")))

  test("delete: the ref goes away and the listing shrinks"):
    withRepo: repo =>
      seed(repo)
      snapshot(repo, "s1")
      repo.write("a.txt", "changed")
      val kept = snapshot(repo, "s2")
      assertEquals(Snapshot.delete(repo.dir, "s1"), Right(()))
      assertEquals(Snapshot.list(repo.dir), Right(List(SnapshotEntry("s2", kept.commit))))
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Left(GitError.SnapshotNotFound("s1")))
      assert(repo.tryGit("rev-parse", "--verify", "refs/auk/snapshots/s1").isLeft)

  test("delete: removing an unknown snapshot is not found"):
    withRepo: repo =>
      seed(repo)
      assertEquals(Snapshot.delete(repo.dir, "nope"), Left(GitError.SnapshotNotFound("nope")))

  // -------------------------------------------------------------------------------------------
  // Where the repository is (S9)
  // -------------------------------------------------------------------------------------------

  test("location: a directory outside any repository is not a repository"):
    withPlainDir: dir =>
      assertEquals(Snapshot.create(dir, "s1"), Left(GitError.NotARepository(dir)))

  test("location: list, resolve and delete report the same thing"):
    withPlainDir: dir =>
      assertEquals(Snapshot.list(dir), Left(GitError.NotARepository(dir)))
      assertEquals(Snapshot.resolve(dir, "s1"), Left(GitError.NotARepository(dir)))
      assertEquals(Snapshot.delete(dir, "s1"), Left(GitError.NotARepository(dir)))

  test("location: a snapshot taken from a subdirectory still covers the whole repository"):
    withRepo: repo =>
      seed(repo)
      repo.write("sub/deep/c.txt", "deep")
      repo.write("root.txt", "root")
      val result = created(Snapshot.create(repo.path("sub/deep"), "s1"))
      assertEquals(
        repo.treePaths(result.commit),
        List("a.txt", "root.txt", "sub/b.txt", "sub/deep/c.txt")
      )
      assertEquals(Snapshot.resolve(repo.dir, "s1"), Right(result.commit))

  // -------------------------------------------------------------------------------------------
  // Temp index hygiene (S2)
  // -------------------------------------------------------------------------------------------

  test("hygiene: a successful snapshot leaves no temp index behind"):
    withRepo: repo =>
      seed(repo)
      dirty(repo)
      val before = tempIndexes()
      snapshot(repo, "s1")
      assertEquals(tempIndexes().diff(before), Set.empty[String])

  test("hygiene: a refused snapshot leaves no temp index behind"):
    withRepo: repo =>
      seed(repo)
      snapshot(repo, "s1")
      val before = tempIndexes()
      assertEquals(Snapshot.create(repo.dir, "s1"), Left(GitError.SnapshotExists("s1")))
      assertEquals(tempIndexes().diff(before), Set.empty[String])

  test("hygiene: a snapshot that fails midway cleans up and reports the command that failed"):
    assume(TestOs.platform() != "win32", "the fixture relies on POSIX permission bits")
    withRepo: repo =>
      seed(repo)
      repo.write("unreadable.txt", "secret")
      repo.chmod("unreadable.txt", 0)
      val before = tempIndexes()

      Snapshot.create(repo.dir, "s1") match
        case Left(GitError.CommandFailed(args, exitCode, stderr)) =>
          assertEquals(args.take(2), List("-C", repo.dir))
          assert(args.contains("add"), clue(args))
          assertNotEquals(exitCode, 0)
          assert(stderr.contains("Permission denied"), clue(stderr))
        case other => fail(s"expected the staging step to fail, got $other")

      assertEquals(tempIndexes().diff(before), Set.empty[String])
      assertEquals(Snapshot.list(repo.dir), Right(Nil))
      repo.chmod("unreadable.txt", Integer.parseInt("644", 8))

  // -------------------------------------------------------------------------------------------
  // Hooks (S2)
  // -------------------------------------------------------------------------------------------

  test("hooks: a pre-commit hook cannot veto a snapshot"):
    assume(TestOs.platform() != "win32", "the hook is a POSIX shell script")
    withRepo: repo =>
      seed(repo)
      val marker = repo.path("hook-ran")
      repo.write(".git/hooks/pre-commit", s"#!/bin/sh\ntouch '$marker'\nexit 1\n")
      repo.makeExecutable(".git/hooks/pre-commit")
      repo.write("a.txt", "changed")

      val result = snapshot(repo, "s1")
      assert(!repo.exists("hook-ran"), "the pre-commit hook ran")
      assertEquals(repo.show(result.commit, "a.txt"), "changed")

      // The hook is real: a porcelain commit of the same state is refused by it.
      val porcelain =
        repo.tryGit("-c", "user.name=Test", "-c", "user.email=t@e", "commit", "-am", "nope")
      assert(porcelain.isLeft, "the fixture hook was supposed to block a porcelain commit")
      assert(repo.exists("hook-ran"), "the fixture hook never ran at all")
