package auk.snapshot

/** Tests for the read-only corners of [[Snapshot]] that layers above use to compare recorded states:
  * looking a tree up, dating a commit, hashing the working tree without recording it, and diffing
  * two trees.
  */
class TreeOfSuite extends munit.FunSuite:

  /** Suites run concurrently in separate processes and snapshots leave a temp index in the OS temp
    * directory while they are written, so this one keeps its temp files somewhere private rather
    * than in the shared directory [[SnapshotSuite]]'s hygiene tests inspect. `os.tmpdir()` reads
    * `TMPDIR` on every call, so redirecting it moves this process's temp files and nobody else's.
    */
  private var releaseTmpdir: () => Unit = () => ()

  override def beforeAll(): Unit =
    val previous = NodeProcess.env.get("TMPDIR")
    val own = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "auk-suite-"))
    NodeProcess.env("TMPDIR") = own
    releaseTmpdir = () =>
      previous match
        case Some(value) => NodeProcess.env("TMPDIR") = value
        case None        => NodeProcess.env -= "TMPDIR"
      val options = scala.scalajs.js.Dictionary.empty[scala.scalajs.js.Any]
      options("recursive") = true
      options("force") = true
      TestFs.rmSync(own, options)

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

  test("treeOf: a commit resolves to the tree it records"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      val head = repo.commit("init")
      assertEquals(Snapshot.treeOf(repo.dir, head), Right(repo.git("rev-parse", "HEAD^{tree}")))
      assertEquals(Snapshot.treeOf(repo.dir, "HEAD"), Right(repo.git("rev-parse", "HEAD^{tree}")))

  test("treeOf: a snapshot resolves by ref name or by commit to its own tree"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      repo.write("untracked.txt", "loose")
      val snapshot = created(Snapshot.create(repo.dir, "s1"))
      assertEquals(Snapshot.treeOf(repo.dir, snapshot.ref), Right(snapshot.tree))
      assertEquals(Snapshot.treeOf(repo.dir, snapshot.commit), Right(snapshot.tree))
      // The snapshot caught the untracked file, so its tree is not HEAD's.
      assertNotEquals(snapshot.tree, repo.git("rev-parse", "HEAD^{tree}"))

  test("treeOf: an unresolvable revision comes back as the failed git command"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      Snapshot.treeOf(repo.dir, "no-such-revision") match
        case Left(GitError.CommandFailed(args, exitCode, stderr)) =>
          assert(args.contains("rev-parse"), clue(args))
          assertNotEquals(exitCode, 0)
          assert(stderr.nonEmpty, "git was supposed to explain itself")
        case other => fail(s"expected CommandFailed, got $other")

  test("committedAt: a commit reports its committer date as ISO-8601"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      val head = repo.commit("init")
      Snapshot.committedAt(repo.dir, head) match
        case Right(date) =>
          assert(date.matches("""\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.*"""), clue(date))
          assertEquals(date, repo.git("show", "-s", "--format=%cI", head))
        case other => fail(s"expected a timestamp, got $other")

  test("committedAt: a snapshot's ref name works as well as its commit"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      val snapshot = created(Snapshot.create(repo.dir, "s1"))
      assertEquals(
        Snapshot.committedAt(repo.dir, snapshot.ref),
        Snapshot.committedAt(repo.dir, snapshot.commit)
      )

  test("committedAt: an unresolvable revision comes back as the failed git command"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      Snapshot.committedAt(repo.dir, "no-such-revision") match
        case Left(GitError.CommandFailed(args, exitCode, stderr)) =>
          assert(args.contains("show"), clue(args))
          assertNotEquals(exitCode, 0)
          assert(stderr.nonEmpty, "git was supposed to explain itself")
        case other => fail(s"expected CommandFailed, got $other")

  // -------------------------------------------------------------------------------------------
  // currentTree and diffTrees
  // -------------------------------------------------------------------------------------------

  /** Leftover temp indexes belonging to this process. The suite runs in its own temp directory, and
    * the pid in the name narrows it further.
    */
  private def tempIndexes(): Set[String] =
    TestFs.readdirSync(TestOs.tmpdir()).toSet.filter(_.startsWith(s"auk-index-${NodeProcess.pid}-"))

  test("currentTree: is the tree a snapshot of the same state would record"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      repo.write("a.txt", "edited")
      repo.write("untracked.txt", "loose")
      val snapshot = created(Snapshot.create(repo.dir, "s1"))
      assertEquals(Snapshot.currentTree(repo.dir), Right(snapshot.tree))

  test("currentTree: honors gitignore, like every other capture"):
    withRepo: repo =>
      repo.write(".gitignore", "ignored.txt\n")
      repo.write("a.txt", "hello")
      repo.commit("init")
      val before = Snapshot.currentTree(repo.dir)
      repo.write("ignored.txt", "noise")
      assertEquals(Snapshot.currentTree(repo.dir), before)

  test("currentTree: writes a tree and nothing else"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      val head = repo.commit("init")
      repo.write("staged.txt", "staged")
      repo.git("add", "staged.txt")
      repo.write("a.txt", "unstaged edit")
      val status = repo.status
      val staged = repo.git("diff", "--cached")
      val indexes = tempIndexes()

      assert(Snapshot.currentTree(repo.dir).isRight)

      assertEquals(repo.status, status)
      assertEquals(repo.git("diff", "--cached"), staged)
      assertEquals(repo.head, head)
      assertEquals(Snapshot.list(repo.dir), Right(Nil))
      assertEquals(tempIndexes().diff(indexes), Set.empty[String])

  test("currentTree: an unborn HEAD still yields a tree"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      val tree = Snapshot.currentTree(repo.dir)
      assertEquals(tree, Right(created(Snapshot.create(repo.dir, "s1")).tree))

  test("diffTrees: equal trees produce nothing at all"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      val tree = repo.git("rev-parse", "HEAD^{tree}")
      assertEquals(Snapshot.diffTrees(repo.dir, tree, tree), Right(""))

  test("diffTrees: a change produces a patch that keeps its trailing newline"):
    withRepo: repo =>
      repo.write("a.txt", "before\n")
      repo.commit("init")
      val baseline = repo.git("rev-parse", "HEAD^{tree}")
      repo.write("a.txt", "after\n")
      val target = Snapshot.currentTree(repo.dir) match
        case Right(tree) => tree
        case Left(error) => fail(s"expected a tree, got $error")
      Snapshot.diffTrees(repo.dir, baseline, target) match
        case Right(patch) =>
          assert(patch.contains("diff --git a/a.txt b/a.txt"), clue(patch))
          assert(patch.contains("-before"), clue(patch))
          assert(patch.contains("+after"), clue(patch))
          assert(patch.endsWith("\n"), clue(patch))
        case other => fail(s"expected a patch, got $other")

  test("diffTrees: an unresolvable tree comes back as the failed git command"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      val tree = repo.git("rev-parse", "HEAD^{tree}")
      Snapshot.diffTrees(repo.dir, tree, "no-such-tree") match
        case Left(GitError.CommandFailed(args, exitCode, stderr)) =>
          assert(args.contains("diff"), clue(args))
          assertNotEquals(exitCode, 0)
          assert(stderr.nonEmpty, "git was supposed to explain itself")
        case other => fail(s"expected CommandFailed, got $other")

  test("treeOf: a directory outside a repository is not a repository"):
    val dir = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "auk-plain-"))
    val options = scala.scalajs.js.Dictionary.empty[scala.scalajs.js.Any]
    options("recursive") = true
    options("force") = true
    try assertEquals(Snapshot.treeOf(dir, "HEAD"), Left(GitError.NotARepository(dir)))
    finally TestFs.rmSync(dir, options)
