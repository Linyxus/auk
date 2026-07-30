package auk

import auk.platform.Platform
import auk.snapshot.{GitError, ResetError, ResetOutcome, Snapshot, Worktree}

/** Guards the seam between the WasmGC root and the `snapshot` subproject.
  *
  * The subproject has a suite of its own, and this is not a second copy of it: everything about what
  * a snapshot or a reset *does* is covered there, against real repositories. What is covered here is
  * the one thing that suite cannot see — that the engine is reachable from the root at all.
  *
  * Root depends on `snapshot` in build.sbt, but a dependency nothing references is dead code, and
  * the Scala.js linker drops it. So `sbt test` passing proves nothing about this link until some
  * root-side code calls into the package; until the loop feature does, this test is that code. It
  * also crosses the riskier boundary in the same breath: `snapshot` reaches Node through
  * `@JSImport("node:...")` facades that have to link as real ESM imports under WebAssembly, which is
  * a different backend from the plain-JS one the subproject's own tests run on. A break in either
  * shows up here as a module that will not load rather than as an assertion failure.
  *
  * The directory is a real one that is simply not a repository, rather than a path that does not
  * exist. The difference matters: git refuses a missing directory before it ever looks for a
  * repository, so a nonexistent path would answer [[GitError.NotARepository]] even if repository
  * detection were broken, and the assertion would mean nothing. A real directory makes git walk up
  * from it, find nothing, and say so — which is the answer being claimed here.
  */
class SnapshotSeamSuite extends munit.FunSuite:

  private var dir: String = ""

  override def beforeAll(): Unit = dir = TestFs.tempDir("auk-seam")
  override def afterAll(): Unit = Platform.fs.removeAll(dir)

  test("the snapshot engine links and executes under the Wasm root"):
    // A value out of the package, so the object is linked rather than merely referenced.
    assertEquals(Snapshot.RefPrefix, "refs/auk/snapshots/")

    // The read side, and the side that edits the working tree — the two the loop feature calls.
    assertEquals(Snapshot.treeOf(dir, "HEAD"), Left(GitError.NotARepository(dir)))
    assertEquals(Worktree.reset(dir, "HEAD"), Left(ResetError.Git(GitError.NotARepository(dir))))

    // The result types themselves, so neither can be dropped or drift out from under a caller.
    assertEquals(ResetOutcome.Unchanged, ResetOutcome(0, 0, Nil))
    assertEquals(ResetError.WouldClobberIgnored(List("x")).paths, List("x"))
