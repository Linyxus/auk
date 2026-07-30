package auk.snapshot

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Node builtins the tests need beyond what the implementation already facades. */
@js.native
@JSImport("node:fs", JSImport.Namespace)
private[snapshot] object TestFs extends js.Object:
  def mkdtempSync(prefix: String): String = js.native
  def mkdirSync(path: String, options: js.Any): js.Any = js.native
  def writeFileSync(path: String, data: String): Unit = js.native
  def readFileSync(path: String, encoding: String): String = js.native
  def readdirSync(path: String): js.Array[String] = js.native
  def existsSync(path: String): Boolean = js.native
  def chmodSync(path: String, mode: Int): Unit = js.native
  def statSync(path: String): TestStats = js.native
  def rmSync(path: String, options: js.Any): Unit = js.native

@js.native
private[snapshot] trait TestStats extends js.Object:
  def mtimeMs: Double = js.native

@js.native
@JSImport("node:os", JSImport.Namespace)
private[snapshot] object TestOs extends js.Object:
  def tmpdir(): String = js.native
  def platform(): String = js.native

@js.native
@JSImport("node:path", JSImport.Namespace)
private[snapshot] object TestPath extends js.Object:
  def join(parts: String*): String = js.native
  def dirname(path: String): String = js.native

/** A throwaway git repository living under the OS temp directory.
  *
  * Every git command a test runs goes through here, and every one of them is bound to [[dir]] — the
  * directory `mkdtempSync` handed out. That is the point: a bug in the code under test must not be
  * able to reach the repository auk itself lives in, so tests never name a repository any other
  * way.
  *
  * The repository is initialized with `git init -b main` and is deliberately never given a
  * `user.name`/`user.email` of its own. Test commits carry an inline identity instead, which leaves
  * the snapshot identity assertions (S4) meaningful: nothing local could have supplied one.
  */
final class TempRepo private (val dir: String):
  require(
    dir.startsWith(TestOs.tmpdir()),
    s"a TempRepo must live under the OS temp dir, got '$dir'"
  )

  /** Absolute path of a repository-relative path. */
  def path(relative: String): String = TestPath.join(dir, relative)

  def write(relative: String, content: String): Unit =
    val target = path(relative)
    val options = js.Dictionary.empty[js.Any]
    options("recursive") = true
    TestFs.mkdirSync(TestPath.dirname(target), options)
    TestFs.writeFileSync(target, content)

  def read(relative: String): String = TestFs.readFileSync(path(relative), "utf8")

  def exists(relative: String): Boolean = TestFs.existsSync(path(relative))

  def mtime(relative: String): Double = TestFs.statSync(path(relative)).mtimeMs

  /** `mode` is a plain permission bit pattern; write it as `Integer.parseInt("755", 8)`, since
    * Scala has no octal literals.
    */
  def chmod(relative: String, mode: Int): Unit = TestFs.chmodSync(path(relative), mode)

  def makeExecutable(relative: String): Unit = chmod(relative, Integer.parseInt("755", 8))

  def remove(relative: String): Unit =
    val options = js.Dictionary.empty[js.Any]
    options("recursive") = true
    options("force") = true
    TestFs.rmSync(path(relative), options)

  /** Runs `git -C <dir> <args...>`, failing loudly: a broken fixture should not look like a failed
    * assertion.
    */
  def git(args: String*): String =
    tryGit(args*) match
      case Right(output) => output
      case Left(error) =>
        throw IllegalStateException(
          s"fixture command 'git ${args.mkString(" ")}' failed (${error.exitCode}): ${error.stderr}"
        )

  def tryGit(args: String*): Either[GitError.CommandFailed, String] =
    GitCmd.run(dir, Map.empty, args*)

  /** Stages everything and commits it under a throwaway identity supplied per invocation. */
  def commit(message: String): String =
    git("add", "-A")
    git("-c", "user.name=Test", "-c", "user.email=test@example.com", "commit", "-m", message)
    head

  def head: String = git("rev-parse", "HEAD")

  def status: String = git("status", "--porcelain=v2")

  /** Paths recorded in a commit's tree, recursively. */
  def treePaths(commit: String): List[String] =
    git("ls-tree", "-r", "--name-only", commit).linesIterator.filter(_.nonEmpty).toList

  /** Contents of a path as recorded in a commit. */
  def show(commit: String, relative: String): String = git("show", s"$commit:$relative")

  def dispose(): Unit =
    val options = js.Dictionary.empty[js.Any]
    options("recursive") = true
    options("force") = true
    TestFs.rmSync(dir, options)

object TempRepo:
  def create(): TempRepo =
    val dir = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "auk-test-"))
    val repo = new TempRepo(dir)
    repo.git("init", "-b", "main")
    repo
