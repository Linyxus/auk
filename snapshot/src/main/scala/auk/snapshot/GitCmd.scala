package auk.snapshot

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Node builtins used by this package.
  *
  * These are facades, not shims: each `@JSImport` becomes a static ESM import in the linker output,
  * which is why nothing here reaches for `require`. Filesystem work goes through `node:fs` for the
  * same reason — `java.io`/`java.nio` do not exist under Scala.js.
  */
@js.native
@JSImport("node:child_process", JSImport.Namespace)
private[snapshot] object NodeChildProcess extends js.Object:
  def execFileSync(file: String, args: js.Array[String], options: js.Any): js.Any = js.native

@js.native
@JSImport("node:fs", JSImport.Namespace)
private[snapshot] object NodeFs extends js.Object:
  def rmSync(path: String, options: js.Any): Unit = js.native

@js.native
@JSImport("node:os", JSImport.Namespace)
private[snapshot] object NodeOs extends js.Object:
  def tmpdir(): String = js.native

@js.native
@JSImport("node:path", JSImport.Namespace)
private[snapshot] object NodePath extends js.Object:
  def join(parts: String*): String = js.native

@js.native
@JSImport("node:process", JSImport.Default)
private[snapshot] object NodeProcess extends js.Object:
  def env: js.Dictionary[String] = js.native
  def pid: Int = js.native

/** Runs the `git` binary synchronously and turns its exit status into an `Either`.
  *
  * Every call is scoped twice over: `cwd` is the caller's directory *and* the argument vector opens
  * with `-C <dir>`, so a relative path can never be resolved against the process's own working
  * directory. stdin is closed so git can never block on a credential or editor prompt, and stderr is
  * piped rather than inherited — Node's default is to forward stderr to the parent, which would both
  * pollute the console and leave `error.stderr` empty.
  */
private[snapshot] object GitCmd:

  /** 64 MiB; Node's 1 MiB default is easy to exceed with `for-each-ref` on a busy repository. */
  private val MaxBuffer = 64 * 1024 * 1024

  /** Runs `git -C <dir> <args...>` with `extraEnv` layered over the ambient environment and returns
    * trimmed stdout, which is what almost every caller wants: git terminates an object id or a ref
    * name with a newline nobody is interested in.
    */
  def run(
    dir: String,
    extraEnv: Map[String, String],
    args: String*
  ): Either[GitError.CommandFailed, String] =
    raw(dir, extraEnv, args*).map(_.trim)

  /** [[run]] without the trim: stdout exactly as git wrote it.
    *
    * For output that is content rather than an answer — a patch, a blob — the bytes are the point,
    * and a trailing newline is part of them. Trimming and then adding one back would be a guess.
    *
    * `execFileSync` replaces the environment wholesale rather than extending it, so the ambient one
    * is copied in first: dropping it would lose PATH, HOME and the git config they point at.
    */
  def raw(
    dir: String,
    extraEnv: Map[String, String],
    args: String*
  ): Either[GitError.CommandFailed, String] =
    val argv = "-C" :: dir :: args.toList
    try Right(text(NodeChildProcess.execFileSync("git", js.Array(argv*), options(dir, extraEnv))))
    catch
      case js.JavaScriptException(error) =>
        val thrown = error.asInstanceOf[js.Dynamic]
        Left(GitError.CommandFailed(argv, status(thrown), text(thrown.stderr).trim))

  private def options(dir: String, extraEnv: Map[String, String]): js.Any =
    val env = js.Dictionary.empty[String]
    NodeProcess.env.foreach((name, value) => env(name) = value)
    extraEnv.foreach((name, value) => env(name) = value)
    val options = js.Dictionary.empty[js.Any]
    options("cwd") = dir
    options("env") = env
    options("encoding") = "utf8"
    options("stdio") = js.Array[js.Any]("ignore", "pipe", "pipe")
    options("maxBuffer") = MaxBuffer
    options

  /** A process killed by a signal reports `status = null`; report it as -1 rather than crashing. */
  private def status(error: js.Dynamic): Int =
    val code = error.status
    if code == null || js.isUndefined(code) then -1 else code.asInstanceOf[Int]

  /** `encoding = "utf8"` makes these strings already, but a stream that was never opened comes back
    * as `null`/`undefined`.
    */
  private def text(value: js.Any): String =
    if value == null || js.isUndefined(value) then "" else value.toString
