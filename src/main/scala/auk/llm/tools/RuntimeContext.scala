package auk.llm.tools

import gears.async.Async
import auk.platform.{FileSystem, PathOps, Platform}

/** Ambient environment handed to every [[Tool.execute]] call.
  *
  * Tools stay stateless: anything they need about *where* and *under what
  * authority* they run arrives through this context rather than being baked in
  * at construction or read from process-global state. That keeps them trivially
  * testable — a test constructs a context pointing at a temp directory with an
  * auto-approving policy and runs the tool.
  *
  * The runtime owns one context per agent session and passes it (as a `using`
  * parameter) into the dispatcher; see `auk.runtime.ToolRegistry`.
  *
  * This is intentionally small. Foreseeable additions — a filesystem
  * abstraction for sandboxing, a read-before-edit tracker, a cancellation
  * deadline — slot in as further fields without touching the [[Tool]] signature.
  */
final case class RuntimeContext(
    workingDirectory: String,
    approvals: ApprovalPolicy = ApprovalPolicy.AllowAll,
    fs: FileSystem = Platform.fs,
    progress: ProgressSink = ProgressSink.Noop
):
  /** Resolve a model-supplied path against [[workingDirectory]]. Absolute paths
    * are returned as-is; relative ones are anchored at the working directory.
    * The result is normalised (`..`/`.` collapsed) but not symlink-resolved.
    */
  def resolve(path: String): String =
    PathOps.resolve(workingDirectory, path)

  /** Emit a live progress update for the running tool. A no-op unless the
    * runtime has bound a [[ProgressSink]] to this call (see [[withProgress]]);
    * tests and headless runs simply discard it. The `update` uses the same
    * `Map[String, String]` vocabulary as [[ToolResult.metadata]], so a tool can
    * report a growing total mid-run and a final one on completion the same way.
    */
  def reportProgress(update: Map[String, String])(using Async): Unit =
    progress.report(update)

  /** A copy whose live progress flows to `sink`. The runtime uses this to bind a
    * sink tagged with the active tool call before dispatching it. */
  def withProgress(sink: ProgressSink): RuntimeContext = copy(progress = sink)

object RuntimeContext:
  /** A context rooted at the current process working directory, approving every
    * action. Handy for the CLI smoke loop and tests.
    */
  def cwd(approvals: ApprovalPolicy = ApprovalPolicy.AllowAll): RuntimeContext =
    RuntimeContext(Platform.cwd(), approvals)

/** A sink for the live, structured progress a tool emits *while it runs* — the
  * streaming counterpart to [[ToolResult.metadata]], which a tool can only
  * report once, at the end. The runtime binds a concrete sink to the active tool
  * call (tagging updates with its id and forwarding them to the UI); everything
  * else uses the [[ProgressSink.Noop]] default and pays nothing.
  *
  * `report` runs under `Async` so an interactive sink can hand the update to a
  * channel without blocking the tool's own work.
  */
trait ProgressSink:
  def report(update: Map[String, String])(using Async): Unit

object ProgressSink:
  /** Discard every update. The default for tests and headless runs. */
  object Noop extends ProgressSink:
    def report(update: Map[String, String])(using Async): Unit = ()

  /** Build a sink from a context function — the form callers actually want, so
    * an interactive runtime can write `ProgressSink(update => channel.send(...))`
    * rather than spelling out an anonymous class. */
  def apply(f: Map[String, String] => Async ?=> Unit): ProgressSink =
    new ProgressSink:
      def report(update: Map[String, String])(using Async): Unit = f(update)

/** A request for permission to perform a side-effecting action. */
final case class ApprovalRequest(
    toolName: String,
    /** A short, human-readable description of what is about to happen, e.g. the
      * shell command to run or the file about to be overwritten.
      */
    summary: String
)

/** Decides whether a side-effecting tool action may proceed.
  *
  * A tool that mutates the world (writes a file, runs a command) calls
  * [[request]] before acting and aborts with a [[ToolResult.error]] when denied.
  * Read-only tools need not consult it. The policy runs under `Async` so an
  * interactive implementation can prompt the user without blocking the event
  * loop.
  */
trait ApprovalPolicy:
  def request(req: ApprovalRequest)(using Async): Boolean

object ApprovalPolicy:
  /** Approve everything. Suitable for trusted/headless runs and tests. */
  object AllowAll extends ApprovalPolicy:
    def request(req: ApprovalRequest)(using Async): Boolean = true

  /** Deny everything. Useful to neutralise mutating tools in a read-only mode. */
  object DenyAll extends ApprovalPolicy:
    def request(req: ApprovalRequest)(using Async): Boolean = false
