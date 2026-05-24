package auk.llm.tools

import java.nio.file.{Path, Paths}
import gears.async.Async

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
    workingDirectory: Path,
    approvals: ApprovalPolicy = ApprovalPolicy.AllowAll
):
  /** Resolve a model-supplied path against [[workingDirectory]]. Absolute paths
    * are returned as-is; relative ones are anchored at the working directory.
    * The result is normalised (`..`/`.` collapsed) but not symlink-resolved.
    */
  def resolve(path: String): Path =
    workingDirectory.resolve(path).nn.normalize().nn

object RuntimeContext:
  /** A context rooted at the current process working directory, approving every
    * action. Handy for the CLI smoke loop and tests.
    */
  def cwd(approvals: ApprovalPolicy = ApprovalPolicy.AllowAll): RuntimeContext =
    RuntimeContext(Paths.get("").nn.toAbsolutePath().nn, approvals)

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
