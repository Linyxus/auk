package auk.runtime.mcp

import auk.platform.js.LineProcess

/** The narrow seam between [[McpClient]] and the outside world: a bidirectional
  * stream of newline-framed JSON lines.
  *
  * This is the ONLY thing the client touches to reach a server, and it is
  * deliberately tiny — write a line, learn of an inbound line or an exit through
  * the callbacks, kill, ask if it is alive. That smallness is the point: the
  * production impl ([[LineProcessTransport]]) spawns a real child process, while
  * unit tests inject a scripted fake, so the JSON-RPC correlation, handshake, and
  * timeout logic in [[McpClient]] are exercised without ever launching a process.
  *
  * A transport is constructed already wired to the client's `onLine`/`onExit`
  * callbacks (both fire on the JS event loop; the client bridges them into gears
  * via `Future.Promise`). `onExit` fires at most once. */
trait McpTransport:
  /** Queue `line` (a newline is appended by the transport) for the server.
    * `false` means the channel was already closed and nothing was sent. */
  def writeLine(line: String): Boolean

  /** Tear the transport down (kill the child). Best effort, idempotent. */
  def kill(): Unit

  /** Whether the transport is still usable (the child has not exited). */
  def alive: Boolean

/** The production [[McpTransport]]: a long-lived stdio child process, framed line
  * by line by [[LineProcess]]. MCP stdio messages are newline-delimited UTF-8
  * JSON with no embedded newlines, which is exactly what [[LineProcess]] frames.
  *
  * A server's stderr is diagnostic chatter (many servers log there); Phase 1
  * drops it rather than surfacing it, keeping the transport's contract limited to
  * the JSON-RPC line stream. */
object LineProcessTransport:
  /** Build the factory [[McpClient]]/[[McpHub]] use: given the client's
    * callbacks, spawn the configured child and adapt its handle to
    * [[McpTransport]]. The process is not spawned until the factory is invoked
    * (on the client's first connect), which is what makes hub clients lazy. */
  def factory(config: McpServerConfig): (String => Unit, Int => Unit) => McpTransport =
    (onLine, onExit) => spawn(config, onLine, onExit)

  private def spawn(config: McpServerConfig, onLine: String => Unit, onExit: Int => Unit): McpTransport =
    val handle = LineProcess.spawn(
      argv = config.command :: config.args,
      extraEnv = config.env,
      onLine = onLine,
      onStderr = _ => (),
      onExit = onExit
    )
    new McpTransport:
      def writeLine(line: String): Boolean = handle.writeLine(line)
      def kill(): Unit = handle.kill()
      def alive: Boolean = handle.alive
