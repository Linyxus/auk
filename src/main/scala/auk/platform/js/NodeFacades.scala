package auk.platform.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, JSGlobal}

/** Thin `@JSImport`/`@JSGlobal` facades over the Node/Bun built-in modules.
  *
  * These are the ONLY declarations in the codebase that touch raw JS interop for
  * the runtime services; everything else goes through the `auk.platform` traits.
  * `node:*` specifiers are implemented by both Bun and Node, so the same facades
  * run on either runtime (see RUN.md).
  */

@js.native
@JSImport("node:fs", JSImport.Namespace)
private[platform] object NodeFs extends js.Object:
  def existsSync(p: String): Boolean = js.native
  def readFileSync(p: String, encoding: String): String = js.native
  /** No-encoding read: returns the raw bytes (a Node Buffer, a Uint8Array). */
  def readFileSync(p: String): js.typedarray.Uint8Array = js.native
  def writeFileSync(p: String, data: String, encoding: String): Unit = js.native
  def writeFileSync(p: String, data: js.typedarray.Uint8Array): Unit = js.native
  def appendFileSync(p: String, data: String, encoding: String): Unit = js.native
  def mkdirSync(p: String, options: js.Object): Unit = js.native
  def readdirSync(p: String): js.Array[String] = js.native
  def statSync(p: String): NodeStats = js.native
  def createReadStream(p: String): NodeReadable = js.native
  def renameSync(from: String, to: String): Unit = js.native
  def rmSync(p: String, options: js.Object): Unit = js.native
  def chmodSync(p: String, mode: Int): Unit = js.native

@js.native
private[platform] trait NodeStats extends js.Object:
  def isDirectory(): Boolean = js.native
  def isFile(): Boolean = js.native
  val size: Double = js.native
  val mtimeMs: Double = js.native

@js.native
@JSImport("node:path", JSImport.Namespace)
private[platform] object NodePath extends js.Object:
  def resolve(paths: String*): String = js.native
  def normalize(p: String): String = js.native
  def dirname(p: String): String = js.native
  def basename(p: String): String = js.native
  def join(paths: String*): String = js.native

@js.native
@JSImport("node:net", JSImport.Namespace)
private[platform] object NodeNet extends js.Object:
  def createServer(connectionListener: js.Function1[NodeSocket, Unit]): NodeServer = js.native

@js.native
private[platform] trait NodeServer extends js.Object:
  def listen(path: String, cb: js.Function0[Unit]): NodeServer = js.native
  def close(): NodeServer = js.native
  def on(event: String, cb: js.Function1[js.Any, Unit]): NodeServer = js.native

@js.native
private[platform] trait NodeSocket extends js.Object:
  def on(event: String, cb: js.Function1[js.Any, Unit]): NodeSocket = js.native
  def write(s: String): Boolean = js.native
  def end(): Unit = js.native
  def setEncoding(encoding: String): Unit = js.native

@js.native
@JSImport("node:http", JSImport.Namespace)
private[platform] object NodeHttp extends js.Object:
  def createServer(handler: js.Function2[NodeHttpRequest, NodeHttpResponse, Unit]): NodeHttpServer = js.native

@js.native
private[platform] trait NodeHttpServer extends js.Object:
  def listen(port: Int, cb: js.Function0[Unit]): NodeHttpServer = js.native
  def close(): NodeHttpServer = js.native
  /** Forcibly drop live connections (SSE streams never end on their own). */
  def closeAllConnections(): Unit = js.native
  /** Let the process exit even while the server is listening / has open sockets. */
  def unref(): NodeHttpServer = js.native
  def address(): js.Dynamic = js.native

@js.native
private[platform] trait NodeHttpRequest extends js.Object:
  def url: String = js.native
  def method: String = js.native
  def on(event: String, cb: js.Function1[js.Any, Unit]): NodeHttpRequest = js.native

@js.native
private[platform] trait NodeHttpResponse extends js.Object:
  def writeHead(status: Int, headers: js.Object): NodeHttpResponse = js.native
  def write(chunk: String): Boolean = js.native
  def end(): Unit = js.native
  def end(chunk: String): Unit = js.native
  def end(chunk: js.typedarray.Uint8Array): Unit = js.native
  def on(event: String, cb: js.Function1[js.Any, Unit]): NodeHttpResponse = js.native

@js.native
@JSImport("node:crypto", "randomUUID")
private[platform] def nodeRandomUUID(): String = js.native

@js.native
@JSImport("node:os", "tmpdir")
private[platform] def nodeTmpdir(): String = js.native

/** Today's date as `YYYY-MM-DD` (UTC), for dating a session in the prompt. */
private[platform] def nodeToday(): String = new js.Date().toISOString().take(10)

@js.native
@JSImport("node:child_process", JSImport.Namespace)
private[platform] object NodeChildProcess extends js.Object:
  def spawn(command: String, args: js.Array[String], options: js.Object): ChildProcess = js.native
  def spawnSync(command: String, args: js.Array[String], options: js.Object): SpawnSyncResult = js.native

@js.native
private[platform] trait SpawnSyncResult extends js.Object:
  val stdout: js.UndefOr[String] = js.native
  val status: js.UndefOr[Int] = js.native

@js.native
private[platform] trait ChildProcess extends js.Object:
  val stdin: NodeWritable = js.native
  val stdout: NodeReadable = js.native
  val stderr: NodeReadable = js.native
  /** The child's OS process id — `undefined` when the spawn itself failed. */
  val pid: js.UndefOr[Int] = js.native
  def kill(signal: String): Boolean = js.native
  /** Drop the child from the parent's event-loop refcount, so a detached
    * fire-and-forget spawn never keeps the process alive. */
  def unref(): Unit = js.native
  // `close` passes (code, signal); `error` passes (err). A 2-arg listener works
  // for both (the missing arg is undefined).
  def on(event: String, cb: js.Function2[js.Any, js.Any, Unit]): ChildProcess = js.native

@js.native
private[platform] trait NodeWritable extends js.Object:
  def write(s: String): Boolean = js.native
  def end(): Unit = js.native

@js.native
private[platform] trait NodeReadable extends js.Object:
  def on(event: String, cb: js.Function1[js.Any, Unit]): NodeReadable = js.native
  def setEncoding(encoding: String): Unit = js.native
  def destroy(): Unit = js.native

/** The global `process`, exposed by both Node and Bun. */
@js.native
@JSGlobal("process")
private[platform] object GlobalProcess extends js.Object:
  def cwd(): String = js.native
  def exit(code: Int): Unit = js.native
  def on(event: String, cb: js.Function1[js.Any, Unit]): Unit = js.native
  /** The raw process arguments. The executable/script prefix varies across
    * node/Bun/SEA, so callers scan for flags rather than index into this. */
  val argv: js.Array[String] = js.native
  val env: js.Dictionary[String] = js.native
  val stdin: NodeStdin = js.native
  val stdout: NodeStdout = js.native
  /** The executable running this process: `node` in dev, the auk SEA binary in
    * a packaged build. What [[ReplArtifacts]] spawns the REPL worker with. */
  val execPath: String = js.native
  /** Node's OS tag: `darwin` / `linux` / `win32`. */
  val platform: String = js.native
  val pid: Int = js.native

@js.native
private[platform] trait NodeStdin extends js.Object:
  def setRawMode(mode: Boolean): NodeStdin = js.native
  def setEncoding(encoding: String): NodeStdin = js.native
  def on(event: String, cb: js.Function1[js.Any, Unit]): NodeStdin = js.native
  def removeListener(event: String, cb: js.Function1[js.Any, Unit]): NodeStdin = js.native
  def read(): js.Any = js.native
  def resume(): NodeStdin = js.native
  def pause(): NodeStdin = js.native
  def ref(): NodeStdin = js.native
  def unref(): NodeStdin = js.native
  val isTTY: js.UndefOr[Boolean] = js.native

/** The `TextEncoder` global (Node/Bun/browser): UTF-8 encode a string to bytes. */
@js.native
@JSGlobal("TextEncoder")
private[platform] class TextEncoder extends js.Object:
  def encode(input: String): js.typedarray.Uint8Array = js.native

/** The `TextDecoder` global: UTF-8 decode bytes to a string. */
@js.native
@JSGlobal("TextDecoder")
private[platform] class TextDecoder extends js.Object:
  def decode(input: js.typedarray.ArrayBuffer): String = js.native

@js.native
private[platform] trait NodeStdout extends js.Object:
  def write(s: String): Boolean = js.native
  def on(event: String, cb: js.Function1[js.Any, Unit]): NodeStdout = js.native
  val columns: js.UndefOr[Int] = js.native
  val rows: js.UndefOr[Int] = js.native
  val isTTY: js.UndefOr[Boolean] = js.native
