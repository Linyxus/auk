package auk.webui.dev

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Minimal `node:http` facade (the codebase has only a `node:net` one), in the
  * same `@JSImport` style as `auk.platform.js.NodeFacades`. */
@js.native
@JSImport("node:http", JSImport.Namespace)
object NodeHttp extends js.Object:
  def createServer(handler: js.Function2[ServerRequest, ServerResponse, Unit]): HttpServer = js.native
  def get(url: String, cb: js.Function1[ClientResponse, Unit]): js.Dynamic = js.native

@js.native
trait HttpServer extends js.Object:
  def listen(port: Int, cb: js.Function0[Unit]): HttpServer = js.native
  def close(): HttpServer = js.native
  def address(): js.Dynamic = js.native

/** A server-side request (we only need method + url for routing). */
@js.native
trait ServerRequest extends js.Object:
  def url: String = js.native
  def method: String = js.native

@js.native
trait ServerResponse extends js.Object:
  def writeHead(status: Int, headers: js.Object): ServerResponse = js.native
  def write(chunk: String): Boolean = js.native
  def end(): Unit = js.native
  def end(chunk: String): Unit = js.native
  def end(chunk: js.typedarray.Uint8Array): Unit = js.native

/** A client-side response (used by the integration test's `NodeHttp.get`). */
@js.native
trait ClientResponse extends js.Object:
  def statusCode: Int = js.native
  def headers: js.Dynamic = js.native
  def setEncoding(enc: String): Unit = js.native
  def on(event: String, cb: js.Function1[js.Any, Unit]): ClientResponse = js.native

/** Minimal `node:fs` facade for serving static files (module-local; the root's
  * `NodeFs` is not visible here). */
@js.native
@JSImport("node:fs", JSImport.Namespace)
object NodeFs extends js.Object:
  def existsSync(path: String): Boolean = js.native
  def readFileSync(path: String, encoding: String): String = js.native
  /** No-encoding read: returns the raw bytes (a Node Buffer, a Uint8Array). */
  def readFileSync(path: String): js.typedarray.Uint8Array = js.native
