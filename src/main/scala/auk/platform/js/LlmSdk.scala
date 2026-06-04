package auk.platform.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Facades over the npm `openai` and `@anthropic-ai/sdk` clients.
  *
  * Requests are built as plain JS objects (the wire JSON shape) and responses
  * read back as `js.Dynamic`, so we don't need a typed facade for every field —
  * the SDKs handle auth, retries, and streaming. Non-streaming `create` resolves
  * to the parsed response; streaming `create` (`stream: true`) resolves to an
  * async-iterable of chunks (drive it with [[Interop.forEachAsync]]).
  */

@js.native
@JSImport("openai", JSImport.Default)
class OpenAI(opts: js.Object) extends js.Object:
  val chat: OpenAIChat = js.native
  val responses: OpenAIResponses = js.native

@js.native
trait OpenAIChat extends js.Object:
  val completions: OpenAICompletionsApi = js.native

@js.native
trait OpenAICompletionsApi extends js.Object:
  def create(body: js.Object): js.Promise[js.Dynamic] = js.native

@js.native
trait OpenAIResponses extends js.Object:
  def create(body: js.Object): js.Promise[js.Dynamic] = js.native

@js.native
@JSImport("@anthropic-ai/sdk", JSImport.Default)
class Anthropic(opts: js.Object) extends js.Object:
  val messages: AnthropicMessages = js.native

@js.native
trait AnthropicMessages extends js.Object:
  def create(body: js.Object): js.Promise[js.Dynamic] = js.native
