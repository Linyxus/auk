package auk.runtime.mcp

import auk.llm.tools.Json

/** The host-side, decoded shapes of the MCP wire results.
  *
  * These are the currency the whole feature trades in: [[McpClient]] decodes raw
  * JSON-RPC results into them, and in Phase 2 [[McpHub]]'s bridge re-serializes
  * them (via each type's `toJson`) so the library-side proxy can reconstruct
  * them across the UDS boundary. Because those encoders ARE the bridge wire
  * format, their field names are a contract: keep them stable. Every encoder
  * emits all fields, using `null` for an absent optional, so the decoder on the
  * far side never has to reason about missing keys.
  *
  * The decoders (`fromJson` / `decode`) are deliberately lenient: real servers
  * omit optional fields freely and occasionally return shapes we only partially
  * model (image/audio data, binary blobs), so a slightly-off result yields a
  * best-effort value rather than an error. Genuine failures — a JSON-RPC `error`
  * object, a dead transport — are surfaced elsewhere as `Left`; these decoders
  * only ever run on a successful `result` payload.
  */

/** A tool advertised by `tools/list`. `inputSchema` is the JSON Schema object the
  * model is shown as data; it is carried verbatim (never a model-visible
  * `ToolSchema`). */
final case class McpToolInfo(name: String, description: String, inputSchema: Json):
  def toJson: Json = Json.Obj(
    List(
      "name" -> Json.Str(name),
      "description" -> Json.Str(description),
      "inputSchema" -> inputSchema
    )
  )

object McpToolInfo:
  private def fromJson(j: Json): McpToolInfo =
    McpToolInfo(
      name = JsonView.str(j, "name").getOrElse(""),
      description = JsonView.str(j, "description").getOrElse(""),
      // Some tools legitimately carry no schema; default to an empty object so
      // the model always sees a well-formed (if permissive) schema.
      inputSchema = JsonView.field(j, "inputSchema").getOrElse(Json.Obj(Nil))
    )

  /** Decode a `tools/list` result (`{tools:[...]}`). */
  def decodeList(result: Json): List[McpToolInfo] =
    JsonView.arr(result, "tools").getOrElse(Nil).map(fromJson)

/** A resource advertised by `resources/list`. `description`/`mimeType` are
  * optional in the spec. */
final case class McpResourceInfo(
    uri: String,
    name: String,
    description: Option[String],
    mimeType: Option[String]
):
  def toJson: Json = Json.Obj(
    List(
      "uri" -> Json.Str(uri),
      "name" -> Json.Str(name),
      "description" -> JsonView.strOrNull(description),
      "mimeType" -> JsonView.strOrNull(mimeType)
    )
  )

object McpResourceInfo:
  private def fromJson(j: Json): McpResourceInfo =
    McpResourceInfo(
      uri = JsonView.str(j, "uri").getOrElse(""),
      name = JsonView.str(j, "name").getOrElse(""),
      description = JsonView.str(j, "description"),
      mimeType = JsonView.str(j, "mimeType")
    )

  /** Decode a `resources/list` result (`{resources:[...]}`). */
  def decodeList(result: Json): List[McpResourceInfo] =
    JsonView.arr(result, "resources").getOrElse(Nil).map(fromJson)

/** One block of returned content, spanning both `tools/call` content items
  * (`{type, text, ...}`) and `resources/read` contents (`{uri, mimeType,
  * text|blob}`).
  *
  * `kind` normalises the two shapes: it is the `type` tag when present, else
  * inferred (`text` when a `text` field is present, `blob` for binary). Phase 1
  * carries `text` verbatim and records `mimeType`/`uri`; it does NOT decode
  * base64 image/audio/blob payloads (their `kind` is preserved with `text =
  * None`), which keeps the text path — the overwhelmingly common one — exact. */
final case class McpContentBlock(
    kind: String,
    text: Option[String],
    mimeType: Option[String],
    uri: Option[String]
):
  def toJson: Json = Json.Obj(
    List(
      "kind" -> Json.Str(kind),
      "text" -> JsonView.strOrNull(text),
      "mimeType" -> JsonView.strOrNull(mimeType),
      "uri" -> JsonView.strOrNull(uri)
    )
  )

object McpContentBlock:
  /** Decode one content/contents item. Handles the `type:"resource"` shape,
    * which nests the real payload under a `resource` object. */
  def fromJson(j: Json): McpContentBlock =
    JsonView.obj(j, "resource") match
      case Some(res) =>
        McpContentBlock(
          kind = "resource",
          text = JsonView.str(res, "text"),
          mimeType = JsonView.str(res, "mimeType"),
          uri = JsonView.str(res, "uri")
        )
      case None =>
        val text = JsonView.str(j, "text")
        val kind = JsonView.str(j, "type").getOrElse {
          if text.isDefined then "text"
          else if JsonView.field(j, "blob").isDefined then "blob"
          else "unknown"
        }
        McpContentBlock(
          kind = kind,
          text = text,
          mimeType = JsonView.str(j, "mimeType"),
          uri = JsonView.str(j, "uri")
        )

/** The result of `tools/call`: content blocks plus the tool-level `isError`
  * flag (an error the model should see and reason about, NOT a protocol
  * failure — those surface as `Left`). */
final case class McpCallResult(content: List[McpContentBlock], isError: Boolean):
  def toJson: Json = Json.Obj(
    List(
      "content" -> Json.Arr(content.map(_.toJson)),
      "isError" -> Json.Bool(isError)
    )
  )

object McpCallResult:
  def decode(result: Json): McpCallResult =
    McpCallResult(
      content = JsonView.arr(result, "content").getOrElse(Nil).map(McpContentBlock.fromJson),
      isError = JsonView.bool(result, "isError").getOrElse(false)
    )

/** The result of `resources/read`: one or more content blocks. */
final case class McpReadResult(contents: List[McpContentBlock]):
  def toJson: Json = Json.Obj(List("contents" -> Json.Arr(contents.map(_.toJson))))

object McpReadResult:
  def decode(result: Json): McpReadResult =
    McpReadResult(
      contents = JsonView.arr(result, "contents").getOrElse(Nil).map(McpContentBlock.fromJson)
    )

/** Small typed accessors over the [[Json]] AST, shared by the decoders above.
  * They turn "field is present and of the right kind" into an `Option`, so a
  * misshapen or absent field is simply `None` rather than a match error. */
private[mcp] object JsonView:
  def field(j: Json, key: String): Option[Json] = j match
    case o: Json.Obj => o.get(key)
    case _           => None

  def str(j: Json, key: String): Option[String] =
    field(j, key).collect { case Json.Str(s) => s }

  def bool(j: Json, key: String): Option[Boolean] =
    field(j, key).collect { case Json.Bool(b) => b }

  def arr(j: Json, key: String): Option[List[Json]] =
    field(j, key).collect { case Json.Arr(es) => es }

  def obj(j: Json, key: String): Option[Json.Obj] =
    field(j, key).collect { case o: Json.Obj => o }

  /** Encode an optional string as a JSON string or explicit `null`. */
  def strOrNull(s: Option[String]): Json = s.map(Json.Str(_)).getOrElse(Json.Null)
