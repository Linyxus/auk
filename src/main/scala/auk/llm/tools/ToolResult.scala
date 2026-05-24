package auk.llm.tools

/** The outcome of running a [[Tool]].
  *
  * `output` is the text handed back to the model as the tool result. `isError`
  * distinguishes a failed call (bad arguments, the action could not be
  * completed) from a successful one; it maps directly onto the `isError` flag of
  * `auk.llm.endpoint.Content.ToolResult`, so the model can tell the difference.
  * `metadata` carries structured side-channel information that is *not* shown to
  * the model (e.g. a shell exit code, the number of bytes read) for logging,
  * telemetry, or the TUI.
  */
final case class ToolResult(
    output: String,
    isError: Boolean = false,
    metadata: Map[String, String] = Map.empty
)

object ToolResult:
  /** A successful result carrying `output` text. */
  def ok(output: String, metadata: Map[String, String] = Map.empty): ToolResult =
    ToolResult(output, isError = false, metadata)

  /** A failed result whose `message` is fed back to the model as an error. */
  def error(message: String, metadata: Map[String, String] = Map.empty): ToolResult =
    ToolResult(message, isError = true, metadata)
