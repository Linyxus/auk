package auk.llm.tools

import gears.async.Async

/** A model-callable tool: a name, a description, and a typed handler.
  *
  * The argument type carries its own JSON [[Schema]] and decoder through
  * [[ToolInput]], so a concrete tool only has to name itself, describe what it
  * does, and say what to do once the arguments have been decoded. Schema
  * generation and argument parsing come for free.
  *
  * [[execute]] runs under `Async`, so a tool can perform cancellable I/O
  * (spawn a process, read a file, await a network call) and be interrupted when
  * the surrounding turn is cancelled. It receives a [[RuntimeContext]] for the
  * working directory and approval policy, and returns a structured
  * [[ToolResult]] that distinguishes success from failure rather than encoding
  * errors in free text.
  */
trait Tool:
  /** The decoded shape of this tool's arguments. */
  type Params

  /** The name the model uses to invoke the tool. */
  def name: String

  /** A human/model readable description of what the tool does. */
  def description: String

  /** Schema and decoder for [[Params]]. */
  def input: ToolInput[Params]

  /** Run the tool against already-decoded arguments.
    *
    * Implementations should turn expected failures into
    * [[ToolResult.error]] rather than throwing; the dispatcher catches stray
    * exceptions defensively, but a clean error gives the model a better message.
    */
  def execute(params: Params)(using RuntimeContext, Async): ToolResult

  /** The JSON schema advertised to the model for this tool's arguments. */
  final def parametersSchema: Json = input.schemaJson

  /** A pre-built JSON Schema to advertise verbatim, bypassing the flattening
    * conversion in [[auk.runtime.ToolBridge]].
    *
    * That conversion is lossy for nested shapes (it keeps only top-level
    * properties and one level of array `items`), which is fine for the flat
    * parameter objects native tools use but would corrupt a server-authored MCP
    * schema. A tool whose schema must reach the endpoint intact overrides this
    * with `Some(schema)`; the default `None` keeps the derived-and-flattened path.
    */
  def rawParametersSchema: Option[Json] = None

  /** Decode the raw JSON `arguments` sent by the model and run the tool.
    *
    * Decoding failures are returned as an error [[ToolResult]] rather than
    * thrown, so they can be fed straight back to the model as a tool result.
    */
  final def call(arguments: String)(using RuntimeContext, Async): ToolResult =
    input.parse(arguments) match
      case Left(err) =>
        ToolResult.error(s"invalid arguments for '$name': ${err.render}")
      case Right(params) => execute(params)
