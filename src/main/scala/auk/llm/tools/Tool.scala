package auk.llm.tools

/** A model-callable tool: a name, a description, and a typed handler.
  *
  * The argument type carries its own JSON [[Schema]] and decoder through
  * [[ToolInput]], so a concrete tool only has to name itself, describe what it
  * does, and say what to do once the arguments have been decoded. Schema
  * generation and argument parsing come for free.
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

  /** Run the tool against already-decoded arguments, returning the result text
    * to hand back to the model.
    */
  def execute(params: Params): String

  /** The JSON schema advertised to the model for this tool's arguments. */
  final def parametersSchema: Json = input.schemaJson

  /** Decode the raw JSON `arguments` sent by the model and run the tool.
    *
    * Decoding failures are returned as an error string rather than thrown, so
    * they can be fed straight back to the model as a tool result.
    */
  final def call(arguments: String): String =
    input.parse(arguments) match
      case Left(err)     => s"Error: invalid arguments for '$name': ${err.render}"
      case Right(params) => execute(params)
