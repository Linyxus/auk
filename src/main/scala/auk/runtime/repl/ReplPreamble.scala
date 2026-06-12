package auk.runtime.repl

/** The Scala source evaluated as the first entry of every REPL session.
  *
  * It sets the stage for evaluated code: imports the preloaded runtime library
  * (`library.bin`, see [[auk.platform.js.ReplArtifacts]]) and binds the
  * [[auk.library.AukInterface]] instance that code calls as `lib`. The same
  * text is shown verbatim in the agent's system prompt
  * ([[auk.agent.SystemPrompt]]), so the model knows exactly what is already in
  * scope — keep the two in sync by construction: this constant is the single
  * source of truth.
  *
  * Loaded by [[ScalaRepl]] on every fresh worker spawn (first use and every
  * respawn after a crash or timeout), so `lib` survives session restarts even
  * though user definitions do not.
  */
object ReplPreamble:
  val Source: String =
    """import auk.library.*
      |val lib: AukInterface = new AukImpl()""".stripMargin
