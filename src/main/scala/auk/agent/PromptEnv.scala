package auk.agent

import auk.platform.{FileSystem, Process, Platform}

/** The live-session facts a [[DynamicSection]] may read while assembling the
  * system prompt.
  *
  * Passed explicitly rather than reached for through process globals, so a
  * dynamic section stays a pure function of its inputs: a test constructs a
  * [[PromptEnv]] pointing at a fixture directory (with fake [[fs]]/[[process]]
  * if it likes) and asserts on the rendered body. The platform seams default to
  * the live ones so production callers only supply what is genuinely per-session.
  */
final case class PromptEnv(
    workingDirectory: String,
    modelName: String,
    today: String,
    fs: FileSystem = Platform.fs,
    process: Process = Platform.process
)
