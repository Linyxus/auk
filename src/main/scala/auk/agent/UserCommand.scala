package auk.agent

/** Commands flowing from the UI to the [[Engine]] — the TUI's output alphabet. */
enum UserCommand:
  /** Submit a line of user input for the agent to act on. */
  case Submit(text: String)

  /** Ask the engine for resumable sessions, newest first. */
  case ListSessions

  /** Switch the active engine history to an existing session. */
  case ResumeSession(id: String)

  /** Switch the active engine history to a fresh session. */
  case NewSession

  /** Switch the active model to `(providerName, modelId)` for the rest of this
    * instance (and persist the choice). */
  case SwitchModel(providerName: String, modelId: String)
