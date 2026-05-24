package auk.agent

/** Commands flowing from the UI to the [[Engine]] — the TUI's output alphabet. */
enum UserCommand:
  /** Submit a line of user input for the agent to act on. */
  case Submit(text: String)

  /** Interrupt the in-flight turn. Forward-compat; the echo Engine ignores it. */
  case Interrupt
