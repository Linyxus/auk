package auk.agent

/** Control-plane commands flowing from the UI to the [[Engine]] — session and
  * model operations. Conversation input (user messages, system notices) travels
  * separately, on the [[Inbox]] channel, so it can be queued and steered into a
  * running turn; this channel carries only between-turn control. */
enum UserCommand:
  /** Ask the engine for resumable sessions, newest first. */
  case ListSessions

  /** Switch the active engine history to an existing session. */
  case ResumeSession(id: String)

  /** Switch the active engine history to a fresh session. */
  case NewSession

  /** Switch the active model to `(providerName, modelId)` for the rest of this
    * instance (and persist the choice). */
  case SwitchModel(providerName: String, modelId: String)

  /** Compact the current model-facing context into a durable checkpoint.
    * `requestedAtMs` lets the engine drop duplicate compaction requests that were
    * queued while an earlier compaction was still running. */
  case CompactContext(requestedAtMs: Long)

  /** Pause (kill) a running workflow; finished sub-agents stay cached. */
  case PauseWorkflow(runId: String)

  /** Resume a paused workflow: re-run it, reusing cached sub-agent outputs. */
  case ResumeWorkflow(runId: String)
