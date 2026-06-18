package auk.agent

/** A pending input to the agent's conversation — the "inbox" alphabet.
  *
  * Both kinds flow from producers (the TUI for user input, harness/automation
  * code for notices) to the [[Engine]] over a single channel, and the engine
  * incorporates them at the earliest safe point: it wakes an idle agent into a
  * new turn, or drains them into a running turn at the next tool-calling round
  * boundary. Distinct from [[UserCommand]], which is the control plane (session
  * and model operations); this is the conversation plane.
  *
  * `text` is the carried text, common to both kinds.
  */
enum Inbox(val text: String):
  /** A line the user submitted, to be acted on as conversation input. */
  case UserMessage(message: String) extends Inbox(message)

  /** An out-of-band notification from the harness (a hook, a watcher, an
    * automation layer). Folded into the conversation as a user-role message
    * wrapped in `<system-reminder>` tags so the model reads it as ambient
    * context, not as the user speaking. */
  case SystemNotice(notice: String) extends Inbox(notice)
