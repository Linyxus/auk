package auk.session

import auk.llm.endpoint.{Content, Message}

/** Lightweight metadata for presenting a durable session in the TUI. */
final case class SessionSummary(
    id: String,
    modifiedAtMs: Option[Long],
    messageCount: Int,
    preview: String
)

/** A reopened or freshly created session, including the durable events needed
  * to rebuild the model and UI histories. */
final case class SessionSnapshot(
    summary: SessionSummary,
    events: List[SessionEvent]
)

object SessionSummary:
  def from(id: String, modifiedAtMs: Option[Long], events: List[SessionEvent]): SessionSummary =
    SessionSummary(
      id = id,
      modifiedAtMs = modifiedAtMs,
      messageCount = events.count {
        case _: SessionEvent.UserSubmitted      => true
        case _: SessionEvent.AssistantResponded => true
        case _                                  => false
      },
      preview = preview(events)
    )

  private def preview(events: List[SessionEvent]): String =
    val latestUser = events.reverseIterator.collectFirst:
      case SessionEvent.UserSubmitted(text) if text.trim.nonEmpty => text
    val latestAssistant = events.reverseIterator.collectFirst:
      case SessionEvent.AssistantResponded(message) if messageText(message).trim.nonEmpty =>
        messageText(message)
    latestUser.orElse(latestAssistant).map(truncate(_, 48)).getOrElse("Empty session")

  private def messageText(message: Message): String =
    message.content.collect:
      case Content.Text(text)        => text
      case Content.Thinking(text, _) => text
    .mkString(" ")
    .replaceAll("\\s+", " ")
    .trim

  private def truncate(text: String, max: Int): String =
    val normalized = text.replaceAll("\\s+", " ").trim
    if normalized.length <= max then normalized
    else normalized.take(max - 1) + "…"
