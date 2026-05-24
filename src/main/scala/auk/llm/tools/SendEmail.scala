package auk.llm.tools

/** Arguments for the [[SendEmail]] tool.
  *
  * `derives ToolInput` generates the JSON schema and decoder from these fields;
  * the `@desc` annotations become the `description` of each schema property,
  * and `cc` being an `Option` makes it the only non-required field.
  */
case class SendEmailParams(
    @desc("Recipient email address, e.g. \"alice@example.com\"")
    to: String,
    @desc("Subject line of the email")
    subject: String,
    @desc("Plain-text body of the email")
    body: String,
    @desc("Optional list of additional CC email addresses")
    cc: Option[List[String]] = None
) derives ToolInput

/** A minimal email-sending tool.
  *
  * Delivery is stubbed: [[execute]] formats the message and reports success.
  * Hand `params` to a real transport (SMTP, a mail API client, ...) where the
  * TODO is marked to make it actually send.
  */
object SendEmail extends Tool:
  type Params = SendEmailParams

  val name = "send_email"

  val description =
    "Send an email to a recipient. Provide the recipient address, a subject " +
      "line, and the plain-text body; CC recipients are optional."

  val input: ToolInput[SendEmailParams] = ToolInput[SendEmailParams]

  def execute(params: SendEmailParams): String =
    // TODO: hand `params` to a real mail transport here.
    val ccNote = params.cc.filter(_.nonEmpty) match
      case Some(addrs) => s", cc ${addrs.mkString(", ")}"
      case None        => ""
    s"Email sent to ${params.to}$ccNote (subject: \"${params.subject}\")."
