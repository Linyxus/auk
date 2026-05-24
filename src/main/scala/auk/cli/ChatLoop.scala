package auk.cli

import scala.io.StdIn
import gears.async.Async
import gears.async.default.{*, given}

import auk.llm.endpoint.*

/** A bare-bones interactive chat loop for exercising the LLM clients.
  *
  * This is a manual smoke test, not part of the agent: it wires an endpoint to
  * stdin/stdout and streams the model's reasoning and answer back token by
  * token. Tool calling is intentionally out of scope.
  *
  * Run with `OPENROUTER_API_KEY` set:
  * {{{
  *   sbt "runMain auk.cli.chat"
  * }}}
  */
object ChatLoop:

  private val Model = "deepseek/deepseek-v4-flash"

  // ANSI helpers — thinking is dimmed so it reads as scaffolding, not answer.
  private val Reset = "[0m"
  private val Dim = "[2m"
  private val Cyan = "[36m"
  private val Green = "[32m"

  /** Which kind of delta we last printed, so we know when to print a header and
    * separate the reasoning stream from the answer stream. */
  private enum Section:
    case None, Thinking, Answer

  def run(): Unit =
    val endpoint = OpenRouterEndpoint.createFromEnv()
    val config = LLMConfig(
      model = Model,
      // Turn reasoning on. Auto maps to a medium reasoning effort, which
      // OpenRouter forwards to the model.
      thinking = Some(ThinkingMode.Auto)
    )

    println(s"${Cyan}auk chat loop${Reset} ${Dim}· model=$Model · thinking=on${Reset}")
    println(s"${Dim}Type a message and press Enter. Ctrl-D or 'exit' to quit.${Reset}\n")

    // Full conversation, grown after each completed turn so the model has
    // context across messages.
    var history = List.empty[Message]

    Async.blocking:
      var running = true
      while running do
        print(s"${Cyan}you ▸${Reset} ")
        Console.out.flush()
        val line = StdIn.readLine()
        if line == null || line.trim == "exit" || line.trim == "quit" then
          running = false
        else if line.trim.isEmpty then ()
        else
          history = history :+ Message.user(line)
          streamTurn(endpoint, config, history).foreach: reply =>
            history = history :+ reply

    println(s"\n${Dim}bye${Reset}")

  /** Stream one assistant turn, rendering reasoning and answer as they arrive.
    * Returns the assembled assistant message to append to history, or None if
    * the request failed. */
  private def streamTurn(
      endpoint: Endpoint,
      config: LLMConfig,
      messages: List[Message]
  )(using Async.Spawn): Option[Message] =
    val ch = endpoint.stream(messages, config)
    var section = Section.None
    var result: Option[Message] = None
    var streaming = true

    while streaming do
      ch.read() match
        case Left(_) =>
          // Channel closed without a Done event.
          streaming = false

        case Right(Left(err)) =>
          println(s"\n${Dim}[error] ${err.description}${Reset}")
          streaming = false

        case Right(Right(event)) =>
          event match
            case StreamEvent.ThinkingDelta(text) =>
              if section != Section.Thinking then
                print(s"\n${Dim}thinking ▸ ")
                section = Section.Thinking
              print(text)
              Console.out.flush()

            case StreamEvent.Delta(text) =>
              if section != Section.Answer then
                // Close the dimmed reasoning block before the answer.
                if section == Section.Thinking then print(Reset)
                print(s"\n${Green}auk ▸${Reset} ")
                section = Section.Answer
              print(text)
              Console.out.flush()

            case StreamEvent.Done(response) =>
              if section == Section.Thinking then print(Reset)
              println()
              result = Some(response.message)
              response.usage.foreach: u =>
                println(
                  s"${Dim}[${u.inputTokens} in / ${u.outputTokens} out tokens]${Reset}"
                )
              streaming = false

            case _ =>
              // Tool-call events are not expected in this loop.
              ()

    result

@main def chat(): Unit = ChatLoop.run()
