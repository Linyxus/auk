package auk.cli

import scala.io.StdIn
import gears.async.Async
import gears.async.default.{*, given}

import auk.llm.endpoint.*
import auk.llm.tools.{SendEmail, RuntimeContext}
import auk.runtime.ToolRegistry

/** A bare-bones interactive chat loop for exercising the LLM clients.
  *
  * This is a manual smoke test, not part of the agent: it wires an endpoint to
  * stdin/stdout and streams the model's reasoning and answer back token by
  * token. It also registers the [[SendEmail]] tool, so a turn can fan out into
  * tool calls that are executed locally and fed back to the model until it
  * produces a final answer.
  *
  * Run with `OPENROUTER_API_KEY` set:
  * {{{
  *   sbt "runMain auk.cli.chat"
  * }}}
  */
object ChatLoop:

  private val Model = "deepseek/deepseek-v4-flash"

  /** The tool-calling runtime: which tools the model may call, and how calls
    * are advertised and dispatched. */
  private val registry: ToolRegistry = ToolRegistry.of(SendEmail)

  /** Where tools run and under what authority. The smoke loop trusts the model,
    * so it auto-approves and roots tools at the process working directory. */
  private given RuntimeContext = RuntimeContext.cwd()

  /** Cap on consecutive tool rounds in a single turn, to bound runaway loops. */
  private val MaxToolRounds = 8

  // ANSI helpers — thinking is dimmed so it reads as scaffolding, not answer.
  private val Reset = "[0m"
  private val Dim = "[2m"
  private val Cyan = "[36m"
  private val Green = "[32m"

  /** Which kind of delta we last printed, so we know when to print a header and
    * separate the reasoning, answer, and tool-call streams. */
  private enum Section:
    case None, Thinking, Answer, Tool

  def run(): Unit =
    val endpoint = OpenRouterEndpoint.createFromEnv()
    val config = LLMConfig(
      model = Model,
      // Turn reasoning on. Auto maps to a medium reasoning effort, which
      // OpenRouter forwards to the model.
      thinking = Some(ThinkingMode.Auto),
      tools = registry.schemas
    )

    println(s"${Cyan}auk chat loop${Reset} ${Dim}· model=$Model · thinking=on · tools=${registry.tools.map(_.name).mkString(",")}${Reset}")
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
          history = converse(endpoint, config, history :+ Message.user(line))

    println(s"\n${Dim}bye${Reset}")

  /** Drive a single user turn to completion: stream the assistant's reply, and
    * while it asks for tools, run them, feed the results back, and stream again.
    * Returns the conversation grown with every message exchanged. */
  private def converse(
      endpoint: Endpoint,
      config: LLMConfig,
      initial: List[Message]
  )(using Async.Spawn): List[Message] =
    var messages = initial
    var round = 0
    var turning = true
    while turning do
      streamTurn(endpoint, config, messages) match
        case None =>
          // Request failed; error already printed by streamTurn.
          turning = false
        case Some(assistantMsg) =>
          messages = messages :+ assistantMsg
          val toolUses = assistantMsg.content.collect {
            case t: Content.ToolUse => t
          }
          round += 1
          if toolUses.isEmpty then turning = false
          else if round >= MaxToolRounds then
            println(s"${Dim}[stopped after $MaxToolRounds tool rounds]${Reset}")
            turning = false
          else
            // The runtime decodes arguments, runs each tool, and packages the
            // results (with their isError flags) back into content blocks.
            val results = registry.runToolCalls(toolUses)
            results.foreach(r => println(s"${Dim}  ↳ ${r.content}${Reset}"))
            messages = messages :+ Message(Role.User, results)
    messages

  /** Stream one assistant turn, rendering reasoning, answer, and tool calls as
    * they arrive. Returns the assembled assistant message (which may contain
    * `ToolUse` content), or None if the request failed. */
  private def streamTurn(
      endpoint: Endpoint,
      config: LLMConfig,
      messages: List[Message]
  )(using Async.Spawn): Option[Message] =
    val ch = endpoint.stream(messages, config)
    var section = Section.None
    var result: Option[Message] = None
    var streaming = true

    // Emit the trailing characters for whatever section is currently open, so
    // the next header (or the final newline) starts cleanly.
    def closeSection(): Unit =
      section match
        case Section.Thinking => print(Reset)
        case Section.Tool     => print(")")
        case _                => ()

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
                closeSection()
                print(s"\n${Dim}thinking ▸ ")
                section = Section.Thinking
              print(text)
              Console.out.flush()

            case StreamEvent.Delta(text) =>
              if section != Section.Answer then
                closeSection()
                print(s"\n${Green}auk ▸${Reset} ")
                section = Section.Answer
              print(text)
              Console.out.flush()

            case StreamEvent.ToolCallStart(_, _, name) =>
              closeSection()
              print(s"\n${Cyan}tool ▸${Reset} $name(")
              section = Section.Tool
              Console.out.flush()

            case StreamEvent.ToolCallDelta(_, argumentDelta) =>
              print(argumentDelta)
              Console.out.flush()

            case StreamEvent.Done(response) =>
              closeSection()
              println()
              result = Some(response.message)
              response.usage.foreach: u =>
                println(
                  s"${Dim}[${u.inputTokens} in / ${u.outputTokens} out tokens]${Reset}"
                )
              streaming = false

            // Tool-run progress events are injected by the agent loop, not the
            // endpoint, so the smoke loop never sees them.
            case StreamEvent.ToolRunStart(_, _) | StreamEvent.ToolRunEnd(_, _, _) => ()

    result

@main def chat(): Unit = ChatLoop.run()
