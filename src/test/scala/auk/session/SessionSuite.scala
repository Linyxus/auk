package auk.session

import auk.TestFs
import auk.llm.endpoint.{ChatResponse, Content, FinishReason, Message, ReasoningBlock, Role, Usage}

class SessionSuite extends munit.FunSuite:

  private def tempDir(): String = TestFs.tempDir("auk-session")

  private def provider(dir: String): SessionProvider =
    SessionProvider.directory(TestFs.join(dir, SessionProvider.RelativePath))

  private def responded(
      message: Message,
      finishReason: FinishReason = FinishReason.Stop,
      usage: Option[Usage] = None
  ): SessionEvent =
    SessionEvent.AssistantResponded(ChatResponse(message, finishReason, usage))

  private val sampleEvents = List(
    SessionEvent.UserSubmitted("hello"),
    responded(
      Message(Role.Assistant, List(
        Content.Thinking("let me check"),
        Content.Text("on it"),
        Content.ToolUse("call_1", "read", """{"path":"a.txt"}""")
      )),
      finishReason = FinishReason.ToolUse,
      usage = Some(Usage(inputTokens = 1200, outputTokens = 340))
    ),
    SessionEvent.ToolResultsReceived(List(
      Content.ToolResult("call_1", "file contents", isError = false)
    )),
    SessionEvent.SystemNotice("the background build finished"),
    responded(Message.assistant("done"), usage = Some(Usage(2000, 90))),
    SessionEvent.Interrupted
  )

  test("each event round-trips through JSON encode/decode"):
    sampleEvents.foreach: ev =>
      val line = SessionEvent.encode(ev)
      assertEquals(SessionEvent.decode(line), Right(ev))

  test("a thinking block round-trips its signature, and a redacted block its data"):
    val ev = responded(
      Message(Role.Assistant, List(
        Content.Thinking("reasoning", Some("sig-xyz")),
        Content.RedactedThinking("encrypted-blob"),
        Content.ToolUse("t1", "read", "{}")
      )),
      finishReason = FinishReason.ToolUse
    )
    assertEquals(SessionEvent.decode(SessionEvent.encode(ev)), Right(ev))

  test("a reasoning block round-trips its blocks and fields"):
    val ev = responded(
      Message(Role.Assistant, List(
        Content.Reasoning(List(
          ReasoningBlock("reasoning.text", text = Some("ponder"), signature = Some("sig-1"),
            format = Some("anthropic-claude-v1"), id = Some("r1"), index = Some(0)),
          ReasoningBlock("reasoning.encrypted", data = Some("BLOB"), index = Some(1)),
          ReasoningBlock("reasoning.summary", summary = Some("gist"))
        )),
        Content.ToolUse("t1", "read", "{}")
      )),
      finishReason = FinishReason.ToolUse
    )
    assertEquals(SessionEvent.decode(SessionEvent.encode(ev)), Right(ev))

  test("the finish reason and token usage survive a round-trip"):
    val ev = responded(Message.assistant("hi"), FinishReason.MaxTokens, Some(Usage(123, 45)))
    SessionEvent.decode(SessionEvent.encode(ev)) match
      case Right(SessionEvent.AssistantResponded(r, _)) =>
        assertEquals(r.finishReason, FinishReason.MaxTokens)
        assertEquals(r.usage, Some(Usage(123, 45)))
      case other => fail(s"expected an AssistantResponded, got $other")

  test("the model that produced a reply survives a round-trip"):
    val ev = SessionEvent.AssistantResponded(
      ChatResponse(Message.assistant("hi"), FinishReason.Stop, Some(Usage(10, 5))),
      Some(ModelInfo("zai", "glm-5.2", "GLM 5.2", 128000))
    )
    assertEquals(SessionEvent.decode(SessionEvent.encode(ev)), Right(ev))

  test("a context compaction checkpoint survives a round-trip"):
    val ev = SessionEvent.ContextCompacted(
      "## Current Goal\nKeep going",
      Some(ModelInfo("zai", "glm-5.2", "GLM 5.2", 1_000_000)),
      Some(48_000L)
    )
    assertEquals(SessionEvent.decode(SessionEvent.encode(ev)), Right(ev))

  test("an old context compaction line without an estimate decodes leniently"):
    // Logs written before `estimatedTokens` existed must still decode — to None,
    // so resume falls back to estimating the summary text.
    val line = """{"type":"context_compacted","summary":"older checkpoint"}"""
    assertEquals(
      SessionEvent.decode(line),
      Right(SessionEvent.ContextCompacted("older checkpoint", None, None))
    )

  test("modelContextEvents starts at the latest context compaction checkpoint"):
    val first = SessionEvent.ContextCompacted("first")
    val second = SessionEvent.ContextCompacted("second")
    val after = SessionEvent.UserSubmitted("after")
    val events = List(
      SessionEvent.UserSubmitted("before"),
      first,
      SessionEvent.UserSubmitted("middle"),
      second,
      after
    )
    assertEquals(SessionEvent.modelContextEvents(events), List(second, after))

  test("per-tool execution metadata round-trips, keyed by tool-use id"):
    val ev = SessionEvent.ToolResultsReceived(
      List(Content.ToolResult("call_1", "done", isError = false)),
      Map("call_1" -> Map("rounds" -> "3", "inputTokens" -> "1200", "outputTokens" -> "340"))
    )
    assertEquals(SessionEvent.decode(SessionEvent.encode(ev)), Right(ev))

  test("a tool_results line without metadata decodes to empty metadata (back-compat)"):
    val line = """{"type":"tool_results_received","results":[{"kind":"tool_result","toolUseId":"c1","content":"x","isError":false}]}"""
    assertEquals(
      SessionEvent.decode(line),
      Right(SessionEvent.ToolResultsReceived(List(Content.ToolResult("c1", "x", isError = false))))
    )

  test("a pre-ChatResponse line decodes with a default finish reason and no usage (back-compat)"):
    // Old logs stored only the message, with no finishReason/usage fields.
    val line = """{"type":"assistant_responded","message":{"role":"assistant","content":[{"kind":"thinking","text":"r"}]}}"""
    assertEquals(
      SessionEvent.decode(line),
      Right(SessionEvent.AssistantResponded(
        ChatResponse(Message(Role.Assistant, List(Content.Thinking("r", None))), FinishReason.Stop, None)
      ))
    )

  test("reading an empty (never-appended) session yields no events"):
    val p = provider(tempDir())
    val s = p.create().toOption.get
    assertEquals(s.events, Right(Nil))

  test("appended events read back in order"):
    val s = provider(tempDir()).create().toOption.get
    sampleEvents.foreach(ev => assertEquals(s.append(ev), Right(())))
    assertEquals(s.events, Right(sampleEvents))

  test("a session persists across reopen by the provider"):
    val dir = tempDir()
    val id = locally:
      val s = provider(dir).create().toOption.get
      sampleEvents.foreach(s.append)
      s.id
    // A fresh provider over the same directory reopens the same log.
    val reopened = provider(dir).open(id)
    assert(reopened.toOption.flatten.isDefined)
    assertEquals(reopened.toOption.flatten.get.events, Right(sampleEvents))

  test("opening an unknown id returns None, not an error"):
    assertEquals(provider(tempDir()).open("nope"), Right(None))

  test("list reports created sessions and latest resumes one of them"):
    val dir = tempDir()
    val p = provider(dir)
    val a = p.create().toOption.get
    a.append(SessionEvent.UserSubmitted("a"))
    val b = p.create().toOption.get
    b.append(SessionEvent.UserSubmitted("b"))
    val ids = p.list().toOption.get
    assertEquals(ids.toSet, Set(a.id, b.id))
    assert(p.latest().toOption.flatten.isDefined)

  test("summaries include preview, message count, and modified time"):
    val dir = tempDir()
    val p = provider(dir)
    val s = p.create().toOption.get
    s.append(SessionEvent.UserSubmitted("how do I resume this?"))
    s.append(responded(Message.assistant("like this")))

    val summaries = p.summaries().toOption.get
    val summary = summaries.find(_.id == s.id).get
    assertEquals(summary.preview, "how do I resume this?")
    assertEquals(summary.messageCount, 2)
    assert(summary.modifiedAtMs.isDefined)

  test("a corrupt log line surfaces an error with its position"):
    val dir = tempDir()
    val s = provider(dir).create().toOption.get
    s.append(SessionEvent.UserSubmitted("ok"))
    // Append a torn/invalid line directly to the underlying file.
    val logFile = TestFs.join(TestFs.join(dir, SessionProvider.RelativePath), s.id + ".jsonl")
    TestFs.append(logFile, "{ not json\n")
    val r = s.events
    assert(r.isLeft, s"expected a corruption error, got $r")
    assert(r.left.toOption.get.contains("corrupt"))
    assert(r.left.toOption.get.contains("line 2"))
