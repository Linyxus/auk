package auk.session

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import auk.llm.endpoint.{Content, Message, Role}

class SessionSuite extends munit.FunSuite:

  private def tempDir(): Path = Files.createTempDirectory("auk-session").nn

  private def provider(dir: Path): SessionProvider =
    SessionProvider.directory(dir.resolve(SessionProvider.RelativePath).nn)

  private val sampleEvents = List(
    SessionEvent.UserSubmitted("hello"),
    SessionEvent.AssistantResponded(
      Message(Role.Assistant, List(
        Content.Thinking("let me check"),
        Content.Text("on it"),
        Content.ToolUse("call_1", "read", """{"path":"a.txt"}""")
      ))
    ),
    SessionEvent.ToolResultsReceived(List(
      Content.ToolResult("call_1", "file contents", isError = false)
    )),
    SessionEvent.AssistantResponded(Message.assistant("done"))
  )

  test("each event round-trips through JSON encode/decode"):
    sampleEvents.foreach: ev =>
      val line = SessionEvent.encode(ev)
      assertEquals(SessionEvent.decode(line), Right(ev))

  test("reading an empty (never-appended) session yields no events"):
    val p = provider(tempDir())
    val s = p.create().toOption.get
    assertEquals(s.events, Right(Nil))

  test("appended events read back in order"):
    val s = provider(tempDir()).create().toOption.get
    sampleEvents.foreach(ev => assertEquals(s.append(ev), Right(())))
    assertEquals(s.events, Right(sampleEvents))

  test("history folds events into the conversation the model sees"):
    val s = provider(tempDir()).create().toOption.get
    sampleEvents.foreach(s.append)
    val expected = List(
      Message.user("hello"),
      sampleEvents(1).asInstanceOf[SessionEvent.AssistantResponded].message,
      Message(Role.User, List(Content.ToolResult("call_1", "file contents"))),
      Message.assistant("done")
    )
    assertEquals(s.history, Right(expected))

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

  test("a corrupt log line surfaces an error with its position"):
    val dir = tempDir()
    val s = provider(dir).create().toOption.get
    s.append(SessionEvent.UserSubmitted("ok"))
    // Append a torn/invalid line directly to the underlying file.
    val logFile = dir.resolve(SessionProvider.RelativePath).nn.resolve(s.id + ".jsonl").nn
    Files.writeString(logFile, "{ not json\n", UTF_8, java.nio.file.StandardOpenOption.APPEND)
    val r = s.events
    assert(r.isLeft, s"expected a corruption error, got $r")
    assert(r.left.toOption.get.contains("corrupt"))
    assert(r.left.toOption.get.contains("line 2"))
