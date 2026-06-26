package auk.library

import scala.scalajs.js

import SessionHistoryImpl.*

/** [[SessionHistory]] / `SessionHistoryImpl` — a read-only view over the JSONL
  * session logs under `.auk/sessions`. Exercised against a real Node file system
  * via a per-test temp dir (see [[LibSuite]]); the pure helpers are tested directly.
  * Test JSONL is built with `js.JSON.stringify` so it matches the host's encoding
  * (`auk.session.SessionEvent`) exactly. */
class SessionHistorySuite extends LibSuite:

  /** A fixed "now", so relative-time assertions are deterministic. */
  private val Now = 1_700_000_000_000L

  private def nodeFs: js.Dynamic = js.Dynamic.global.require("node:fs")

  /** A history store rooted at a fresh `sessions/` under the test's temp dir. */
  private def store(d: FsDir): SessionHistoryImpl =
    new SessionHistoryImpl((d.path / "sessions").toString, () => Now)

  /** Write `<id>.jsonl` with the given lines and set its mtime (for ordering). */
  private def writeSession(d: FsDir, id: String, mtimeMs: Long, lines: String*): Unit =
    val dir = d.path / "sessions"
    nodeFs.mkdirSync(dir.toString, js.Dynamic.literal(recursive = true))
    val p = dir / s"$id.jsonl"
    nodeFs.writeFileSync(p.toString, lines.mkString("\n") + "\n", "utf8")
    setMtime(p, mtimeMs.toDouble)

  // -- JSONL builders (faithful to auk.session.SessionEvent's encoding) ---------

  private def userLine(text: String): String =
    js.JSON.stringify(js.Dynamic.literal(`type` = "user_submitted", text = text))

  private def noticeLine(text: String): String =
    js.JSON.stringify(js.Dynamic.literal(`type` = "system_notice", text = text))

  private val interruptedLine: String =
    js.JSON.stringify(js.Dynamic.literal(`type` = "interrupted"))

  private def assistantLine(blocks: js.Any*): String =
    js.JSON.stringify(js.Dynamic.literal(
      `type` = "assistant_responded",
      message = js.Dynamic.literal(role = "assistant", content = js.Array(blocks*))
    ))

  private def text(t: String): js.Any = js.Dynamic.literal(kind = "text", text = t)
  private def thinking(t: String): js.Any = js.Dynamic.literal(kind = "thinking", text = t)
  private def toolUse(id: String, name: String, input: String): js.Any =
    js.Dynamic.literal(kind = "tool_use", id = id, name = name, input = input)

  private def resultsLine(rows: (String, String, Boolean)*): String =
    js.JSON.stringify(js.Dynamic.literal(
      `type` = "tool_results_received",
      results = js.Array(rows.map((tid, c, e) =>
        js.Dynamic.literal(kind = "tool_result", toolUseId = tid, content = c, isError = e).asInstanceOf[js.Any]
      )*)
    ))

  // -- round-trip via real JSONL on disk ---------------------------------------

  tmp.test("a user→assistant(text+tool)→results log reconstructs paired messages"): d =>
    writeSession(d, "sess-1", Now - 3600_000,
      userLine("what does the build do?"),
      assistantLine(thinking("check build.sbt"), toolUse("t1", "eval_scala", "lib.fs.read(\"build.sbt\")"), text("It builds three subprojects.")),
      resultsLine(("t1", "ThisBuild / scalaVersion := \"3.8.3\"", false))
    )
    val s = store(d).get("sess-1").getOrElse(fail("expected the session to load"))
    assertEquals(s.id, "sess-1")
    assertEquals(s.messageCount, 2)
    assertEquals(s.messages.map(_.role), List("user", "assistant"))
    val user = s.messages.head
    assertEquals(user.text, "what does the build do?")
    val asst = s.messages(1)
    assertEquals(asst.text, "It builds three subprojects.")
    assertEquals(asst.reasoning, "check build.sbt")
    assertEquals(
      asst.toolCalls.map(c => (c.name, c.arguments, c.output, c.isError)),
      List(("eval_scala", "lib.fs.read(\"build.sbt\")", "ThisBuild / scalaVersion := \"3.8.3\"", false))
    )
    assertEquals(s.preview, "It builds three subprojects.")

  tmp.test("an error tool result is surfaced with isError"): d =>
    writeSession(d, "e1", Now,
      userLine("run it"),
      assistantLine(toolUse("t9", "eval_scala", "boom")),
      resultsLine(("t9", "java.lang.RuntimeException: boom", true))
    )
    val call = store(d).get("e1").get.messages(1).toolCalls.head
    assertEquals((call.output, call.isError), ("java.lang.RuntimeException: boom", true))

  tmp.test("an empty store lists nothing, gets None, and overviews as empty"): d =>
    val h = store(d)
    assertEquals(h.all, Nil)
    assertEquals(h.get("anything"), None)
    assert(captured(h.overview()).contains("(no conversations yet)"))

  tmp.test("sessions list newest-first by mtime"): d =>
    writeSession(d, "older", Now - 10_000_000, userLine("old"))
    writeSession(d, "newer", Now - 1_000_000, userLine("new"))
    assertEquals(store(d).all.map(_.id), List("newer", "older"))

  tmp.test("nested subagent dirs are ignored (top-level .jsonl only)"): d =>
    writeSession(d, "main", Now, userLine("hi"))
    // A subagent dir under the sessions root must not be read as a session.
    nodeFs.mkdirSync((d.path / "sessions" / "main" / "subagents").toString, js.Dynamic.literal(recursive = true))
    assertEquals(store(d).all.map(_.id), List("main"))

  // -- id resolution ------------------------------------------------------------

  tmp.test("get/read resolve a full id or an unambiguous prefix"): d =>
    writeSession(d, "abcd1234", Now, userLine("hello world"))
    val h = store(d)
    assertEquals(h.get("abcd1234").map(_.id), Some("abcd1234"))
    assertEquals(h.get("abcd").map(_.id), Some("abcd1234")) // unique prefix
    assert(captured(h.read("abcd")).contains("hello world"))

  tmp.test("an ambiguous prefix is None for get and explained for read"): d =>
    writeSession(d, "ab11", Now, userLine("one"))
    writeSession(d, "ab22", Now - 1000, userLine("two"))
    val h = store(d)
    assertEquals(h.get("ab"), None)
    assert(captured(h.read("ab")).toLowerCase.contains("ambiguous"))

  tmp.test("an unknown id is None for get and a not-found note for read"): d =>
    val h = store(d)
    assertEquals(h.get("ghost"), None)
    assert(captured(h.read("ghost")).contains("no conversation with id 'ghost'"))

  // -- read transcript ----------------------------------------------------------

  tmp.test("read prints the user text, the assistant answer, and the tool call + output"): d =>
    writeSession(d, "t", Now - 7200_000,
      userLine("read the build"),
      assistantLine(toolUse("t1", "eval_scala", "lib.fs.read(\"build.sbt\")"), text("Here it is.")),
      resultsLine(("t1", "scalaVersion := 3.8.3", false))
    )
    val out = captured(store(d).read("t"))
    assert(out.contains("▌ user"), out)
    assert(out.contains("read the build"), out)
    assert(out.contains("▌ assistant"), out)
    assert(out.contains("⚙ eval_scala"), out)
    assert(out.contains("→ scalaVersion := 3.8.3"), out)
    assert(out.contains("Here it is."), out)

  // -- search -------------------------------------------------------------------

  tmp.test("search prints matching conversations with a snippet, newest-first"): d =>
    writeSession(d, "s-old", Now - 10_000_000, userLine("about workflow resume semantics"))
    writeSession(d, "s-new", Now - 1_000_000, userLine("the slash command palette"))
    val h = store(d)
    val hit = captured(h.search("workflow"))
    assert(hit.contains("s-old"), hit)
    assert(hit.toLowerCase.contains("workflow"), hit)
    assert(!hit.contains("s-new"), hit)
    assert(captured(h.search("nonexistent-term")).contains("(no conversations match"))

  // -- robustness & other event types ------------------------------------------

  tmp.test("a corrupt/partial trailing line is skipped; valid lines still parse"): d =>
    writeSession(d, "c", Now,
      userLine("hi"),
      "{ this is not valid json",
      assistantLine(text("still here"))
    )
    val msgs = store(d).get("c").get.messages
    assertEquals(msgs.map(_.role), List("user", "assistant"))
    assertEquals(msgs(1).text, "still here")

  tmp.test("interrupted and system_notice become system messages"): d =>
    writeSession(d, "sys", Now,
      userLine("go"),
      interruptedLine,
      noticeLine("a background run finished")
    )
    val msgs = store(d).get("sys").get.messages
    assertEquals(msgs.map(_.role), List("user", "system", "system"))
    assertEquals(msgs(1).text, "(interrupted)")
    assertEquals(msgs(2).text, "a background run finished")
    // System messages don't count toward the user/assistant message count.
    assertEquals(store(d).get("sys").get.messageCount, 1)

  // -- pure helpers -------------------------------------------------------------

  test("parseLine decodes each surfaced event type and rejects the rest"):
    assertEquals(parseLine(userLine("hi")), Some(UserEv("hi")))
    assertEquals(parseLine(noticeLine("woke")), Some(NoticeEv("woke")))
    assertEquals(parseLine(interruptedLine), Some(InterruptedEv))
    assertEquals(
      parseLine(assistantLine(thinking("hmm"), toolUse("i", "n", "a"), text("ans"))),
      Some(AssistantEv("ans", "hmm", List(RawCall("i", "n", "a"))))
    )
    assertEquals(parseLine(resultsLine(("t", "out", true))), Some(ResultsEv(List(ResultRow("t", "out", true)))))
    assertEquals(parseLine("not json at all"), None)
    assertEquals(parseLine("""{"type":"some_future_event"}"""), None)

  test("messagesFrom pairs tool calls with results by id and drops fully-empty turns"):
    val events = List(
      UserEv("do it"),
      AssistantEv("done", "because", List(RawCall("t1", "eval_scala", "1+1"))),
      ResultsEv(List(ResultRow("t1", "2", false))),
      AssistantEv("", "", Nil) // an empty assistant turn is dropped
    )
    val msgs = messagesFrom(events)
    assertEquals(msgs.map(_.role), List("user", "assistant"))
    val a = msgs(1)
    assertEquals((a.text, a.reasoning), ("done", "because"))
    assertEquals(a.toolCalls.map(c => (c.name, c.output, c.isError)), List(("eval_scala", "2", false)))

  test("messagesFrom keeps an assistant turn that only made tool calls"):
    val msgs = messagesFrom(List(AssistantEv("", "", List(RawCall("t1", "grep", "x")))))
    assertEquals(msgs.map(_.role), List("assistant"))
    assertEquals(msgs.head.toolCalls.head.name, "grep")
    assertEquals(msgs.head.toolCalls.head.output, "") // no matching result ⇒ empty

  test("previewOf takes the latest user/assistant text, normalized and truncated"):
    assertEquals(previewOf(Nil), "(no messages)")
    assertEquals(
      previewOf(List(HistoryMessageImpl("user", "first", "", Nil), HistoryMessageImpl("assistant", "second\n line", "", Nil))),
      "second line"
    )
    val long = "x" * 80
    assert(previewOf(List(HistoryMessageImpl("user", long, "", Nil))).length <= 48)

  test("relativeTime renders compact ages"):
    assertEquals(relativeTime(5_000L), "just now")
    assertEquals(relativeTime(5 * 60_000L), "5m ago")
    assertEquals(relativeTime(3 * 3600_000L), "3h ago")
    assertEquals(relativeTime(2 * 86_400_000L), "2d ago")
    assertEquals(relativeTime(60L * 86_400_000L), "2mo ago")

  test("shortId takes the first 8 chars"):
    assertEquals(shortId("4a376536-c908-45bc"), "4a376536")
    assertEquals(shortId("short"), "short")

  test("snippetAround returns a window centered on the match"):
    val snip = snippetAround("alpha beta gamma delta epsilon", "gamma")
    assert(snip.contains("gamma"), snip)
    assert(snip.length < "alpha beta gamma delta epsilon".length + 1, snip)
