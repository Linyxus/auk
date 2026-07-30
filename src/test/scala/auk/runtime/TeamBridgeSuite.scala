package auk.runtime

import scala.concurrent.duration.{Duration, DurationInt}
import scala.scalajs.js
import scala.util.Success

import gears.async.{Async, Future, UnboundedChannel}
import gears.async.default.given

import auk.agent.TeamMemberView
import auk.llm.provider.ModelSession
import auk.llm.endpoint.{Endpoint, LLMConfig, ChatResponse, Message, Content, Role, FinishReason, StreamEvent, LLMError, Usage}
import auk.llm.tools.{RuntimeContext, ApprovalPolicy}
import auk.platform.Platform
import auk.runtime.repl.ScalaRepl
import auk.utils.Result
import auk.workflow.{Transcript, TranscriptEvent, TranscriptItem}

/** Host-level tests for the [[TeamBridge]]: a raw Unix-domain-socket client drives
  * the wire protocol directly (hello / new_member / send), member turns are driven
  * by a scripted endpoint (no LLM, no REPL eval), and the lead's system notices are
  * captured through the bridge's `notifyLead` callback. No REPL artifacts required —
  * the end-to-end DSL path is covered by [[TeamDslSuite]].
  */
class TeamBridgeSuite extends munit.FunSuite:

  override def munitTimeout: Duration = 120.seconds

  private type ReadableChannelT = gears.async.ReadableChannel[Result[StreamEvent, LLMError]]

  // -- scripted member endpoints ----------------------------------------------

  /** Records the full message list of every turn it serves (so a test can assert a
    * member's stored history is threaded into a later turn), replying with a fixed
    * text each time. */
  private class RecordingEndpoint(reply: String) extends Endpoint:
    val seen = scala.collection.mutable.ListBuffer.empty[List[Message]]
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannelT =
      seen += messages
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, List(Content.Text(reply))), FinishReason.Stop)))))
      ch.asReadable

  /** Finishes each turn with a fixed reply AND per-round token usage, so the
    * bridge's roster snapshots have real figures to accumulate. */
  private class UsageEndpoint(reply: String, usage: Usage) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannelT =
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(
        ChatResponse(Message(Role.Assistant, List(Content.Text(reply))), FinishReason.Stop, Some(usage))
      ))))
      ch.asReadable

  /** Fails every turn with an LLM error (a closed/error stream). */
  private class ErrorEndpoint(msg: String) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannelT =
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Left(LLMError(msg))))
      ch.asReadable

  /** Finishes a turn immediately with a fixed assistant reply. */
  private class ReplyEndpoint(reply: String) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannelT =
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future(ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, List(Content.Text(reply))), FinishReason.Stop)))))
      ch.asReadable

  /** Streams the reply as one prose delta before finishing, so the turn produces
    * real transcript activity (a `Done`-only stub produces none). */
  private class StreamingReplyEndpoint(reply: String) extends Endpoint:
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannelT =
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        ch.send(Right(StreamEvent.Delta(reply)))
        ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, List(Content.Text(reply))), FinishReason.Stop))))
      ch.asReadable

  /** Records the team-tagged user messages seen on each turn and gates only the
    * FIRST turn, so a test can queue more messages while the member is busy and
    * observe that they batch into a single later turn. */
  private class BatchGateEndpoint extends Endpoint:
    val turns = scala.collection.mutable.ListBuffer.empty[List[String]]
    val firstTurnStarted = Future.Promise[Unit]()
    val gate = Future.Promise[Unit]()
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannelT =
      val teamTexts = messages.collect { case Message(Role.User, c) => c.collect { case Content.Text(t) => t }.mkString }
        .filter(_.startsWith("[team message from"))
      turns += teamTexts
      val index = turns.size
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        if index == 1 then
          try firstTurnStarted.complete(Success(())) catch case _: Throwable => ()
          gate.await
        ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, List(Content.Text("done"))), FinishReason.Stop))))
      ch.asReadable

  /** Gates every turn behind `gate`, tracking how many turns started and the peak
    * concurrency, so a test can observe the permit cap and clean cancellation. */
  private class GateEndpoint(gate: Future[Unit]) extends Endpoint:
    var started = 0
    var live = 0
    var maxLive = 0
    val firstStarted = Future.Promise[Unit]()
    def invoke(messages: List[Message], config: LLMConfig)(using Async): Result[ChatResponse, LLMError] =
      Left(LLMError("streams only"))
    def stream(messages: List[Message], config: LLMConfig)(using Async.Spawn): ReadableChannelT =
      val ch = UnboundedChannel[Result[StreamEvent, LLMError]]()
      Future:
        started += 1
        live += 1
        if live > maxLive then maxLive = live
        try firstStarted.complete(Success(())) catch case _: Throwable => ()
        gate.await
        live -= 1
        ch.send(Right(StreamEvent.Done(ChatResponse(Message(Role.Assistant, List(Content.Text("done"))), FinishReason.Stop))))
      ch.asReadable

  // -- raw wire client ---------------------------------------------------------

  /** A raw UDS client: writes JSON lines and buffers received lines into a channel. */
  private class WireClient(sockPath: String):
    private val net = js.Dynamic.global.require("node:net")
    val incoming = UnboundedChannel[String]()
    private var buf = ""
    private val sock = net.createConnection(sockPath).asInstanceOf[js.Dynamic]
    sock.setEncoding("utf8")
    sock.on(
      "data",
      ((chunk: js.Any) =>
        buf += chunk.asInstanceOf[String]
        var idx = buf.indexOf("\n")
        while idx >= 0 do
          val line = buf.substring(0, idx)
          buf = buf.substring(idx + 1)
          if line.nonEmpty then incoming.sendImmediately(line)
          idx = buf.indexOf("\n")
        ()
      ): js.Function1[js.Any, Unit]
    )
    private def line(obj: js.Dynamic): Unit = { sock.write(js.JSON.stringify(obj) + "\n"); () }
    def hello(me: String): Unit = line(js.Dynamic.literal(t = "hello", me = me))
    def newMember(id: String, desc: String): Unit = line(js.Dynamic.literal(t = "new_member", id = id, desc = desc))
    def retire(id: String): Unit = line(js.Dynamic.literal(t = "retire", id = id))
    def send(to: String, text: String): Unit = line(js.Dynamic.literal(t = "send", to = to, text = text))
    def close(): Unit = try { sock.end(); () } catch case _: Throwable => ()

  // -- helpers -----------------------------------------------------------------

  private def tmpSock(name: String): String =
    val os = js.Dynamic.global.require("node:os")
    val path = js.Dynamic.global.require("node:path")
    path.join(os.tmpdir(), s"auk-team-$name-${js.Dynamic.global.process.pid}.sock").asInstanceOf[String]

  private def makeBridge(
      name: String,
      endpoint: Endpoint,
      notices: UnboundedChannel[String],
      maxConcurrent: Int = 4,
      onActivity: (String, TranscriptEvent) => Unit = (_, _) => (),
      onTeam: Vector[TeamMemberView] => Unit = _ => ()
  ): TeamBridge =
    TeamBridge(
      socketPath = tmpSock(name),
      models = ModelSession.of(endpoint, LLMConfig(model = "test")),
      makeRepl = _ => ScalaRepl(),
      baseTools = _ => Nil,
      memberPrompt = (_, _) => "You are a team member.",
      context = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll),
      notifyLead = msg => notices.sendImmediately(msg),
      maxConcurrent = maxConcurrent,
      onActivity = onActivity,
      onTeam = onTeam
    )

  private def start(bridge: TeamBridge)(using Async.Spawn): Unit =
    val ready = Future.Promise[Unit]()
    bridge.start(() => ready.complete(Success(())))
    ready.asFuture.await

  private def readLine(ch: UnboundedChannel[String])(using Async): String =
    ch.read() match
      case Right(l) => l
      case Left(_)  => fail("channel closed")

  /** Read lines until one satisfies `pred`, returning it (discarding earlier ones). */
  private def awaitMatch(ch: UnboundedChannel[String], pred: String => Boolean)(using Async): String =
    var found: String | Null = null
    while found == null do
      val l = readLine(ch)
      if pred(l) then found = l
    found

  private def isUpdate(id: String, status: String): String => Boolean =
    l => l.contains("\"t\":\"update\"") && l.contains(s"\"id\":\"$id\"") && l.contains(s"\"status\":\"$status\"")

  /** Read roster snapshots until one satisfies `pred`, returning it. */
  private def awaitRoster(
      ch: UnboundedChannel[Vector[TeamMemberView]],
      pred: Vector[TeamMemberView] => Boolean
  )(using Async): Vector[TeamMemberView] =
    var found: Vector[TeamMemberView] | Null = null
    while found == null do
      ch.read() match
        case Right(r) => if pred(r) then found = r
        case Left(_)  => fail("snapshot channel closed")
    found.nn

  // -- tests -------------------------------------------------------------------

  test("hello is answered with an empty roster when no members exist"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("hello", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        val roster = awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        assert(roster.contains("\"members\":[]"), roster)
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("new_member spawns a member; its scripted turn produces an idle notice with the final text"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("spawn", new ReplyEndpoint("all handled"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("worker", "does the work")
        // Creation broadcast: idle, no `last` yet.
        val created = awaitMatch(lead.incoming, isUpdate("worker", "idle"))
        assert(created.contains("\"desc\":\"does the work\""), created)
        assert(!created.contains("\"last\":"), created)
        lead.send("worker", "please do it")
        // The turn runs and, when the mailbox drains, the lead is notified. The
        // notice announces idleness but never carries the response body — the
        // lead is pointed at `lastResponse` instead.
        val notice = awaitMatch(notices, _.contains("Team member 'worker' finished its turn"))
        assert(!notice.contains("all handled"), notice)
        assert(notice.contains("lastResponse"), notice)
        // The idle broadcast now carries the member's last response.
        val done = awaitMatch(lead.incoming, l => isUpdate("worker", "idle")(l) && l.contains("\"last\":"))
        assert(done.contains("\"last\":\"all handled\""), done)
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("a message sent to a member enters its transcript as a Received item"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val activity = scala.collection.mutable.ListBuffer.empty[(String, TranscriptEvent)]
      val bridge = makeBridge("received", new StreamingReplyEndpoint("all handled"), notices,
        onActivity = (id, ev) => activity += ((id, ev)))
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("worker", "does the work")
        awaitMatch(lead.incoming, isUpdate("worker", "idle"))
        lead.send("worker", "please do it")
        awaitMatch(notices, _.contains("Team member 'worker' finished its turn"))
        val events = activity.collect { case ("worker", ev) => ev }.toVector
        // Emitted at seed time, so it leads the turn the member then works through.
        assertEquals(events.head, TranscriptEvent.Received("team", "worker", "lead", "please do it"))
        val items = events.foldLeft(Transcript.empty)(_.update(_)).items
        assertEquals(items.head, TranscriptItem.Received("lead", "please do it"))
        assert(items.exists { case TranscriptItem.Said(t) => t.contains("all handled"); case _ => false }, items.toString)
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("messages queued while a member is busy batch into a single later turn"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val endpoint = new BatchGateEndpoint
      val bridge = makeBridge("batch", endpoint, notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m", "worker")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        lead.send("m", "first")
        // Turn 1 is now running and gated; the mailbox has been drained.
        endpoint.firstTurnStarted.asFuture.await
        lead.send("m", "second")
        lead.send("m", "third")
        // Same-connection FIFO: once this later create is acknowledged, the two
        // sends above are guaranteed to have reached the member's mailbox.
        lead.newMember("barrier", "sync")
        awaitMatch(lead.incoming, isUpdate("barrier", "idle"))
        endpoint.gate.complete(Success(()))
        // Turn 1 finishes with a non-empty mailbox, so it loops without going idle;
        // the queued pair runs as one batched turn, then the member goes idle once.
        awaitMatch(notices, _.contains("Team member 'm' finished its turn"))
        assertEquals(endpoint.turns.size, 2, s"the two queued messages must batch into one turn: ${endpoint.turns.toList}")
        assert(endpoint.turns(1).exists(_.contains("second")), endpoint.turns.toList.toString)
        assert(endpoint.turns(1).exists(_.contains("third")), endpoint.turns.toList.toString)
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("an update broadcast reaches every connected client"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("broadcast", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      val watcher = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        watcher.hello("watcher")
        awaitMatch(watcher.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("shared", "seen by all")
        // Both the creator and the passive observer receive the upsert.
        awaitMatch(lead.incoming, isUpdate("shared", "idle"))
        awaitMatch(watcher.incoming, isUpdate("shared", "idle"))
      finally
        lead.close()
        watcher.close()
        Async.fromSync(bridge.close())

  test("a send addressed to the lead becomes a message notice"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("to-lead", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val reporter = WireClient(bridge.socketPath)
      try
        reporter.hello("reporter")
        awaitMatch(reporter.incoming, _.contains("\"t\":\"roster\""))
        reporter.send("lead", "status update")
        val notice = awaitMatch(notices, _.contains("sent you a message"))
        assertEquals(notice, "Team member 'reporter' sent you a message:\nstatus update")
      finally
        reporter.close()
        Async.fromSync(bridge.close())

  test("a send to an unknown member is rejected with an error line and a lead notice"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("unknown-send", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.send("ghost", "hi")
        val err = awaitMatch(lead.incoming, _.contains("\"t\":\"error\""))
        assert(err.contains("unknown team member 'ghost'"), err)
        val notice = awaitMatch(notices, _.contains("rejected message"))
        assertEquals(notice, "[team] rejected message from 'lead' to unknown member 'ghost'")
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("a duplicate new_member is rejected with an error line and a lead notice"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("dup", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("dup", "first")
        awaitMatch(lead.incoming, isUpdate("dup", "idle"))
        lead.newMember("dup", "second")
        val err = awaitMatch(lead.incoming, _.contains("\"t\":\"error\""))
        assert(err.contains("duplicate member id 'dup'"), err)
        val notice = awaitMatch(notices, _.contains("rejected creating member"))
        assert(notice.contains("duplicate member id 'dup'"), notice)
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("only the lead may create members: a non-lead connection is rejected"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("lead-only", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val impostor = WireClient(bridge.socketPath)
      try
        impostor.hello("member-1")
        awaitMatch(impostor.incoming, _.contains("\"t\":\"roster\""))
        impostor.newMember("x", "should not exist")
        val err = awaitMatch(impostor.incoming, _.contains("\"t\":\"error\""))
        assert(err.contains("only the lead can create team members"), err)
        val notice = awaitMatch(notices, _.contains("rejected creating member"))
        assertEquals(notice, "[team] rejected creating member 'x': only the lead can create team members")
      finally
        impostor.close()
        Async.fromSync(bridge.close())

  test("the permit cap bounds how many member turns run at once"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val gate = Future.Promise[Unit]()
      val endpoint = new GateEndpoint(gate.asFuture)
      val bridge = makeBridge("cap", endpoint, notices, maxConcurrent = 1)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m1", "one")
        awaitMatch(lead.incoming, isUpdate("m1", "idle"))
        lead.newMember("m2", "two")
        awaitMatch(lead.incoming, isUpdate("m2", "idle"))
        lead.send("m1", "go")
        lead.send("m2", "go")
        // Barrier on the lead's own connection: the create is processed after both
        // sends, so by the time its update lands, both members have been messaged.
        lead.newMember("barrier", "sync")
        awaitMatch(lead.incoming, isUpdate("barrier", "idle"))
        endpoint.firstStarted.asFuture.await
        assertEquals(endpoint.started, 1, "only one turn may run while the single permit is held")
        gate.complete(Success(()))
        // Both members eventually finish; the second only after the first frees the permit.
        awaitMatch(notices, _.contains("finished its turn and is now idle"))
        awaitMatch(notices, _.contains("finished its turn and is now idle"))
        assertEquals(endpoint.started, 2, "both turns ran once the permit was freed")
        assertEquals(endpoint.maxLive, 1, "the cap of 1 was never exceeded")
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("a member's stored history is threaded into its next turn"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val endpoint = new RecordingEndpoint("noted")
      val bridge = makeBridge("history", endpoint, notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m", "worker")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        // Turn 1, run to completion (idle notice) before the second message so the
        // two never batch.
        lead.send("m", "first")
        awaitMatch(notices, _.contains("Team member 'm' finished its turn"))
        lead.send("m", "second")
        awaitMatch(notices, _.contains("Team member 'm' finished its turn"))
        assertEquals(endpoint.seen.size, 2, "two separate turns ran")
        val turn2 = endpoint.seen(1).map(_.text)
        // Turn 2's request is turn 1's seed, then turn 1's assistant reply, then the
        // new seed — the stored history threaded in ahead of the fresh message.
        val iFirst = turn2.indexWhere(_.contains("[team message from lead] first"))
        val iReply = turn2.indexOf("noted")
        val iSecond = turn2.indexWhere(_.contains("[team message from lead] second"))
        assert(iFirst >= 0 && iReply >= 0 && iSecond >= 0, s"turn 2 missing expected messages: $turn2")
        assert(iFirst < iReply && iReply < iSecond, s"turn 2 must be [first seed, turn 1 reply, second seed] in order: $turn2")
        assert(endpoint.seen(1).size > endpoint.seen(0).size, "the conversation grew across turns")
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("a member turn that hits an LLM error notifies the lead with a failure notice"):
    Async.fromSync:
      import gears.async.AsyncOperations.sleep
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("llm-error", new ErrorEndpoint("boom"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m", "worker")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        lead.send("m", "go")
        // The failure notice is the ONLY notice for a failed cycle — reading the first
        // notice and asserting it directly proves no idle notice preceded it.
        val notice = readLine(notices)
        assertEquals(notice, TeamBridge.failureNotice("m", "boom"))
        sleep(150)
        assert(notices.readSource.poll().isEmpty, "a failed turn fires no idle notice")
        // The contract: lastResponse becomes the error text, so the idle update carries it.
        val update = awaitMatch(lead.incoming, l => isUpdate("m", "idle")(l) && l.contains("\"last\":"))
        assert(update.contains("\"last\":\"boom\""), update)
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("onTeam snapshots track the roster, status flips, and accumulated token usage"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val snapshots = UnboundedChannel[Vector[TeamMemberView]]()
      val bridge = TeamBridge(
        socketPath = tmpSock("onteam"),
        models = ModelSession.of(new UsageEndpoint("ok", Usage(7, 5)), LLMConfig(model = "test")),
        makeRepl = _ => ScalaRepl(),
        baseTools = _ => Nil,
        memberPrompt = (_, _) => "You are a team member.",
        context = RuntimeContext(Platform.cwd(), ApprovalPolicy.AllowAll),
        notifyLead = msg => notices.sendImmediately(msg),
        onTeam = roster => snapshots.sendImmediately(roster)
      )
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      def awaitSnapshot(pred: Vector[TeamMemberView] => Boolean)(using Async): Vector[TeamMemberView] =
        var found: Vector[TeamMemberView] | Null = null
        while found == null do
          snapshots.read() match
            case Right(r) => if pred(r) then found = r
            case Left(_)  => fail("snapshot channel closed")
        found.nn
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("worker", "does the work")
        // Creation snapshot: one idle member, no tokens yet.
        val created = awaitSnapshot(_.exists(_.id == "worker"))
        assertEquals(created.map(m => (m.id, m.desc, m.working, m.outputTokens)), Vector(("worker", "does the work", false, 0L)))
        lead.send("worker", "go")
        // The turn flips it to working...
        awaitSnapshot(_.exists(m => m.id == "worker" && m.working))
        // ...and the settled idle snapshot carries the turn's exact usage.
        val settled = awaitSnapshot(_.exists(m => m.id == "worker" && !m.working && m.outputTokens > 0))
        val worker = settled.find(_.id == "worker").get
        assertEquals((worker.inputTokens, worker.outputTokens), (7L, 5L))
        // A second turn accumulates on top of the first.
        lead.send("worker", "again")
        val twice = awaitSnapshot(_.exists(m => m.id == "worker" && !m.working && m.outputTokens >= 10))
        val worker2 = twice.find(_.id == "worker").get
        assertEquals((worker2.inputTokens, worker2.outputTokens), (14L, 10L))
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("close() cancels an in-flight member turn cleanly"):
    Async.fromSync:
      import gears.async.AsyncOperations.sleep
      val notices = UnboundedChannel[String]()
      val gate = Future.Promise[Unit]() // never released: the turn is cancelled by close()
      val endpoint = new GateEndpoint(gate.asFuture)
      val bridge = makeBridge("close", endpoint, notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m", "worker")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        lead.send("m", "go")
        endpoint.firstStarted.asFuture.await // the member turn is running, blocked on the gate
        Async.fromSync(bridge.close())       // cancels the member fiber mid-turn
        // The cancelled turn must not report an idle notice.
        sleep(200)
        assert(notices.readSource.poll().isEmpty, "a cancelled turn fires no idle notice")
      finally
        lead.close()

  // -- retiring a member -------------------------------------------------------

  test("retire takes a member out of play but leaves its record on the roster"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("retire", new ReplyEndpoint("all handled"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("worker", "does the work")
        awaitMatch(lead.incoming, isUpdate("worker", "idle"))
        lead.send("worker", "please do it")
        awaitMatch(notices, _.contains("Team member 'worker' finished its turn"))
        lead.retire("worker")
        // The retired broadcast still carries the member's description and its last
        // response: retiring ends the work, not the record.
        val retired = awaitMatch(lead.incoming, isUpdate("worker", "retired"))
        assert(retired.contains("\"desc\":\"does the work\""), retired)
        assert(retired.contains("\"last\":\"all handled\""), retired)
        // A retire the lead asked for needs no notice back, and a fresh hello sees
        // the member on the roster exactly as it was left.
        assert(notices.readSource.poll().isEmpty, "a successful retire notifies nobody")
        lead.hello("lead")
        val roster = awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        assert(roster.contains("\"status\":\"retired\""), roster)
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("a send to a retired member is rejected with an error line and a lead notice"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("retired-send", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m", "worker")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        lead.retire("m")
        awaitMatch(lead.incoming, isUpdate("m", "retired"))
        lead.send("m", "one more thing")
        val err = awaitMatch(lead.incoming, _.contains("\"t\":\"error\""))
        assert(err.contains("has been retired"), err)
        val notice = awaitMatch(notices, _.contains("rejected message"))
        assertEquals(notice, "[team] rejected message from 'lead' to retired member 'm'")
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("a retired member's id is never reused: new_member is rejected and says why"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("retired-id", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m", "first")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        lead.retire("m")
        awaitMatch(lead.incoming, isUpdate("m", "retired"))
        lead.newMember("m", "second")
        val err = awaitMatch(lead.incoming, _.contains("\"t\":\"error\""))
        assert(err.contains("belonged to a retired member"), err)
        val notice = awaitMatch(notices, _.contains("rejected creating member"))
        assert(notice.contains("choose a new id"), notice)
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("retiring an unknown or already-retired member is rejected"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("retire-twice", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.retire("ghost")
        val unknown = awaitMatch(lead.incoming, _.contains("\"t\":\"error\""))
        assert(unknown.contains("unknown team member 'ghost'"), unknown)
        assertEquals(
          awaitMatch(notices, _.contains("rejected retiring")),
          "[team] rejected retiring member 'ghost': unknown team member 'ghost'"
        )
        lead.newMember("m", "worker")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        lead.retire("m")
        awaitMatch(lead.incoming, isUpdate("m", "retired"))
        lead.retire("m")
        val again = awaitMatch(lead.incoming, _.contains("\"t\":\"error\""))
        assert(again.contains("has already been retired"), again)
        assert(awaitMatch(notices, _.contains("rejected retiring")).contains("already been retired"))
      finally
        lead.close()
        Async.fromSync(bridge.close())

  test("only the lead may retire members: a non-lead connection is rejected"):
    Async.fromSync:
      val notices = UnboundedChannel[String]()
      val bridge = makeBridge("retire-lead-only", new ReplyEndpoint("ok"), notices)
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      val impostor = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m", "worker")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        impostor.hello("member-1")
        awaitMatch(impostor.incoming, _.contains("\"t\":\"roster\""))
        impostor.retire("m")
        val err = awaitMatch(impostor.incoming, _.contains("\"t\":\"error\""))
        assert(err.contains("only the lead can retire team members"), err)
        assertEquals(
          awaitMatch(notices, _.contains("rejected retiring")),
          "[team] rejected retiring member 'm': only the lead can retire team members"
        )
        // The member is untouched: it still takes messages and runs its turn.
        lead.send("m", "carry on")
        awaitMatch(notices, _.contains("Team member 'm' finished its turn"))
      finally
        lead.close()
        impostor.close()
        Async.fromSync(bridge.close())

  test("retire cancels the member's in-flight turn and the roster snapshot marks it retired"):
    Async.fromSync:
      import gears.async.AsyncOperations.sleep
      val notices = UnboundedChannel[String]()
      val snapshots = UnboundedChannel[Vector[TeamMemberView]]()
      val gate = Future.Promise[Unit]() // never released: the turn is cancelled by retire
      val endpoint = new GateEndpoint(gate.asFuture)
      val bridge = makeBridge("retire-cancel", endpoint, notices, onTeam = r => snapshots.sendImmediately(r))
      start(bridge)
      val lead = WireClient(bridge.socketPath)
      try
        lead.hello("lead")
        awaitMatch(lead.incoming, _.contains("\"t\":\"roster\""))
        lead.newMember("m", "worker")
        awaitMatch(lead.incoming, isUpdate("m", "idle"))
        lead.send("m", "go")
        endpoint.firstStarted.asFuture.await // the turn is running, blocked on the gate
        lead.retire("m")
        awaitMatch(lead.incoming, isUpdate("m", "retired"))
        // The cancelled turn reports nothing — no idle notice, no failure notice —
        // and the fiber cannot write a later status over the retired one.
        sleep(200)
        assert(notices.readSource.poll().isEmpty, "a retired member's cancelled turn fires no notice")
        val settled = awaitRoster(snapshots, _.exists(m => m.id == "m" && m.retired))
        val view = settled.find(_.id == "m").get
        assert(!view.working, s"a retired member is never working: $view")
        // Messages queued for it while it worked are dropped with it.
        lead.send("m", "and this too")
        assert(awaitMatch(lead.incoming, _.contains("\"t\":\"error\"")).contains("has been retired"))
      finally
        lead.close()
        Async.fromSync(bridge.close())
