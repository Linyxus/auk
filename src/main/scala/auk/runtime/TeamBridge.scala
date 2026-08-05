package auk.runtime

import scala.collection.mutable

import gears.async.{Async, CancellationException, Future, UnboundedChannel}

import auk.agent.TeamMemberView
import auk.llm.endpoint.Message
import auk.llm.provider.ModelSession
import auk.llm.tools.{Json, RuntimeContext, Tool}
import auk.platform.js.SocketServer
import auk.runtime.repl.ScalaRepl
import auk.session.{JsonlLog, SessionProvider, SessionRef}
import auk.workflow.{TranscriptEvent, WireCodec, WireMessage}

/** The host side of the agent-team bridge: a Unix-domain-socket server the lead's
  * REPL worker and every member's dedicated worker connect to
  * (`auk.library.TeamClient`). It owns the models, tools, and member REPLs, and
  * runs each member as a long-lived gears fiber.
  *
  * A member fiber idles on its mailbox, wakes on a message, batch-drains whatever
  * else is queued, and runs ONE [[HeadlessAgent.runConversation]] turn seeded with
  * the member's stored history plus the new `[team message from <sender>]` user
  * messages. When it settles (the mailbox drains empty) it goes idle, broadcasts
  * an `update`, and notifies the lead. Turns across all members share a permit
  * channel capped at `maxConcurrent`.
  *
  * The lead never appears in the member roster: the host tracks only real members
  * (creation order), and the library synthesizes the lead's own handle. Host-side
  * validation of `new_member`/`retire`/`send` is defense-in-depth — the library's
  * own guards are primary — so a rejection is both an `error` line back to the
  * caller and a rejection notice to the lead.
  *
  * A retired member (`retire`) keeps its record forever: it is cancelled and shut
  * down, but stays in the roster with its last response and its id reserved, so
  * nothing the lead already knows about it can go stale or be reused.
  *
  * A member may also be created by a worker that is not the lead but acts with the
  * lead's authority — a loop's generation worker. Such a member carries the OWNER its
  * creator was spawned with (`AUK_TEAM_OWNER`, on the wire as `owner`), and the host
  * retires it through [[retireOwnedBy]] when that work ends: a member hired to reason
  * about one generation must not outlive it. Members with no owner are the session's
  * own and are never swept.
  */
final class TeamBridge(
    val socketPath: String,
    models: ModelSession,
    makeRepl: Map[String, String] => ScalaRepl,
    baseTools: ScalaRepl => List[Tool],
    memberPrompt: (String, String) => String,
    context: RuntimeContext,
    notifyLead: String => Unit,
    maxConcurrent: Int = 4,
    onActivity: (String, TranscriptEvent) => Unit = (_, _) => (),
    onTeam: Vector[TeamMemberView] => Unit = _ => (),
    sessionRef: Option[SessionRef] = None
):
  import TeamBridge.*

  private val incoming = UnboundedChannel[(SocketServer.Conn, Json)]()
  private var server: SocketServer.Handle | Null = null
  // Live connections → their hello identity (`"lead"` or a member id). A worker
  // reconnects whenever it restarts; a fresh hello just re-binds and re-snapshots.
  private val conns = mutable.Map.empty[SocketServer.Conn, String]
  // Members in creation order (roster/update ordering). Never contains the lead.
  private val members = mutable.LinkedHashMap.empty[String, MemberState]

  /** Per-member host state: the dedicated REPL + tools, the mailbox its fiber
    * idles on, and the conversation/status the fiber owns (mutated only on the
    * single JS event loop, so no locking). */
  private final class MemberState(
      val id: String,
      val desc: String,
      val repl: ScalaRepl,
      val registry: ToolRegistry,
      /** The work this member was created for, if it was created by a worker acting
        * for one (see [[retireOwnedBy]]). `None` for the lead's own members. */
      val owner: Option[String]
  ):
    val mailbox: UnboundedChannel[(String, String)] = UnboundedChannel()
    var history: List[Message] = Nil
    var status: String = "idle"
    var lastResponse: Option[String] = None
    var fiber: Future[Unit] | Null = null
    // Token accounting for the TUI roster: totals from settled turns, plus the
    // in-flight turn's running totals (onRound reports cumulative-within-turn
    // figures, so `turn*` is overwritten each round, then folded into `base*`
    // and zeroed when the turn's outcome lands).
    var baseInputTokens: Long = 0
    var baseOutputTokens: Long = 0
    var turnInputTokens: Long = 0
    var turnOutputTokens: Long = 0
    /** Retired ([[handleRetire]]): kept in the roster, but out of play — it runs no
      * turn, takes no message, and has nothing left to cancel or close. */
    def retired: Boolean = status == "retired"

  /** Bind the socket and start servicing clients. Spawns the dispatch fiber in the
    * caller's scope; returns immediately. */
  def start(onReady: () => Unit = () => ())(using Async.Spawn): Unit =
    val permits = UnboundedChannel[Unit]()
    var i = 0
    while i < maxConcurrent.max(1) do
      permits.sendImmediately(())
      i += 1
    server = SocketServer.listen(socketPath, onReady): conn =>
      conn.onClose(() => { conns.remove(conn); () })
      conn.onLine: line =>
        Json.parse(line) match
          case Right(msg) => incoming.sendImmediately((conn, msg))
          case Left(_)    => ()
    Future:
      var running = true
      while running do
        incoming.read() match
          case Right((conn, msg)) => dispatch(conn, msg, permits)
          case Left(_)            => running = false

  private def dispatch(
      conn: SocketServer.Conn,
      msg: Json,
      permits: UnboundedChannel[Unit]
  )(using Async.Spawn): Unit =
    field(msg, "t") match
      case Some("hello")      => handleHello(conn, str(msg, "me"))
      case Some("new_member") =>
        handleNewMember(conn, str(msg, "id"), str(msg, "desc"), field(msg, "owner").filter(_.nonEmpty), permits)
      case Some("retire")     => handleRetire(conn, str(msg, "id"))
      case Some("send")       => handleSend(conn, str(msg, "to"), str(msg, "text"))
      case _                  => ()

  private def handleHello(conn: SocketServer.Conn, me: String): Unit =
    conns(conn) = me
    conn.write(rosterMsg().render)

  private def handleNewMember(
      conn: SocketServer.Conn,
      id: String,
      desc: String,
      owner: Option[String],
      permits: UnboundedChannel[Unit]
  )(using Async.Spawn): Unit =
    // Only the lead may create members. Defense-in-depth over the library's own
    // guard: a member worker (or one that never sent `hello`) is rejected here even
    // if its client-side check is bypassed.
    val reason: Option[String] =
      if !conns.get(conn).contains(LeadId) then Some("only the lead can create team members")
      else validateNewMember(id, desc)
    reason match
      case Some(reason) =>
        conn.write(errorMsg(reason).render)
        notifyLead(s"[team] rejected creating member '$id': $reason")
      case None =>
        val repl = makeRepl(Map("AUK_TEAM_SOCK" -> socketPath, "AUK_TEAM_ID" -> id))
        val member = new MemberState(id, desc, repl, ToolRegistry.of(baseTools(repl)*), owner)
        members(id) = member
        member.fiber = Future(runMember(member, permits))
        broadcastUpdate(member)

  /** Retire a member (LEAD ONLY): cancel the turn it is running, drop whatever is
    * queued for it (the cancelled fiber never reads the mailbox again), close its
    * worker, and mark the record retired. The record itself STAYS in `members`
    * forever — its last response remains readable and its id stays reserved — so
    * this is the one member operation that ends work without ending the roster
    * entry. Cancellation and the status flip are synchronous on the single event
    * loop, so the cancelled fiber can never write a later status over "retired";
    * only the REPL teardown suspends, so it goes last, after the roster has been
    * told. */
  private def handleRetire(conn: SocketServer.Conn, id: String)(using Async): Unit =
    val reason: Option[String] =
      if !conns.get(conn).contains(LeadId) then Some("only the lead can retire team members")
      else validateRetire(id)
    reason match
      case Some(reason) =>
        conn.write(errorMsg(reason).render)
        notifyLead(s"[team] rejected retiring member '$id': $reason")
      case None => retireMember(members(id))

  /** Retire every live member created for `owner` — the work they were hired to
    * reason about has ended — and answer with the ids that were retired (empty for
    * an owner nobody worked for). Called by the loop bridge when a generation
    * settles, however it settles.
    *
    * Deliberately the same act as a lead's `retire`, member by member: the record
    * stays on the roster, the id stays reserved, and the same `update` reaches every
    * client. There is no storm to suppress — a successful retire tells the lead
    * nothing (only a REFUSED one does), so a sweep of five members is five roster
    * updates and no notices at all, which is what a lead that never hired them
    * should hear. */
  def retireOwnedBy(owner: String)(using Async): List[String] =
    val doomed = members.valuesIterator.filter(m => !m.retired && m.owner.contains(owner)).toList
    doomed.foreach(retireMember)
    doomed.map(_.id)

  /** End one member's working life: cancel its turn, settle its token accounting,
    * mark the record retired, tell the roster, and close its worker. The suspending
    * teardown goes last, after the roster has been told, so the cancelled fiber can
    * never write a later status over "retired". */
  private def retireMember(m: MemberState)(using Async): Unit =
    if m.fiber != null then m.fiber.nn.cancel()
    // The cancelled turn reports no outcome, so fold its running totals in by
    // hand: the roster keeps reporting everything the member actually spent.
    m.baseInputTokens += m.turnInputTokens
    m.baseOutputTokens += m.turnOutputTokens
    m.turnInputTokens = 0
    m.turnOutputTokens = 0
    m.status = "retired"
    broadcastUpdate(m)
    m.repl.close()

  private def handleSend(conn: SocketServer.Conn, to: String, text: String): Unit =
    val from = conns.getOrElse(conn, "")
    if to == from then
      conn.write(errorMsg("cannot send a message to yourself").render)
    else if to == LeadId then
      notifyLead(messageNotice(from, text))
    else
      members.get(to) match
        case Some(member) if member.retired =>
          conn.write(errorMsg(s"member '$to' has been retired and can no longer be messaged").render)
          notifyLead(s"[team] rejected message from '$from' to retired member '$to'")
        case Some(member) => member.mailbox.sendImmediately((from, text))
        case None =>
          conn.write(errorMsg(s"unknown team member '$to'").render)
          notifyLead(s"[team] rejected message from '$from' to unknown member '$to'")

  /** A member's whole life: idle on the mailbox, and on each wake run turns until
    * the mailbox drains empty, then go idle and notify the lead. Cancellation (from
    * [[close]] or [[handleRetire]]) or a closed mailbox ends it. */
  private def runMember(m: MemberState, permits: UnboundedChannel[Unit])(using Async): Unit =
    try
      var alive = true
      while alive do
        m.mailbox.read() match
          case Left(_) => alive = false
          case Right(first) =>
            m.status = "working"
            broadcastUpdate(m)
            var batch = first :: drainMailbox(m.mailbox)
            var lastError: Option[String] = None
            var working = true
            while working do
              lastError = runOneTurn(m, batch, permits)
              val more = drainMailbox(m.mailbox)
              if more.nonEmpty then batch = more
              else working = false
            m.status = "idle"
            broadcastUpdate(m)
            lastError match
              case Some(err) => notifyLead(failureNotice(m.id, err))
              case None       => notifyLead(idleNotice(m.id))
    catch case _: CancellationException => ()

  /** Run one turn for `batch`, holding a concurrency permit for its duration.
    * Stores the grown history and final text (or the error text) back onto `m`;
    * returns the LLM error, if any, so the caller can pick the lead notice. */
  private def runOneTurn(
      m: MemberState,
      batch: List[(String, String)],
      permits: UnboundedChannel[Unit]
  )(using Async): Option[String] =
    permits.read()
    try
      given RuntimeContext = context
      val seeds = batch.map((from, text) => Message.user(s"[team message from $from] $text"))
      // Record what entered the history, at the point it entered it, so the
      // transcript shows the member being asked and not just its replies.
      batch.foreach((from, text) => emitReceived(m.id, from, text))
      val outcome = HeadlessAgent.runConversation(
        m.history ++ seeds,
        models,
        m.registry,
        memberPrompt(m.id, m.desc),
        onRound = (in, out) =>
          m.turnInputTokens = in
          m.turnOutputTokens = out
          emitTeam(),
        onActivity = a => emitActivity(m.id, a)
      )
      m.baseInputTokens += outcome.inputTokens
      m.baseOutputTokens += outcome.outputTokens
      m.turnInputTokens = 0
      m.turnOutputTokens = 0
      m.history = outcome.messages
      outcome.llmError match
        case Some(err) =>
          m.lastResponse = Some(err.description)
          Some(err.description)
        case None =>
          m.lastResponse = Some(outcome.finalText)
          None
    finally permits.sendImmediately(())

  /** Non-blocking drain of a member's mailbox (FIFO); stops on empty or close. */
  private def drainMailbox(mailbox: UnboundedChannel[(String, String)])(using Async): List[(String, String)] =
    val acc = List.newBuilder[(String, String)]
    var more = true
    while more do
      mailbox.readSource.poll() match
        case Some(Right(item)) => acc += item
        case _                 => more = false
    acc.result()

  private def emitActivity(memberId: String, a: HeadlessAgent.Activity): Unit =
    publish(memberId, WorkflowBridge.transcriptOf(a, "team", memberId))

  /** An inbound message, as a transcript event. Built here rather than mapped
    * from a [[HeadlessAgent.Activity]]: it is not the agent's own output. */
  private def emitReceived(memberId: String, from: String, text: String): Unit =
    publish(memberId, TranscriptEvent.Received("team", memberId, from, text))

  private def publish(memberId: String, ev: TranscriptEvent): Unit =
    onActivity(memberId, ev)
    logMember(memberId, WireMessage.Activity(ev))

  // Per-member transcript tee to `.auk/sessions/<session>/team/<member>.jsonl`,
  // the same WireMessage JSONL the dashboard consumes. Best-effort; `None`
  // (tests) disables it. The session id is read per-write so a later `/new` or
  // `/resume` files subsequent activity under the current session.
  // Encoding through `encodeDurable` (not `encode`) is what keeps an ephemeral
  // event off disk: it yields None for one, so this tee cannot persist a
  // `TranscriptEvent.ToolProgressed` even by mistake. See the contract on that
  // event — the enforcement has to live at the tees, outside the protocol module.
  private def logMember(memberId: String, m: WireMessage): Unit =
    sessionRef.foreach: ref =>
      val p = SessionRef.teamLog(context.resolve(SessionProvider.RelativePath), ref.id, memberId)
      WireCodec.encodeDurable(m).foreach(JsonlLog.append(p, _))
      ()

  private def validateNewMember(id: String, desc: String): Option[String] =
    if id.isEmpty || !id.matches("[A-Za-z0-9_-]+") then
      Some(s"invalid member id '$id': use only letters, digits, '-' and '_'")
    else if id == LeadId then
      Some("member id 'lead' is reserved for the main agent")
    else if members.get(id).exists(_.retired) then
      Some(s"member id '$id' belonged to a retired member; ids are permanent for the session, choose a new id")
    else if members.contains(id) then
      Some(s"duplicate member id '$id': a team member with this id already exists")
    else if desc.trim.isEmpty then
      Some("member description is empty")
    else None

  private def validateRetire(id: String): Option[String] =
    members.get(id) match
      case None                 => Some(s"unknown team member '$id'")
      case Some(m) if m.retired => Some(s"member '$id' has already been retired")
      case Some(_)              => None

  /** One member on the wire. `owner` is omitted for the session's own members, so
    * the common record is unchanged and a client can read "no owner" as absence. */
  private def memberRecord(m: MemberState): Json =
    val base = List(
      "id" -> Json.Str(m.id),
      "desc" -> Json.Str(m.desc),
      "status" -> Json.Str(m.status)
    ) ++ m.owner.map(o => "owner" -> Json.Str(o))
    val fields = m.lastResponse match
      case Some(r) => base :+ ("last" -> Json.Str(r))
      case None    => base
    Json.Obj(fields)

  private def rosterMsg(): Json =
    Json.Obj(List("t" -> Json.Str("roster"), "members" -> Json.Arr(members.valuesIterator.map(memberRecord).toList)))

  private def broadcastUpdate(m: MemberState): Unit =
    val line = Json.Obj(List("t" -> Json.Str("update"), "member" -> memberRecord(m))).render
    conns.keysIterator.foreach(_.write(line))
    emitTeam()

  /** Push a full roster snapshot to the TUI (`onTeam`): every member — retired ones
    * included, they stay on the roster — with its live status and cumulative tokens
    * (settled turns + the in-flight turn). */
  private def emitTeam(): Unit =
    val roster = members.valuesIterator.map { m =>
      TeamMemberView(
        id = m.id,
        desc = m.desc,
        working = m.status == "working",
        inputTokens = m.baseInputTokens + m.turnInputTokens,
        outputTokens = m.baseOutputTokens + m.turnOutputTokens,
        retired = m.retired
      )
    }.toVector
    onTeam(roster)

  def close()(using Async): Unit =
    // A retired member was cancelled and closed when it was retired, and its record
    // lives on only to be read: shutting it down again has nothing to shut down.
    val live = members.valuesIterator.filterNot(_.retired).toList
    live.foreach(m => if m.fiber != null then m.fiber.nn.cancel())
    live.foreach(_.repl.close())
    if server != null then server.nn.close()

object TeamBridge:
  /** The reserved id of the lead (the main agent). Never a member. One authority,
    * shared with the views that read it off the wire. */
  val LeadId: String = TranscriptEvent.LeadSender

  /** A per-process temp socket path for the bridge (the server unlinks any stale
    * file on bind). */
  def defaultSocketPath(): String =
    val os = scala.scalajs.js.Dynamic.global.require("node:os")
    val pid = scala.scalajs.js.Dynamic.global.process.pid
    s"${os.tmpdir().asInstanceOf[String]}/auk-team-$pid.sock"

  /** The system notice the lead receives when a member finishes its turn and goes
    * idle. Deliberately does NOT carry the member's response — the lead reads the
    * handle's `lastResponse` in a fresh eval when the content matters, keeping
    * long member replies out of every notice. */
  def idleNotice(id: String): String =
    s"Team member '$id' finished its turn and is now idle. When its response matters, read `team.getMember(\"$id\").lastResponse` in a fresh eval."

  /** The system notice the lead receives when a member's turn fails (an LLM
    * error). */
  def failureNotice(id: String, error: String): String =
    s"Team member '$id' failed its turn: $error"

  /** The system notice the lead receives when a member sends it a message. */
  def messageNotice(from: String, text: String): String =
    s"Team member '$from' sent you a message:\n$text"

  // -- message field accessors ------------------------------------------------

  private def field(j: Json, k: String): Option[String] = j match
    case o: Json.Obj => o.get(k).collect { case Json.Str(s) => s }
    case _           => None
  private def str(j: Json, k: String): String = field(j, k).getOrElse("")

  private def errorMsg(msg: String): Json =
    Json.Obj(List("t" -> Json.Str("error"), "msg" -> Json.Str(msg)))
