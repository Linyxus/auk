package auk.library

import scala.scalajs.js

/** The worker side of the agent-team bridge: a persistent Unix-domain-socket
  * client to the host's `TeamBridge`. Unlike [[WorkflowClient]] (one connection
  * per `wf.start` run), there is exactly ONE `TeamClient` per worker process — the
  * lead worker and every member worker each open one, bound to their own identity.
  *
  * The host owns the members (it runs their sub-agents, tools, and LLM endpoints);
  * this client only announces its identity, forwards the lead's create/message
  * operations, and keeps a local **roster mirror** the DSL reads synchronously.
  *
  * Wire format: one JSON object per line, `t` tagging the kind.
  *   worker -> host: `hello` (announce identity first), `new_member` (lead only),
  *                   `send` (async message to `lead` or a member id).
  *   host -> worker: `roster` (full snapshot, reply to `hello`, creation order),
  *                   `update` (upsert one member record — broadcast on any change),
  *                   `error` (host rejected the last op; ignored — the DSL's own
  *                   guards are primary).
  *
  * The socket path comes from `AUK_TEAM_SOCK` (injected by the host). The mirror
  * only advances while the worker is idle between evals — the event loop services
  * this socket then — so DSL reads see a snapshot that refreshes *between* evals,
  * never within one. Writes issued before the connection completes are buffered by
  * Node, so `hello` (sent from the constructor) and a same-eval `new_member` are safe.
  */
private[library] final class TeamClient(sockPath: String, me: String):
  import TeamClient.MemberRecord

  private def net: js.Dynamic = js.Dynamic.global.require("node:net")

  // Insertion order is the creation order: the host sends roster/updates in order,
  // and same-eval local echoes append in call order, so a LinkedHashMap preserves it.
  private val members = scala.collection.mutable.LinkedHashMap.empty[String, MemberRecord]
  private var buffer = ""
  private var closed = false
  // Set once on socket error/close; every later write rethrows it so operations fail
  // with a clear reason rather than silently no-op'ing against a dead socket.
  private var closeMsg = "team socket closed"

  private val socket: js.Dynamic = net.connect(sockPath).asInstanceOf[js.Dynamic]
  socket.setEncoding("utf8")
  socket.on("data", ((chunk: js.Any) => onData(chunk.asInstanceOf[String])): js.Function1[js.Any, Unit])
  socket.on("error", ((e: js.Any) => markClosed(s"team socket closed: $e")): js.Function1[js.Any, Unit])
  socket.on("close", ((_: js.Any) => markClosed("team socket closed: the host closed the connection")): js.Function1[js.Any, Unit])

  // Announce identity as the very first line so the host binds this connection
  // before any operation and replies with the roster.
  hello()

  private def onData(chunk: String): Unit =
    buffer += chunk
    var idx = buffer.indexOf("\n")
    while idx >= 0 do
      val line = buffer.substring(0, idx)
      buffer = buffer.substring(idx + 1)
      if line.nonEmpty then handleLine(line)
      idx = buffer.indexOf("\n")

  private def handleLine(line: String): Unit =
    try
      val msg = js.JSON.parse(line)
      msg.t.asInstanceOf[String] match
        case "roster" =>
          val arr = msg.members.asInstanceOf[js.Array[js.Dynamic]]
          var i = 0
          while i < arr.length do
            upsert(parseRecord(arr(i)))
            i += 1
        case "update" =>
          upsert(parseRecord(msg.member.asInstanceOf[js.Dynamic]))
        case _ => () // `error` and anything unknown: ignore (DSL guards are primary)
    catch case _: Throwable => () // ignore malformed lines

  private def parseRecord(m: js.Dynamic): MemberRecord =
    val id = m.id.asInstanceOf[String]
    val desc = m.desc.asInstanceOf[String]
    val status = m.status.asInstanceOf[String]
    val lastRaw = m.last
    val last =
      if lastRaw == null || js.isUndefined(lastRaw) then None
      else Some(lastRaw.asInstanceOf[String])
    MemberRecord(id, desc, status, last)

  // Upsert keyed by id: LinkedHashMap keeps an existing key's original position, so
  // a host `update` refreshes a member in place without reordering the roster.
  private def upsert(rec: MemberRecord): Unit =
    members(rec.id) = rec

  private def markClosed(msg: String): Unit =
    if !closed then
      closed = true
      closeMsg = msg

  private def send(pairs: (String, js.Any)*): Unit =
    if closed then throw new RuntimeException(closeMsg)
    socket.write(js.JSON.stringify(LibToolInput.jsObj(pairs*)) + "\n")

  /** Announce this connection's identity as the first message so the host binds it
    * (and answers with the full `roster`). Re-sent on reconnect after a restart. */
  def hello(): Unit = send("t" -> "hello", "me" -> me)

  /** Ask the host to create a new member. Lead-only and validated by the DSL before
    * this is called; the host validates again as defense-in-depth. */
  def newMember(id: String, desc: String): Unit =
    send("t" -> "new_member", "id" -> id, "desc" -> desc)

  /** Fire-and-forget a message to `to` (`"lead"` or a member id). `from` is the
    * host-resolved hello identity, never trusted from the payload. */
  def sendMessage(to: String, text: String): Unit =
    send("t" -> "send", "to" -> to, "text" -> text)

  /** Record a just-created member in the mirror immediately (idle, no last response),
    * so a same-eval `getMember`/`listMembers` sees it before the host's `update`
    * arrives. A no-op if the id is already mirrored. */
  def echo(id: String, desc: String): Unit =
    if !members.contains(id) then members(id) = MemberRecord(id, desc, "idle", None)

  /** The mirrored record for `id`, or `None` if this worker has not seen it. */
  def get(id: String): Option[MemberRecord] = members.get(id)

  /** Every mirrored member, in creation order. Excludes the lead (synthesized by
    * [[Team]], never present in the wire roster). */
  def snapshot: List[MemberRecord] = members.values.toList

  def close(): Unit =
    if !closed then
      closed = true
      try socket.end() catch case _: Throwable => ()

private[library] object TeamClient:
  /** One member as mirrored from the host. `last` is `None` until the member has
    * completed its first turn (the wire omits the field entirely until then). */
  final case class MemberRecord(id: String, desc: String, status: String, last: Option[String])
