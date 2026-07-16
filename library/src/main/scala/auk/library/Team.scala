package auk.library

import scala.scalajs.js

/** A team member's status, as observed through the roster mirror.
  *
  *   - [[MemberStatus.Idle]]: the member has finished its last turn and is waiting
  *     for the next message.
  *   - [[MemberStatus.Working]]: the member is currently running a turn.
  *   - [[MemberStatus.Lead]]: not tracked — this is the lead handle, which has no
  *     working/idle state of its own.
  */
enum MemberStatus:
  case Idle, Working, Lead

/** A handle to one participant in the agent team — either a member you created (or
  * were told about) or the lead. Handles are thin: they hold only the id and read
  * the rest through the shared roster mirror at call time, so a handle stashed in a
  * `val` reflects the member's *current* [[status]] and [[lastResponse]] in a later
  * eval (the mirror refreshes between evals — see [[Team]]).
  */
final class Member private[library] (val id: String, team: Team):
  /** This member's one-line description (what it is responsible for). For the lead
    * handle, a fixed label. */
  def description: String =
    if id == Team.LeadId then Team.LeadDescription
    else team.recordOf(id).map(_.desc).getOrElse("")

  /** This member's current [[MemberStatus]] as of the last mirror refresh: `Working`
    * while it runs a turn, `Idle` when waiting, `Lead` for the lead handle. Reflects
    * the state observed *between* evals, so do not spin on it inside one eval. */
  def status: MemberStatus =
    if id == Team.LeadId then MemberStatus.Lead
    else
      team.recordOf(id).map(_.status) match
        case Some("working") => MemberStatus.Working
        case _               => MemberStatus.Idle

  /** This member's final message from its most recently completed turn. The idle
    * system notice already delivers this to you automatically when the turn ends;
    * this accessor is for reading it again later. Throws
    * [[IllegalStateException]] if this is the lead handle, or if the member has not
    * completed a turn yet. */
  def lastResponse: String =
    if id == Team.LeadId then
      throw new IllegalStateException("the lead's responses are not tracked")
    else
      team.recordOf(id).flatMap(_.last) match
        case Some(r) => r
        case None    => throw new IllegalStateException(s"member '$id' has not completed a turn yet")

  /** Send this member a message, asynchronously. Returns immediately — the member
    * runs the message on its own; you do NOT await a reply here. When it finishes
    * the turn it goes idle and you receive a system notice carrying its response.
    * Throws [[IllegalArgumentException]] if the target is yourself or the text is
    * empty. */
  def sendMessage(text: String): Unit =
    val c = team.conn
    if id == team.self then throw new IllegalArgumentException("cannot send a message to yourself")
    if text == null || text.trim.isEmpty then throw new IllegalArgumentException("message is empty")
    c.sendMessage(id, text)

  override def toString: String =
    val label = status match
      case MemberStatus.Idle    => "idle"
      case MemberStatus.Working => "working"
      case MemberStatus.Lead    => "lead"
    s"Member($id: $description, $label)"

/** The agent-team entry point, reached as `team` in scope. See [[AukInterface.team]]
  * for the model-facing overview.
  *
  * The team is a set of long-lived agents. The **lead** (the main agent) creates
  * **members** with [[newMember]]; everyone exchanges asynchronous messages via
  * [[Member.sendMessage]]. Nothing here blocks: sends are fire-and-forget, and a
  * member's reply arrives on its own as a system notice when it finishes its turn.
  *
  * Reads go through a local roster mirror the host pushes to this worker. The mirror
  * advances only *between* evals (the worker services the socket while idle), so a
  * member's [[Member.status]]/[[Member.lastResponse]] reflect the last refresh —
  * observe changes in a *later* eval, never by looping inside one.
  *
  * Construction is inert (it reads no environment and opens no socket); the socket
  * to the host is opened lazily on the first operation, and only the lead worker and
  * member workers have one. In a context with no team (a `sub_agent` REPL, a
  * workflow node), any operation throws a clear "team unavailable" error.
  */
final class Team private[library] ():
  // Lazy so construction touches no environment: the REPL preamble builds a Team in
  // every worker, including ones with no team socket. `selfId` reads AUK_TEAM_ID (no
  // socket needed); forcing `client` opens the connection or throws "team unavailable".
  private lazy val selfId: String = Team.resolveSelfId()
  private lazy val client: TeamClient = Team.connect(selfId)

  private def leadHandle: Member = new Member(Team.LeadId, this)

  /** Create a new team member (LEAD ONLY) and return a handle to it. The member is a
    * fresh, long-lived agent identified by `id` with the given `description`; it
    * starts idle and runs whenever it is sent a message. Send it its first task with
    * `handle.sendMessage(...)`.
    *
    * `id` must be non-empty and use only letters, digits, `-` and `_`; it is
    * permanent for the session, so choose a short, stable name. Throws:
    *   - [[IllegalStateException]] if you are not the lead;
    *   - [[IllegalArgumentException]] for an invalid id, the reserved id `"lead"`, a
    *     duplicate id, or an empty description. */
  def newMember(id: String, description: String): Member =
    val c = client // availability check + ensures the mirror is being fed
    if selfId != Team.LeadId then
      throw new IllegalStateException("only the lead can create team members")
    if id == null || !id.matches("[A-Za-z0-9_-]+") then
      throw new IllegalArgumentException(s"invalid member id '$id': use only letters, digits, '-' and '_'")
    if id == Team.LeadId then
      throw new IllegalArgumentException("member id 'lead' is reserved for the main agent")
    if c.get(id).isDefined then
      throw new IllegalArgumentException(s"duplicate member id '$id': a team member with this id already exists")
    if description == null || description.trim.isEmpty then
      throw new IllegalArgumentException("member description is empty")
    c.newMember(id, description)
    c.echo(id, description) // local echo so a same-eval getMember/listMembers sees it
    new Member(id, this)

  /** The handle for `id`. `"lead"` resolves to the lead handle. Throws
    * [[IllegalArgumentException]] for an unknown member id. */
  def getMember(id: String): Member =
    val c = client
    if id == Team.LeadId then leadHandle
    else
      c.get(id) match
        case Some(_) => new Member(id, this)
        case None    => throw new IllegalArgumentException(s"unknown team member '$id'; team.listMembers lists the roster")

  /** Every participant: the lead first, then the members in creation order. */
  def listMembers: List[Member] =
    val c = client
    leadHandle :: c.snapshot.map(r => new Member(r.id, this))

  /** The lead handle, for a member to message the lead
    * (`team.lead.sendMessage(...)`). Throws [[IllegalStateException]] if you ARE the
    * lead (you would be messaging yourself). */
  def lead: Member =
    client // availability check
    if selfId == Team.LeadId then throw new IllegalStateException("you are the lead")
    leadHandle

  // -- package-private accessors for Member handles --------------------------------
  private[library] def self: String = selfId
  private[library] def conn: TeamClient = client
  private[library] def recordOf(id: String): Option[TeamClient.MemberRecord] = client.get(id)

private[library] object Team:
  /** The reserved identity of the lead (the main agent). */
  val LeadId: String = "lead"
  /** The lead's fixed description (the lead has no wire roster record). */
  val LeadDescription: String = "the team lead (the main agent)"

  private def env: js.Dynamic = js.Dynamic.global.process.env

  /** This worker's own identity: `AUK_TEAM_ID` if the host injected one (member
    * workers), else the lead. */
  def resolveSelfId(): String =
    val id = env.AUK_TEAM_ID
    if id == null || js.isUndefined(id) then LeadId else id.asInstanceOf[String]

  /** Open the persistent connection to the host bridge using `AUK_TEAM_SOCK`, or
    * fail clearly when there is no team (e.g. a `sub_agent` or workflow-node REPL). */
  def connect(me: String): TeamClient =
    val sock = env.AUK_TEAM_SOCK
    if sock == null || js.isUndefined(sock) then
      throw new RuntimeException(
        "the agent team is unavailable: AUK_TEAM_SOCK is not set (the host team bridge is not connected)")
    new TeamClient(sock.asInstanceOf[String], me)
