package auk.library

import scala.scalajs.js

/** Implementation of [[Member]]: a thin handle holding the id and reading through
  * the [[TeamImpl]] roster mirror at call time. See [[Member]] for the contract. */
private[library] final class MemberImpl(val id: String, team: TeamImpl) extends Member:
  def description: String =
    if id == TeamImpl.LeadId then TeamImpl.LeadDescription
    else team.recordOf(id).map(_.desc).getOrElse("")

  def status: MemberStatus =
    if id == TeamImpl.LeadId then MemberStatus.Lead
    else
      team.recordOf(id).map(_.status) match
        case Some("working") => MemberStatus.Working
        case Some("retired") => MemberStatus.Retired
        case _               => MemberStatus.Idle

  def lastResponse: String =
    if id == TeamImpl.LeadId then
      throw new IllegalStateException("the lead's responses are not tracked")
    else
      team.recordOf(id).flatMap(_.last) match
        case Some(r) => r
        case None    => throw new IllegalStateException(s"member '$id' has not completed a turn yet")

  def sendMessage(text: String): Unit =
    val c = team.conn
    if id == team.self then throw new IllegalArgumentException("cannot send a message to yourself")
    if status == MemberStatus.Retired then
      throw new IllegalStateException(s"member '$id' has been retired and can no longer be messaged")
    if text == null || text.trim.isEmpty then throw new IllegalArgumentException("message is empty")
    c.sendMessage(id, text)

  def retire(): Unit =
    val c = team.conn // availability check + ensures the mirror is being fed
    if id == TeamImpl.LeadId then throw new IllegalStateException("the lead cannot be retired")
    if team.self != TeamImpl.LeadId then
      throw new IllegalStateException("only the lead can retire team members")
    if status == MemberStatus.Retired then
      throw new IllegalStateException(s"member '$id' has already been retired")
    c.retire(id)
    c.echoRetire(id) // local echo so a same-eval status/sendMessage sees it

  override def toString: String =
    val label = status match
      case MemberStatus.Idle    => "idle"
      case MemberStatus.Working => "working"
      case MemberStatus.Retired => "retired"
      case MemberStatus.Lead    => "lead"
    s"Member($id: $description, $label)"

/** Implementation of [[Team]]. Construction is inert (reads no environment, opens no
  * socket): the REPL preamble builds one in every worker, including those with no
  * team socket. `selfId` reads `AUK_TEAM_ID` (no socket needed); forcing `client`
  * opens the connection or throws "team unavailable". See [[Team]] for the contract. */
private[library] final class TeamImpl extends Team:
  private lazy val selfId: String = TeamImpl.resolveSelfId()
  private lazy val client: TeamClient = TeamImpl.connect(selfId)

  private def leadHandle: Member = new MemberImpl(TeamImpl.LeadId, this)

  def newMember(id: String, description: String): Member =
    val c = client // availability check + ensures the mirror is being fed
    if selfId != TeamImpl.LeadId then
      throw new IllegalStateException("only the lead can create team members")
    if id == null || !id.matches("[A-Za-z0-9_-]+") then
      throw new IllegalArgumentException(s"invalid member id '$id': use only letters, digits, '-' and '_'")
    if id == TeamImpl.LeadId then
      throw new IllegalArgumentException("member id 'lead' is reserved for the main agent")
    if c.get(id).isDefined then
      throw new IllegalArgumentException(s"duplicate member id '$id': a team member with this id already exists")
    if description == null || description.trim.isEmpty then
      throw new IllegalArgumentException("member description is empty")
    c.newMember(id, description)
    c.echo(id, description) // local echo so a same-eval getMember/listMembers sees it
    new MemberImpl(id, this)

  def getMember(id: String): Member =
    val c = client
    if id == TeamImpl.LeadId then leadHandle
    else
      c.get(id) match
        case Some(_) => new MemberImpl(id, this)
        case None    => throw new IllegalArgumentException(s"unknown team member '$id'; team.listMembers lists the roster")

  def listMembers: List[Member] =
    val c = client
    leadHandle :: c.snapshot.map(r => new MemberImpl(r.id, this))

  def lead: Member =
    client // availability check
    if selfId == TeamImpl.LeadId then throw new IllegalStateException("you are the lead")
    leadHandle

  // -- package-private accessors for MemberImpl handles ----------------------------
  private[library] def self: String = selfId
  private[library] def conn: TeamClient = client
  private[library] def recordOf(id: String): Option[TeamClient.MemberRecord] = client.get(id)

private[library] object TeamImpl:
  /** The reserved identity of the lead (the main agent). */
  val LeadId: String = "lead"
  /** The lead's fixed description (the lead has no wire roster record). */
  val LeadDescription: String = "the team lead (the main agent)"

  private def env: js.Dynamic = js.Dynamic.global.process.env

  // This worker's own identity: AUK_TEAM_ID if the host injected one (member
  // workers), else the lead.
  def resolveSelfId(): String =
    val id = env.AUK_TEAM_ID
    if id == null || js.isUndefined(id) then LeadId else id.asInstanceOf[String]

  // Open the persistent connection using AUK_TEAM_SOCK, or fail clearly when there
  // is no team (e.g. a workflow-node REPL).
  def connect(me: String): TeamClient =
    val sock = env.AUK_TEAM_SOCK
    if sock == null || js.isUndefined(sock) then
      throw new RuntimeException(
        "the agent team is unavailable: AUK_TEAM_SOCK is not set (the host team bridge is not connected)")
    new TeamClient(sock.asInstanceOf[String], me)
