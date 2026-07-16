package auk.session

import auk.platform.PathOps

/** A mutable holder for the id of the session that is *currently* live.
  *
  * The parent session id is not in scope where sub-agents run — the workflow
  * bridge and the `sub_agent` tool sit below the [[auk.agent.Engine]] — and the
  * live session can change at runtime (`/new`, `/resume`). This shared, volatile
  * holder bridges that gap so those sites file their logs under the right
  * session: the engine [[set]]s it on every switch, and the sub-agent writers read
  * [[id]] when a node starts.
  */
final class SessionRef(initial: String):
  @volatile private var current: String = initial
  def id: String = current
  def set(id: String): Unit = current = id

object SessionRef:
  /** Path of a sub-agent's log under its parent session,
    * `<sessionsDir>/<sessionId>/subagents/<subId>.jsonl`. `sessionsDir` is the
    * resolved [[SessionProvider.RelativePath]] (`.auk/sessions`). */
  def subagentLog(sessionsDir: String, sessionId: String, subId: String): String =
    val dir = PathOps.join(PathOps.join(sessionsDir, sessionId), "subagents")
    PathOps.join(dir, subId + ".jsonl")

  /** Path of a team member's transcript log under its parent session,
    * `<sessionsDir>/<sessionId>/team/<memberId>.jsonl`. `sessionsDir` is the
    * resolved [[SessionProvider.RelativePath]] (`.auk/sessions`). */
  def teamLog(sessionsDir: String, sessionId: String, memberId: String): String =
    val dir = PathOps.join(PathOps.join(sessionsDir, sessionId), "team")
    PathOps.join(dir, memberId + ".jsonl")
