package auk.library

import scala.scalajs.js

/** The worker side of the refinement-loop bridge: a persistent Unix-domain-socket
  * client to the host's `LoopBridge`. Like [[TeamClient]] there is at most ONE per
  * worker process, opened lazily the first time `lib.loop` needs the host.
  *
  * The host owns the loops (their ledgers, their generations, their agents); this
  * client submits a definition, forwards park/resume, and keeps a local **status
  * mirror** the DSL reads synchronously.
  *
  * Wire format: one JSON object per line, `t` tagging the kind.
  *   worker -> host: `hello` (a new loop's whole configuration — the definition's
  *                   source follows out-of-band, captured from the eval),
  *                   `bound` (attach-mode acknowledgement: the captured source was
  *                   re-evaluated and its checker registered in this session),
  *                   `park`, `resume`.
  *   host -> worker: `status` (a snapshot of every loop the host knows, as
  *                   id → phase; it REPLACES the mirror, so a loop the host no
  *                   longer has disappears from it), `error` (the host refused an
  *                   operation; when it names a loop, that loop's mirrored phase
  *                   becomes `failed: <msg>`).
  *
  * The socket path comes from `AUK_LOOP_SOCK` (injected by the host). As with the
  * team mirror, it only advances while the worker is idle between evals — the event
  * loop services this socket then — so DSL reads see a snapshot that refreshes
  * *between* evals, never within one. Writes issued before the connection completes
  * are buffered by Node, so a `hello` sent from the eval that starts a loop is safe.
  */
private[library] final class LoopClient(sockPath: String):

  private def net: js.Dynamic = js.Dynamic.global.require("node:net")

  // Insertion order is the order loops were first heard of; the host sends its
  // snapshots in creation order, so a LinkedHashMap preserves it.
  private val phases = scala.collection.mutable.LinkedHashMap.empty[String, String]
  private var buffer = ""
  private var closed = false
  private var closeMsg = "loop socket closed"

  private val socket: js.Dynamic = net.connect(sockPath).asInstanceOf[js.Dynamic]
  socket.setEncoding("utf8")
  socket.on("data", ((chunk: js.Any) => onData(chunk.asInstanceOf[String])): js.Function1[js.Any, Unit])
  socket.on("error", ((e: js.Any) => markClosed(s"loop socket closed: $e")): js.Function1[js.Any, Unit])
  socket.on("close", ((_: js.Any) => markClosed("loop socket closed: the host closed the connection")): js.Function1[js.Any, Unit])

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
        case "status" =>
          // A snapshot REPLACES the mirror rather than merging into it: it is the
          // host's whole truth, so a loop it does not name is one the host does not
          // have — a definition that failed to validate never became a loop, and must
          // not linger here in the phase it was last seen in. The `error` line that
          // follows such a removal names the loop again as `failed: <msg>`, which is
          // how a loop that never existed still explains itself.
          val arr = msg.loops.asInstanceOf[js.Array[js.Dynamic]]
          phases.clear()
          var i = 0
          while i < arr.length do
            val entry = arr(i)
            phases(entry.id.asInstanceOf[String]) = entry.phase.asInstanceOf[String]
            i += 1
        case "error" =>
          // A refusal that names a loop is that loop's whole story — the host holds
          // no record of it — so it lands in the mirror where `status` can report it.
          val id = msg.loopId
          if id != null && !js.isUndefined(id) then
            phases(id.asInstanceOf[String]) = s"failed: ${msg.msg.asInstanceOf[String]}"
        case _ => ()
    catch case _: Throwable => () // ignore malformed lines

  private def markClosed(msg: String): Unit =
    if !closed then
      closed = true
      closeMsg = msg

  private def send(pairs: (String, js.Any)*): Unit =
    if closed then throw new RuntimeException(closeMsg)
    socket.write(js.JSON.stringify(LibToolInput.jsObj(pairs*)) + "\n")

  /** Submit a new loop's configuration. The definition's source is NOT sent here: the
    * host captures the whole eval that called `loop.start` once it completes, which is
    * strictly after this message. */
  def hello(
      loopId: String,
      goal: String,
      rubric: String,
      budgets: LoopBudgets,
      artifactSchema: String,
      checkerRegistered: Boolean
  ): Unit =
    send(
      "t" -> "hello",
      "loopId" -> loopId,
      "goal" -> goal,
      "rubric" -> rubric,
      "budgets" -> LibToolInput.jsObj(
        "maxGenerations" -> budgets.maxGenerations,
        "patience" -> budgets.patience,
        "maxAttemptsPerGeneration" -> budgets.maxAttemptsPerGeneration
      ),
      "artifactSchema" -> artifactSchema,
      "checkerRegistered" -> checkerRegistered
    )

  /** Acknowledge, from an attach-mode session, that the captured definition ran here
    * and bound its checker — the host's proof that the source validates. */
  def bound(loopId: String): Unit = send("t" -> "bound", "loopId" -> loopId)

  def park(loopId: String): Unit = send("t" -> "park", "loopId" -> loopId)

  def resume(loopId: String): Unit = send("t" -> "resume", "loopId" -> loopId)

  /** Record a phase locally so the eval that started a loop can already read it back,
    * before the host's first `status` snapshot arrives. Never overwrites a phase the
    * host has reported. */
  def echo(loopId: String, phase: String): Unit =
    if !phases.contains(loopId) then phases(loopId) = phase

  /** The mirrored phase of `loopId`, or `None` if this worker has not seen it. */
  def phase(loopId: String): Option[String] = phases.get(loopId)

  /** Every mirrored loop as `(id, phase)`, in the order they were first seen. */
  def snapshot: List[(String, String)] = phases.toList

  def close(): Unit =
    if !closed then
      closed = true
      try socket.end() catch case _: Throwable => ()
