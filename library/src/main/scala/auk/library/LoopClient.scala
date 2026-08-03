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
  *                   re-evaluated and its checker registered in this session, plus
  *                   the configuration that source declares — see [[bound]]),
  *                   `reconfigure` (a data-only amendment naming only the fields it
  *                   changes), `park`, `resume`.
  *   host -> worker: `status` (a snapshot of every loop the host knows — where each
  *                   one stands and the lineage it has accepted; it REPLACES the
  *                   mirror, so a loop the host no longer has disappears from it),
  *                   `error` (the host refused an operation; when it names a loop,
  *                   that loop's mirrored status becomes [[LoopStatus.Failed]]).
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
  private val statuses = scala.collection.mutable.LinkedHashMap.empty[String, LoopStatus]
  // Each loop's accepted lineage, oldest first, as of the last snapshot. Beside the
  // statuses rather than inside them: a lineage is what a loop has ACHIEVED rather than
  // where it stands, and it reads the same whichever of the two a loop is in.
  private val lineages = scala.collection.mutable.HashMap.empty[String, List[LoopGen[js.Dynamic]]]
  // Whether the host has ever sent a snapshot. Until it has, an empty mirror means
  // "nothing has arrived yet" rather than "there is nothing" — and the two have to be
  // told apart, because the host sends its first snapshot on connect and the mirror
  // only advances between evals. So the eval that opens the connection is exactly the
  // one that must not conclude a loop does not exist.
  private var seeded = false
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
          // not linger here in the state it was last seen in. The `error` line that
          // follows such a removal names the loop again as [[LoopStatus.Failed]], which
          // is how a loop that never existed still explains itself.
          val arr = msg.loops.asInstanceOf[js.Array[js.Dynamic]]
          seeded = true
          statuses.clear()
          lineages.clear()
          var i = 0
          while i < arr.length do
            val entry = arr(i)
            val id = entry.id.asInstanceOf[String]
            statuses(id) = LoopClient.statusOf(entry.status)
            lineages(id) = LoopClient.generationsOf(entry.generations)
            i += 1
        case "error" =>
          // A refusal that names a loop is that loop's whole story — the host holds
          // no record of it — so it lands in the mirror where `status` can report it.
          val id = msg.loopId
          if id != null && !js.isUndefined(id) then
            statuses(id.asInstanceOf[String]) = LoopStatus.Failed(msg.msg.asInstanceOf[String])
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
      "budgets" -> budgetsJs(budgets),
      "artifactSchema" -> artifactSchema,
      "checkerRegistered" -> checkerRegistered
    )

  /** Acknowledge, from an attach-mode session, that the captured definition ran here
    * and bound its checker — the host's proof that the source validates.
    *
    * It carries the configuration the source declares as well as the acknowledgement,
    * because for two of the three attach-mode paths that is the ONLY way the host can
    * learn it. An amendment sends no `hello`, so its goal, rubric and budgets arrive
    * here; and when a later session re-validates a stored definition, the schema
    * derived HERE — from the definition as it compiles today — is what the host
    * compares against the one its ledger recorded, which is what catches an artifact
    * type that has changed underneath a loop. */
  def bound(
      loopId: String,
      goal: String,
      rubric: String,
      budgets: LoopBudgets,
      artifactSchema: String
  ): Unit =
    send(
      "t" -> "bound",
      "loopId" -> loopId,
      "goal" -> goal,
      "rubric" -> rubric,
      "budgets" -> budgetsJs(budgets),
      "artifactSchema" -> artifactSchema
    )

  /** Retune a live loop without touching its checker. Only the fields named here are
    * sent, and only those the host overlays — an absent field keeps whatever the loop
    * has, which is why this cannot be one plain "here is the new configuration" call. */
  def reconfigure(
      loopId: String,
      goal: Option[String],
      rubric: Option[String],
      budgets: Option[LoopBudgets]
  ): Unit =
    val pairs = List.newBuilder[(String, js.Any)]
    pairs += "t" -> "reconfigure"
    pairs += "loopId" -> loopId
    goal.foreach(g => pairs += "goal" -> g)
    rubric.foreach(r => pairs += "rubric" -> r)
    budgets.foreach(b => pairs += "budgets" -> budgetsJs(b))
    send(pairs.result()*)

  private def budgetsJs(budgets: LoopBudgets): js.Any =
    LibToolInput.jsObj(
      "maxGenerations" -> budgets.maxGenerations,
      "patience" -> budgets.patience,
      "maxAttemptsPerGeneration" -> budgets.maxAttemptsPerGeneration
    )

  def park(loopId: String): Unit = send("t" -> "park", "loopId" -> loopId)

  def resume(loopId: String): Unit = send("t" -> "resume", "loopId" -> loopId)

  /** Record a status locally so the eval that started a loop can already read it back,
    * before the host's first `status` snapshot arrives. Never overwrites a status the
    * host has reported. */
  def echo(loopId: String, status: LoopStatus): Unit =
    if !statuses.contains(loopId) then statuses(loopId) = status

  /** The mirrored status of `loopId`, or `None` if this worker has not seen it. */
  def status(loopId: String): Option[LoopStatus] = statuses.get(loopId)

  /** The mirrored lineage of `loopId`, oldest first; empty for a loop that has accepted
    * nothing, and for one this worker has not seen. */
  def generations(loopId: String): List[LoopGen[js.Dynamic]] = lineages.getOrElse(loopId, Nil)

  /** Whether the host has described the world to this worker yet. An empty mirror is
    * only evidence of a loop's absence once this is true. */
  def informed: Boolean = seeded

  /** Every mirrored loop's id, in the order they were first seen. */
  def ids: List[String] = statuses.keys.toList

  def close(): Unit =
    if !closed then
      closed = true
      try socket.end() catch case _: Throwable => ()

/** Reading the host's half of the wire. Everything here is total: a snapshot that says
  * something this worker does not understand costs one field rather than the whole
  * mirror, since a client that dropped a line would be a client whose loops silently
  * vanish. */
private[library] object LoopClient:
  /** One loop's status object, `{"kind":…}` — [[LoopStatus.Unknown]] for a kind this
    * worker has no case for, which keeps the loop itself listed and reachable. */
  def statusOf(v: js.Dynamic): LoopStatus =
    if v == null || js.isUndefined(v) then LoopStatus.Unknown
    else
      str(v.kind) match
        case "validating" => LoopStatus.Validating
        case "adopting"   => LoopStatus.Adopting
        case "running"    => LoopStatus.Running(num(v.gen).toInt)
        case "parked"     => LoopStatus.Parked(reasonOf(v.reason))
        case "orphaned"   => LoopStatus.Orphaned
        case _            => LoopStatus.Unknown

  /** A park reason, `{"kind":…,"detail":…}`. An unrecognised one is reported as the
    * anomaly it is rather than dropped: a loop that stopped for a reason this worker
    * cannot name has still stopped. */
  private def reasonOf(v: js.Dynamic): ParkReason =
    val kind = if v == null || js.isUndefined(v) then "" else str(v.kind)
    val detail = if v == null || js.isUndefined(v) then "" else str(v.detail)
    kind match
      case "goal_reached"       => ParkReason.GoalReached
      case "budget_exhausted"   => ParkReason.BudgetExhausted
      case "patience_exhausted" => ParkReason.PatienceExhausted
      case "user_requested"     => ParkReason.UserRequested
      case "api_failure"        => ParkReason.ApiFailure(detail)
      case "anomaly"            => ParkReason.Anomaly(detail)
      case other                => ParkReason.Anomaly(if other.isEmpty then "the host gave no reason" else other)

  /** One loop's accepted lineage, oldest first. The artifact is carried through as the
    * raw JSON value it arrived as: this worker does not know the loop's artifact type —
    * it may not even hold its definition — so decoding is the reader's business. */
  def generationsOf(v: js.Dynamic): List[LoopGen[js.Dynamic]] =
    if !js.Array.isArray(v) then Nil
    else
      v.asInstanceOf[js.Array[js.Dynamic]].toList.map: g =>
        LoopGen(
          gen = num(g.gen).toInt,
          artifact = g.artifact,
          description = str(g.description),
          commit = str(g.commit),
          metrics = metricsOf(g.metrics)
        )

  private def str(v: js.Dynamic): String =
    if js.typeOf(v) == "string" then v.asInstanceOf[String] else ""

  private def num(v: js.Dynamic): Double =
    if js.typeOf(v) == "number" then v.asInstanceOf[Double] else 0.0

  private def metricsOf(v: js.Dynamic): Map[String, Double] =
    if v == null || js.isUndefined(v) || js.typeOf(v) != "object" then Map.empty
    else
      js.Object
        .keys(v.asInstanceOf[js.Object])
        .toList
        .flatMap: key =>
          val value = v.selectDynamic(key)
          if js.typeOf(value) == "number" then Some(key -> value.asInstanceOf[Double]) else None
        .toMap
