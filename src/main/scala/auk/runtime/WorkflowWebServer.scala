package auk.runtime

import scala.collection.mutable.LinkedHashMap
import scala.scalajs.js

import auk.llm.tools.RuntimeContext
import auk.loop.{LoopHistory, LoopStore}
import auk.platform.js.{WebServer, WebUiAssets}
import auk.session.{SessionProvider, SessionRef}
import auk.snapshot.Snapshot
import auk.workflow.{Forest, LoopWire, Transcript, TranscriptEvent, WireCodec, WireMessage}

/** The host side of the live workflow dashboard: lazily starts a [[WebServer]] on
  * a spare port when the first workflow event arrives, serves the prebuilt webui
  * bundle, and streams the forest + each sub-agent's transcript to connected
  * browsers over SSE.
  *
  * A browser that connects mid-run gets a forest [[WireMessage.Snapshot]] plus
  * every stored [[Transcript]] replayed as activity, so it converges to the live
  * state. All folds use the shared `workflow-protocol` types, so the host and the
  * browser can never drift.
  *
  * Refinement loops ride the same server. They differ from workflows in two ways
  * that shape everything below: a loop is sent whole rather than as deltas (it
  * changes a handful of times per generation, so there is nothing a delta protocol
  * would save), and a loop outlives the session that started it — so the loops a
  * browser sees are this session's own PLUS whatever the project's `.auk/loops`
  * holds, read off disk on startup and on every connect. Their transcripts are
  * ordinary activity frames keyed `runId = loopId`, which is why they share the
  * workflow transcript store instead of having one of their own.
  *
  * `loopContext` is the seam to all of that: without it the server is workflows
  * only, and with it the loop store, the git working directory a diff is computed
  * in, and the session directory holding transcript tees are all derived from the
  * one value.
  *
  * Every method is plain (no Gears `Async`): the server runs on the JS event loop
  * and all state is folded/broadcast synchronously on the single JS thread, so the
  * mutable maps and client set need no locking. The on-demand API is synchronous
  * for the same reason — git and the filesystem are reached through blocking Node
  * calls — though it answers through a callback, so a payload that one day needs to
  * be awaited can be without changing the transport. */
final class WorkflowWebServer(
    onStarted: String => Unit,
    onError: String => Unit,
    heartbeatMs: Int = WorkflowWebServer.HeartbeatMs,
    onControl: (String, String) => Unit = (_, _) => (),
    loopContext: Option[RuntimeContext] = None
):
  private enum State:
    case Disabled, Starting, Failed
    case Running(port: Int)

  private var state: State = State.Disabled
  private var handle: WebServer.Handle | Null = null
  private val forests = LinkedHashMap.empty[String, Forest]
  private val transcripts = LinkedHashMap.empty[String, LinkedHashMap[String, Transcript]]
  private val loops = LinkedHashMap.empty[String, LoopWire]
  private val clients = scala.collection.mutable.Set.empty[WebServer.Sse]
  private val loopStore: Option[LoopStore] = loopContext.map(LoopStore.in)

  /** Start the server on the first call (idempotent). Resolves the bundle dir; on
    * failure surfaces the reason once via `onError` and stays disabled. */
  def ensureStarted(): Unit = state match
    case State.Disabled =>
      WebUiAssets.resolve() match
        case Left(msg) =>
          state = State.Failed
          onError(msg)
        case Right(dir) =>
          state = State.Starting // guard re-entry before the async listen callback fires
          rescanLoops()
          handle = WebServer.serve(
            dir,
            "/events",
            port => { state = State.Running(port); onStarted(s"http://localhost:$port") },
            heartbeatMs,
            controlPath = "/control",
            onControl = handleControl,
            apiPath = WorkflowWebServer.ApiPath,
            onApi = handleApi
          )(onClient)
    case _ => ()

  /** Parse a `{ "action": "pause"|"resume", "runId": "…" }` control body from the
    * browser and forward it; malformed bodies are ignored. */
  private def handleControl(body: String): Unit =
    try
      val d = js.JSON.parse(body)
      val action = if js.typeOf(d.action) == "string" then d.action.asInstanceOf[String] else ""
      val runId = if js.typeOf(d.runId) == "string" then d.runId.asInstanceOf[String] else ""
      if action.nonEmpty && runId.nonEmpty then onControl(action, runId)
    catch case _: Throwable => ()

  /** Fold an event into the dashboard state and broadcast it to every client.
    * Folding happens whether or not the server is up yet, so the snapshot is ready
    * when a client connects. A `Snapshot` is connect-only and must never be passed
    * here. */
  def publish(m: WireMessage): Unit =
    fold(m)
    broadcast(m)

  /** Upsert one loop and tell every client.
    *
    * Settling a generation is also what frees its transcripts: once a generation is
    * accepted or abandoned its two logs stop changing, and the durable copy is on
    * disk (the browser fetches it from [[WorkflowWebServer.ApiPath]] when someone
    * looks). Dropping them here is what keeps a loop that has run fifty generations
    * from holding fifty generations of prose in memory — at most the one in flight
    * is kept. */
  def publishLoop(loop: LoopWire): Unit =
    loops(loop.id) = loop
    pruneSettledTranscripts(loop)
    broadcast(WireMessage.Loop(loop))

  /** One line from a loop agent, folded and broadcast exactly as a workflow node's
    * is — the frames are the same shape, keyed by the loop's id. */
  def publishLoopActivity(ev: TranscriptEvent): Unit = publish(WireMessage.Activity(ev))

  def close(): Unit =
    if handle != null then handle.nn.close()

  private def broadcast(m: WireMessage): Unit =
    val frame = WireCodec.encode(m)
    clients.foreach(_.send(frame))

  private def onClient(sse: WebServer.Sse): Unit =
    // Synchronous: send the snapshots, replay stored transcripts, then register — no
    // suspension between, so no concurrently-published event is missed. The order is
    // a contract the browser relies on: workflows, then loops, then the transcripts,
    // which by then address things the client has heard of.
    rescanLoops()
    sse.send(WireCodec.encode(WireMessage.Snapshot(forests.toList)))
    sse.send(WireCodec.encode(WireMessage.LoopSnapshot(loops.values.toList)))
    for
      (runId, perNode) <- transcripts
      (nodeId, t) <- perNode
      ev <- t.toEvents(runId, nodeId)
    do sse.send(WireCodec.encode(WireMessage.Activity(ev)))
    clients += sse
    sse.onClose(() => { clients -= sse; () })

  private def fold(m: WireMessage): Unit = m match
    case WireMessage.Event(ev) =>
      val cur = forests.getOrElse(ev.runId, Forest.empty)
      // A resumed sub-agent runs fresh: drop the interrupted attempt's stored
      // transcript so a client that connects after resume doesn't get it replayed
      // spliced onto the new one. Mirrors the browser's `AppState.reduce`.
      cur.restartsInterrupted(ev).foreach(nid => transcripts.get(ev.runId).foreach(_.remove(nid)))
      forests(ev.runId) = cur.update(ev)
    case WireMessage.Activity(ev) =>
      val perNode = transcripts.getOrElseUpdate(ev.runId, LinkedHashMap.empty)
      perNode(ev.nodeId) = perNode.getOrElse(ev.nodeId, Transcript.empty).update(ev)
    case _: WireMessage.Snapshot | _: WireMessage.LoopSnapshot | _: WireMessage.Loop => ()

  private def pruneSettledTranscripts(loop: LoopWire): Unit =
    transcripts.get(loop.id).foreach: perLabel =>
      for
        g <- loop.generations if g.state != "running"
        label <- List(LoopBridge.workerTranscriptLabel(g.gen), LoopBridge.evalTranscriptLabel(g.gen))
      do perLabel.remove(label)

  /** Take in the loops sitting in `.auk/loops` that nobody here is driving.
    *
    * A loop outlives its session, so a project's loops are not this session's to
    * enumerate — the disk is. A held loop is skipped: the bridge feeds those live,
    * and a ledger read cannot know the phase of a loop being adopted or which agent
    * is running, so letting a disk read win would replace what is happening with what
    * has been written down. Ledgers that will not fold are left out, exactly as the
    * session-open scan leaves them out. */
  private def rescanLoops(): Unit =
    loopStore.foreach: store =>
      store.list().foreach: id =>
        if !loops.get(id).exists(_.held) then LoopWirer.fromDisk(store, id).foreach(loops(id) = _)

  // -- the on-demand API -------------------------------------------------------

  /** Payloads too large to push to every client, served to the one asking for them.
    *
    * Answered inline, because everything behind these routes is a synchronous read
    * (git through `execFileSync`, the ledger and the tee files through the
    * filesystem). The callback shape is the transport's, not this handler's. */
  private def handleApi(subPath: String, respond: WebServer.ApiRespond): Unit =
    respond(answer(subPath.split("/").iterator.filter(_.nonEmpty).toList))

  private def answer(segments: List[String]): Option[String] = segments match
    case "loop" :: id :: "transcript" :: label :: Nil => transcriptOf(id, label)
    case "loop" :: id :: "diff" :: gen :: attempt :: Nil =>
      for
        g <- gen.toIntOption
        a <- attempt.toIntOption
        text <- diffOf(id, g, a)
      yield text
    case _ => None

  /** One settled generation agent's transcript, as the JSONL the tee wrote — already
    * the dashboard's own wire format, so the browser folds it with the same code it
    * folds a live stream with.
    *
    * Which file it is takes the ledger to answer: the tees live under the session
    * that ran the generation, and only its `generation_started` names that session.
    * The filename is rebuilt from the parsed generation number rather than from the
    * label as given, so nothing a caller writes reaches the path. */
  private def transcriptOf(loopId: String, label: String): Option[String] =
    for
      ctx     <- loopContext
      history <- historyOf(loopId)
      (gen, safeLabel) <- WorkflowWebServer.parseLabel(label)
      generation <- history.generation(gen)
      session = generation.sessionId
      if WorkflowWebServer.isPlainName(session)
      path = SessionRef.loopLog(ctx.resolve(SessionProvider.RelativePath), session, loopId, safeLabel)
      if ctx.fs.isRegularFile(path)
    yield ctx.fs.readString(path)

  /** What one attempt changed, as a unified diff.
    *
    * Read against what the generation started from — its parent's accepted commit,
    * or the loop's baseline — so the patch is the generation's own contribution
    * rather than everything since the loop began. An abandoned generation that never
    * snapshotted an attempt falls back to its rescue snapshot, which is the only
    * record left of what it was doing when it was rolled back. Capped exactly as the
    * evaluator's copy is; a patch past that is one nobody reads in full anyway. */
  private def diffOf(loopId: String, gen: Int, attempt: Int): Option[String] =
    for
      ctx        <- loopContext
      history    <- historyOf(loopId)
      generation <- history.generation(gen)
      candidate <- generation
        .attemptAt(attempt)
        .flatMap(_.snapshotCommits.headOption)
        .orElse(generation.rescueSnapshotId.flatMap(id => Snapshot.resolve(ctx.workingDirectory, id).toOption))
      diff <- Snapshot.diffTrees(ctx.workingDirectory, history.baseFor(gen), candidate).toOption
    yield LoopBridge.capPatch(diff)

  private def historyOf(loopId: String): Option[LoopHistory] =
    loopStore.flatMap(_.readAll(loopId).flatMap(LoopHistory.fold).toOption)

object WorkflowWebServer:
  /** Default SSE keep-alive interval (ms). Comfortably under the idle-connection
    * timeouts of common OSes/proxies, so the browser's `EventSource` stays put. */
  val HeartbeatMs: Int = 15000

  /** Where the on-demand payloads live. Everything under it is dynamic; everything
    * else a GET can reach is a file in the bundle. */
  val ApiPath: String = "/api/"

  /** A transcript label as its generation number and the label rebuilt from it, or
    * `None` when it is not one of the two a generation has. Rebuilding is the point:
    * it leaves a caller no way to reach a path of their choosing. */
  private[runtime] def parseLabel(label: String): Option[(Int, String)] =
    val digits = label.stripPrefix("gen-").takeWhile(_.isDigit)
    digits.toIntOption.flatMap: gen =>
      if label == LoopBridge.workerTranscriptLabel(gen) then Some((gen, LoopBridge.workerTranscriptLabel(gen)))
      else if label == LoopBridge.evalTranscriptLabel(gen) then Some((gen, LoopBridge.evalTranscriptLabel(gen)))
      else None

  /** Whether a name out of a ledger can be a single path component. The ledgers are
    * the host's own files, but they sit in a tree the loop is meanwhile editing, so a
    * session id that would climb out of the sessions directory is refused rather than
    * resolved. */
  private[runtime] def isPlainName(name: String): Boolean = name.matches("[A-Za-z0-9_.-]+") && name != ".."
