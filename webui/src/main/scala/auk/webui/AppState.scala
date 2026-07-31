package auk.webui

import auk.workflow.{Forest, LoopWire, Transcript, TranscriptEvent, WireCodec, WireMessage}

/** Connection state of the SSE stream, surfaced as a small badge in the UI. */
enum ConnStatus:
  case Connecting, Open, Closed
  case Error(message: String)

/** What the dashboard is looking at: one workflow run, or one loop.
  *
  * The two share the switcher, the board and the URL fragment, and nothing shows
  * both at once — so this is one field rather than two `Option`s that could
  * disagree about which of them the page is on.
  */
enum Target:
  case Run(id: String)
  case Loop(id: String)

/** Which of the generation window's three body panes is showing. They are the tabs
  * of the Worker —◆— Evaluator phase line: the two agents' transcripts, and the
  * generation's own account of itself between them. */
enum GenPane:
  case Worker, Overview, Evaluator

/** What the main panel is focused on within the selected run or loop. */
enum Focus:
  case Unfocused
  case Code
  case Node(id: String)
  /** A generation of the selected loop, carrying the window's own two selections:
    * the body pane, and the attempt everything under the phase line is scoped to.
    * `attempt = None` means the latest, which is what a reader wants until they say
    * otherwise — and it keeps meaning that as further attempts arrive, where a
    * number pinned at open time would quietly go stale. */
  case Generation(gen: Int, pane: GenPane, attempt: Option[Int])

/** A settled generation's transcript, fetched once from the host's on-demand API.
  * `Ready` means the fold has landed in [[AppState.transcripts]] — the same place a
  * live stream lands — so nothing downstream needs to know where it came from. */
enum FetchState:
  case Loading
  case Ready
  case Failed(message: String)

/** One attempt's patch, fetched on demand. Unlike a transcript it has no shared
  * home to fold into (nothing else on the wire produces diffs), so it is held
  * whole here. */
enum DiffState:
  case Loading
  case Ready(text: String)
  case Failed(message: String)

/** The whole UI state: one [[Forest]] per run, one [[LoopWire]] per loop, the
  * per-`(run, node)` streamed [[Transcript]]s, both insertion orders (so neither
  * switcher section reshuffles), what is selected, a target the URL asked for but
  * that hasn't arrived yet, the focused panel, and the connection status. The two
  * on-demand caches — settled loop transcripts and per-attempt diffs — sit here too,
  * so a pane that has already been fetched re-opens without a round trip.
  *
  * `pending` exists because the URL fragment names its target before any of them
  * are known: the snapshots only land after mount, and further runs and loops can
  * show up later. It is an *intent*, kept until the thing is actually seen (or until
  * a click overrides it), at which point it becomes the selection — see [[desire]].
  *
  * Loops arrive whole rather than as deltas, so their half of the reducer is an
  * upsert and a replace where the workflow half is a fold. What they share is the
  * transcript map: a loop agent's frames are keyed `runId = loopId`,
  * `nodeId = <label>`, the same shape a workflow node's are, so one map and one
  * fold serve both.
  *
  * The reducer [[reduce]] and the small mutators are pure, so they are unit-tested
  * without a DOM. The Laminar layer only holds an `AppState` in a `Var` and calls
  * these.
  */
final case class AppState(
    forests: Map[String, Forest] = Map.empty,
    transcripts: Map[String, Map[String, Transcript]] = Map.empty,
    order: Vector[String] = Vector.empty,
    loops: Map[String, LoopWire] = Map.empty,
    loopOrder: Vector[String] = Vector.empty,
    selected: Option[Target] = None,
    pending: Option[Target] = None,
    focus: Focus = Focus.Unfocused,
    fetches: Map[(String, String), FetchState] = Map.empty,
    diffs: Map[(String, Int, Int), DiffState] = Map.empty,
    conn: ConnStatus = ConnStatus.Connecting
):
  /** Fold one wire message into the state.
    *
    *   - `Snapshot` replaces the forest set AND clears the transcripts (re-validating
    *     the selection and focus). It is connect-only and the host follows it with a
    *     fresh replay of every transcript, so the accumulated transcripts must be
    *     dropped here: otherwise an `EventSource` reconnect folds that replay into
    *     the existing transcripts and duplicates them without bound (progressive lag
    *     until the page is reloaded). The replay rebuilds them cleanly. A selected
    *     loop survives untouched — a workflow snapshot says nothing about loops.
    *   - `Event` folds into its run's forest, creating it — and auto-selecting the
    *     run — when the run is first seen, or when it is the run the URL asked for.
    *     When the event restarts a node that was `Interrupted` (a resume re-running
    *     it from scratch), its stale transcript is dropped first so the discarded
    *     attempt isn't spliced onto the new one.
    *   - `Activity` folds into the addressed `(run, node)` transcript, creating it
    *     on first sight. Only a pending target switching the selection changes the
    *     focus.
    *   - `LoopSnapshot` replaces the loop set, mirroring `Snapshot`'s selection
    *     semantics — but it clears nothing, because it arrives BETWEEN the workflow
    *     snapshot and the transcript replay that answers both of them.
    *   - `Loop` upserts one loop, mirroring `Event`'s.
    */
  def reduce(msg: WireMessage): AppState = msg match
    case WireMessage.Snapshot(fs) =>
      val m = fs.toMap
      val ord = fs.map(_._1).toVector
      val wanted = pending.collect { case t @ Target.Run(r) if m.contains(r) => t }
      val sel = wanted
        .orElse(selected.filter(present(_, m, loops)))
        .orElse(ord.headOption.map(Target.Run.apply))
      copy(
        forests = m,
        transcripts = Map.empty,
        // The fetched transcripts went with them; forgetting they were ever asked
        // for is what makes the panes ask again rather than show nothing.
        fetches = Map.empty,
        order = ord,
        selected = sel,
        pending = if wanted.isDefined then None else pending,
        focus = revalidate(sel, m, loops, focus)
      )
    case WireMessage.Event(ev) =>
      val rid = ev.runId
      val cur = forests.getOrElse(rid, Forest.empty)
      // A resumed sub-agent runs fresh; clear the interrupted attempt's transcript
      // the moment it re-enters the run (its first queued/started event).
      val nextTranscripts = cur.restartsInterrupted(ev) match
        case Some(nid) => transcripts.get(rid).map(per => transcripts.updated(rid, per - nid)).getOrElse(transcripts)
        case None      => transcripts
      // The run the URL asked for has finally shown up: take it over, exactly as a
      // manual switch would (the focus belonged to what we're leaving).
      val target = Target.Run(rid)
      val wanted = pending.contains(target)
      val switches = wanted && selected.exists(_ != target)
      copy(
        forests = forests.updated(rid, cur.update(ev)),
        transcripts = nextTranscripts,
        order = if order.contains(rid) then order else order :+ rid,
        selected = if wanted then Some(target) else selected.orElse(Some(target)),
        pending = if wanted then None else pending,
        focus = if switches then Focus.Unfocused else focus
      )
    case WireMessage.Activity(ev) =>
      val rid = ev.runId
      val perRun = transcripts.getOrElse(rid, Map.empty)
      val cur = perRun.getOrElse(ev.nodeId, Transcript.empty)
      copy(transcripts = transcripts.updated(rid, perRun.updated(ev.nodeId, cur.update(ev))))
    case WireMessage.LoopSnapshot(ls) =>
      val m = ls.map(l => l.id -> l).toMap
      val ord = ls.map(_.id).toVector
      val wanted = pending.collect { case t @ Target.Loop(id) if m.contains(id) => t }
      val sel = wanted
        .orElse(selected.filter(present(_, forests, m)))
        .orElse(ord.headOption.map(Target.Loop.apply))
      copy(
        loops = m,
        loopOrder = ord,
        selected = sel,
        pending = if wanted.isDefined then None else pending,
        focus = revalidate(sel, forests, m, focus)
      )
    case WireMessage.Loop(l) =>
      val target = Target.Loop(l.id)
      val wanted = pending.contains(target)
      val switches = wanted && selected.exists(_ != target)
      copy(
        loops = loops.updated(l.id, l),
        loopOrder = if loopOrder.contains(l.id) then loopOrder else loopOrder :+ l.id,
        selected = if wanted then Some(target) else selected.orElse(Some(target)),
        pending = if wanted then None else pending,
        focus = if switches then Focus.Unfocused else focus
      )

  def withConn(c: ConnStatus): AppState = copy(conn = c)

  /** Switch the selection, clearing the focus (a different run's nodes, a different
    * loop's generations) and any pending URL intent (an explicit switch outranks a
    * stale fragment). Ignores a target nothing is known about. */
  def select(t: Target): AppState =
    if knows(t) then copy(selected = Some(t), pending = None, focus = Focus.Unfocused) else this

  /** Ask for the target the URL named: select it outright if it is already known,
    * otherwise remember it as [[pending]] and leave the current selection alone
    * until it turns up in a snapshot or an upsert. */
  def desire(t: Target): AppState = if knows(t) then select(t) else copy(pending = Some(t))

  /** The target the URL should name — the pending intent ahead of the selection.
    * An unfulfilled intent must keep being reproduced in the fragment, so that a
    * refresh restores this same state (the fallback on screen, still waiting on the
    * asked-for one) rather than dropping the request. It can't go stale: the intent
    * is cleared the moment the target arrives, or the moment [[select]] overrides
    * it. */
  def urlTarget: Option[Target] = pending.orElse(selected)

  /** The selected run's id, when a run is what is selected. */
  def selectedRun: Option[String] = selected.collect { case Target.Run(r) => r }

  /** The selected loop's id, when a loop is what is selected. */
  def selectedLoop: Option[String] = selected.collect { case Target.Loop(l) => l }

  /** The selected loop itself. */
  def loop: Option[LoopWire] = selectedLoop.flatMap(loops.get)

  /** Focus a node within the current run, ignoring an unknown id. */
  def selectNode(nid: String): AppState =
    if selectedRun.exists(r => forests.get(r).exists(_.nodes.exists(_.id == nid)))
    then copy(focus = Focus.Node(nid))
    else this

  /** Focus the source code of whatever is selected — the workflow's, or the loop
    * definition the lineage was measured by. */
  def selectCode: AppState =
    val has = selected.exists:
      case Target.Run(r)  => forests.get(r).exists(_.code.isDefined)
      case Target.Loop(l) => loops.get(l).exists(_.defSource.nonEmpty)
    if has then copy(focus = Focus.Code) else this

  /** Open a generation of the selected loop, on the overview pane and its latest
    * attempt. Re-selecting the generation already open is idempotent, so clicking
    * its circle again does not throw away a pane or attempt the reader chose. */
  def selectGeneration(gen: Int): AppState = focus match
    case Focus.Generation(g, _, _) if g == gen => this
    case _ =>
      if generation(gen).isDefined then copy(focus = Focus.Generation(gen, GenPane.Overview, None)) else this

  /** Switch the open generation's body pane (the phase line's tabs). */
  def selectPane(pane: GenPane): AppState = focus match
    case Focus.Generation(g, _, a) => copy(focus = Focus.Generation(g, pane, a))
    case _                         => this

  /** Scope the open generation's window to one attempt. Not checked against the
    * ledger here: the attempt in flight has no record yet and still wears a pill, so
    * the projection is what decides which numbers exist and clamps to them. */
  def selectAttempt(attempt: Int): AppState = focus match
    case Focus.Generation(g, pane, _) => copy(focus = Focus.Generation(g, pane, Some(attempt)))
    case _                            => this

  /** Drop the focus (closing the drawer). */
  def clearFocus: AppState = copy(focus = Focus.Unfocused)

  /** The currently-focused node id, if a node is focused. */
  def focusedNode: Option[String] = focus match
    case Focus.Node(id) => Some(id)
    case _              => None

  /** The currently-open generation number, if one is open. */
  def focusedGeneration: Option[Int] = focus match
    case Focus.Generation(g, _, _) => Some(g)
    case _                         => None

  /** The transcript for the focused node, or empty. */
  def selectedTranscript: Transcript =
    (for r <- selectedRun; n <- focusedNode; t <- transcripts.get(r).flatMap(_.get(n)) yield t)
      .getOrElse(Transcript.empty)

  /** A loop agent's transcript by its label, live-streamed or fetched — the two are
    * indistinguishable by the time they are here, which is the point. */
  def loopTranscript(loopId: String, label: String): Option[Transcript] =
    transcripts.get(loopId).flatMap(_.get(label))

  // -- the on-demand payloads --------------------------------------------------

  /** Whether a settled generation's transcript is still worth asking for. False once
    * a request is in flight, has landed, or has failed: a 404 is an answer, and
    * re-asking on every re-render would turn a missing file into a request loop. */
  def needsTranscript(loopId: String, label: String): Boolean = !fetches.contains((loopId, label))

  def transcriptRequested(loopId: String, label: String): AppState =
    copy(fetches = fetches.updated((loopId, label), FetchState.Loading))

  /** Fold a fetched tee file into the map a live stream folds into. The file is
    * JSONL in the wire's own format — one `Activity` frame per line — so this is the
    * ordinary transcript fold over frames the file already carries. Frames
    * addressing anything else are dropped: a file is served for one label, and
    * nothing else may write through this door.
    *
    * A label the loop is streaming into right now keeps its live transcript. The
    * fetch is only ever started for a settled one, but a generation can settle
    * between the request and the answer, and the stream is the fresher account. */
  def transcriptLoaded(loopId: String, label: String, jsonl: String): AppState =
    val streaming = loops.get(loopId).flatMap(_.liveLabel).contains(label)
    val folded = AppState
      .transcriptEvents(jsonl, loopId, label)
      .foldLeft(Transcript.empty)((t, e) => t.update(e))
    copy(
      transcripts =
        if streaming then transcripts
        else transcripts.updated(loopId, transcripts.getOrElse(loopId, Map.empty).updated(label, folded)),
      fetches = fetches.updated((loopId, label), FetchState.Ready)
    )

  def transcriptFailed(loopId: String, label: String, message: String): AppState =
    copy(fetches = fetches.updated((loopId, label), FetchState.Failed(message)))

  /** Whether an attempt's patch is still worth asking for — the same rule, and for
    * the same reason, as [[needsTranscript]]. */
  def needsDiff(loopId: String, gen: Int, attempt: Int): Boolean = !diffs.contains((loopId, gen, attempt))

  def diffRequested(loopId: String, gen: Int, attempt: Int): AppState =
    copy(diffs = diffs.updated((loopId, gen, attempt), DiffState.Loading))

  def diffLoaded(loopId: String, gen: Int, attempt: Int, text: String): AppState =
    copy(diffs = diffs.updated((loopId, gen, attempt), DiffState.Ready(text)))

  def diffFailed(loopId: String, gen: Int, attempt: Int, message: String): AppState =
    copy(diffs = diffs.updated((loopId, gen, attempt), DiffState.Failed(message)))

  private def knows(t: Target): Boolean = present(t, forests, loops)

  private def generation(gen: Int) = loop.flatMap(_.generations.find(_.gen == gen))

  private def present(t: Target, fs: Map[String, Forest], ls: Map[String, LoopWire]): Boolean = t match
    case Target.Run(r)  => fs.contains(r)
    case Target.Loop(l) => ls.contains(l)

  /** Drop a focus that points at something the new snapshot does not have — a
    * vanished node, code a run never had, a generation of a loop that is no longer
    * selected. Total by construction: anything unrecognised closes the drawer. */
  private def revalidate(sel: Option[Target], fs: Map[String, Forest], ls: Map[String, LoopWire], f: Focus): Focus =
    val ok = f match
      case Focus.Unfocused => true
      case Focus.Node(id) =>
        sel.exists:
          case Target.Run(r) => fs.get(r).exists(_.nodes.exists(_.id == id))
          case _             => false
      case Focus.Code =>
        sel.exists:
          case Target.Run(r)  => fs.get(r).exists(_.code.isDefined)
          case Target.Loop(l) => ls.get(l).exists(_.defSource.nonEmpty)
      case Focus.Generation(gen, _, _) =>
        sel.exists:
          case Target.Loop(l) => ls.get(l).exists(_.generations.exists(_.gen == gen))
          case _              => false
    if ok then f else Focus.Unfocused

object AppState:
  /** The transcript events a fetched tee file holds for one `(loop, label)`.
    * Undecodable lines are skipped rather than failing the file: the tee is appended
    * to while a generation runs, so the last line of a file read mid-write can be a
    * partial one. */
  private[webui] def transcriptEvents(jsonl: String, runId: String, nodeId: String): Vector[TranscriptEvent] =
    jsonl.linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(line => WireCodec.decode(line).toOption)
      .collect { case WireMessage.Activity(e) if e.runId == runId && e.nodeId == nodeId => e }
      .toVector
