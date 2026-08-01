package auk.webui

import auk.workflow.NodeStatus

/** A status kind decoupled from glyph/CSS, so the view-model stays pure data and
  * the Laminar binding maps kind -> class mechanically. Mirrors [[NodeStatus]],
  * plus `Paused` for a whole run (no per-node equivalent). */
enum StatusKind:
  case Pending, Queued, Running, Done, Failed, Interrupted, Paused

object StatusKind:
  def of(s: NodeStatus): StatusKind = s match
    case NodeStatus.Pending     => Pending
    case NodeStatus.Queued      => Queued
    case NodeStatus.Running     => Running
    case NodeStatus.Done        => Done
    case NodeStatus.Failed      => Failed
    case NodeStatus.Interrupted => Interrupted

  def cssClass(k: StatusKind): String = k match
    case Pending     => "is-pending"
    case Queued      => "is-queued"
    case Running     => "is-running"
    case Done        => "is-done"
    case Failed      => "is-failed"
    case Interrupted => "is-interrupted"
    case Paused      => "is-paused"

  /** The lowercase name used for the `data-status` attribute and for badges. */
  def name(k: StatusKind): String = k match
    case Pending     => "pending"
    case Queued      => "queued"
    case Running     => "running"
    case Done        => "done"
    case Failed      => "failed"
    case Interrupted => "interrupted"
    case Paused      => "paused"

/** A loop's state at a glance, for the switcher's dot and the board's phase pill.
  *
  * Deliberately its own vocabulary rather than a [[StatusKind]]: a loop is never
  * "queued" or "pending", and it has one state a workflow node cannot have — an
  * orphan, recorded as running with nobody driving it, which reads as a hollow ring
  * rather than a filled dot because there is nothing behind it. */
enum LoopDot:
  case Live, Parked, Reached, Orphaned

object LoopDot:
  def cssClass(d: LoopDot): String = d match
    case Live     => "is-running"
    case Parked   => "is-paused"
    case Reached  => "is-done"
    case Orphaned => "is-orphaned"

  def name(d: LoopDot): String = d match
    case Live     => "running"
    case Parked   => "parked"
    case Reached  => "goal reached"
    case Orphaned => "orphaned"

/** A run switcher entry (the dropdown is only shown when more than one run or loop
  * is live). `statusKind` is the run's overall state (for the menu's status dot) and
  * `settled`/`total` are its finished / declared sub-agent counts. */
final case class RunTab(
    runId: String,
    label: String,
    selected: Boolean,
    statusKind: StatusKind,
    settled: Int,
    total: Int
)

/** A loop switcher entry: the loop's id, its state dot, and its accepted/started
  * generation count as a dim suffix — the loop's answer to a run's settled/total. */
final case class LoopTab(
    loopId: String,
    label: String,
    selected: Boolean,
    dot: LoopDot,
    accepted: Int,
    started: Int
)

/** One sub-agent card on the canvas. `tokensText`/`toolText`/`promptHint` are ""
  * when absent; `selected` highlights the card whose transcript is open in the
  * floating window. */
final case class AgentCard(
    id: String,
    statusKind: StatusKind,
    tokensText: String,
    toolText: String,
    promptHint: String,
    selected: Boolean
)

/** One group's card on the canvas; `name` is None for the trailing ungrouped
  * card. `key` is a stable identity for keyed rendering ("g:<id>" or
  * "~ungrouped"); `settled`/`total` are the card's finished / declared agent
  * counts (the header's progress figure — the filmstrip itself is derived from
  * `agents`). */
final case class GroupCard(
    key: String,
    name: Option[String],
    description: String,
    agents: Vector[AgentCard],
    settled: Int,
    total: Int
)

/** The canvas: one card per non-empty group (declared order, ungrouped last) and
  * any workflow-level log lines. `nodeCount` distinguishes a run with no agents
  * yet from no run at all. */
final case class CanvasView(cards: Vector[GroupCard], nodeCount: Int, logs: Vector[String])

/** The loop board: a masthead over the lineage.
  *
  * The masthead is three answers to three questions — what is it doing, what is it
  * for, what is it judged by — and they are set as one aligned list rather than as a
  * headline with columns beside it, because the goal is somebody's prose and prose
  * blown up to display size is not a headline, only large prose. `status` is the
  * loop's live activity sentence where it has one and its phase otherwise: a loop
  * with activity is by definition running, so saying both would say it twice.
  *
  * All three have had their hard wrapping undone — where an editor's window ended a
  * line says nothing about where this page's should — and all three are clamped by
  * the stylesheet, so the full text lives in each row's tooltip. `rubric` is empty
  * for a loop that declared none, and the board simply does not draw that row.
  */
final case class LoopBoard(
    id: String,
    status: String,
    goal: String,
    rubric: String,
    lineage: Lineage
)

/** What fills the dotted board: a workflow's group columns, or a loop's lineage.
  * One or the other — the selection is one thing, so the board is too. */
enum BoardView:
  case Workflow(canvas: CanvasView)
  case Loop(board: LoopBoard)

/** The top bar's "workflow code" / "loop code" button, present when the selected
  * thing has source to show. */
final case class CodeButton(selected: Boolean, label: String)

/** One cell of the top bar's figures strip: a mono micro-label over a serif
  * numeral, optionally accented, optionally trailed by a dim `note` (the loop's
  * "was 0.82" — where the headline metric stood a generation ago). */
final case class MetricCell(label: String, value: String, accent: Boolean = false, note: String = "")

/** One rendered line of a sub-agent's transcript.
  *
  * `Prose`/`Thought` carry the streamed text as its append-only `chunks` (mirroring
  * [[auk.workflow.TranscriptItem]]), so the Laminar binding can render one node per
  * chunk and a streaming delta only appends a node rather than re-rendering the
  * whole run. The string-taking `apply` overloads keep the common single-chunk case
  * (and tests) concise; equality is by chunks, so appending a chunk makes the row
  * unequal and the binding updates. */
sealed trait TranscriptRow
object TranscriptRow:
  final case class Prose(chunks: Vector[String]) extends TranscriptRow
  object Prose:
    def apply(text: String): Prose = Prose(Vector(text))
  /** A reasoning block. `done` is true once the agent has moved on (a later row
    * exists, or the agent stopped streaming), so the binding folds it; a still-
    * active thought (the last row of a streaming agent) stays open. */
  final case class Thought(chunks: Vector[String], done: Boolean) extends TranscriptRow
  object Thought:
    def apply(text: String, done: Boolean): Thought = Thought(Vector(text), done)
  /** A tool call card: `input` is the syntax-highlighted argument source
    * (Scala tokens for `eval_scala`, a single plain token otherwise) and
    * `output` is None while the tool is still running.
    *
    * `title` and `hint` are the projected display strings — the human-readable
    * name and a one-line argument digest. They are computed here rather than in
    * the DOM layer so the binding stays branch-free and the copy has one home;
    * `name` is kept as the raw wire name, since that is the identity. */
  final case class Tool(
      callId: String,
      name: String,
      title: String,
      hint: String,
      input: Vector[HlToken],
      output: Option[String],
      isError: Boolean
  ) extends TranscriptRow

  /** A message another agent sent *to* this one, shown where it entered the
    * conversation. Atomic (it never streams), so it holds plain text rather than
    * chunks. `from` is the raw sender id; the binding names it only when it is
    * not the lead, whose messages are the expected kind. */
  final case class Received(from: String, text: String) extends TranscriptRow

/** The header + streamed transcript of the selected sub-agent. `streaming` is true
  * while the node is still running, so the binding can show a live caret. */
final case class AgentView(
    id: String,
    statusKind: StatusKind,
    tokensText: String,
    toolText: String,
    prompt: Option[String],
    summary: Option[String],
    rows: Vector[TranscriptRow],
    streaming: Boolean
)

/** How one of the phase line's three stations is drawn: waiting its turn, working
  * right now, and the two ways of being finished with it. */
enum PhaseKind:
  case Idle, Live, Ok, Bad

object PhaseKind:
  def cssClass(k: PhaseKind): String = k match
    case Idle => "is-idle"
    case Live => "is-live"
    case Ok   => "is-ok"
    case Bad  => "is-bad"

/** One station of the Worker —◆— Evaluator line, which doubles as the window's tab
  * bar: `pane` is the body it selects, `selected` marks it as the open tab. */
final case class PhaseStation(pane: GenPane, label: String, kind: PhaseKind, selected: Boolean)

/** The generation's three stations, left to right. The checker is the diamond in
  * the middle because that is where it sits in the cycle: the worker submits, the
  * checker measures, and only what it passes reaches the evaluator. */
final case class PhaseLine(worker: PhaseStation, checker: PhaseStation, evaluator: PhaseStation):
  def stations: Vector[PhaseStation] = Vector(worker, checker, evaluator)

/** One attempt pill. `kind` reads the attempt's furthest gate: blue while it is
  * still in flight, red where the checker refused it, amber where the evaluator
  * did, green where it was accepted. */
final case class AttemptPill(attempt: Int, label: String, kind: StatusKind, selected: Boolean)

/** What the mechanical checker reported about the selected attempt. */
final case class CheckReport(pass: Boolean, reasons: Vector[String], metrics: Vector[(String, String)])

/** What the evaluator agent said about it. */
final case class VerdictNote(accepted: Boolean, goalReached: Boolean, feedback: String)

/** The generation's patch, fetched per attempt. `available` is false when the
  * attempt recorded no snapshot, in which case there is nothing to ask for. */
final case class DiffPane(available: Boolean, state: Option[DiffState])

/** The generation's own account of itself, under the diamond: what the attempt
  * claimed to do, what it committed, what it measured, and how the two gates
  * received it.
  *
  * `submitted` is false for the attempt in flight — the one the driver is counting
  * but the ledger has not written down yet — which is the difference between "there
  * is nothing to say about this" and "this has not happened". */
final case class GenOverview(
    submitted: Boolean,
    description: String,
    commit: String,
    metrics: Vector[(String, String)],
    artifact: String,
    check: Option[CheckReport],
    verdict: Option[VerdictNote],
    diff: DiffPane
)

/** How a generation agent's transcript stands. `Live` is a stream arriving now;
  * `Settled` has been fetched and folded; `Loading` is either not yet asked for or
  * in flight (the render asks, and asking twice is the caller's problem to dedupe);
  * `Missing` is an answer — a generation whose tee was never written, or a host
  * that could not serve it. */
enum PaneStatus:
  case Live, Settled, Loading
  case Missing(message: String)

/** One transcript pane of the generation window. */
final case class TranscriptPane(
    label: String,
    rows: Vector[TranscriptRow],
    streaming: Boolean,
    status: PaneStatus
)

/** Which body the generation window is showing — the pane the phase line's selected
  * station names. */
enum GenBody:
  case Overview(view: GenOverview)
  case Transcript(pane: TranscriptPane)

/** One generation, whole, as the floating window draws it.
  *
  * `attempt` is the attempt everything below the phase line is scoped to — the pills
  * choose it, and it is 0 only for a generation that has not submitted one yet.
  * `stats` are the window head's cells, as label/value pairs, because a generation's
  * interesting figures are its own (attempts, its headline metric) rather than the
  * fixed three a sub-agent has.
  */
final case class GenerationView(
    loopId: String,
    gen: Int,
    title: String,
    state: String,
    stateKind: StatusKind,
    stats: Vector[(String, String)],
    phase: PhaseLine,
    attempts: Vector[AttemptPill],
    attempt: Int,
    body: GenBody
)

/** What fills the right-hand drawer. */
enum PanelView:
  /** Nothing focused — the drawer is closed. */
  case Closed
  /** Show the selected agent's transcript. */
  case Agent(view: AgentView)
  /** Show the workflow's, or the loop definition's, source code. The title travels
    * with it because the two panels are the same panel and only their subject
    * differs. */
  case Code(title: String, tokens: Vector[HlToken])
  /** Show one generation of the selected loop. */
  case Generation(view: GenerationView)

/** The whole rendered page, as pure data. Split into independent sub-models
  * (`runs`/`board`/`panel`) so the Laminar layer can bind each to its own signal
  * and only rebuild the part that changed (the transcript streams without
  * rebuilding the board). */
final case class View(
    conn: ConnStatus,
    runs: Vector[RunTab],
    loops: Vector[LoopTab],
    codeButton: Option[CodeButton],
    runControl: Option[RunTab],
    metrics: Vector[MetricCell],
    board: BoardView,
    panel: PanelView
):
  /** The workflow canvas — empty when a loop is what the board is showing. */
  def canvas: CanvasView = board match
    case BoardView.Workflow(c) => c
    case BoardView.Loop(_)     => CanvasView(Vector.empty, 0, Vector.empty)

/** Everything the DOM layer can ask of the state, in one value.
  *
  * Bundled rather than passed one argument at a time because the render layer needs
  * all of it and only some of it at each depth: a lineage circle wants
  * [[selectGeneration]] and nothing else, but it is five calls down from the page.
  * The two `need*` entries are requests for a payload that is not on the wire — the
  * caller is expected to ignore one it has already answered, so the render can ask
  * every time a pane mounts without knowing whether it is the first time.
  */
final case class Actions(
    selectRun: String => Unit,
    selectLoop: String => Unit,
    selectNode: String => Unit,
    selectGeneration: Int => Unit,
    selectPane: GenPane => Unit,
    selectAttempt: Int => Unit,
    selectCode: () => Unit,
    close: () => Unit,
    needTranscript: (String, String) => Unit,
    needDiff: (String, Int, Int) => Unit
)
