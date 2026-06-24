package auk.library

import scala.scalajs.js
import scala.concurrent.{Future, ExecutionContext}

/** A handle to a sub-agent (or a composition of them) running on the host.
  *
  * Eager: constructing one via [[agent]] admits the work to the host immediately;
  * the value is delivered asynchronously. Compose dependencies with [[map]] /
  * [[flatMap]] / [[Agent.all]] — `flatMap(r => …)` hands you the resolved `r`.
  * The result is produced by awaiting the terminal `Agent` returned from
  * [[Workflow.start]]; there is no blocking `await` mid-graph.
  */
final class Agent[R] private[library] (
    private[library] val nodeId: String,
    private[library] val future: Future[R],
    private[library] val terminals: List[String],
    private[library] val rt: WorkflowRuntime
):
  /** Transform the result. Adds no sub-agent. */
  def map[S](f: R => S): Agent[S] =
    given ExecutionContext = rt.ec
    new Agent(nodeId, future.map(f), terminals, rt)

  /** Sequence a dependent computation: `f` runs once this agent resolves, with
    * its value, and returns the next `Agent`. Sub-agents created inside `f`
    * depend on this one. */
  def flatMap[S](f: R => Agent[S]): Agent[S] =
    given ExecutionContext = rt.ec
    val parentTerminals = terminals
    val newFut = future.flatMap: r =>
      val saved = rt.currentFrontier
      rt.currentFrontier = parentTerminals
      val child = try f(r)
      finally rt.currentFrontier = saved
      child.future
    new Agent("", newFut, parentTerminals, rt)

object Agent:
  /** Join: wait for every agent and collect their results, preserving order.
    * Fails fast if any fails. */
  def all[R](agents: List[Agent[R]])(using wc: WorkflowContext): Agent[List[R]] =
    wc.allAgents(agents)

  /** Lift an already-known value into an `Agent` — no sub-agent, no node, no
    * dependency; it is resolved immediately. Use it as the terminal of a branch
    * that has nothing left to delegate, e.g. a loop's accepted case returning the
    * value in hand: `if done then Agent.pure(work) else attempt(round + 1)`. */
  def pure[R](value: R)(using wc: WorkflowContext): Agent[R] =
    wc.pureAgent(value)

/** A named, described group that sub-agents are organized under (for the live
  * forest UI). Declare with [[group]], populate with [[inGroup]]. */
final class Group private[library] (private[library] val id: String, val name: String)

/** Shared, single-threaded workflow state for one `wf.start` run: the bridge
  * client, id minting, and the current dependency frontier (set by `flatMap`). */
private[library] final class WorkflowRuntime(client: WorkflowClient):
  given ec: ExecutionContext = WorkflowClient.queue
  var currentFrontier: List[String] = Nil
  private var groupCounter = 0
  def nextGroupId(): String = { groupCounter += 1; "g" + groupCounter }

  private val usedNodeIds = scala.collection.mutable.Set.empty[String]

  /** Reserve an author-supplied agent id for this run, failing hard on a
    * duplicate. The id is the correlation key on both sides of the bridge — a
    * collision would overwrite a pending call, scramble which result lands where,
    * and orphan a sub-agent — so reject it at build time rather than misbehave
    * silently. */
  def claimNodeId(id: String): Unit =
    if id == null || id.trim.isEmpty then
      throw new IllegalArgumentException("agent id must be a non-empty string")
    if !usedNodeIds.add(id) then
      throw new IllegalArgumentException(
        s"duplicate agent id '$id': within a workflow every agent id must be unique")

  def declareGroup(id: String, name: String, desc: String, parent: String | Null): Unit =
    client.declareGroup(id, name, desc, parent)
  def declareNode(id: String, group: String | Null, deps: List[String]): Unit =
    client.declareNode(id, group, deps)
  def call(id: String, prompt: String, schema: js.Any): Future[js.Any] =
    client.call(id, prompt, schema)
  def log(message: String): Unit = client.log(message)
  def close(): Unit = client.close()

/** The implicit capability threaded through a workflow body. `inGroup` rebinds it
  * with a new current group, so sub-agents created lexically inside attach there
  * (captured by closures, so `flatMap` frontiers land in the right group too). */
final class WorkflowContext private[library] (
    private[library] val rt: WorkflowRuntime,
    private[library] val groupId: String | Null
):
  def group(name: String, description: String): Group =
    val id = rt.nextGroupId()
    rt.declareGroup(id, name, description, groupId)
    new Group(id, name)

  def inGroup[A](g: Group)(body: WorkflowContext ?=> A): A =
    body(using new WorkflowContext(rt, g.id))

  def agent[R](prompt: String, id: String)(using ti: LibToolInput[R]): Agent[R] =
    given ExecutionContext = rt.ec
    rt.claimNodeId(id)
    rt.declareNode(id, groupId, rt.currentFrontier)
    val fut = rt.call(id, prompt, ti.schema).map: jsVal =>
      ti.decode(jsVal) match
        case Right(r) => r
        case Left(e)  => throw new RuntimeException(s"agent '$id' returned an undecodable result: $e")
    new Agent(id, fut, List(id), rt)

  def allAgents[R](agents: List[Agent[R]]): Agent[List[R]] =
    given ExecutionContext = rt.ec
    val terminals = agents.flatMap(_.terminals)
    new Agent("", Future.sequence(agents.map(_.future)), terminals, rt)

  def pureAgent[R](value: R): Agent[R] =
    new Agent("", Future.successful(value), Nil, rt)

  def log(message: String): Unit = rt.log(message)

/** The lifecycle status of a [[WorkflowRun]], as observed from the worker handle.
  *
  *   - [[WorkflowStatus.Running]]: the run is in flight (or has not settled yet).
  *   - [[WorkflowStatus.Done]]: the run has settled; `isOk` says whether it
  *     succeeded (`true`) or failed (`false`).
  *   - [[WorkflowStatus.Paused]]: the user paused the run from the UI. The handle
  *     stays paused; the run resumes separately (in a fresh worker) and its result
  *     arrives as the host's completion system notice, not through this handle.
  *
  * Distinct from `auk.workflow.RunStatus`, which models the host/forest/UI view. */
enum WorkflowStatus:
  case Running
  case Done(isOk: Boolean)
  case Paused

/** A handle to a background workflow run started by [[Workflow.start]].
  *
  * The run proceeds on the worker's event loop *between* eval calls (while the
  * worker is idle waiting for the next eval, the loop services the side channel
  * and resolves the underlying `Future`). So you cannot observe progress within
  * the eval that started it — store the handle in a val and poll it in a *later*
  * eval (`status` / `getResult`), or simply wait for the host's completion system
  * notice, which carries the full result and this run's [[id]]. */
trait WorkflowRun[R]:
  /** The run's unique id; matches the id in the host's completion system notice. */
  def id: String
  /** The run's current [[WorkflowStatus]]: `Running`, `Done(isOk)`, or `Paused`. */
  def status: WorkflowStatus
  /** The result. Requires [[status]] is `Done`; rethrows the workflow's error if it
    * failed. Throws if the run is still running or paused. */
  def getResult: R
  /** The error if the run failed, else `None`. Requires [[status]] is `Done`. */
  def getError: Option[Throwable]
  /** Whether the run succeeded. Requires [[status]] is `Done`. */
  def isOk: Boolean

private[library] final class WorkflowRunImpl[R](
    val id: String,
    fut: Future[R],
    paused: () => Boolean = () => false
) extends WorkflowRun[R]:
  def status: WorkflowStatus =
    if paused() then WorkflowStatus.Paused
    else
      fut.value match
        case None    => WorkflowStatus.Running
        case Some(t) => WorkflowStatus.Done(t.isSuccess)

  // The settled Try, or a clear error if accessed before the run has settled. The
  // worker cannot await, so a not-yet-settled run has no value to give. A paused run
  // reports as paused rather than surfacing the socket failure its cancelled calls
  // leave on the underlying Future.
  private def settled: scala.util.Try[R] =
    if paused() then
      throw new IllegalStateException(
        "workflow is paused (the user paused it); it resumes separately — wait for the completion system notice")
    else
      fut.value.getOrElse(throw new IllegalStateException(
        "workflow still running; check status first, or wait for the completion system notice"))
  def isOk: Boolean = settled.isSuccess
  def getResult: R = settled.get
  def getError: Option[Throwable] = settled.failed.toOption
  override def toString: String =
    if paused() then s"WorkflowRun(id=$id, paused)"
    else
      fut.value match
        case None                        => s"WorkflowRun(id=$id, still running)"
        case Some(scala.util.Success(_)) => s"WorkflowRun(id=$id, done)"
        case Some(scala.util.Failure(e)) => s"WorkflowRun(id=$id, error: ${Option(e.getMessage).getOrElse("workflow failed")})"

/** The workflow entry point, reached as `lib.wf`. */
final class Workflow private[library] ():
  /** Launch a workflow in the background: build the agent graph synchronously
    * (eager), then return a [[WorkflowRun]] handle immediately — the run keeps
    * going on its own.
    *
    * Non-blocking: the worker cannot await a `Future`, so the resolved `R` is not
    * returned here. Instead the run settles asynchronously; the host delivers the
    * full result/error as a system notice (and the live forest panel shows
    * progress). Store the returned handle in a val and poll its [[WorkflowRun.status]]
    * in a later eval, or wait for the notice. The handle runs to completion even if
    * you drop it — the notice still arrives. Keep the terminal `Agent[R]` as the
    * block's last expression, as before.
    *
    * If the user pauses the run from the UI, the handle's status becomes `Paused`;
    * the run is then resumed separately (in a fresh worker) and its result still
    * arrives as the completion notice — the paused handle itself does not settle. */
  def start[R](body: WorkflowContext ?=> Agent[R]): WorkflowRun[R] =
    val runId = Workflow.nextRunId()
    val client = WorkflowClient.fromEnv(runId)
    val rt = new WorkflowRuntime(client)
    given ExecutionContext = rt.ec
    // Build the agent graph synchronously. A failure here (e.g. a duplicate agent
    // id) must fail the eval hard, so tear down the side channel and rethrow
    // *before* announcing the run: a build error is reported as the eval result,
    // and the host must NOT also register the run (no spurious completion notice).
    val terminal =
      try body(using new WorkflowContext(rt, null))
      catch
        case e: Throwable =>
          client.close()
          throw e
    // Build succeeded: announce the run so the host binds this connection to the
    // run id and tracks it as active (the basis for completion + disconnect
    // handling). Sent after the build so a failed build registers nothing.
    client.hello()
    // The in-band marker (carrying the run id) on the captured stdout tells the
    // host's eval_scala a workflow ran, so it can announce the source for this id.
    js.Dynamic.global.process.stdout.write(s"${Workflow.StartMarker}:$runId\n")
    terminal.future.onComplete { result =>
      result match
        case scala.util.Success(r) => client.sendDone(ok = true, value = s"$r", error = "")
        case scala.util.Failure(e) => client.sendDone(ok = false, value = "", error = Option(e.getMessage).getOrElse("workflow failed"))
      // The result has already arrived and the returned handle's Future is
      // resolved (it keeps its value), so closing the side channel now is safe.
      client.close()
    }
    new WorkflowRunImpl(runId, terminal.future, () => client.isPaused)

object Workflow:
  /** Printed (as `"$StartMarker:$runId"` on its own line) to the captured stdout
    * by [[Workflow.start]] so the host's eval_scala tool knows a workflow ran (and
    * which run id) and can announce its source. Must match the prefix in
    * `auk.runtime.EvalScala.WorkflowStartMarker`. */
  val StartMarker: String = "auk:workflow:start"

  // A monotonic per-worker-process counter; combined with the pid it yields a run
  // id unique across this Auk session (a worker restart gets a fresh pid).
  private var runCounter = 0
  // A resume worker is spawned with `AUK_WF_RUN_ID` set to the original run id so
  // its (single) `wf.start` reuses that id — the host then applies the saved
  // sub-agent cache for it. Consumed once: a second `wf.start` in the same process
  // mints a fresh id as usual.
  private var usedEnvRunId = false
  def nextRunId(): String =
    val envId = js.Dynamic.global.process.env.AUK_WF_RUN_ID
    if !usedEnvRunId && envId != null && !js.isUndefined(envId) then
      usedEnvRunId = true
      envId.asInstanceOf[String]
    else
      runCounter += 1
      s"wf-${js.Dynamic.global.process.pid}-$runCounter"

/** Top-level DSL — brought into scope by the preamble's `import auk.library.*`,
  * and resolved against the contextual [[WorkflowContext]] inside `wf.start`. */
def group(name: String, description: String)(using wc: WorkflowContext): Group =
  wc.group(name, description)

def inGroup[A](g: Group)(body: WorkflowContext ?=> A)(using wc: WorkflowContext): A =
  wc.inGroup(g)(body)

def agent[R](prompt: String, id: String)(using wc: WorkflowContext, ti: LibToolInput[R]): Agent[R] =
  wc.agent(prompt, id)

def log(message: String)(using wc: WorkflowContext): Unit =
  wc.log(message)
