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

  def log(message: String): Unit = rt.log(message)

/** The workflow entry point, reached as `lib.wf`. */
final class Workflow private[library] ():
  /** Run a workflow: build the agent graph synchronously (eager), then report its
    * settled result to the host over the side channel.
    *
    * Returns [[Unit]], deliberately — not the result. The worker cannot await a
    * `Future`, so the resolved `R` never exists as a value here. Instead the
    * host's `eval_scala` waits for the reported result and surfaces it as this
    * call's tool output. So `wf.start` is the terminal action of an eval: make it
    * the last expression and read the report from the tool output — do not try to
    * bind its return value or call methods on it. There is no in-REPL await; the
    * worker stays untouched.
    */
  def start[R](body: WorkflowContext ?=> Agent[R]): Unit =
    val client = WorkflowClient.fromEnv()
    val rt = new WorkflowRuntime(client)
    given ExecutionContext = rt.ec
    // Build the agent graph synchronously. A failure here (e.g. a duplicate
    // agent id) must fail the eval hard, so tear down the side channel and
    // rethrow *before* writing the start marker: without the marker the host's
    // eval_scala renders this exception as the tool result instead of awaiting a
    // `done` that would never arrive (a silent hang).
    val terminal =
      try body(using new WorkflowContext(rt, null))
      catch
        case e: Throwable =>
          client.close()
          throw e
    // The build succeeded: the in-band marker on the captured stdout tells the
    // host's eval_scala a workflow ran and it must wait for `done` over the side
    // channel.
    js.Dynamic.global.process.stdout.write(Workflow.StartMarker)
    terminal.future.onComplete { result =>
      result match
        case scala.util.Success(r) => client.sendDone(ok = true, value = s"$r", error = "")
        case scala.util.Failure(e) => client.sendDone(ok = false, value = "", error = Option(e.getMessage).getOrElse("workflow failed"))
      client.close()
    }

object Workflow:
  /** Printed to the captured stdout by [[Workflow.start]] so the host's eval_scala
    * tool knows a workflow ran and should await its `done` from the bridge. Must
    * match `auk.runtime.EvalScala.WorkflowStartMarker`. */
  val StartMarker: String = "auk:workflow:start"

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
