package auk.library

import scala.scalajs.js

/** One loop definition as this worker holds it: the checker to run and the codec for
  * the artifacts it will be handed.
  *
  * Both are stored at `Any`, since the id is the only key the host has and a registry
  * cannot be typed by a value that arrives as JSON. The cast is sound in the direction
  * that matters: a loop's artifact type is fixed by the definition that registered it,
  * and the only artifacts ever handed to this checker are ones `input` decoded, so they
  * really are values of that type — erased or not. */
private[library] final case class RegisteredLoop(
    id: String,
    checker: LoopChecker[Any],
    input: LibToolInput[Any]
)

/** The loop definitions bound in THIS worker process.
  *
  * A checker is a closure — it cannot be sent over a socket — so the host re-evaluates
  * the definition's source in a dedicated worker instead, where `loop.start` binds the
  * checker here rather than creating a loop (attach mode, see [[LoopImpl]]). The
  * registry is what that session hands the checker back through when the host asks for
  * a candidate to be checked.
  *
  * Process-scoped rather than per-[[LoopImpl]]: the session preamble builds one `lib`,
  * and everything that reaches a checker goes through it.
  *
  * The object itself is public solely because of [[runCheck]]: the host reaches it by
  * evaluating a call in the gate session, and the code it composes is compiled with
  * nothing but `import auk.library.*` in scope. Everything else stays package-private,
  * and none of it is in the interface the model reads (only `AukInterface.scala` is
  * embedded in the system prompt).
  */
object LoopRegistry:
  private val loops = scala.collection.mutable.LinkedHashMap.empty[String, RegisteredLoop]

  private[library] def register[A](id: String, checker: LoopChecker[A], input: LibToolInput[A]): Unit =
    loops(id) = RegisteredLoop(id, checker.asInstanceOf[LoopChecker[Any]], input.asInstanceOf[LibToolInput[Any]])

  private[library] def get(id: String): Option[RegisteredLoop] = loops.get(id)

  private[library] def ids: List[String] = loops.keys.toList

  /** Drop every registration. For tests, which run many sessions in one process. */
  private[library] def clear(): Unit = loops.clear()

  /** Run `loopId`'s registered checker over one candidate and print the verdict as a
    * single marker line on stdout. Called by the host, which composes the call as Scala
    * source and evaluates it in the gate session that holds the checker.
    *
    * `prevJson` is the newest accepted generation as
    * `{artifact, gen, description, commit, metrics}` (or `null` for the first
    * generation) and `candJson` is the candidate as `{artifact, description, commits}`;
    * the two `artifact` fields are decoded with the [[LibToolInput]] the definition
    * registered, which is what turns opaque JSON back into the typed value the checker
    * was written against.
    *
    * The one line it prints is
    * `auk:loop:check:{"passed":…,"reasons":[…],"metrics":{…}}`, or
    * `auk:loop:check:{"error":"…"}` when the candidate could never reach the checker —
    * an unregistered loop, or an artifact this loop's schema cannot decode. That
    * distinction is the point: a checker that THROWS is a failed check (its message
    * becomes the reason, so a buggy checker rejects the candidate and the loop carries
    * on), while an artifact that will not decode is a broken loop, and only the host can
    * decide what to do about it.
    */
  def runCheck(loopId: String, prevJson: String | Null, candJson: String): Unit =
    val report: Either[String, CheckResult] =
      get(loopId) match
        case None =>
          Left(s"loop '$loopId' has no checker registered in this session")
        case Some(reg) =>
          for
            prev <- decodePrev(reg, prevJson)
            cand <- decodeCandidate(reg, candJson)
          yield invoke(reg, prev, cand)
    val payload = report match
      case Right(result) =>
        LibToolInput.jsObj(
          "passed" -> (result.passed: js.Any),
          "reasons" -> js.Array(result.reasons.map(r => r: js.Any)*),
          "metrics" -> metricsToJs(result.metrics)
        )
      case Left(error) => LibToolInput.jsObj("error" -> (error: js.Any))
    js.Dynamic.global.process.stdout.write(s"${LoopImpl.CheckMarker}:${js.JSON.stringify(payload)}\n")
    ()

  /** A checker is arbitrary user code: whatever it throws is reported as a rejection
    * rather than escaping, so a bug in a checker costs one candidate instead of
    * derailing the loop. */
  private def invoke(reg: RegisteredLoop, prev: Option[LoopGen[Any]], cand: LoopCandidate[Any]): CheckResult =
    try reg.checker(prev, cand)
    catch case t: Throwable => CheckResult.fail(s"the checker threw an exception: ${describe(t)}")

  private def describe(t: Throwable): String =
    val message = t.getMessage
    if message == null || message.isEmpty then t.toString else message

  private def decodePrev(reg: RegisteredLoop, json: String | Null): Either[String, Option[LoopGen[Any]]] =
    json match
      case null => Right(None)
      case text: String =>
        parse(text, "the previous generation").flatMap: d =>
          reg.input
            .decode(d.artifact.asInstanceOf[js.Any])
            .left
            .map(e => s"the previous generation's artifact does not match this loop's schema: $e")
            .map: artifact =>
              Some(
                LoopGen(
                  gen = num(d.gen).toInt,
                  artifact = artifact,
                  description = str(d.description),
                  commit = str(d.commit),
                  metrics = metricsOf(d.metrics),
                  // Absent on a host that predates the counting, and zero is what that means.
                  inputTokens = num(d.inputTokens).toLong,
                  outputTokens = num(d.outputTokens).toLong
                )
              )

  private def decodeCandidate(reg: RegisteredLoop, json: String): Either[String, LoopCandidate[Any]] =
    parse(json, "the candidate").flatMap: d =>
      reg.input
        .decode(d.artifact.asInstanceOf[js.Any])
        .left
        .map(e => s"the candidate's artifact does not match this loop's schema: $e")
        .map(artifact => new Candidate(artifact, str(d.description), strList(d.commits)))

  private final class Candidate(val artifact: Any, val description: String, val commits: List[String])
      extends LoopCandidate[Any]

  private def parse(text: String, what: String): Either[String, js.Dynamic] =
    try Right(js.JSON.parse(text))
    catch case t: Throwable => Left(s"$what was not readable as JSON: ${describe(t)}")

  private def str(v: js.Dynamic): String =
    if js.typeOf(v) == "string" then v.asInstanceOf[String] else ""

  private def num(v: js.Dynamic): Double =
    if js.typeOf(v) == "number" then v.asInstanceOf[Double] else 0.0

  private def strList(v: js.Dynamic): List[String] =
    if js.Array.isArray(v) then
      v.asInstanceOf[js.Array[js.Any]].toList.filter(e => js.typeOf(e) == "string").map(_.asInstanceOf[String])
    else Nil

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

  private def metricsToJs(metrics: Map[String, Double]): js.Any =
    val o = js.Dynamic.literal()
    metrics.foreach((k, v) => o.updateDynamic(k)(v))
    o

/** Implementation of [[LoopHandle]]: the id plus a read-through to the status mirror,
  * and the steering calls, which are the same acts [[LoopImpl]] performs with the id
  * already supplied. See [[LoopHandle]] for the contract. */
private[library] final class LoopHandleImpl(val id: String, loops: LoopImpl) extends LoopHandle:
  def status: LoopStatus = loops.statusOf(id)
  def park(): Unit = loops.conn.park(id)
  def resume(): Unit = loops.conn.resume(id)

  def amend[A](goal: String, rubric: String, budgets: LoopBudgets = LoopBudgets())(
      checker: LoopChecker[A]
  )(using ti: LibToolInput[A]): Unit =
    loops.define(id, goal, rubric, budgets, checker, ti, amending = true)
    ()

  def reconfigure(
      goal: Option[String] = None,
      rubric: Option[String] = None,
      budgets: Option[LoopBudgets] = None
  ): Unit = loops.retune(id, goal, rubric, budgets)

  def generations: List[LoopGen[js.Dynamic]] = loops.lineageOf(id)

  override def toString: String = s"Loop($id: ${status.render})"

/** Implementation of [[LoopApi]]. Construction is inert (reads no environment, opens no
  * socket): the REPL preamble builds one in every worker, including those with no loop
  * socket. Forcing `client` opens the connection or throws "loops are unavailable".
  *
  * Two modes, decided by the environment the host spawned this worker with:
  *
  *   - NORMAL (`AUK_LOOP_SOCK` only): `start` submits the loop to the host and prints
  *     the in-band marker that tells `eval_scala` to capture this eval's source.
  *     [[LoopHandle.amend]] prints its own marker and submits nothing at all: a
  *     redefinition IS its source, and the host reads the rest of it out of the gate
  *     session.
  *   - ATTACH (`AUK_LOOP_ATTACH=<id>`): this worker exists only to hold one loop's
  *     definition. `start` for that id binds the checker and acknowledges it; `start`
  *     for any other id is an error, since the captured source is supposed to define
  *     exactly the loop it was captured for. An amendment behaves identically there —
  *     a stored definition is re-evaluated by whichever call wrote it, and both are
  *     the same act from the gate session's point of view. A stored amendment reads
  *     `lib.loop.get("<id>").amend(...)`, so [[get]] answers for the attach target
  *     without consulting the mirror at all: the gate session is told what to bind by
  *     the environment it was spawned with, and its mirror may know nothing yet.
  *
  * Both modes register the checker locally, so the two paths differ only in what they
  * tell the host.
  *
  * See [[LoopApi]] for the contract.
  */
private[library] final class LoopImpl(shell: Shell) extends LoopApi:
  private lazy val client: LoopClient = LoopImpl.connect()

  private lazy val attachTarget: Option[String] = LoopImpl.envOpt("AUK_LOOP_ATTACH")

  def start[A](id: String, goal: String, rubric: String, budgets: LoopBudgets = LoopBudgets())(
      checker: LoopChecker[A]
  )(using ti: LibToolInput[A]): LoopHandle =
    define(id, goal, rubric, budgets, checker, ti, amending = false)

  /** [[start]] and [[LoopHandle.amend]] are one act with two announcements: register the
    * checker here, then tell the host either "this is a new loop" (a `hello` plus the
    * start marker) or "this is a new definition of one it already has" (the amend marker
    * alone). In attach mode neither announcement is made — the host is re-running a
    * definition it already holds, and all it wants back is the binding. */
  private[library] def define[A](
      id: String,
      goal: String,
      rubric: String,
      budgets: LoopBudgets,
      checker: LoopChecker[A],
      ti: LibToolInput[A],
      amending: Boolean
  ): LoopHandle =
    val verb = if amending then "amend" else "start"
    if id == null || !id.matches("[A-Za-z0-9_-]+") then
      throw new IllegalArgumentException(s"invalid loop id '$id': use only letters, digits, '-' and '_'")
    if goal == null || goal.trim.isEmpty then throw new IllegalArgumentException("loop goal is empty")
    if rubric == null || rubric.trim.isEmpty then throw new IllegalArgumentException("loop rubric is empty")
    val c = client // availability check + ensures the mirror is being fed
    attachTarget match
      case Some(target) if target != id =>
        throw new IllegalArgumentException(
          s"attach-mode session may only bind '$target', not '$id': the captured definition must define the loop it was captured for")
      case Some(_) =>
        LoopRegistry.register(id, checker, ti)
        c.bound(id, goal, rubric, budgets, js.JSON.stringify(ti.schema))
      case None =>
        if amending && LoopImpl.notSteerable(c, id) then
          throw new IllegalArgumentException(
            s"unknown loop '$id': amend redefines a loop that already exists; lib.loop.list shows the ones this session knows about")
        LoopRegistry.register(id, checker, ti)
        if !amending then
          c.hello(id, goal, rubric, budgets, js.JSON.stringify(ti.schema), checkerRegistered = true)
          c.echo(id, LoopStatus.Validating)
        // The in-band marker on the captured stdout tells the host's eval_scala that a
        // loop was defined here, so it can hand this eval's whole source to the bridge
        // once the eval completes. Which marker decides what the bridge does with it.
        val marker = if amending then LoopImpl.AmendMarker else LoopImpl.StartMarker
        js.Dynamic.global.process.stdout.write(s"$marker:$id\n")
    new LoopHandleImpl(id, this)

  /** [[LoopHandle.reconfigure]] with the id already supplied. */
  private[library] def retune(
      id: String,
      goal: Option[String],
      rubric: Option[String],
      budgets: Option[LoopBudgets]
  ): Unit =
    val c = client // availability check
    if goal.isEmpty && rubric.isEmpty && budgets.isEmpty then
      throw new IllegalArgumentException(
        s"reconfigure of loop '$id' names nothing to change: pass at least one of goal, rubric or budgets")
    if LoopImpl.notSteerable(c, id) then
      throw new IllegalArgumentException(
        s"unknown loop '$id'; lib.loop.list shows the loops this session knows about")
    c.reconfigure(id, goal, rubric, budgets)

  def get(id: String): LoopHandle =
    val c = client // availability check
    attachTarget match
      // The gate session is told which loop it exists for by its environment, and a
      // stored amendment reaches its loop through `get`. Its mirror may hold nothing at
      // all — the host has said nothing to it yet — so the target is answered from the
      // environment rather than from the mirror, and everything else is refused for the
      // same reason `start` refuses it.
      case Some(target) if target == id => new LoopHandleImpl(id, this)
      case Some(target) =>
        throw new IllegalArgumentException(
          s"attach-mode session may only reach '$target', not '$id': the captured definition must define the loop it was captured for")
      case None =>
        if LoopImpl.unheardOf(c, id) then
          throw new IllegalArgumentException(
            s"unknown loop '$id'; lib.loop.list shows the loops this session knows about")
        new LoopHandleImpl(id, this)

  def list: List[LoopHandle] = client.ids.map(id => new LoopHandleImpl(id, this))

  def diff(fromCommit: String, toCommit: String, paths: String*): String =
    val args = LoopImpl.diffArgs(fromCommit, toCommit, paths.toList)
    val r = shell.run("git", args*)
    if r.ok then r.stdout
    else throw new RuntimeException(s"git diff $fromCommit..$toCommit failed: ${r.output.trim}")

  // -- package-private accessors for LoopHandleImpl --------------------------------
  private[library] def conn: LoopClient = client
  private[library] def statusOf(id: String): LoopStatus = client.status(id).getOrElse(LoopStatus.Unknown)
  private[library] def lineageOf(id: String): List[LoopGen[js.Dynamic]] = client.generations(id)

private[library] object LoopImpl:
  /** Printed (as `"$StartMarker:$loopId"` on its own line) to the captured stdout by
    * [[LoopImpl.start]] so the host's eval_scala tool knows a loop was started here and
    * can hand it the eval's source. Must match the prefix in
    * `auk.runtime.EvalScala.LoopStartMarker`. */
  val StartMarker: String = "auk:loop:start"

  /** The same, for [[LoopImpl.amend]]: the eval is a REDEFINITION of a loop that
    * already exists, so the bridge validates it against the loop's recorded artifact
    * schema and attaches it as a new definition version rather than creating anything.
    * Must match the prefix in `auk.runtime.EvalScala.LoopAmendMarker`. */
  val AmendMarker: String = "auk:loop:amend"

  /** Printed (as `"$CheckMarker:$json"` on its own line) by [[LoopRegistry.runCheck]],
    * which the host evaluates in a loop's gate session: it is how a checker's verdict
    * gets back out of a REPL that can only answer with text. Must match the prefix in
    * `auk.runtime.LoopBridge.CheckMarker`. */
  val CheckMarker: String = "auk:loop:check"

  /** Whether this worker can say `id` is not a loop at all — what [[LoopImpl.get]] asks
    * before refusing to hand out a handle.
    *
    * Two ways to know it IS one: the host named it in a snapshot (a loop this project
    * holds, whoever started it), or this session bound its checker. A client the host
    * has not described the world to yet knows neither, so it claims nothing: the first
    * `lib.loop` call in a session is the one that OPENS the connection, and answering it
    * from an empty mirror would tell a lead its project has no loops in the one eval
    * that cannot know. A [[LoopStatus.Failed]] loop still counts as heard of — that
    * status IS how a refusal is read back ([[LoopHandle.status]]), so a handle has to be
    * reachable. */
  def unheardOf(client: LoopClient, id: String): Boolean =
    client.informed && client.status(id).isEmpty && LoopRegistry.get(id).isEmpty

  /** The same question for the steering calls, which need a loop that EXISTS rather
    * than one that has been heard of: [[LoopStatus.Failed]] is the host's record of a
    * loop it refused, and there is nothing there to retune or redefine. */
  def notSteerable(client: LoopClient, id: String): Boolean =
    client.informed && client.status(id).forall(_.isInstanceOf[LoopStatus.Failed]) && LoopRegistry.get(id).isEmpty

  private def env: js.Dynamic = js.Dynamic.global.process.env

  def envOpt(name: String): Option[String] =
    val v = env.selectDynamic(name)
    if v == null || js.isUndefined(v) then None else Some(v.asInstanceOf[String])

  /** Open the persistent connection using `AUK_LOOP_SOCK`, or fail clearly when this
    * worker has no host loop bridge (a workflow-node or team-member REPL). */
  def connect(): LoopClient =
    envOpt("AUK_LOOP_SOCK") match
      case Some(sock) => new LoopClient(sock)
      case None       => throw new RuntimeException(unavailable)

  /** Why there is no loop bridge here. A loop's own generation worker is the one
    * case worth naming: it has no loop socket ON PURPOSE, so the honest answer is
    * the rule rather than the missing variable — a generation that could start a
    * loop of its own would be spending a budget nobody granted it, inside a tree
    * another loop is already snapshotting. Every other socketless worker (a
    * workflow node, a team member) gets the plain reason. */
  private def unavailable: String =
    envOpt("AUK_LOOP_WORKER") match
      case Some(where) =>
        s"loops cannot be nested: this worker is $where, and a generation may not start or " +
          "amend a loop of its own. Workflows (lib.wf) and team members (lib.team) are " +
          "available here; do the work in one of those instead."
      case None =>
        "loops are unavailable: AUK_LOOP_SOCK is not set (the host loop bridge is not connected)"

  /** The `git diff` invocation behind [[LoopApi.diff]], armored so the patch depends on
    * the two trees and nothing else: no color, no external diff driver, no textconv
    * filter, the `a/`/`b/` prefixes pinned against config that would rename or drop
    * them, paths never quoted or escaped, and renames spelled out as a delete plus an
    * add (a rename reported as such hides the content a reader needs). Comparing
    * `^{tree}`s rather than the commits themselves keeps it working for a commit whose
    * parentage is irrelevant here. */
  def diffArgs(fromCommit: String, toCommit: String, paths: List[String]): List[String] =
    val base = List(
      "-c", "core.quotePath=false",
      "-c", "diff.mnemonicPrefix=false",
      "-c", "diff.noprefix=false",
      "diff",
      "--no-color",
      "--no-ext-diff",
      "--no-textconv",
      "--no-renames",
      s"$fromCommit^{tree}",
      s"$toCommit^{tree}"
    )
    if paths.isEmpty then base else base ++ ("--" :: paths)
