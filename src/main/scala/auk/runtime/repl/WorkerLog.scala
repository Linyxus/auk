package auk.runtime.repl

import scala.scalajs.js
import scala.util.control.NonFatal

import auk.llm.tools.Json
import auk.platform.{FileSystem, PathOps, Platform}

/** Append-only JSONL event log for one REPL worker process.
  *
  * The record is what the *parent* observed: request lines exactly as written
  * to the child's stdin, stdout lines exactly as read plus the classification
  * the protocol layer gave them, and every decision taken about the child
  * (kill, settle, exit). Nothing is truncated or summarised — a log that
  * paraphrases cannot answer "what did the worker actually say".
  *
  * Appends are synchronous, so a crashed or SIGKILLed parent loses no event
  * that had already happened. Every write is guarded: the first IO failure
  * disables the instance permanently (after one stderr warning) and all later
  * events are dropped, because logging must never be able to fail an eval.
  *
  * Environment is recorded as key names only; values may hold credentials and
  * are never written, which is why [[spawned]] takes no map.
  */
trait WorkerLog:
  def spawned(argv: List[String], generation: Int, envKeys: List[String], pid: Option[Int]): Unit
  def spawnFailed(error: String): Unit

  /** The exact request line written to the child's stdin (no trailing newline). */
  def sent(line: String): Unit

  /** An exact stdout line and how the protocol layer classified it:
    * `reply` | `hello` | `received` | `tick` | `progress` | `unknown` |
    * `garbage`. */
  def recv(line: String, kind: String): Unit

  def stderrChunk(chunk: String): Unit

  /** How an in-flight request ended: `completed` | `failed` | `timed-out`. */
  def settled(status: String, detail: String): Unit

  /** An observed liveness transition, with the silence that triggered it and
    * the pipeline stage the worker had last named (`None` when it had named
    * none). The phase rides along with the state; it never triggers an event of
    * its own, so this stream stays a record of liveness transitions rather than
    * of the worker's chatter — a post-mortem reads one event and has the whole
    * picture, without correlating it against nearby `recv` lines. */
  def liveness(state: String, silentMs: Long, phase: Option[String] = None): Unit

  /** Why the parent killed the child: `timeout` | `cancel` | `close` |
    * `preamble-failure` | `shutdown`. */
  def killed(cause: String): Unit

  /** Child exit. `stale` marks an exit the generation guard discarded. */
  def exit(code: Int, stale: Boolean): Unit

object WorkerLog:
  /** Discards everything; the instance used when logging is off. */
  val Noop: WorkerLog = new WorkerLog:
    def spawned(argv: List[String], generation: Int, envKeys: List[String], pid: Option[Int]): Unit = ()
    def spawnFailed(error: String): Unit = ()
    def sent(line: String): Unit = ()
    def recv(line: String, kind: String): Unit = ()
    def stderrChunk(chunk: String): Unit = ()
    def settled(status: String, detail: String): Unit = ()
    def liveness(state: String, silentMs: Long, phase: Option[String]): Unit = ()
    def killed(cause: String): Unit = ()
    def exit(code: Int, stale: Boolean): Unit = ()

/** Factories for file-backed [[WorkerLog]]s. */
object WorkerLogs:
  /** Log files kept in a directory; the oldest beyond this are pruned when a
    * new worker starts, so an unattended session cannot fill the disk. */
  val MaxFiles = 50

  private val Suffix = ".jsonl"

  /** A per-worker log factory writing `<dir>/<label>-gen<N>-pid<P>.jsonl`
    * (`P` = `na` when the spawn gave no pid). Each call prunes `dir` to the
    * newest [[MaxFiles]] - 1 files, then opens a new one; failures anywhere
    * yield a disabled log rather than an exception. */
  def apply(dir: String, label: String): (Int, Option[Int]) => WorkerLog =
    (generation, pid) =>
      val name = s"$label-gen$generation-pid${pid.fold("na")(_.toString)}$Suffix"
      FileWorkerLog(dir, PathOps.join(dir, name), Platform.fs)

  val NoopFactory: (Int, Option[Int]) => WorkerLog = (_, _) => WorkerLog.Noop

  /** Appends one JSON object per line, self-disabling on the first IO error. */
  private final class FileWorkerLog(dir: String, path: String, fs: FileSystem) extends WorkerLog:
    private var disabled = false

    guarded:
      fs.createDirectories(dir)
      prune()

    def spawned(argv: List[String], generation: Int, envKeys: List[String], pid: Option[Int]): Unit =
      emit(
        "spawned",
        List(
          "argv"       -> Json.Arr(argv.map(Json.Str.apply)),
          "generation" -> Json.num(generation),
          "envKeys"    -> Json.Arr(envKeys.map(Json.Str.apply))
        ) ++ pid.map(p => "pid" -> Json.num(p))
      )

    def spawnFailed(error: String): Unit =
      emit("spawn-failed", List("error" -> Json.Str(error)))

    def sent(line: String): Unit =
      emit("sent", List("line" -> Json.Str(line)))

    def recv(line: String, kind: String): Unit =
      emit("recv", List("line" -> Json.Str(line), "kind" -> Json.Str(kind)))

    def stderrChunk(chunk: String): Unit =
      emit("stderr", List("chunk" -> Json.Str(chunk)))

    def settled(status: String, detail: String): Unit =
      emit("settled", List("status" -> Json.Str(status), "detail" -> Json.Str(detail)))

    def liveness(state: String, silentMs: Long, phase: Option[String]): Unit =
      emit(
        "liveness",
        List("state" -> Json.Str(state), "silentMs" -> Json.num(silentMs)) ++
          phase.map(p => "phase" -> Json.Str(p))
      )

    def killed(cause: String): Unit =
      emit("killed", List("cause" -> Json.Str(cause)))

    def exit(code: Int, stale: Boolean): Unit =
      emit("exit", List("code" -> Json.num(code), "stale" -> Json.Bool(stale)))

    private def emit(ev: String, fields: List[(String, Json)]): Unit =
      if !disabled then
        val line = Json.Obj(
          ("t" -> Json.num(js.Date.now().toLong)) :: ("ev" -> Json.Str(ev)) :: fields
        ).render
        guarded(fs.appendString(path, line + "\n"))

    /** Drop the newest-[[MaxFiles]] tail's older siblings, leaving room for
      * the file about to be opened. Ordering is by mtime: the names carry a
      * generation counter that restarts with every session. */
    private def prune(): Unit =
      val logs = fs.listDir(dir).filter(e => e.isFile && e.name.endsWith(Suffix))
      if logs.size >= MaxFiles then
        logs
          .sortBy(_.mtimeMs)
          .take(logs.size - MaxFiles + 1)
          .foreach(e => fs.removeAll(PathOps.join(dir, e.name)))

    private def guarded(body: => Unit): Unit =
      try body
      catch
        case NonFatal(e) =>
          disabled = true
          warn(s"auk: worker log disabled ($path): ${e.getMessage}\n")

    private def warn(message: String): Unit =
      try js.Dynamic.global.process.stderr.write(message)
      catch case NonFatal(_) => ()
      ()
