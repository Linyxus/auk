package auk.runtime

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import gears.async.Async
import auk.llm.tools.{Tool, ToolInput, ToolResult, RuntimeContext, Json, desc}

/** Project-scoped, persistent key-value memory.
  *
  * Memory is stored as a single JSON object at `.auk/memory.json` under the
  * session's working directory, so it is per-project and survives across
  * sessions. Entries keep their insertion order; updating an existing key
  * rewrites its value in place.
  *
  * Reads and writes are serialized on a process-wide lock: the agent loop fans
  * tool calls out concurrently, so two `write_memory` calls in the same turn
  * could otherwise read-modify-write the file on top of each other. The lock is
  * coarse (one for all projects), but memory operations are small and rare, so
  * the simplicity is worth more than the lost parallelism.
  */
object MemoryStore:
  /** Location of the store, relative to the working directory. */
  val RelativePath = ".auk/memory.json"

  private val lock = new AnyRef

  private def file(ctx: RuntimeContext): Path = ctx.resolve(RelativePath)

  /** All entries in stored order. A missing file is an empty store; a malformed
    * or wrongly-shaped file is an error so a corrupt store is not silently
    * treated as empty (which would invite overwriting it).
    */
  def load(ctx: RuntimeContext): Either[String, List[(String, String)]] =
    lock.synchronized:
      val path = file(ctx)
      if !Files.exists(path) then Right(Nil)
      else
        try
          Json.parse(Files.readString(path, UTF_8).nn) match
            case Left(err) => Left(s"memory store is corrupt: $err")
            case Right(Json.Obj(fields)) =>
              Right(fields.collect { case (k, Json.Str(v)) => (k, v) })
            case Right(other) =>
              Left(s"memory store has an unexpected shape: ${other.typeName}")
        catch
          case e: IOException =>
            Left(s"failed to read memory store: ${e.getMessage}")

  /** Upsert `content` under `key`. Returns whether the key already existed (so
    * the caller can report "updated" vs "stored").
    */
  def update(
      ctx: RuntimeContext,
      key: String,
      content: String
  ): Either[String, Boolean] =
    lock.synchronized:
      load(ctx).flatMap: entries =>
        val existed = entries.exists(_._1 == key)
        val next =
          if existed then
            entries.map { case (k, v) => if k == key then (k, content) else (k, v) }
          else entries :+ (key -> content)
        save(ctx, next).map(_ => existed)

  private def save(
      ctx: RuntimeContext,
      entries: List[(String, String)]
  ): Either[String, Unit] =
    val path = file(ctx)
    try
      val parent = path.getParent
      if parent != null then Files.createDirectories(parent)
      val json = Json.Obj(entries.map((k, v) => k -> Json.Str(v)))
      Files.writeString(path, json.render, UTF_8)
      Right(())
    catch
      case e: IOException =>
        Left(s"failed to write memory store: ${e.getMessage}")

case class WriteMemoryParams(
    @desc(
      "A short, stable key identifying this memory, e.g. \"build_command\" or " +
        "\"test_db_setup\". Reusing a key overwrites its value."
    )
    key: String,
    @desc("The text to store under this key. Replaces any existing value.")
    content: String
) derives ToolInput

/** Save a durable, project-specific note the agent can recall in later turns and
  * sessions. Writes only to the agent's own `.auk/memory.json`, so — like a
  * read-only tool — it does not consult the approval policy.
  */
object WriteMemory extends Tool:
  type Params = WriteMemoryParams

  val name = "write_memory"

  val description =
    "Save a project-specific note under a key so it can be recalled later with " +
      "get_memory. Memory persists across sessions for this project. Use it to " +
      "remember durable facts (build/test commands, conventions, where things " +
      "live) — not transient details of the current task. Reusing a key " +
      "overwrites its value."

  val input: ToolInput[WriteMemoryParams] = ToolInput[WriteMemoryParams]

  def execute(params: WriteMemoryParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    val key = params.key.trim
    if key.isEmpty then ToolResult.error("memory key cannot be empty")
    else
      MemoryStore.update(ctx, key, params.content) match
        case Left(err) => ToolResult.error(err)
        case Right(existed) =>
          val verb = if existed then "updated" else "stored"
          ToolResult.ok(s"$verb memory '$key'", metadata = Map("existed" -> existed.toString))

case class GetMemoryParams(
    @desc(
      "The key to recall. Omit to list every stored memory (keys and contents)."
    )
    key: Option[String] = None
) derives ToolInput

/** Recall project-specific notes saved with [[WriteMemory]]. */
object GetMemory extends Tool:
  type Params = GetMemoryParams

  val name = "get_memory"

  val description =
    "Recall project-specific notes saved with write_memory. Provide a key to " +
      "fetch one memory, or omit the key to list every stored memory. Memory " +
      "persists across sessions for this project."

  val input: ToolInput[GetMemoryParams] = ToolInput[GetMemoryParams]

  def execute(params: GetMemoryParams)(using ctx: RuntimeContext, async: Async): ToolResult =
    MemoryStore.load(ctx) match
      case Left(err) => ToolResult.error(err)
      case Right(entries) =>
        params.key.map(_.trim).filter(_.nonEmpty) match
          case Some(key) => fetchOne(key, entries)
          case None      => listAll(entries)

  private def fetchOne(key: String, entries: List[(String, String)]): ToolResult =
    entries.find(_._1 == key) match
      case Some((_, value)) => ToolResult.ok(value)
      case None =>
        // A miss is a valid query with no result, not a tool failure: report it
        // (and the known keys) so the model can pick a real key or move on.
        val known =
          if entries.isEmpty then "no memories are stored yet"
          else s"stored keys: ${entries.map(_._1).mkString(", ")}"
        ToolResult.ok(s"no memory found for '$key' ($known)")

  private def listAll(entries: List[(String, String)]): ToolResult =
    if entries.isEmpty then ToolResult.ok("(no memories stored)")
    else
      val rendered = entries.map((k, v) => s"## $k\n$v").mkString("\n\n")
      ToolResult.ok(rendered, metadata = Map("count" -> entries.length.toString))
