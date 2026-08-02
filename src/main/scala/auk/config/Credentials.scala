package auk.config

import scala.collection.immutable.VectorMap
import scala.util.control.NonFatal

import auk.platform.{PathOps, Platform}

/** `~/.auk/credentials`: a `[keys]` section of lowercase provider name → API
  * key. An open section, so adding a provider needs no schema change.
  */
case class CredentialsFile(
    keys: VectorMap[String, String]
) derives ConfigSchema

object CredentialsFile:
  val empty: CredentialsFile = CredentialsFile(VectorMap.empty)

/** The user-level API-key store: `~/.auk/credentials`, written with mode 0600.
  *
  * This is the fallback behind the provider env vars: `Provider.apiKey` reads
  * the env var first, then this store, so scripts and CI keep overriding per
  * process while an interactive user adds a key once via /login. User-level
  * rather than project-level on purpose — a key inside a project's `.auk/` is
  * one `git add` away from a leak.
  *
  * Reads are cached (the TUI shows key status per frame); [[save]] refreshes
  * the cache, and a file edited by hand mid-session is picked up on restart. A
  * malformed file degrades reads to "no keys" (surfaced once via [[problem]])
  * but makes [[save]] refuse, so whatever else the file held is never silently
  * clobbered. `AUK_NO_KEYS=1` makes reads see an empty store — the keyless-
  * start escape hatch for testing — while saves still write the real file.
  */
object Credentials:
  /** Env flag: pretend the store is empty (testing keyless behaviour). */
  val NoKeysEnv = "AUK_NO_KEYS"

  /** Where the store lives, or None when HOME is unset. */
  def path: Option[String] =
    Platform.env.get("HOME").map(home => PathOps.join(PathOps.join(home, ".auk"), "credentials"))

  private var cache: Option[CredentialsFile] = None

  /** POSIX `0600`: readable and writable by the owner only. */
  private val Mode600 = 384

  /** The cached store content. Empty on an absent, unreadable or malformed
    * file — the store degrades on reads; only the writers refuse. The
    * `AUK_NO_KEYS` flag masks the whole store, so keyless testing sees a
    * truly fresh auk. */
  private def snapshot: CredentialsFile =
    if Platform.env.get(NoKeysEnv).contains("1") then CredentialsFile.empty
    else
      cache.getOrElse {
        val loaded = load().toOption.flatten.getOrElse(CredentialsFile.empty)
        cache = Some(loaded)
        loaded
      }

  /** Stored keys by lowercase provider name. */
  def keys: VectorMap[String, String] = snapshot.keys

  /** The stored key for `provider` (case-insensitive), if any. */
  def get(provider: String): Option[String] =
    keys.get(provider.toLowerCase).map(_.trim).filter(_.nonEmpty)

  /** Why reads see nothing despite a file being present, if that is the case —
    * the one-line diagnosis for a startup notice. */
  def problem: Option[String] = load().left.toOption

  /** Add or replace `provider`'s key and write the whole file back (directory
    * created, mode 0600). Refuses on a malformed existing file rather than
    * dropping whatever else it held. */
  def save(provider: String, key: String): Either[String, Unit] =
    write(f => f.copy(keys = f.keys.updated(provider.toLowerCase, key.trim)))

  /** Load-merge-write under the malformed-file guard. */
  private def write(update: CredentialsFile => CredentialsFile): Either[String, Unit] =
    path match
      case None => Left("HOME is not set, so there is no ~/.auk/credentials to write")
      case Some(p) =>
        load() match
          case Left(err) => Left(s"refusing to overwrite it: $err")
          case Right(existing) =>
            val merged = update(existing.getOrElse(CredentialsFile.empty))
            try
              PathOps.parent(p).foreach(Platform.fs.createDirectories)
              Platform.fs.writeString(p, render(merged))
              Platform.fs.chmod(p, Mode600)
              cache = Some(merged)
              Right(())
            catch case NonFatal(e) => Left(s"could not write $p: ${e.getMessage}")

  /** Test seam: drop the read cache so the next read hits the file. */
  private[auk] def invalidate(): Unit = cache = None

  /** Parse the file: Right(None) when absent (or HOME unset). */
  private def load(): Either[String, Option[CredentialsFile]] =
    path match
      case None => Right(None)
      case Some(p) =>
        if !Platform.fs.exists(p) then Right(None)
        else
          try
            Config.parse[CredentialsFile](Platform.fs.readString(p)) match
              case Right(c)   => Right(Some(c))
              case Left(errs) => Left(s"$p: " + errs.map(_.render).mkString("; "))
          catch case NonFatal(e) => Left(s"could not read $p: ${e.getMessage}")

  private def render(file: CredentialsFile): String =
    val sb = new StringBuilder("[keys]\n")
    file.keys.foreach((name, k) => sb.append(s"$name = ${AppConfig.scalarValue(k)}\n"))
    sb.toString
