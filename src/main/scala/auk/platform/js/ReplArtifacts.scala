package auk.platform.js

import scala.scalajs.js

/** Locates the Scala REPL worker artifacts and builds the spawn spec for the
  * worker process.
  *
  * The worker is the JSONL stdio REPL from the scala3-js fork: a linked
  * Scala.js script (`repl-worker.js`) plus the two binary archives the
  * JS-hosted compiler loads at runtime (`classpath.bin`, the .class/.tasty
  * compilation classpath; `linker-libs.bin`, the stdlib .sjsir the interpreter
  * executes), and `library.bin` — the auk runtime library (.tasty + .sjsir,
  * packed by `sbt packLibraryBin`), preloaded into the session through the
  * worker's `--classpath` flag so evaluated code can call it. They are looked
  * up, in order:
  *
  *   1. `$AUK_REPL_DIR` — explicit override, must hold all four files.
  *   2. The `node:sea` assets of the running single-executable, extracted to a
  *      content-tagged cache under the system temp dir (the tag is the
  *      `repl-manifest` asset, written by packageBinary).
  *   3. `vendor/repl/` under the working directory — the dev default,
  *      populated by `sbt vendorRepl` and `sbt packLibraryBin`.
  *
  * The worker script reaches Node built-ins through a CommonJS-style global
  * `require` (and so does the Scala code it evaluates), which exists in
  * neither an ES-module nor a SEA context — so the worker is never imported
  * directly. Outside a SEA, the generated [[BootstrapSource]] module installs
  * `globalThis.require` and then loads it; inside one, the same logic sits
  * behind the binary's own `--repl-worker` entry branch (packageBinary's
  * entry.mjs), because `process.execPath` there is the auk binary itself, not
  * a plain `node`.
  */
object ReplArtifacts:

  /** How to launch the worker: the argv to spawn plus the env vars it reads. */
  final case class Spawn(argv: List[String], env: Map[String, String])

  val WorkerFile = "repl-worker.js"
  val ClasspathFile = "classpath.bin"
  val LinkerLibsFile = "linker-libs.bin"
  val LibraryFile = "library.bin"
  val ManifestAsset = "repl-manifest"
  private val Files = List(WorkerFile, ClasspathFile, LinkerLibsFile, LibraryFile)

  def resolve(): Either[String, Spawn] =
    NodeEnv.get("AUK_REPL_DIR") match
      case Some(dir) => fromDir(dir)
      case None if SeaAssets.isSea => fromSea()
      case None =>
        val vendored = NodePath.join(GlobalProcess.cwd(), "vendor", "repl")
        if Files.forall(f => NodeFs.existsSync(NodePath.join(vendored, f))) then fromDir(vendored)
        else
          Left(
            "Scala REPL artifacts not found. Run `sbt vendorRepl packLibraryBin` to " +
              s"populate vendor/repl/, or point AUK_REPL_DIR at a directory holding " +
              s"${Files.mkString(", ")}."
          )

  private def fromDir(dir: String): Either[String, Spawn] =
    val missing = Files.filterNot(f => NodeFs.existsSync(NodePath.join(dir, f)))
    if missing.nonEmpty then
      Left(s"Scala REPL artifacts missing from $dir: ${missing.mkString(", ")}")
    else
      Right(
        Spawn(
          List(GlobalProcess.execPath, writeBootstrap()) ::: classpathArgs(dir),
          envFor(dir)
        )
      )

  private def fromSea(): Either[String, Spawn] =
    SeaAssets.string(ManifestAsset).map(_.trim.nn) match
      case None =>
        Left("this auk binary was built without the REPL assets (repl-manifest missing)")
      case Some(tag) =>
        extracted(tag).map: dir =>
          Spawn(
            List(GlobalProcess.execPath, "--repl-worker") ::: classpathArgs(dir),
            envFor(dir)
          )

  /** Preload the auk runtime library: the worker splits `--classpath` off its
    * argv (`ReplBootstrap.extractClasspathArgs` in the fork) in both spawn
    * shapes — after the bootstrap script path, and after `--repl-worker`.
    */
  private def classpathArgs(dir: String): List[String] =
    List("--classpath", NodePath.join(dir, LibraryFile))

  private def envFor(dir: String): Map[String, String] =
    Map(
      "AUK_REPL_WORKER_JS" -> NodePath.join(dir, WorkerFile),
      "DOTTY_CLASSPATH_BIN" -> NodePath.join(dir, ClasspathFile),
      "DOTTY_LINKER_LIBS_BIN" -> NodePath.join(dir, LinkerLibsFile)
    )

  /** The SEA assets on disk, extracted once per content tag. Extraction goes to
    * a process-private directory first and is renamed into place, so a
    * concurrent auk never observes a half-written cache; losing the rename race
    * just means using the winner's identical copy.
    */
  private def extracted(tag: String): Either[String, String] =
    val dir = NodePath.join(nodeTmpdir(), s"auk-repl-$tag")
    def complete = Files.forall(f => NodeFs.existsSync(NodePath.join(dir, f)))
    if complete then Right(dir)
    else
      val tmp = s"$dir.tmp-${GlobalProcess.pid}"
      try
        NodeFs.mkdirSync(tmp, js.Dynamic.literal(recursive = true).asInstanceOf[js.Object])
        val missing = Files.find: f =>
          SeaAssets.bytes(f) match
            case Some(b) => NodeFs.writeFileSync(NodePath.join(tmp, f), b); false
            case None => true
        missing match
          case Some(f) =>
            Left(s"this auk binary was built without the REPL assets ($f missing)")
          case None =>
            if AssetCache.publish(tmp, dir, complete) then Right(dir)
            else Left(s"failed to extract the REPL assets to $dir")
      finally
        if NodeFs.existsSync(tmp) then
          try
            NodeFs.rmSync(
              tmp,
              js.Dynamic.literal(recursive = true, force = true).asInstanceOf[js.Object]
            )
          catch case _: Throwable => ()

  /** Write the non-SEA bootstrap module and return its path. Rewritten on every
    * spawn; the content is constant, so concurrent writers are harmless. */
  private def writeBootstrap(): String =
    val path = NodePath.join(nodeTmpdir(), "auk-repl-bootstrap.mjs")
    NodeFs.writeFileSync(path, BootstrapSource, "utf8")
    path

  /** Keep in sync with the `--repl-worker` branch of packageBinary's entry.mjs,
    * which is this module's in-SEA twin. The worker is loaded with a CJS
    * `require`, not `import()`: inside a SEA, dynamic import resolves only
    * built-in modules, and the CJS path behaves identically outside one. The
    * same `require` goes on `globalThis`, where the worker (and the Scala code
    * it evaluates) looks up Node built-ins. */
  private val BootstrapSource =
    """// Generated by auk: boots the Scala REPL JSONL worker.
      |import { createRequire } from "node:module";
      |const worker = process.env.AUK_REPL_WORKER_JS;
      |if (!worker) { console.error("auk repl bootstrap: AUK_REPL_WORKER_JS is not set"); process.exit(2); }
      |const req = createRequire(worker);
      |globalThis.require = req;
      |req(worker);
      |""".stripMargin
