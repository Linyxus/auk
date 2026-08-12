package auk.platform.js

import scala.scalajs.js

/** Locates the prebuilt `webui` browser bundle directory the host dashboard
  * serves. Mirrors [[ReplArtifacts]]'s three tiers:
  *
  *   1. `$AUK_WEBUI_DIR` — explicit override.
  *   2. The `node:sea` assets of the running single-executable, extracted to a
  *      content-tagged temp cache (the `webui-manifest` asset lists the files).
  *   3. `vendor/webui/` under the working directory — the dev default, populated
  *      by `./mill vendorWebUI`.
  *
  * Unlike the REPL's fixed file list, the bundle's set is dynamic (FewestModules
  * may emit several `*.js`), so the SEA tier reads `webui-manifest` (first line a
  * content tag, remaining lines the file names) to know what to extract. All
  * tiers resolve to a directory of flat files (`index.html`, `main.js` + chunks,
  * `styles.css`) that the server reads per request. */
object WebUiAssets:
  val ManifestAsset = "webui-manifest"
  private val Entry = "index.html"
  /** SEA asset key prefix for bundle files, so the webui's `main.js` never
    * collides with the app's own `main.js` (see `packageBinary`). */
  private val AssetPrefix = "webui/"

  def resolve(): Either[String, String] =
    NodeEnv.get("AUK_WEBUI_DIR") match
      case Some(dir)               => fromDir(dir)
      case None if SeaAssets.isSea => fromSea()
      case None =>
        val vendored = NodePath.join(GlobalProcess.cwd(), "vendor", "webui")
        if NodeFs.existsSync(NodePath.join(vendored, Entry)) then Right(vendored)
        else
          Left(
            "web dashboard assets not found. Run `./mill vendorWebUI` to populate vendor/webui/, " +
              "or point AUK_WEBUI_DIR at a directory holding the built webui bundle."
          )

  private def fromDir(dir: String): Either[String, String] =
    if NodeFs.existsSync(NodePath.join(dir, Entry)) then Right(dir)
    else Left(s"web dashboard assets missing from $dir (no $Entry)")

  private def fromSea(): Either[String, String] =
    SeaAssets.string(ManifestAsset).map(_.trim.nn) match
      case None | Some("") =>
        Left("this auk binary was built without the web dashboard assets (webui-manifest missing)")
      case Some(manifest) =>
        manifest.linesIterator.map(_.trim.nn).filter(_.nonEmpty).toList match
          case tag :: files if files.nonEmpty => extracted(tag, files)
          case _                              => Left("webui-manifest is empty")

  /** Extract the listed SEA assets into a content-tagged temp dir, once — writing
    * to a process-private dir and renaming into place (à la `ReplArtifacts.extracted`). */
  private def extracted(tag: String, files: List[String]): Either[String, String] =
    val dir = NodePath.join(nodeTmpdir(), s"auk-webui-$tag")
    def complete = NodeFs.existsSync(NodePath.join(dir, Entry))
    if complete then Right(dir)
    else
      val tmp = s"$dir.tmp-${GlobalProcess.pid}"
      try
        NodeFs.mkdirSync(tmp, js.Dynamic.literal(recursive = true).asInstanceOf[js.Object])
        val missing = files.find: f =>
          SeaAssets.bytes(AssetPrefix + f) match
            case Some(b) => NodeFs.writeFileSync(NodePath.join(tmp, f), b); false
            case None    => true
        missing match
          case Some(f) => Left(s"this auk binary is missing web dashboard asset '$f'")
          case None =>
            if AssetCache.publish(tmp, dir, complete) then Right(dir)
            else Left(s"failed to extract web dashboard assets to $dir")
      finally
        if NodeFs.existsSync(tmp) then
          try NodeFs.rmSync(tmp, js.Dynamic.literal(recursive = true, force = true).asInstanceOf[js.Object])
          catch case _: Throwable => ()
