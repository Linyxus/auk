import org.scalajs.linker.interface.{ModuleKind, ModuleSplitStyle, ESVersion}
import scala.sys.process.{Process, ProcessLogger}

ThisBuild / scalaVersion := "3.8.3"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"

// Standalone single-file `auk` binary, produced as a Node.js single-executable
// application (SEA). Auk runs on V8/Node — JavaScriptCore's WebAssembly JSPI
// (stack switching) crashes intermittently, so Bun is not a supported runtime.
lazy val packageBinary = taskKey[File]("Build a standalone `auk` binary as a Node SEA (dist/auk).")

// The eval_scala tool drives a JSONL REPL worker built from the scala3-js fork
// (the Scala 3 compiler linked to JS + the sjsir interpreter). vendorRepl pulls
// the three runtime artifacts out of that checkout into vendor/repl/, which is
// where dev runs and tests find them and where packageBinary embeds them from.
lazy val vendorRepl = taskKey[File](
  "Copy the Scala REPL worker and compiler archives from a scala3-js checkout (SCALA3_JS_HOME) into vendor/repl/."
)

// The fourth artifact in vendor/repl/ is built here, not vendored: the auk
// runtime library that evaluated code calls (the `library` subproject), packed
// into library.bin and preloaded into every session via the worker's
// `--classpath` flag.
lazy val packLibraryBin = taskKey[File](
  "Pack the library subproject's .tasty + .sjsir output into vendor/repl/library.bin for the REPL worker's --classpath."
)

lazy val startTestWebUI = taskKey[Unit](
  "Link webui + webui-dev (fastLinkJS), assemble a served dir, and run the mock SSE server under node in the foreground (Ctrl+C to stop)."
)

// The live workflow dashboard the host serves: vendorWebUI links the browser
// bundle (fullLinkJS) and stages it into vendor/webui/ — where `sbt run` finds it
// and packageBinary embeds it from. Consumed at runtime by auk.platform.js.WebUiAssets.
lazy val vendorWebUI = taskKey[File](
  "Link the webui browser bundle (fullLinkJS) and copy it + a manifest into vendor/webui/ for the host dashboard."
)

// --- helpers for packageBinary (operate on the Scala.js linker output) ---

// Apply `target` -> `repl` exactly once; fail loudly if the anchor is missing,
// since that means the Scala.js loader format drifted and the patch is stale.
def patchOnce(src: String, target: String, repl: String, what: String): String = {
  if (!src.contains(target))
    sys.error(s"packageBinary: could not find $what in __loader.js — the Scala.js " +
      "loader format changed; update the patch in build.sbt.")
  src.replace(target, repl)
}

// Write the packed `.bin` archive the REPL worker loads (`ClasspathBlob` in
// the scala3-js fork, same format as classpath.bin / linker-libs.bin):
// [4 bytes: index length, big-endian uint32][JSON index path -> [offset,
// size]][concatenated file bytes].
def writeBinArchive(entries: Seq[(String, File)], outputFile: File): Unit = {
  val data = new java.io.ByteArrayOutputStream()
  val index = new StringBuilder("{")
  var offset = 0
  for (((rel, file), i) <- entries.zipWithIndex) {
    val bytes = IO.readBytes(file)
    if (i > 0) index.append(',')
    index.append('"').append(rel).append("\":[").append(offset).append(',').append(bytes.length).append(']')
    data.write(bytes)
    offset += bytes.length
  }
  index.append('}')
  val indexBytes = index.toString.getBytes("UTF-8")
  val out = new java.io.BufferedOutputStream(new java.io.FileOutputStream(outputFile))
  try {
    out.write((indexBytes.length >> 24) & 0xff)
    out.write((indexBytes.length >> 16) & 0xff)
    out.write((indexBytes.length >> 8) & 0xff)
    out.write(indexBytes.length & 0xff)
    out.write(indexBytes)
    data.writeTo(out)
  } finally {
    out.close()
  }
}

// Rewrite the linker's `__loader.js` so the Wasm loads from inside a Node
// single-executable (SEA). A SEA has no sibling `main.wasm` on disk, so when
// running as one we read it from the embedded asset via `node:sea`. Outside a
// SEA (e.g. `node main.js` in dev) the original file/fetch path still applies,
// so this patch is safe to apply unconditionally.
def patchLoaderForSea(src: String): String = {
  val target = "  const resolvedURL = new URL(wasmFileURL, import.meta.url);"
  val seaBranch =
    """  // [auk packageBinary] In a Node single-executable, main.wasm is an
      |  // embedded asset, not a file on disk — read it from node:sea. Falls
      |  // through to the file/fetch path below when not running as a SEA.
      |  {
      |    const { createRequire } = await import("node:module");
      |    const __sea = createRequire(import.meta.url)("node:sea");
      |    if (__sea.isSea && __sea.isSea()) {
      |      const __key = wasmFileURL.replace(/^\.\//, "");
      |      return WebAssembly.instantiate(__sea.getAsset(__key), importsObj, options);
      |    }
      |  }
      |  const resolvedURL = new URL(wasmFileURL, import.meta.url);""".stripMargin
  patchOnce(src, target, seaBranch, "the Wasm-loading block")
}

// The auk runtime library preloaded into eval_scala REPL sessions. Compiled
// with the regular toolchain — the fork's 3.10-based REPL compiler reads
// 3.8.3 TASTy, and both builds emit Scala.js 1.21 IR — then packed by
// packLibraryBin and loaded by the worker, never linked into auk itself.
lazy val library = (project in file("library"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "auk-library",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),

    // Tests run as plain Scala.js under Node (the default jsEnv), so munit
    // exercises the real `auk.library` against a real file system and real
    // child processes — the same Node `fs`/`path`/`child_process` modules the
    // production REPL preloads, just reached directly instead of through a
    // worker. (The root project's FsLibrarySuite/ShellLibrarySuite cover the
    // packed-and-preloaded path end-to-end; these cover the code in isolation.)
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
    testFrameworks += new TestFramework("munit.Framework"),

    // The library reaches Node built-ins via `js.Dynamic.global.require`, which
    // in production is injected onto globalThis by the REPL worker bootstrap.
    // Under the default Node jsEnv `require` is module-local, not global, so a
    // prelude script publishes it before the test code loads.
    Test / jsEnvInput := {
      val prelude = (Test / resourceManaged).value / "require-global.js"
      IO.write(prelude, "globalThis.require = require;\n")
      org.scalajs.jsenv.Input.Script(prelude.toPath) +: (Test / jsEnvInput).value
    },
  )

// Shared workflow domain (OrchestrationEvent, Forest + its pure fold) and the
// host->browser JSON codecs (js.JSON-based, so they link under both the WasmGC
// root and the plain-JS web UI). No linker-config override: the depender's config
// governs how this module's .sjsir is emitted.
lazy val workflowProtocol = (project in file("workflow-protocol"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "auk-workflow-protocol",
    // No -language:experimental.modularity: it would mark every type here
    // @experimental, which the non-experimental webui/webuiDev couldn't consume.
    // The moved types don't use modularity features anyway.
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Yexplicit-nulls", "-Wsafe-init"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
  )

lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(workflowProtocol)
  .settings(
    name := "auk",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("auk.main"),
    scalacOptions ++= Seq(
      "-deprecation", "-feature", "-unchecked",
      "-Yexplicit-nulls", "-Wsafe-init",
      "-language:experimental.modularity",
    ),
    // Scala.js experimental WebAssembly backend (WasmGC + JSPI). Requires ESModule
    // output and an ES version with async/await (ES2017+) for JSPI suspension.
    scalaJSLinkerConfig ~= { c =>
      c.withExperimentalUseWebAssembly(true)
        .withModuleKind(ModuleKind.ESModule)
        .withESFeatures(_.withESVersion(ESVersion.ES2017))
        .withModuleSplitStyle(ModuleSplitStyle.FewestModules)
    },
    // gears' Scala.js/Wasm build is published only as a snapshot
    resolvers += "central-snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
    libraryDependencies += "ch.epfl.lamp" %%% "gears" % "0.3.0+2-ea3d0958-SNAPSHOT",
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.38.12",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,

    // Embed the runtime library's interface source as a string constant
    // (auk.generated.LibrarySource), so the agent can show the exact API that
    // is preloaded into REPL sessions in its system prompt. Regenerated when
    // AukInterface.scala changes; left untouched otherwise so incremental
    // compilation stays quiet.
    Compile / sourceGenerators += Def.task {
      val interfaceFile =
        (library / Compile / scalaSource).value / "auk" / "library" / "AukInterface.scala"
      if (!interfaceFile.exists)
        sys.error(s"LibrarySource generator: $interfaceFile not found")
      def lit(s: String): String = s.flatMap {
        case '\\' => "\\\\"
        case '"'  => "\\\""
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case c    => c.toString
      }
      val content =
        s"""package auk.generated
           |
           |/** Generated by build.sbt from library/src/main/scala/auk/library/AukInterface.scala — do not edit. */
           |object LibrarySource:
           |  /** The source text of [[auk.library.AukInterface]], verbatim. */
           |  val interface: String = "${lit(IO.read(interfaceFile))}"
           |""".stripMargin
      val out = (Compile / sourceManaged).value / "auk" / "generated" / "LibrarySource.scala"
      if (!out.exists || IO.read(out) != content) IO.write(out, content)
      Seq(out)
    }.taskValue,

    vendorRepl := {
      val log = streams.value.log
      val home = file(sys.env.getOrElse("SCALA3_JS_HOME", sys.props("user.home") + "/workspace/scala3-js"))
      if (!home.isDirectory)
        sys.error(s"vendorRepl: scala3-js checkout not found at $home — set SCALA3_JS_HOME")

      // Pick the newest build product when several Scala-version dirs exist.
      def newest(base: File, name: String, parent: String, hint: String): File = {
        val all = (base ** name).get().filter(f => parent.isEmpty || f.getParentFile.getName == parent)
        if (all.isEmpty) sys.error(s"vendorRepl: $name not found under $base — build it first ($hint)")
        all.maxBy(_.lastModified)
      }
      val workerTargets = home / "repl-js-json" / "target"
      // Prefer the fully-optimised worker; fall back to the fastLink output.
      val fullOpt = (workerTargets ** "main.js").get().filter(_.getParentFile.getName == "scala3-repl-json-opt")
      val worker =
        if (fullOpt.nonEmpty) fullOpt.maxBy(_.lastModified)
        else newest(workerTargets, "main.js", "scala3-repl-json-fastopt", "sbt scala3-repl-json-sjs/fastLinkJS")
      val cliTargets = home / "compiler-js-cli" / "target"
      val classpath = newest(cliTargets, "classpath.bin", "", "sbt scala3-compiler-cli-sjs/packClasspath")
      val linkerLibs = newest(cliTargets, "linker-libs.bin", "", "sbt scala3-compiler-cli-sjs/packLinkerLibs")

      val dir = baseDirectory.value / "vendor" / "repl"
      IO.createDirectory(dir)
      IO.copyFile(worker, dir / "repl-worker.js")
      IO.copyFile(classpath, dir / "classpath.bin")
      IO.copyFile(linkerLibs, dir / "linker-libs.bin")
      log.info(s"vendorRepl: $worker")
      log.info(s"vendorRepl: $classpath")
      log.info(s"vendorRepl: $linkerLibs")
      log.info(s"vendorRepl: -> $dir")
      dir
    },

    packLibraryBin := {
      val log = streams.value.log
      val _ = (library / Compile / compile).value
      val classes = (library / Compile / classDirectory).value
      val base = classes.toPath
      def rel(f: File) = base.relativize(f.toPath).toString.replace('\\', '/')
      // Same selection as the fork's packLibBin task: .tasty carries the API
      // for the REPL compiler, .sjsir the code for its interpreter; .class
      // files are kept only when no .tasty sibling exists.
      val tastyFiles = (classes ** "*.tasty").get()
      val tastyPaths = tastyFiles.map(f => rel(f).stripSuffix(".tasty")).toSet
      val classFiles = (classes ** "*.class").get()
        .filterNot(f => tastyPaths.contains(rel(f).stripSuffix(".class")))
      val sjsirFiles = (classes ** "*.sjsir").get()
      val entries = (tastyFiles ++ classFiles ++ sjsirFiles).map(f => (rel(f), f)).sortBy(_._1)
      if (entries.isEmpty)
        sys.error("packLibraryBin: the library subproject produced no .tasty/.sjsir output")
      val out = baseDirectory.value / "vendor" / "repl" / "library.bin"
      IO.createDirectory(out.getParentFile)
      writeBinArchive(entries, out)
      log.info(s"packLibraryBin: ${entries.size} entries (${out.length} bytes) -> $out")
      out
    },

    vendorWebUI := {
      val log = streams.value.log
      val _ = (webui / Compile / fullLinkJS).value
      val webuiOut = (webui / Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
      val dir = baseDirectory.value / "vendor" / "webui"
      IO.delete(dir)
      IO.createDirectory(dir)
      // The linked bundle (FewestModules may emit several files) + static assets,
      // all flat (index.html references ./main.js and ./styles.css relatively).
      (webuiOut * "*").get().filter(_.isFile).foreach(f => IO.copyFile(f, dir / f.getName))
      IO.copyDirectory((webui / baseDirectory).value / "web", dir, overwrite = true)
      val files = (dir * "*").get().filter(_.isFile).sortBy(_.getName)
      if (!files.exists(_.getName == "index.html"))
        sys.error("vendorWebUI: no index.html was produced — check webui/web/index.html")
      // Manifest = a content tag (names the SEA extraction cache so a stale one is
      // never reused) + the served file list, for WebUiAssets to extract from a SEA.
      val tag = Hash.toHex(Hash(files.map(f => Hash.toHex(Hash(f))).mkString)).take(16)
      IO.write(dir / "webui-manifest", (tag +: files.map(_.getName)).mkString("\n"))
      log.info(s"vendorWebUI: ${files.size} files -> $dir")
      dir
    },

    packageBinary := {
      val log     = streams.value.log
      // Full optimization (auk-opt): smallest Wasm, dead-code eliminated.
      val linkDir = (Compile / fullLinkJSOutput).value
      // Repack the runtime library so vendor/repl/library.bin is never stale.
      val _       = packLibraryBin.value
      val baseDir = baseDirectory.value
      val plog    = ProcessLogger(l => log.info(l), l => log.info(l))
      val devNull = ProcessLogger(_ => (), _ => ())
      val isMac   = System.getProperty("os.name").toLowerCase.contains("mac")

      // The SEA host is the running Node binary; the produced binary IS Node with
      // our app embedded, so it runs on V8 (mature JSPI) rather than Bun/JSC.
      if (Process(Seq("node", "--version")) ! devNull != 0)
        sys.error("packageBinary: `node` not found on PATH — install Node.js 25+")
      val nodeExec = Process(Seq("node", "-e", "process.stdout.write(process.execPath)")).!!.trim
      if (nodeExec.isEmpty) sys.error("packageBinary: could not locate the Node executable")

      // Build tooling (esbuild bundler, postject injector) runs under bun, which
      // is fast and already used for installs; it is NOT the runtime.
      if (Process(Seq("bun", "--version")) ! devNull != 0)
        sys.error("packageBinary: `bun` not found on PATH (used only to run installs/esbuild/postject)")

      // npm SDKs (openai / @anthropic-ai/sdk) get bundled into the app, and the
      // build needs esbuild + postject — ensure all are installed.
      val esbuildBin  = baseDir / "node_modules" / ".bin" / "esbuild"
      val postjectBin = baseDir / "node_modules" / ".bin" / "postject"
      if (!(baseDir / "node_modules").exists || !esbuildBin.exists || !postjectBin.exists) {
        log.info("packageBinary: installing JS deps (incl. esbuild, postject) via `bun install`…")
        if (Process(Seq("bun", "install"), baseDir) ! plog != 0)
          sys.error("packageBinary: `bun install` failed")
      }

      // Stage a patched copy of the linker output (never mutate auk-opt itself,
      // so the dev relink loop stays clean and re-runs are idempotent).
      val stage = target.value / "sea-package"
      IO.delete(stage)
      IO.createDirectory(stage)
      IO.copyFile(linkDir / "main.js", stage / "main.js")
      IO.copyFile(linkDir / "main.wasm", stage / "main.wasm")
      IO.write(stage / "__loader.js", patchLoaderForSea(IO.read(linkDir / "__loader.js")))

      // Stage the embedded Scala REPL: the JSONL worker script, the two
      // compiler archives it loads at runtime, and the auk runtime library it
      // preloads via --classpath. The first three are vendored by
      // `sbt vendorRepl`, library.bin is packed by the packLibraryBin
      // dependency above; consumed at runtime by auk.platform.js.ReplArtifacts.
      val replDir = baseDir / "vendor" / "repl"
      val replFiles = Seq("repl-worker.js", "classpath.bin", "linker-libs.bin", "library.bin")
        .map(replDir / _)
      val replMissing = replFiles.filterNot(_.exists)
      if (replMissing.nonEmpty)
        sys.error(s"packageBinary: ${replMissing.map(_.getName).mkString(", ")} not found in " +
          "vendor/repl — run `sbt vendorRepl` first")
      replFiles.foreach(f => IO.copyFile(f, stage / f.getName))
      // Content tag naming the runtime extraction cache (tmpdir/auk-repl-<tag>):
      // any artifact change yields a new tag, so a stale cache is never reused.
      val replTag = Hash.toHex(Hash(replFiles.map(f => Hash.toHex(Hash(f))).mkString)).take(16)
      IO.write(stage / "repl-manifest", replTag)

      // Stage the embedded web dashboard bundle (served by the host
      // WorkflowWebServer; consumed at runtime by auk.platform.js.WebUiAssets).
      // Vendored by `sbt vendorWebUI`. The bundle is staged under stage/webui/ and
      // keyed "webui/<name>" so its main.js never collides with the app's main.js;
      // webui-manifest lists the files to extract.
      val webuiDir = baseDir / "vendor" / "webui"
      val webuiManifestSrc = webuiDir / "webui-manifest"
      if (!webuiManifestSrc.exists)
        sys.error("packageBinary: vendor/webui/webui-manifest not found — run `sbt vendorWebUI` first")
      val webuiStage = stage / "webui"
      IO.createDirectory(webuiStage)
      val webuiFiles = (webuiDir * "*").get().filter(f => f.isFile && f.getName != "webui-manifest")
      webuiFiles.foreach(f => IO.copyFile(f, webuiStage / f.getName))
      IO.copyFile(webuiManifestSrc, stage / "webui-manifest")

      // Optionally bake ZAI_API_KEY into the binary for demo/advertisement
      // distribution — so a downloaded `auk` runs out of the box. OFF by
      // default: only when DANGEROUSLY_PACK_KEY=1 is set AND ZAI_API_KEY is
      // present in the build environment. The key lands in PLAINTEXT inside
      // dist/auk (trivially recoverable via `strings dist/auk`); never bake a
      // production key. `??=` means a user's own ZAI_API_KEY still wins.
      val bakedKeyLine: String =
        if (sys.env.get("DANGEROUSLY_PACK_KEY").contains("1")) {
          val key = sys.env.getOrElse("ZAI_API_KEY", sys.error(
            "packageBinary: DANGEROUSLY_PACK_KEY=1 but ZAI_API_KEY is not set in the build environment"))
          log.warn("packageBinary: DANGEROUSLY baking ZAI_API_KEY into dist/auk — " +
            "it will be extractable in plaintext from the binary")
          val esc = key.replace("\\", "\\\\").replace("\"", "\\\"")
          s"""  process.env.ZAI_API_KEY ??= "$esc";\n"""
        } else ""

      // The SEA entry. `auk --repl-worker` must boot the REPL worker from
      // inside the packaged binary (process.execPath IS auk there, so the
      // parent can't spawn a plain `node worker.js`); this wrapper routes the
      // flag before the app loads. Keep the branch in sync with its non-SEA
      // twin, ReplArtifacts.BootstrapSource.
      val entry = stage / "entry.mjs"
      IO.write(entry,
        s"""if (process.argv.includes("--repl-worker")) {
          |  // Load the worker the parent extracted to disk. CJS require, not
          |  // import(): a SEA's dynamic import only resolves built-in modules
          |  // (ERR_UNKNOWN_BUILTIN_MODULE for disk files), while a
          |  // createRequire'd require reads from disk fine. The same require
          |  // goes on globalThis: the worker (and the Scala code it evaluates)
          |  // reaches Node built-ins through it.
          |  const { createRequire } = await import("node:module");
          |  const worker = process.env.AUK_REPL_WORKER_JS;
          |  if (!worker) { console.error("--repl-worker: AUK_REPL_WORKER_JS is not set"); process.exit(2); }
          |  const req = createRequire(worker);
          |  globalThis.require = req;
          |  req(worker);
          |} else {
          |$bakedKeyLine  await import("./main.js");
          |}
          |""".stripMargin)

      // 1. Bundle the ESM app + npm deps into one .mjs. ESM (not cjs/iife) is
      //    required because the entry uses top-level await; node: builtins stay
      //    external; the banner gives bundled CJS deps a working `require`.
      val bundle = stage / "auk.bundle.mjs"
      val esbuildCmd = Seq(
        esbuildBin.getAbsolutePath,
        entry.getAbsolutePath,
        "--bundle", "--platform=node", "--format=esm", "--target=node25",
        "--banner:js=import { createRequire as __auk_cr } from 'node:module'; const require = __auk_cr(import.meta.url);",
        s"--outfile=${bundle.getAbsolutePath}"
      )
      log.info("packageBinary: bundling app + SDKs with esbuild…")
      if (Process(esbuildCmd, baseDir) ! plog != 0)
        sys.error("packageBinary: esbuild bundling failed")

      // 2. SEA config. mainFormat=module for the ESM/TLA entry; snapshot and code
      //    cache OFF (both are incompatible with ESM / dynamic import). main.wasm
      //    is embedded as an asset and read back via node:sea in the loader. No
      //    JSPI flag: it is on by default in Node 25+ (and the experimental flag
      //    is rejected there).
      val blob      = stage / "sea-prep.blob"
      val seaConfig = stage / "sea-config.json"
      def js(p: File): String = p.getAbsolutePath.replace("\\", "\\\\").replace("\"", "\\\"")
      // Assets are assembled programmatically because the webui file set is dynamic
      // (FewestModules may emit several *.js). The same enumeration drives both the
      // SEA keys here and what WebUiAssets reads back, so they can't drift.
      val assetEntries: Seq[(String, File)] =
        Seq(
          "main.wasm" -> (stage / "main.wasm"),
          "repl-worker.js" -> (stage / "repl-worker.js"),
          "classpath.bin" -> (stage / "classpath.bin"),
          "linker-libs.bin" -> (stage / "linker-libs.bin"),
          "library.bin" -> (stage / "library.bin"),
          "repl-manifest" -> (stage / "repl-manifest"),
          "webui-manifest" -> (stage / "webui-manifest")
        ) ++ webuiFiles.map(f => s"webui/${f.getName}" -> (webuiStage / f.getName))
      val assetsJson = assetEntries.map { case (k, f) => s"""    "$k": "${js(f)}"""" }.mkString(",\n")
      IO.write(seaConfig,
        s"""{
           |  "main": "${js(bundle)}",
           |  "mainFormat": "module",
           |  "output": "${js(blob)}",
           |  "disableExperimentalSEAWarning": true,
           |  "useSnapshot": false,
           |  "useCodeCache": false,
           |  "assets": {
           |$assetsJson
           |  }
           |}
           |""".stripMargin)
      log.info("packageBinary: generating SEA blob…")
      if (Process(Seq("node", "--experimental-sea-config", seaConfig.getAbsolutePath), baseDir) ! plog != 0)
        sys.error("packageBinary: `node --experimental-sea-config` failed")

      // 3. Copy the Node binary and inject the blob into it.
      val dist   = baseDir / "dist"
      IO.createDirectory(dist)
      val outBin = dist / "auk"
      IO.delete(outBin)
      IO.copyFile(file(nodeExec), outBin)
      outBin.setExecutable(true)

      // macOS: a signed binary must have its signature removed before injection
      // and re-applied after, or the kernel refuses to run the modified arm64 image.
      if (isMac) Process(Seq("codesign", "--remove-signature", outBin.getAbsolutePath), baseDir) ! devNull

      val postjectCmd = Seq(
        postjectBin.getAbsolutePath, outBin.getAbsolutePath, "NODE_SEA_BLOB", blob.getAbsolutePath,
        "--sentinel-fuse", "NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2"
      ) ++ (if (isMac) Seq("--macho-segment-name", "NODE_SEA") else Nil)
      log.info("packageBinary: injecting the SEA blob with postject…")
      if (Process(postjectCmd, baseDir) ! plog != 0)
        sys.error("packageBinary: postject injection failed")

      if (isMac && Process(Seq("codesign", "--sign", "-", outBin.getAbsolutePath), baseDir) ! plog != 0)
        sys.error("packageBinary: codesign failed")

      log.info(s"packageBinary: wrote ${outBin.length / (1024 * 1024)}M Node SEA binary at $outBin")
      outBin
    },

    // Dev loop for the web UI: link the browser bundle + the mock server, stage a
    // served directory, and run the server under node in the foreground.
    startTestWebUI := {
      val log = streams.value.log
      (webui / Compile / fastLinkJS).value
      (webuiDev / Compile / fastLinkJS).value
      val webuiOut = (webui / Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
      val devOut = (webuiDev / Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value

      // Stage the served site: the webui bundle (FewestModules may emit several
      // files) plus index.html + styles.css.
      val serveDir = target.value / "webui-serve"
      IO.delete(serveDir)
      IO.createDirectory(serveDir)
      (webuiOut * "*").get().foreach(f => IO.copyFile(f, serveDir / f.getName))
      IO.copyDirectory((webui / baseDirectory).value / "web", serveDir, overwrite = true)

      val serverMain = devOut / "main.js"
      if (!serverMain.exists)
        sys.error(s"startTestWebUI: $serverMain not found after fastLinkJS")

      val port = sys.env.getOrElse("AUK_WEBUI_PORT", "8080")
      log.info(s"startTestWebUI: serving $serveDir on http://localhost:$port")
      log.info("startTestWebUI: scenarios — ?scenario=loop|fanout|bigFanout|failures|flatMapFrontier|multi|paused (Ctrl+C to stop)")
      val plog = ProcessLogger(l => log.info(l), l => log.error(l))
      // Foreground/blocking; SIGINT (Ctrl+C) exits 130, which we treat as clean.
      val code = Process(Seq("node", serverMain.getAbsolutePath, serveDir.getAbsolutePath, port), baseDirectory.value) ! plog
      if (code != 0 && code != 130)
        sys.error(s"startTestWebUI: node server exited with code $code")
    },
  )

// The browser dashboard (Laminar). Plain JS backend (not WasmGC) targeting the
// DOM; ESModule so index.html can `<script type="module">` it. Depends only on
// the shared protocol module — never on the host.
lazy val webui = (project in file("webui"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(workflowProtocol)
  .settings(
    name := "auk-webui",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("auk.webui.main"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    scalaJSLinkerConfig ~= { c =>
      c.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.FewestModules)
    },
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "17.2.1",
      "org.scalameta" %%% "munit" % "1.1.1" % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework"),
  )

// The dev-only mock SSE/HTTP server (run under Node). Replays scripted scenarios
// so the web UI can be developed without the agent runtime. Depends only on the
// shared protocol module; no Laminar/DOM.
lazy val webuiDev = (project in file("webui-dev"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(workflowProtocol)
  .settings(
    name := "auk-webui-dev",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("auk.webui.dev.main"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    scalaJSLinkerConfig ~= { c =>
      c.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.FewestModules)
    },
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
  )
