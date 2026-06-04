import org.scalajs.linker.interface.{ModuleKind, ModuleSplitStyle, ESVersion}
import scala.sys.process.{Process, ProcessLogger}

ThisBuild / scalaVersion := "3.8.3"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"

// Standalone single-file `auk` binary, produced as a Node.js single-executable
// application (SEA). Auk runs on V8/Node — JavaScriptCore's WebAssembly JSPI
// (stack switching) crashes intermittently, so Bun is not a supported runtime.
lazy val packageBinary = taskKey[File]("Build a standalone `auk` binary as a Node SEA (dist/auk).")

// --- helpers for packageBinary (operate on the Scala.js linker output) ---

// Apply `target` -> `repl` exactly once; fail loudly if the anchor is missing,
// since that means the Scala.js loader format drifted and the patch is stale.
def patchOnce(src: String, target: String, repl: String, what: String): String = {
  if (!src.contains(target))
    sys.error(s"packageBinary: could not find $what in __loader.js — the Scala.js " +
      "loader format changed; update the patch in build.sbt.")
  src.replace(target, repl)
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

lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
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

    packageBinary := {
      val log     = streams.value.log
      // Full optimization (auk-opt): smallest Wasm, dead-code eliminated.
      val linkDir = (Compile / fullLinkJSOutput).value
      val baseDir = baseDirectory.value
      val plog    = ProcessLogger(l => log.info(l), l => log.error(l))
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

      // 1. Bundle the ESM app + npm deps into one .mjs. ESM (not cjs/iife) is
      //    required because the entry uses top-level await; node: builtins stay
      //    external; the banner gives bundled CJS deps a working `require`.
      val bundle = stage / "auk.bundle.mjs"
      val esbuildCmd = Seq(
        esbuildBin.getAbsolutePath,
        (stage / "main.js").getAbsolutePath,
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
      IO.write(seaConfig,
        s"""{
           |  "main": "${js(bundle)}",
           |  "mainFormat": "module",
           |  "output": "${js(blob)}",
           |  "disableExperimentalSEAWarning": true,
           |  "useSnapshot": false,
           |  "useCodeCache": false,
           |  "assets": { "main.wasm": "${js(stage / "main.wasm")}" }
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
  )
