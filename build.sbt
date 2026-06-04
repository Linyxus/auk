import org.scalajs.linker.interface.{ModuleKind, ModuleSplitStyle, ESVersion}
import scala.sys.process.{Process, ProcessLogger}

ThisBuild / scalaVersion := "3.8.3"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"

// Standalone single-file `auk` binary, produced by `bun build --compile`.
lazy val packageBinary = taskKey[File]("Build a standalone `auk` binary via Bun (dist/auk).")

// --- helpers for packageBinary (operate on the Scala.js linker output) ---

// Apply `target` -> `repl` exactly once; fail loudly if the anchor is missing,
// since that means the Scala.js loader format drifted and the patch is stale.
def patchOnce(src: String, target: String, repl: String, what: String): String = {
  if (!src.contains(target))
    sys.error(s"packageBinary: could not find $what in __loader.js — the Scala.js " +
      "loader format changed; update the patch in build.sbt.")
  src.replace(target, repl)
}

// Rewrite the linker's `__loader.js` so it works inside a compiled Bun binary:
//   1. Embed the sibling `main.wasm` as a `file` asset (lands in $bunfs), so the
//      binary is self-contained instead of reading a sibling file off disk.
//   2. Load the Wasm from that embedded asset rather than resolving an
//      import.meta.url-relative path that does not exist in the binary.
//   3. Drop the native `js-string` builtins so the bundled JS polyfills are used
//      instead — JavaScriptCore (Bun's engine) has a conformance bug that blanks
//      String.split's last segment, corrupting the TUI.
def patchLoaderForBinary(src: String): String = {
  val withImport =
    "import __aukWasm from \"./main.wasm\" with { type: \"file\" };\n" + src
  val loadBlock =
    """  const resolvedURL = new URL(wasmFileURL, import.meta.url);
      |  if (resolvedURL.protocol === 'file:') {
      |    const { fileURLToPath } = await import("node:url");
      |    const { readFile } = await import("node:fs/promises");
      |    const wasmPath = fileURLToPath(resolvedURL);
      |    const body = await readFile(wasmPath);
      |    return WebAssembly.instantiate(body, importsObj, options);
      |  } else {
      |    return await WebAssembly.instantiateStreaming(fetch(resolvedURL), importsObj, options);
      |  }""".stripMargin
  val embeddedLoad =
    """  // [auk packageBinary] Read the Wasm from the embedded asset — a compiled
      |  // Bun binary has no sibling main.wasm on disk.
      |  const body = await Bun.file(__aukWasm).arrayBuffer();
      |  return WebAssembly.instantiate(body, importsObj, options);""".stripMargin
  val a = patchOnce(withImport, """builtins: ["js-string"]""", "builtins: []",
    "the js-string builtins request")
  patchOnce(a, loadBlock, embeddedLoad, "the Wasm-loading block")
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

      if (Process(Seq("bun", "--version")) ! ProcessLogger(_ => (), _ => ()) != 0)
        sys.error("packageBinary: `bun` not found on PATH — install from https://bun.sh")

      // npm SDKs (openai / @anthropic-ai/sdk) are bundled into the binary by
      // `bun build`, so node_modules must be populated first.
      if (!(baseDir / "node_modules").exists) {
        log.info("packageBinary: node_modules missing — running `bun install`…")
        if (Process(Seq("bun", "install"), baseDir) ! plog != 0)
          sys.error("packageBinary: `bun install` failed")
      }

      // Stage a patched copy of the linker output (never mutate auk-opt itself,
      // so the dev relink loop stays clean and re-runs are idempotent).
      val stage = target.value / "bun-package"
      IO.delete(stage)
      IO.createDirectory(stage)
      IO.copyFile(linkDir / "main.js", stage / "main.js")
      IO.copyFile(linkDir / "main.wasm", stage / "main.wasm")
      IO.write(stage / "__loader.js", patchLoaderForBinary(IO.read(linkDir / "__loader.js")))

      val dist   = baseDir / "dist"
      IO.createDirectory(dist)
      val outBin = dist / "auk"
      log.info(s"packageBinary: bun build --compile -> $outBin")
      val cmd = Seq("bun", "build", "--compile", "--minify",
        (stage / "main.js").getAbsolutePath, "--outfile", outBin.getAbsolutePath)
      if (Process(cmd, baseDir) ! plog != 0)
        sys.error("packageBinary: `bun build --compile` failed")

      log.info(s"packageBinary: wrote ${outBin.length / 1024}K binary at $outBin")
      outBin
    },
  )
