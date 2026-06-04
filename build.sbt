import org.scalajs.linker.interface.{ModuleKind, ModuleSplitStyle, ESVersion}

ThisBuild / scalaVersion := "3.8.3"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"

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
  )
