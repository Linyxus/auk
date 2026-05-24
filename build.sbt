ThisBuild / scalaVersion := "3.8.3"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"

lazy val root = (project in file("."))
  .settings(
    name := "auk",
    scalacOptions ++= Seq(
      "-deprecation", "-feature", "-unchecked",
      "-Yexplicit-nulls", "-Wsafe-init",
      "-language:experimental.modularity",
    ),
    libraryDependencies += "xyz.matthieucourt" %% "layoutz" % "0.7.0",
    libraryDependencies += "com.openai" % "openai-java" % "4.29.1",
    libraryDependencies += "com.anthropic" % "anthropic-java" % "2.18.0",
    libraryDependencies += "ch.epfl.lamp" %% "gears" % "0.2.0",
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.38.12",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
  )
