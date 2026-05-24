ThisBuild / scalaVersion := "3.8.3"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"

lazy val root = (project in file("."))
  .settings(
    name := "auk",
    libraryDependencies += "xyz.matthieucourt" %% "layoutz" % "0.7.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
  )
