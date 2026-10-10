ThisBuild / organization := "io.lengine"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.18"
// sbt-scoverage 2.4.4 defaults to a scalac plugin that predates Scala 2.13.18.
ThisBuild / coverageScalacPluginVersion := "2.5.2"

releaseIgnoreUntrackedFiles := true

lazy val runtime = project in file("lengine-runtime")

lazy val core =
  (project in file("lengine-core")).dependsOn(runtime)

lazy val repl =
  (project in file("lengine-repl")).dependsOn(core, runtime)

lazy val compiler =
  (project in file("lengine-compiler")).dependsOn(core, runtime)

lazy val lengine = (project in file("."))
  .aggregate(
    core,
    runtime,
    repl,
    compiler
  )
