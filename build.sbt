ThisBuild / organization := "io.lengine"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.14"

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
