ThisBuild / organization := "co.gyeongmin.lengine"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.10"

lazy val lengine = (project in file("."))
  .aggregate(
    `lengine-core`,
    `lengine-runtime`,
    `lengine-repl`,
    `lengine-compiler`
  )


lazy val `lengine-runtime` = project in file("lengine-runtime")

lazy val `lengine-core` =
  (project in file("lengine-core")).dependsOn(`lengine-runtime`)

lazy val `lengine-repl` =
  (project in file("lengine-repl")).dependsOn(`lengine-core`, `lengine-runtime`)

lazy val `lengine-compiler` =
  (project in file("lengine-compiler")).dependsOn(`lengine-core`, `lengine-runtime`)
