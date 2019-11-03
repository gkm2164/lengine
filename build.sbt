name := "lengine"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)

mainClass := Some("co.gyeongmin.lisp.Main")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
