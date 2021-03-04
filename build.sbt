name := "lengine"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)

mainClass := Some("co.gyeongmin.lisp.Main")

assemblyJarName in assembly := "lengine.jar"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scalatestplus" %% "easymock-3-2" % "3.2.5.0" % "test"

// https://mvnrepository.com/artifact/org.ow2.asm/asm
libraryDependencies += "org.ow2.asm" % "asm" % "7.2"
