name := "lengine-exec"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)

mainClass := Some("co.gyeongmin.lisp.Main")

assemblyJarName in assembly := "lengine.jar"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.12"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scalatestplus" %% "easymock-3-2" % "3.2.5.0" % "test"
