name := "lengine-repl"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)

mainClass := Some("co.gyeongmin.lisp.Main")

assemblyJarName in assembly := "lengine.jar"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scalatestplus" %% "easymock-3-2" % "3.2.5.0" % "test"

libraryDependencies += "org.ow2.asm" % "asm" % "9.3"
