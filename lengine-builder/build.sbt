name := "lengine-builder"

enablePlugins(JavaAppPackaging)

mainClass := Some("co.gyeongmin.lisp.builder.Main")

assemblyJarName in assembly := "lebi.jar"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
