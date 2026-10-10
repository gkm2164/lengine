name := "lengine-repl"

mainClass := Some("co.gyeongmin.lisp.Main")

assembly / assemblyJarName := "lengine.jar"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.scalatestplus" %% "easymock-5-3" % "3.2.19.0" % "test"

libraryDependencies += "org.ow2.asm" % "asm" % "9.11"
