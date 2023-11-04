name := "lengine-compiler"

enablePlugins(JavaAppPackaging)

mainClass := Some("co.gyeongmin.lisp.compile.Main")

assemblyJarName in assembly := "lenginec.jar"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"
libraryDependencies += "org.ow2.asm" % "asm" % "9.3"

Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
