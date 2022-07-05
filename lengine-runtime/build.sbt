name := "lengine-runtime"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

libraryDependencies += "org.ow2.asm" % "asm" % "9.3"
