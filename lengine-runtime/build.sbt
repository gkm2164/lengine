name := "lengine-runtime"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

assemblyJarName in assembly := "lengine-runtime.jar"

libraryDependencies += "org.ow2.asm" % "asm" % "9.3"
libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.4"