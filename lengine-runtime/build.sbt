name := "lengine-runtime"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

assemblyJarName in assembly := "lengine-runtime.jar"

sources in (Compile,doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false

libraryDependencies += "org.ow2.asm" % "asm" % "9.3"
libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.4"
