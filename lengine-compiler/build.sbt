name := "lengine-compiler"

enablePlugins(JavaAppPackaging)

mainClass := Some("co.gyeongmin.lisp.compile.Main")

scalacOptions := Seq(
  "-deprecation"
)

assembly / assemblyJarName := "lenginec.jar"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.ow2.asm" % "asm" % "9.11"

Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat

// Compiled scripts may call (exit); run them in their own JVM so a stray exit cannot take sbt down.
Test / fork := true
Test / baseDirectory := (ThisBuild / baseDirectory).value
