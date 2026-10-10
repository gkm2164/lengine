package co.gyeongmin.lisp.compile.asmwriter

import java.io.FileOutputStream
import scala.collection.mutable
import scala.util.DynamicVariable

/**
 * Where generated classes go. lenginec writes them under the working directory; a REPL or a test
 * can swap in its own sink (for example an in-memory map) with `ClassOutput.withSink`.
 */
object ClassOutput {
  type Sink = (String, Array[Byte]) => Unit // (internal name a/b/C, bytes)

  val toFiles: Sink = (internalName, bytes) => {
    val out = new FileOutputStream(s"$internalName.class")
    try out.write(bytes)
    finally out.close()
  }

  private val current = new DynamicVariable[Sink](toFiles)

  def write(internalName: String, bytes: Array[Byte]): Unit = current.value(internalName, bytes)

  def withSink[T](sink: Sink)(body: => T): T = current.withValue(sink)(body)
}

/** Class names handed out while compiling one module, so two functions never share a class. */
object ClassNames {
  private val used = mutable.Set[String]()

  def reset(): Unit = synchronized(used.clear())

  /** `<module>$<name>`, made a valid JVM name and suffixed with $2, $3... if already taken. */
  def unique(moduleClass: String, name: String): String = synchronized {
    val safe = name.map(ch => if ("./;[<>".contains(ch)) '_' else ch)
    val base = s"$moduleClass$$$safe"
    val chosen = Iterator.from(1).map(i => if (i == 1) base else s"$base$$$i").find(!used.contains(_)).get
    used += chosen
    chosen
  }
}
