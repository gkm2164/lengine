package co.gyeongmin.lisp.compile

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

/**
 * Runs every example in docs/SPEC.md.
 *
 * An example is a ```lengine id=<id>``` block followed by exactly one of:
 *   ```output```         expected stdout (trailing whitespace ignored)
 *   ```compile-error```  a substring of the compiler's error message
 *   ```runtime-error```  a substring of "<ExceptionClass>: <message>" of the uncaught exception
 * `crlf` on the lengine block writes the source with \r\n line endings.
 * Adding `bug=<slug>` to the lengine block marks a known deviation: the example documents the
 * intended behavior, is expected to fail today, and the suite fails if it starts passing so the
 * marker gets removed.
 */
class SpecConformanceTest extends AnyFlatSpec with Matchers {
  import SpecConformanceTest._

  private val examples = parse(new String(Files.readAllBytes(Paths.get("docs/SPEC.md")), UTF_8))

  "docs/SPEC.md" should "have examples with unique ids" in {
    examples should not be empty
    val dupes = examples.groupBy(_.id).collect { case (id, xs) if xs.size > 1 => id }
    dupes shouldBe empty
  }

  // Compile the libraries the examples may require, once.
  private lazy val libsReady: Unit =
    Seq("prelude", "collections", "stdlib", "math").foreach { lib =>
      Main.run(Array(s"./lengine-code/$lib.lg")) shouldBe 0
    }

  examples.foreach { ex =>
    val title = s"${ex.id} (SPEC.md:${ex.line})" + ex.bug.map(b => s" [known bug: $b]").getOrElse("")
    "spec example" should title in {
      libsReady
      val outcome = runExample(ex)
      val ok = ex.expect.matches(outcome)
      (ex.bug, ok) match {
        case (None, true)     => ()
        case (None, false)    => fail(s"\n--- expected ${ex.expect}\n--- got $outcome\n--- source:\n${ex.source}")
        case (Some(_), false) => cancel(s"known deviation, got $outcome")
        case (Some(b), true)  => fail(s"example now passes; remove bug=$b from SPEC.md")
      }
    }
  }
}

object SpecConformanceTest {
  sealed trait Outcome
  final case class Printed(out: String) extends Outcome {
    override def toString: String = s"output:\n$out"
  }
  final case class CompileFailed(message: String) extends Outcome {
    override def toString: String = s"compile error: $message"
  }
  final case class Crashed(error: String, out: String) extends Outcome {
    override def toString: String = s"runtime error: $error\n(output before it: $out)"
  }

  sealed trait Expect { def matches(o: Outcome): Boolean }
  final case class ExpectOutput(text: String) extends Expect {
    def matches(o: Outcome): Boolean = o match {
      case Printed(out) => norm(out) == norm(text)
      case _            => false
    }
    override def toString: String = s"output:\n$text"
  }
  final case class ExpectCompileError(fragment: String) extends Expect {
    def matches(o: Outcome): Boolean = o match {
      case CompileFailed(m) => m.contains(fragment)
      case _                => false
    }
    override def toString: String = s"compile error containing: $fragment"
  }
  final case class ExpectRuntimeError(fragment: String) extends Expect {
    def matches(o: Outcome): Boolean = o match {
      case Crashed(e, _) => e.contains(fragment)
      case _             => false
    }
    override def toString: String = s"runtime error containing: $fragment"
  }

  final case class Example(id: String, bug: Option[String], source: String, expect: Expect, line: Int, crlf: Boolean)

  private def norm(s: String): String = s.linesIterator.map(_.replaceAll("\\s+$", "")).mkString("\n").trim

  private val Fence: Regex = """(?ms)^```(lengine|output|compile-error|runtime-error)([^\n]*)\n(.*?)^```\s*$""".r
  private val IdAttr: Regex = """id=([A-Za-z0-9-]+)""".r
  private val BugAttr: Regex = """bug=([A-Za-z0-9-]+)""".r

  def parse(md: String): List[Example] = {
    val blocks = Fence.findAllMatchIn(md).map { m =>
      (m.group(1), m.group(2), m.group(3), md.substring(0, m.start).count(_ == '\n') + 1)
    }.toList
    blocks.sliding(2).collect {
      case List(("lengine", attrs, src, line), (kind, _, body, _)) if kind != "lengine" =>
        val id = IdAttr.findFirstMatchIn(attrs).map(_.group(1)).getOrElse(s"line-$line")
        val bug = BugAttr.findFirstMatchIn(attrs).map(_.group(1))
        val expect = kind match {
          case "output"        => ExpectOutput(body)
          case "compile-error" => ExpectCompileError(body.trim)
          case "runtime-error" => ExpectRuntimeError(body.trim)
        }
        Example(id, bug, src, expect, line, crlf = """(^|\s)crlf(\s|$)""".r.findFirstIn(attrs).isDefined)
    }.toList
  }

  /** Loads compiled classes from the working directory on demand (lenginec writes them there). */
  private class FileClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
    override def findClass(name: String): Class[_] = {
      val p = Paths.get(name.replace('.', '/') + ".class")
      if (!Files.exists(p)) throw new ClassNotFoundException(name)
      val bytes = Files.readAllBytes(p)
      defineClass(name, bytes, 0, bytes.length)
    }
  }

  private lazy val loader = new FileClassLoader(getClass.getClassLoader)
  private val srcDir: Path = Paths.get("target/spec-examples")

  final case class ExitCalled(code: Int) extends RuntimeException(s"exit($code)")

  def runExample(ex: Example): Outcome = this.synchronized {
    Files.createDirectories(srcDir)
    val module = s"spec-${ex.id}"
    val file = srcDir.resolve(s"$module.lg")
    val text = s"(module $module)\n${ex.source}"
    Files.write(file, (if (ex.crlf) text.replace("\n", "\r\n") else text).getBytes(UTF_8))

    val compileErr = new ByteArrayOutputStream()
    val compileOut = new ByteArrayOutputStream()
    val (oldOut, oldErr) = (System.out, System.err)
    val status =
      try {
        System.setOut(new PrintStream(compileOut, true, "UTF-8"))
        System.setErr(new PrintStream(compileErr, true, "UTF-8"))
        try Main.run(Array(file.toString))
        catch { case e: Throwable => compileErr.write(s"${e.getClass.getName}: ${e.getMessage}".getBytes(UTF_8)); 1 }
      } finally {
        System.setOut(oldOut)
        System.setErr(oldErr)
      }
    if (status != 0) return CompileFailed(new String(compileErr.toByteArray, UTF_8).trim)

    val out = new ByteArrayOutputStream()
    var error: Option[String] = None
    // Read the lazy val here: touching it from the worker would wait on this object's lock,
    // which the caller holds while it joins the worker.
    val loader = this.loader
    val thread = new Thread(() => {
      try {
        Thread.currentThread().setContextClassLoader(loader)
        val setHandler = loader
          .loadClass("lengine.runtime.ExitHandler")
          .getMethod("set", classOf[java.util.function.IntConsumer])
        val throwOnExit: java.util.function.IntConsumer = code => throw ExitCalled(code)
        val previous = setHandler.invoke(null, throwOnExit)
        try {
          val obj = loader.loadClass(module).getConstructor().newInstance()
          obj.getClass.getMethod("scriptMain").invoke(obj)
        } catch {
          case e: java.lang.reflect.InvocationTargetException =>
            e.getCause match {
              case ExitCalled(_) =>
              case cause         => error = Some(s"${cause.getClass.getName}: ${cause.getMessage}")
            }
        } finally setHandler.invoke(null, previous)
      } catch {
        case e: Throwable => error = Some(s"${e.getClass.getName}: ${e.getMessage}")
      }
    })
    System.setOut(new PrintStream(out, true, "UTF-8"))
    try {
      thread.start()
      thread.join(20000)
      if (thread.isAlive) error = Some("timeout after 20s")
    } finally System.setOut(oldOut)

    val printed = new String(out.toByteArray, UTF_8)
    error.map(Crashed(_, printed)).getOrElse(Printed(printed))
  }
}
