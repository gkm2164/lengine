package co.gyeongmin.lisp.compile

import org.scalatest._
import flatspec._
import lengine.types.LengineObject
import matchers._

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.reflect.io.Path

class LengineTestClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
  private val loadedClass: mutable.Map[String, Class[_]] = mutable.Map[String, Class[_]]()

  private def loadClassByName(className: String): Class[_] = {
    val nameSplit = className.split('.')

    val classBytes =
      Files.readAllBytes(Paths.get(nameSplit.reduce((acc, elem) => s"$acc/$elem") + ".class"))

    val lastClassName = nameSplit.last
    val dirString = nameSplit.init.mkString("/")
    val toPath = Path(if (dirString.isEmpty) "." else dirString)

    toPath.toDirectory.list
      .filter(path =>
        path.name.replace(lastClassName, "").startsWith("$")
          && path.name.endsWith(".class")
      )
      .foreach { path =>
        val bytes = Files.readAllBytes(path.jfile.toPath)
        defineClass(null, bytes, 0, bytes.length)
      }

    defineClass(null, classBytes, 0, classBytes.length)
  }

  def loadFromClassName(className: String): Class[_] =
    loadedClass.getOrElseUpdate(className, loadClassByName(className))
}

class MainTest extends AnyFlatSpec with should.Matchers {

  val classLoader = new LengineTestClassLoader(Thread.currentThread().getContextClassLoader)

  def execute(fileName: String, className: String): Unit = {
    println(fileName)
    Main.main(
      Array(s"./compile-example/$fileName")
    )

    val testingClass = classLoader.loadFromClassName(className)
    val testThread = new Thread(() => {
      Thread.currentThread().setContextClassLoader(classLoader)
      val lengineObject = testingClass
        .getConstructor()
        .newInstance()
        .asInstanceOf[LengineObject]
      System.setSecurityManager(new NoExitSecurityManager())
      try {
        lengineObject.scriptMain()
      } catch {
        case e: ExitException =>
          println("program finished in expected way.")
      }
      System.setSecurityManager(null)
    })

    val startTime = System.currentTimeMillis()
    testThread.setUncaughtExceptionHandler { (t: Thread, e: Throwable) =>
      println("Uncaught exception")
      e.printStackTrace()
      fail("Should've been succeeded, but, failed")
    }
    testThread.start()
    testThread.join()
    println(s"${System.currentTimeMillis() - startTime}ms elapsed to run")
  }

  "compile examples" should "compile only" in {
    Main.main(Array("./lengine-code/prelude.lg"))
    Main.main(Array("./lengine-code/collections.lg"))
    Main.main(Array("./lengine-code/stdlib.lg"))
    Main.main(Array("./lengine-code/math.lg"))
    Main.main(Array("./compile-example/macro.lg"))
    Main.main(Array("./compile-example/process-membrane.lg"))
    Main.main(Array("./compile-example/process-membrane-2.lg"))
  }

  "compile examples" should "compile and no death!" in {
    Main.main(Array("./lengine-code/prelude.lg"))
    Main.main(Array("./lengine-code/collections.lg"))
    Main.main(Array("./lengine-code/stdlib.lg"))
    Main.main(Array("./lengine-code/math.lg"))

    classLoader.loadClass("lengine.types.LengineObject")
    classLoader.loadFromClassName("prelude")
    classLoader.loadFromClassName("collections")
    classLoader.loadFromClassName("std")
    classLoader.loadFromClassName("math")

    execute("module.lg", "gben.libs.module")
    execute("for-when.lg", "gben.tests.for-when")
    execute("lazy-symbol.lg", "gben.tests.lazy-symbol")
    execute("hello.lg", "gben.tests.hello")
    execute("boolean.lg", "gben.tests.boolean")
    execute("runtime.lg", "gben.libs.runtime")
    execute("lengine-objects.lg", "gben.tests.lengine-objects")
    execute("complex-number.lg", "gben.tests.complex-number")
    execute("error-handling.lg", "gben.tests.error-handling")
    execute("read-file-char-test.lg", "gben.tests.read-file-char-test")
    execute("lambda-test.lg", "gben.tests.lambda-test")
    execute("import-module.lg", "gben.tests.import-module")
    execute("seq-test.lg", "gben.tests.seq-test")
    execute("string-test.lg", "gben.tests.string-test")
    execute("seq-module.lg", "gben.libs.seq-module")
    execute("json.lg", "gben.libs.json")
    execute("set-test.lg", "gben.tests.set-test")
    execute("json-async-module.lg", "gben.tests.json-async-module")
    execute("channel-module.lg", "gben.concurrency.channel-module")
    execute("ratio-number.lg", "gben.tests.ratio-number")
  }
}
