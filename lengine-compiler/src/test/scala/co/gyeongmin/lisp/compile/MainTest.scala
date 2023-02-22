package co.gyeongmin.lisp.compile

import org.scalatest.{ FlatSpec, Matchers }

class MainTest extends FlatSpec with Matchers {
  def execute(fileName: String, className: String): Unit = {
    println(fileName)
    Main.main(
      Array(s"./compile-example/$fileName")
    )

    val builder = new ProcessBuilder("/bin/bash", "./leng-debug", className)

    val startTime = System.currentTimeMillis()

    val process = builder.start()
    assert(process.waitFor() == 0)

    println(s"${System.currentTimeMillis() - startTime}ms elapsed to run")
  }

  "compile examples" should "compile and no death!" in {
    execute("for-when.lg", "gben.tests.for-when")
    execute("lazy-symbol.lg", "gben.tests.lazy-symbol")
    execute("hello.lg", "gben.tests.hello")
    execute("boolean.lg", "gben.tests.boolean")
    execute("runtime.lg", "gben.libs.runtime")
    execute("lengine-objects.lg", "gben.tests.lengine-objects")
    execute("math.lg", "gben.libs.math")
    execute("module.lg", "gben.libs.module")
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

  "compile examples" should "compile only" in {
    Main.main(Array("./lengine-code/stdlib.lg"))
    Main.main(Array("./compile-example/process-membrane.lg"))
    Main.main(Array("./compile-example/process-membrane-2.lg"))
  }
}
