package co.gyeongmin.lisp.compile

import org.scalatest.{ FlatSpec, Matchers }

import java.io.{ BufferedReader, InputStreamReader }

class MainTest extends FlatSpec with Matchers {
  def execute(fileName: String, className: String): Unit = {
    println(fileName)
    Main.main(
      Array(s"./compile-example/$fileName")
    )

    val builder = new ProcessBuilder("/bin/bash", "./leng-debug", className)
    val process = builder.start()
    val reader  = new BufferedReader(new InputStreamReader(process.getInputStream))

    while (reader.ready()) {
      println(reader.readLine())
    }

    assert(process.waitFor() == 0)
  }

  "compile examples" should "compile and no death!" in {
    execute("complex.lg", "gben.tests.ComplexNumberModule")
    execute("hello.lg", "gben.tests.Hello")
    execute("boolean.lg", "gben.tests.Boolean")
    execute("runtime.lg", "gben.libs.Runtime")
    execute("map.lg", "gben.tests.MapTest")
    execute("math.lg", "gben.libs.Math")
    execute("module.lg", "gben.libs.Module")
    execute("lambda.lg", "gben.tests.Lambda")
    execute("import-module.lg", "gben.tests.ImportModule")
    execute("seq-test.lg", "gben.tests.SeqTest")
    execute("string.lg", "gben.tests.StringModule")
    execute("seq-module.lg", "gben.libs.SeqModule")
    execute("json.lg", "gben.libs.Json")
    execute("set-test.lg", "gben.tests.SetTest")
    execute("json-async.lg", "gben.tests.JsonAsyncModule")
    execute("channel.lg", "gben.concurrency.ChannelModule")
    execute("ratio.lg", "gben.tests.RatioNumberModule")
  }
}
