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
    execute("hello.lg", "Hello")
    execute("boolean.lg", "Boolean")
    execute("runtime.lg", "Runtime")
    execute("map.lg", "MapTest")
    execute("math.lg", "Math")
    execute("module.lg", "Module")
    execute("lambda.lg", "Lambda")
    execute("import-module.lg", "ImportModule")
    execute("seq-test.lg", "Seq")
    execute("string.lg", "StringModule")
    execute("seq-module.lg", "SeqModule")
    execute("json.lg", "Json")
    execute("set-test.lg", "SetTest")
    execute("json-async.lg", "JsonAsyncModule")
    execute("channel.lg", "ChannelModule")
  }
}
