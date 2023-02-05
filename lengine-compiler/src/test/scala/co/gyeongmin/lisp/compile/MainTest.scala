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
    assert(process.waitFor() == 0)

    while (reader.ready()) {
      println(reader.readLine())
    }
  }

  "compile examples" should "compile and no death!" in {
    execute("hello.lisp", "Hello")
    execute("boolean.lisp", "Boolean")
    execute("map.lisp", "MapTest")
    execute("math.lisp", "Math")
    execute("module.lisp", "Module")
    execute("lambda.lisp", "Lambda")
    execute("import-module.lisp", "ImportModule")
    execute("seq-test.lisp", "Seq")
    execute("string.lisp", "StringModule")
    execute("seq-module.lisp", "SeqModule")
    execute("json.lisp", "Json")
  }
}
