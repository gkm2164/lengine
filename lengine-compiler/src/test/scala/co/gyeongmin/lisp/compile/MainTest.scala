package co.gyeongmin.lisp.compile

import org.scalatest.{FlatSpec, Matchers}
class MainTest extends FlatSpec with Matchers {
  "builtin function with compile" should "run and return" in {
    Main.main(Array(
      "./compile-example/hello.lisp",
      "--className",
      "Hello"
    ))
  }

  "builtin function with compile boolean.lisp" should "run and return" in {
    Main.main(Array(
      "./compile-example/boolean.lisp",
      "--className",
      "BoolTest"
    ))
  }

  "map type object with compile map.lisp" should "run and return" in {
    Main.main(Array(
      "./compile-example/map.lisp",
      "--className",
      "MapTest"
    ))
  }

  "map type object with compile lambda.lisp" should "run and return" in {
    Main.main(Array(
      "./compile-example/lambda.lisp",
      "--className",
      "LambdaTest"
    ))
  }

  "seq type with compile seq-test.lisp" should "run and return" in {
    Main.main(Array(
      "./compile-example/seq-test.lisp",
      "--className",
      "SeqTest"
    ))
  }

  "read-line test compile readline.lisp" should "run and return" in {
    Main.main(Array(
      "./compile-example/readline.lisp",
      "--className",
      "ReadLineTest"
    ))
  }
}
