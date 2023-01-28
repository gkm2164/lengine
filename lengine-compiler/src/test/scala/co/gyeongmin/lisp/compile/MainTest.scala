package co.gyeongmin.lisp.compile

import org.scalatest.{FlatSpec, Matchers}
class MainTest extends FlatSpec with Matchers {
  "builtin function with compile" should "run and return" in {
    Main.main(Array(
      "hello.lisp",
      "--className",
      "Hello"
    ))
  }

  "builtin function with compile boolean.lisp" should "run and return" in {
    Main.main(Array(
      "boolean.lisp",
      "--className",
      "BoolTest"
    ))
  }

  "map type object with compile map.lisp" should "run and return" in {
    Main.main(Array(
      "map.lisp",
      "--className",
      "MapTest"
    ))
  }

  "map type object with compile lambda.lisp" should "run and return" in {
    Main.main(Array(
      "lambda.lisp",
      "--className",
      "LambdaTest"
    ))
  }
}
