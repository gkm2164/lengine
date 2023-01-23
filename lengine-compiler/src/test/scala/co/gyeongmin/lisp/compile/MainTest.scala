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
}
