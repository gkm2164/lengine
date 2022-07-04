package co.gyeongmin.lisp.lexer

import org.scalatest.{FlatSpec, Matchers}

import java.io.ByteArrayInputStream

class StdInReaderTest extends FlatSpec with Matchers {
  "StdInReader" should "read stdin well" in {
    val validInputStream = new ByteArrayInputStream("(a b c)".getBytes)
    Console.withIn(validInputStream) {
      val stdInReader = new StdInReader(Right("prompt"))
      val readString = stdInReader.mkString("")
      readString should be("(a b c)" + 0xffff.toChar)
    }

    val invalidInputStream =
      new ByteArrayInputStream(0xffff.toChar.toString.getBytes)

    Console.withIn(invalidInputStream) {
      val stdInReader = new StdInReader(Right("prompt"))
      stdInReader.mkString("")
    }
  }
}
