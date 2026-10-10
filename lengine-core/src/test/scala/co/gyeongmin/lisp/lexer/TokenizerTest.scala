package co.gyeongmin.lisp.lexer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenizerTest extends AnyFlatSpec with Matchers {
  "edge cases" should "pass" in {
    Tokenizer.apply(""" "\\\n" """).getTokenStream.foreach(_.foreach(println))
    Tokenizer.apply(""" #\LineFeed)""").getTokenStream.foreach(_.foreach(println))
  }
}
