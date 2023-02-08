package co.gyeongmin.lisp.lexer

import org.scalatest.{FlatSpec, Matchers}

class TokenizerTest extends FlatSpec with Matchers {
  "edge cases" should "pass" in {
    Tokenizer.apply(""" "\\\n" """).getTokenStream.foreach(_.foreach(println))
    Tokenizer.apply(""" #\LineFeed)""").getTokenStream.foreach(_.foreach(println))
  }
}
