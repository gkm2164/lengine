package co.gyeongmin.lisp.lexer

import co.gyeongmin.lisp.lexer.Tokenizer.tokenize
import org.scalatest.{FlatSpec, Matchers}

class TokenizerTest extends FlatSpec with Matchers {
  "tokenizer" should "parse statement" in {

    tokenize(new Tokenizer("(a b c)")) should be(
      Right(
        Stream(
          LeftPar,
          EagerSymbol("a"),
          EagerSymbol("b"),
          EagerSymbol("c"),
          RightPar
        )
      )
    )
  }
}
