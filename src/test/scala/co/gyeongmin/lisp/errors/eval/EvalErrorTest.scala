package co.gyeongmin.lisp.errors.eval

import co.gyeongmin.lisp.errors.parser.ParseError
import co.gyeongmin.lisp.errors.tokenizer.TokenizeError
import co.gyeongmin.lisp.lexer.values.LispUnit
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}
import org.scalatest.{FlatSpec, Matchers}

class EvalErrorTest extends FlatSpec with Matchers {
  val mockParseError: ParseError = new ParseError {
    override def message: String = "hello"
  }
  val mockTokenizeError: TokenizeError = new TokenizeError {
    override def message: String = "hello"
  }
  val mockLispSymbol: LispSymbol = EagerSymbol("test")

  it should "pass" in {
    EmptyBodyClauseError.message should be("body of clause is empty")
    EvalParseError(mockParseError).message should be(s"parsing error: hello")
    EvalTokenizeError(mockTokenizeError).message should be(
      s"lexing error: hello"
    )
    FunctionApplyError("something").message should be(
      "function apply error: something"
    )
    InvalidSymbolNameError(mockLispSymbol).message should be(
      s"$mockLispSymbol is not definable"
    )
    InvalidTypeError(LispUnit, "hello").message should be(
      s"$LispUnit is not hello type"
    )
    KeyIsNotReferSymbolError.message should be(
      "given value is not a key type"
    )
    NotAnExecutableError(LispUnit).message should be(
      s"$LispUnit is not executable"
    )
    NotANumberTypeError(LispUnit).message should be(
      s"given $LispUnit is not a number type"
    )
    NotASeqTypeError(LispUnit).message should be(
      s"given $LispUnit is not a sequence type"
    )
    ObjectKeyNotExistError("key").message should be(
      "key is not exist in object"
    )
    ProgramFinishedError.message should be(
      "program has finished successfully"
    )
    StringIsEmptyError.message should be(
      "given value is empty string"
    )
    SymbolNotOverridableError(mockLispSymbol).message should be(
      s"$mockLispSymbol is not an overridable function symbol"
    )
    UnableToFindFunction.message should be(
      "unable to find overridable functions"
    )
    UnimplementedOperationError("+", LispUnit).message should be(
      s"not implemented for + for $LispUnit"
    )
    UnknownSymbolNameError(mockLispSymbol).message should be(
      s"unknown symbol name: ${mockLispSymbol.name}"
    )
  }
}
