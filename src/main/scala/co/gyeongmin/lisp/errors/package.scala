package co.gyeongmin.lisp

import co.gyeongmin.lisp.lexer._

package object errors {

  trait LispError {
    def message: String
  }

  sealed trait EvalError extends LispError

  case object EmptyBodyClauseError extends EvalError {
    override def message: String = "body of clause is empty"
  }

  case class FunctionApplyError(msg: String) extends EvalError {
    override def message: String = s"function apply error: $msg"
  }

  case class UnimplementedOperationError(operation: String, typeToken: LispValue) extends EvalError {
    override def message: String = s"not implemented for $operation for $typeToken"
  }

  case class NotANumberType(k: LispValue) extends EvalError {
    override def message: String = s"given $k is not a number type"
  }

  case class NotAnExecutableError(value: LispValue) extends EvalError {
    override def message: String = s"$value is not executable"
  }

  case class UnknownSymbolNameError(name: LispSymbol) extends EvalError {
    override def message: String = s"unknown symbol name: ${name.name}"
  }

  case object ProgramFinishedError extends EvalError {
    override def message: String = "program has finished successfully"
  }

  case class EvalParseError(error: ParseError) extends EvalError {
    override def message: String = s"parsing error: ${error.message}"
  }

  case class EvalTokenizeError(x: TokenizeError) extends EvalError {
    override def message: String = s"lexing error: ${x.message}"
  }

  case class InvalidSymbolName(lispSymbol: LispSymbol) extends EvalError {
    override def message: String = s"$lispSymbol is not definable"
  }


  sealed trait ParseError extends LispError

  case object EmptyTokenListError extends ParseError {
    override def message: String = s"no more token left to parse"
  }

  case class UnexpectedTokenError(tk: LispToken, msg: String = "") extends ParseError {
    override def message: String = s"unknown token: $tk, $msg"
  }

  case class NotNumberType(k: LispValue) extends ParseError {
    override def message: String = s"given $k is not a number type"
  }

  case class ParseTokenizeError(e: TokenizeError) extends ParseError {
    override def message: String = s"tokenizer error: ${e.message}"
  }

  sealed trait TokenizeError extends LispError

  case class UnknownTokenError(str: String) extends TokenizeError {
    override def message: String = s"Unknown token error"
  }

  case class InvalidNumberType(v: String) extends TokenizeError {
    override def message: String = s"invalid number type: $v"
  }

  case class UnknownMacroError(v: String) extends TokenizeError {
    override def message: String = s"Unknown macro: $v"
  }

  case object RatioUnderZeroNotAllowed extends TokenizeError {
    override def message: String = s"under of rational number should be greater than 0"
  }

  case object WrongEscapeError extends TokenizeError {
    override def message: String = s"wrong escape usages"
  }

  case object EOFError extends TokenizeError {
    override def message: String = s"EOF"
  }

}
