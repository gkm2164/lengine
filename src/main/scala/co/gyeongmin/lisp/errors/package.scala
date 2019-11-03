package co.gyeongmin.lisp

import co.gyeongmin.lisp.lexer._

package object errors {

  trait LispError

  sealed trait EvalError extends LispError

  case class InvalidValueError(errValue: LispValue) extends EvalError

  object EmptyBodyClauseError extends EvalError

  case class FunctionApplyError(msg: String) extends EvalError

  case class UnimplementedOperationError(operation: String) extends EvalError

  case class NotAnExecutableError(value: String) extends EvalError

  case class UnknownSymbolNameError(name: LispSymbol) extends EvalError

  case object UnresolvedSymbolError extends EvalError

  case object ProgramFinishedError extends EvalError


  sealed trait ParseError extends LispError

  case object EmptyTokenListError extends ParseError

  case class UnexpectedTokenError(tk: LispToken, msg: String = "") extends ParseError


  sealed trait TokenizeError

  case class UnknownTokenError(str: String) extends TokenizeError

  case object WrongEscapeError extends TokenizeError

  case object EOFError extends TokenizeError

}
