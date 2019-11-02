package co.gyeongmin.lisp.lexer

object LispLexer {

  sealed trait TokenizeError

  case class UnknownTokenError(str: String) extends TokenizeError

  def tokenize(code: Tokenizer): Either[TokenizeError, LazyList[LispToken]] = Right(code.streamLoop)

  case object WrongEscapeError extends TokenizeError

  case object EOFError extends TokenizeError

}
