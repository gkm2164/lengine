package co.gyeongmin.lisp.tokens

object LispLexer {

  sealed trait TokenizeError

  case class UnknownTokenError(str: String) extends TokenizeError

  class Tokenizer(codes: String) {
    var codeIterator: Iterator[Char] = codes.iterator
    var closing: Option[String] = None

    def next(): Either[TokenizeError, LispToken] = closing match {
      case Some(ch) =>
        closing = None
        LispToken(ch)
      case None =>
        codeIterator = codeIterator.dropWhile(ch => " \t\n".contains(ch))

        @scala.annotation.tailrec
        def takeString(builder: StringBuilder, wrap: Char, escape: Boolean): Either[TokenizeError, String] = {
          if (!codeIterator.hasNext) Left(EOFError)
          else codeIterator.next() match {
            case '\\' if escape => takeString(builder.append("\\"), wrap, escape = false)
            case '\\' => takeString(builder.append("\\"), wrap, escape = true)
            case ch if ch == wrap && escape => takeString(builder.append(wrap), wrap, escape = false)
            case ch if ch == wrap => Right(builder.append(wrap).mkString(""))
            case _ if escape => Left(WrongEscapeError)
            case ch => takeString(builder.append(ch), wrap, escape)
          }
        }

        @scala.annotation.tailrec
        def loop(acc: StringBuilder): Either[TokenizeError, String] =
          if (!codeIterator.hasNext) Left(EOFError)
          else codeIterator.next() match {
            case ' ' | '\t' | '\n' => Right(acc.mkString(""))
            case ch@('(' | '[') => Right(acc.append(ch).mkString(""))
            case ch@(']' | ')') if acc.nonEmpty =>
              closing = Some(ch.toString)
              Right(acc.mkString(""))
            case ch@(']' | ')') => Right(ch.toString)
            case ch@('"' | '\'') => takeString(acc.append(ch), ch, escape = false)
            case ch => loop(acc.append(ch))
          }

        loop(new StringBuilder()).flatMap(x => LispToken(x))
    }

    def foldLeft[B[_]](acc: B[LispToken])(f: (B[LispToken], LispToken) => B[LispToken]): Either[TokenizeError, B[LispToken]] = next() match {
      case Left(EOFError) => Right(acc)
      case Left(e) => Left(e)
      case Right(tk) => foldLeft(f(acc, tk))(f)
    }
  }

  case object WrongEscapeError extends TokenizeError

  case object EmptyTokenError extends TokenizeError

  case object EOFError extends TokenizeError

}
