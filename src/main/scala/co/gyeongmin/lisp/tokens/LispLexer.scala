package co.gyeongmin.lisp.tokens

object LispLexer {

  sealed trait TokenizeError

  case class UnknownTokenError(str: String) extends TokenizeError

  class Tokenizer() {
    var codeIterator: Iterator[Char] = _

    def this(codes: String) {
      this()
      this.codeIterator = codes.iterator
    }

    def this(it: Iterator[Char]) {
      this()
      this.codeIterator = it
    }
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

    def streamLoop: LazyList[LispToken] = next() match {
      case Right(v) => v #:: streamLoop
      case Left(EOFError) => LazyList.empty
      case Left(_) => streamLoop
    }
  }

  def tokenize(code: Tokenizer): Either[TokenizeError, LazyList[LispToken]] = Right(code.streamLoop)

  case object WrongEscapeError extends TokenizeError

  case object EmptyTokenError extends TokenizeError

  case object EOFError extends TokenizeError

}
