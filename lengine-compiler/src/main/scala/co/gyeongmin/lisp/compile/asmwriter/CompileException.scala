package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.TokenLocation

case class CompileException(msg: String, filename: String, location: Option[TokenLocation])
    extends RuntimeException(s"$msg${location
      .map {
        case TokenLocation(line, column) => s" at $filename:$line:$column"
      }
      .getOrElse("")}") {}
