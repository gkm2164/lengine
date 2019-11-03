package co.gyeongmin.lisp

import co.gyeongmin.lisp.lexer._

package object execution {
  type LispEnvironment = Map[LispSymbol, LispValue]
}
