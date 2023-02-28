package co.gyeongmin.lisp.lexer.tokens


case class LispRequire() extends LispToken {
  override def toString: String = "require"
}
