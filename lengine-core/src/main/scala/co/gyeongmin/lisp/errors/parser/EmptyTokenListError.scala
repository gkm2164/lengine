package co.gyeongmin.lisp.errors.parser

case object EmptyTokenListError extends ParseError {
  override def message: String = "no more token left to parse"
}
