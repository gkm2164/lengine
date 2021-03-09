package co.gyeongmin.lisp.errors.parser

case object EmptyTokenListError extends ParseError {
  override def message: String = s"no more token left to parse"
}
