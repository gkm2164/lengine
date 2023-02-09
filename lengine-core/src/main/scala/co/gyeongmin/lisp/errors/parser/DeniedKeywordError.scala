package co.gyeongmin.lisp.errors.parser

case class DeniedKeywordError(deniedKeyword: String, context: String) extends ParseError {
  override def message: String = s"$deniedKeyword can't be used for variable names in $context"
}
