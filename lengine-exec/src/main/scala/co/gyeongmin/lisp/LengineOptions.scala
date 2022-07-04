package co.gyeongmin.lisp

case class LengineOptions(
    compile: Boolean = false,
    verbose: Boolean = false,
    openFilename: Option[String] = None
)
