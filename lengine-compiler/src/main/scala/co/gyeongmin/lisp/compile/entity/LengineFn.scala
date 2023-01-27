package co.gyeongmin.lisp.compile.entity

case class LengineFn(fnName: String,
                     referenceName: String,
                     params: List[String],
                     capturedParams: List[String])