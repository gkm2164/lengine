package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{ExceptionClass, LengineExceptionClass, LengineLambdaClass, ObjectClass}
import co.gyeongmin.lisp.lexer.statements.LispErrorHandler
import org.objectweb.asm.Label

class LispErrorHandlerAsmWriter(errorHandler: LispErrorHandler, typeToBe: Class[_])(
    implicit runtimeEnv: LengineRuntimeEnvironment
) {
  def writeValue(): Unit = {
    val mv = runtimeEnv.methodVisitor

    val LispErrorHandler(tryBody, recoveryBlock) = errorHandler

    val startLabel = new Label()
    val endLabel = new Label()
    val handlerLabel = new Label()
    val exitBlock = new Label()

    mv.visitTryCatchFinally(startLabel, endLabel, handlerLabel)

    mv.visitLabel(startLabel)
    mv.visitLineForValue(tryBody)
    mv.visitLispValue(tryBody, typeToBe)
    mv.visitLabel(endLabel)

    mv.visitLabel(new Label())
    mv.visitGoto(exitBlock)

    mv.visitLabel(handlerLabel)
    mv.visitStaticMethodCall(
      LengineExceptionClass,
      "CONVERT",
      LengineExceptionClass,
      ExceptionClass
    ) // [E]

    new LispFnAsmWriter(recoveryBlock).writeValue() // [E F]
    mv.visitCheckCast(LengineLambdaClass(1)) // [E F]
    mv.visitSwap() // [F E]
    mv.visitInterfaceMethodCall(
      LengineLambdaClass(1),
      "invoke",
      ObjectClass,
      ObjectClass
    )
    mv.visitLabel(exitBlock)
  }
}
