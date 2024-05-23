package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{ExceptionClass, LengineExceptionClass}
import co.gyeongmin.lisp.lexer.ast.{LispErrorHandler, LispRecoverBlock}
import org.objectweb.asm.Label

class LispErrorHandlerAsmWriter(errorHandler: LispErrorHandler, typeToBe: Class[_])(implicit
  runtimeEnv: LengineRuntimeEnvironment
) {
  def writeValue(): Unit = {
    val mv = runtimeEnv.methodVisitor

    val LispErrorHandler(tryBody, recoveryBlock) = errorHandler
    val LispRecoverBlock(symbol, body) = recoveryBlock

    val startLabel = new Label()
    val endLabel = new Label()
    val handlerLabel = new Label()
    val exitBlock = new Label()

    mv.visitTryCatchFinally(startLabel, endLabel, handlerLabel)

    mv.visitLabel(startLabel)
    mv.visitLineForValue(tryBody)
    mv.visitLispValue(tryBody, typeToBe)
    mv.visitAReturn()
    mv.visitLabel(endLabel)

    mv.visitLabel(new Label())
    mv.visitGoto(exitBlock)

    mv.visitLabel(handlerLabel)
    val exceptionLoc = runtimeEnv.allocateNextVar

    mv.visitStaticMethodCall(
      LengineExceptionClass,
      "CONVERT",
      LengineExceptionClass,
      ExceptionClass
    ) // [E]
    mv.visitAStore(exceptionLoc)

    runtimeEnv.registerVariable(symbol, exceptionLoc, LengineExceptionClass)
    mv.visitLispValue(body, typeToBe)
    mv.visitAReturn()
    mv.visitLabel(exitBlock)
  }
}
