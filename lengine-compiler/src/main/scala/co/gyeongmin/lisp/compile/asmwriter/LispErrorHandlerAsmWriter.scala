package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{ExceptionClass, LengineExceptionClass, ObjectClass}
import co.gyeongmin.lisp.lexer.statements.{LispRecoverBlock, LispErrorHandler}
import org.objectweb.asm.Label

class LispErrorHandlerAsmWriter(errorHandler: LispErrorHandler, typeToBe: Class[_])(
    implicit runtimeEnv: LengineRuntimeEnvironment
) {
  def writeValue(): Unit = {
    val mv = runtimeEnv.methodVisitor

    val LispErrorHandler(tryBody, recoveryBlock) = errorHandler
    val LispRecoverBlock(exceptionSymbol, recoveryBody) = recoveryBlock

    val startLabel = new Label()
    val catchLabel = new Label()
    val recoveryLabel = new Label()
    val exitBlock = new Label()

    mv.visitTryCatchFinally(startLabel, catchLabel)
    mv.visitLabel(startLabel)
    mv.visitLineForValue(tryBody)
    mv.visitLispValue(tryBody, typeToBe)
    mv.visitGoto(exitBlock)
    mv.visitLabel(catchLabel)
    val exceptionVarId = runtimeEnv.allocateNextVar
    mv.visitStaticMethodCall(
      LengineExceptionClass,
      "CONVERT",
      LengineExceptionClass,
      ExceptionClass
    )
    mv.visitAStore(exceptionVarId)
    mv.visitLabel(recoveryLabel)
    runtimeEnv.registerVariable(exceptionSymbol, exceptionVarId, LengineExceptionClass)
    mv.visitLispValue(recoveryBody, ObjectClass)
    runtimeEnv.deregisterVariable(exceptionSymbol)
    mv.visitLabel(exitBlock)
  }
}
