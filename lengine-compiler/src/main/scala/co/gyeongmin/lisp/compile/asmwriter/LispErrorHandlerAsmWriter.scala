package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{LengineExceptionClass, ObjectClass}
import co.gyeongmin.lisp.lexer.statements.{LispCatchBlock, LispErrorHandler}
import org.objectweb.asm.Label

class LispErrorHandlerAsmWriter(errorHandler: LispErrorHandler, typeToBe: Class[_])(
    implicit runtimeEnv: LengineRuntimeEnvironment
) {
  def writeValue(): Unit = {
    val mv = runtimeEnv.methodVisitor

    val LispErrorHandler(tryBody, catchBlock, recoverBody) = errorHandler
    val LispCatchBlock(exceptionSymbol, catchBody) = catchBlock

    val startLabel = new Label()
    val endLabel = new Label()
    val catchLabel = new Label()
    val recoveryLabel = new Label()
    val exitBlock = new Label()

    mv.visitTryCatchFinally(startLabel, endLabel, catchLabel)

    mv.visitLabel(startLabel)
    mv.visitLineForValue(tryBody)
    mv.visitLispValue(tryBody, typeToBe)
    mv.visitGoto(exitBlock)
    mv.visitLabel(endLabel)
    mv.visitLabel(catchLabel)
    val exceptionVarId = runtimeEnv.allocateNextVar
    mv.visitAStore(exceptionVarId)
    runtimeEnv.registerVariable(exceptionSymbol, exceptionVarId, LengineExceptionClass)
    mv.visitLispValue(catchBody, ObjectClass)
    mv.visitPop()
    runtimeEnv.deregisterVariable(exceptionSymbol)
    mv.visitLabel(recoveryLabel)
    mv.visitLispValue(recoverBody, typeToBe)

    mv.visitLabel(exitBlock)
  }
}
