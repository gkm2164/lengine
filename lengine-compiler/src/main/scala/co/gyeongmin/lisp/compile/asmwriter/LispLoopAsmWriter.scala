package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.lexer.statements.LispForStmt
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Label

class LispLoopAsmWriter(forStmts: List[LispForStmt],
                        body: LispValue,
                        requestedType: Class[_],
                        tailRecReference: Option[(LispSymbol, Label)] = None)(implicit env: LengineRuntimeEnvironment) {
  def writeValue(): Unit =
    visitForStmt(forStmts, body)

  private def visitForStmt(forStmts: List[LispForStmt], body: LispValue): Unit = {
    forStmts match {
      case Nil =>
        new LispValueAsmWriter(body, requestedType)
          .visitForValue(needReturn = true, tailRecReference = tailRecReference)
      case LispForStmt(symbol, seq) :: tail =>
        val startLoop = new Label()
        val endLoop = new Label()

        val varIdx = env.allocateNextVar
        env.registerVariable(symbol, varIdx, ObjectClass)

        val seqIteratorLoc = env.allocateNextVar
        val newSeqLoc = env.allocateNextVar
        val tmpIdx = env.allocateNextVar

        val mv = env.methodVisitor
        mv.visitLispValue(seq, CreateIteratorClass, needReturn = true)
        mv.visitInterfaceMethodCall(
          CreateIteratorClass,
          "iterator",
          LengineIteratorClass
        )

        mv.visitAStore(seqIteratorLoc)

        mv.visitStaticMethodCall(
          SequenceClass,
          "create",
          SequenceClass
        )
        mv.visitAStore(newSeqLoc)

        mv.visitLabel(startLoop)
        mv.visitALoad(seqIteratorLoc)
        mv.visitInterfaceMethodCall(
          LengineIteratorClass,
          "hasNext",
          BooleanPrimitive
        )
        mv.visitIfEq(endLoop)

        mv.visitALoad(seqIteratorLoc)
        mv.visitInterfaceMethodCall(
          LengineIteratorClass,
          "next",
          ObjectClass
        )
        mv.visitAStore(varIdx)
        visitForStmt(tail, body)

        mv.visitAStore(tmpIdx)
        mv.visitALoad(newSeqLoc)
        mv.visitCheckCast(SequenceClass)
        mv.visitALoad(tmpIdx)
        mv.visitMethodCall(
          SequenceClass,
          "add",
          VoidPrimitive,
          ObjectClass
        )
        env.deregisterVariable(symbol)
        mv.visitGoto(startLoop)
        mv.visitLabel(endLoop)
        mv.visitALoad(newSeqLoc)
    }
  }
}
