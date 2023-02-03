package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.compile.asmwriter.LengineType.{ConsClass, CreateIteratorClass, LengineList, LengineListClass, ObjectClass}
import co.gyeongmin.lisp.lexer.statements.LispForStmt
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import lengine.runtime.{CreateIterator, LengineIterator}
import org.objectweb.asm.{Label, Opcodes}

class LispLoopAsmWriter(forStmts: List[LispForStmt],
                        body: LispValue,
                        requestedType: Class[_],
                        tailRecReference: Option[(LispSymbol, Label)] = None)(implicit env: LengineRuntimeEnvironment) {
  def writeValue(): Unit =
    visitForStmt(forStmts, body)

  def declareSequence(newSeqLoc: Int): Unit = {
    val mv = env.methodVisitor

    mv.visitStaticMethodCall(
      LengineListClass,
      "nil",
      LengineListClass
    )
    mv.visitIntInsn(Opcodes.ASTORE, newSeqLoc)
  }

  private def seqWhileStart(seq: LispValue): (Int, Int, Label, Label) = {
    val startLoop = new Label()
    val endLoop   = new Label()

    val mv = env.methodVisitor
    new LispValueAsmWriter(seq, CreateIteratorClass).visitForValue()
    mv.visitInterfaceMethodCall(
      classOf[CreateIterator],
      "iterator",
      classOf[LengineIterator]
    )
    val seqIteratorLoc = env.allocateNextVar
    val newSeqLoc      = env.allocateNextVar
    mv.visitAStore(seqIteratorLoc)

    declareSequence(newSeqLoc)

    mv.visitLabel(startLoop)
    mv.visitALoad(seqIteratorLoc)
    mv.visitInterfaceMethodCall(
      classOf[LengineIterator],
      "hasNext",
      java.lang.Boolean.TYPE
    )
    mv.visitJumpInsn(Opcodes.IFEQ, endLoop)
    (seqIteratorLoc, newSeqLoc, startLoop, endLoop)
  }

  private def visitForStmt(forStmts: List[LispForStmt], body: LispValue): Unit = {
    val mv = env.methodVisitor
    forStmts match {
      case Nil =>
        new LispValueAsmWriter(body, requestedType)
          .visitForValue(tailRecReference = tailRecReference)
      case LispForStmt(symbol, seq) :: tail =>
        val varIdx = env.allocateNextVar
        val tmpIdx = env.allocateNextVar

        env.registerVariable(symbol, varIdx, ObjectClass)
        val (seqIdx, dstSeqIdx, startLabel, endLabel) = seqWhileStart(seq)
        mv.visitALoad(seqIdx)
        mv.visitInterfaceMethodCall(
          classOf[LengineIterator],
          "next",
          classOf[Object]
        )
        mv.visitAStore(varIdx)
        visitForStmt(tail, body)
        mv.visitALoad(dstSeqIdx)
        mv.visitCheckCast(LengineListClass)
        mv.visitStaticMethodCall(
          LengineListClass,
          "cons",
          ConsClass,
          ObjectClass,
          LengineListClass
        )
        mv.visitAStore(dstSeqIdx)
        env.deregisterVariable(symbol)
        mv.visitJumpInsn(Opcodes.GOTO, startLabel)
        mv.visitLabel(endLabel)
        mv.visitALoad(dstSeqIdx)
    }
  }
}
