package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.statements.LispForStmt
import co.gyeongmin.lisp.lexer.values.LispValue
import lengine.runtime.{Sequence, SequenceIterator}
import org.objectweb.asm.{Label, Opcodes, Type}

class LispLoopAsmWriter(forStmts: List[LispForStmt], body: LispValue)(implicit env: LengineRuntimeEnvironment) {
  def writeValue(): Unit = {
    visitForStmt(forStmts, body)
  }

  def declareSequence(newSeqLoc: Int): Unit = {
    val mv = env.methodVisitor

    mv.visitTypeInsn(Opcodes.NEW, Type.getType(classOf[Sequence]).getInternalName)
    mv.visitInsn(Opcodes.DUP)
    mv.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      Type.getType(classOf[Sequence])
        .getInternalName,
      "<init>",
      Type.getMethodDescriptor(
        Type.getType(Void.TYPE)
      ),
      false
    )
    mv.visitIntInsn(Opcodes.ASTORE, newSeqLoc)
  }

  private def seqWhileStart(seq: LispValue): (Int, Int, Label, Label) = {
    val startLoop = new Label()
    val endLoop = new Label()

    new LispValueAsmWriter(seq).visitForValue()
    val mv = env.methodVisitor
    mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getType(classOf[Sequence]).getInternalName)
    mv.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      Type.getType(classOf[Sequence]).getInternalName,
      "iterator",
      Type.getMethodDescriptor(
        Type.getType(classOf[SequenceIterator])
      ),
      false
    )
    val seqIteratorLoc = env.allocateNextVar
    val newSeqLoc = env.allocateNextVar
    mv.visitIntInsn(Opcodes.ASTORE, seqIteratorLoc)

    declareSequence(newSeqLoc)

    mv.visitLabel(startLoop)
    mv.visitIntInsn(Opcodes.ALOAD, seqIteratorLoc)
    mv.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      Type.getType(classOf[SequenceIterator]).getInternalName,
      "hasNext",
      Type.getMethodDescriptor(
        Type.getType(java.lang.Boolean.TYPE)
      ),
      false
    )
    mv.visitJumpInsn(Opcodes.IFEQ, endLoop)
    (seqIteratorLoc, newSeqLoc, startLoop, endLoop)
  }

  private def visitForStmt(forStmts: List[LispForStmt], body: LispValue): Unit = {
    val mv = env.methodVisitor
    forStmts match {
      case Nil =>
        new LispValueAsmWriter(body).visitForValue()
      case LispForStmt(symbol, seq) :: tail =>
        val varIdx = env.allocateNextVar
        env.registerVariable(symbol, varIdx)
        val (seqIdx, dstSeqIdx, startLabel, endLabel) = seqWhileStart(seq)
        mv.visitIntInsn(Opcodes.ALOAD, seqIdx)
        mv.visitMethodInsn(
          Opcodes.INVOKEVIRTUAL,
          Type.getType(classOf[SequenceIterator]).getInternalName,
          "next",
          Type.getMethodDescriptor(
            Type.getType(classOf[Object])
          ),
          false
        )
        mv.visitIntInsn(Opcodes.ASTORE, varIdx)
        visitForStmt(tail, body)

        val tmpIdx = env.allocateNextVar
        mv.visitIntInsn(Opcodes.ASTORE, tmpIdx)
        mv.visitIntInsn(Opcodes.ALOAD, dstSeqIdx)
        mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getType(classOf[Sequence]).getInternalName)
        mv.visitIntInsn(Opcodes.ALOAD, tmpIdx)
        mv.visitMethodInsn(
          Opcodes.INVOKEVIRTUAL,
          Type.getType(classOf[Sequence]).getInternalName,
          "add",
          Type.getMethodDescriptor(
            Type.getType(Void.TYPE),
            Type.getType(classOf[Object])
          ),
          false
        )
        env.deregisterVariable(symbol)
        mv.visitJumpInsn(Opcodes.GOTO, startLabel)
        mv.visitLabel(endLabel)
        mv.visitIntInsn(Opcodes.ALOAD, dstSeqIdx)
    }
  }
}
