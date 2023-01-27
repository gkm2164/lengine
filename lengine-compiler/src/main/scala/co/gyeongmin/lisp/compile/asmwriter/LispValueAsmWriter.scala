package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.LengineEnv.{Variable, allocateVariable}
import co.gyeongmin.lisp.lexer.statements.{LispFuncDef, LispValueDef}
import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispValue}
import co.gyeongmin.lisp.types._
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

class LispValueAsmWriter(mv: MethodVisitor, value: LispValue)(implicit args: Map[String, Int]) {
  import LengineTypeSystem._
  implicit val mv$: MethodVisitor = mv

  def writeValue(finalCast: Option[LengineType] = None, needBoxing: Boolean = false): Unit = value match {
    case LispChar(ch) =>
      mv.visitLdcInsn(ch)
      finalCast.foreach(toType => value.resolveType.foreach(_.cast(toType)))
    case IntegerNumber(n) =>
      mv.visitLdcInsn(n)
      finalCast.foreach(toType => value.resolveType.foreach(_.cast(toType)))
    case FloatNumber(n) =>
      mv.visitLdcInsn(n)
      finalCast.foreach(toType => value.resolveType.foreach(_.cast(toType)))
    case LispString(str) =>
      mv.visitLdcInsn(str)
      finalCast.foreach(toType => value.resolveType.foreach(_.cast(toType)))
    case LispList(body) =>
      declareSequence(body)
      finalCast.foreach(LengineList.cast)
    case EagerSymbol(varName) =>
      if (args.contains(varName)) {
        args.get(varName).foreach(varLoc => mv.visitIntInsn(Opcodes.ALOAD, varLoc))
      } else {
        LengineEnv.getVarInfo(varName).foreach {
          case Variable(_, varIdx, storedType, _) =>
            storedType match {
              case LengineChar => mv.visitIntInsn(Opcodes.ILOAD, varIdx)
              case LengineInteger => mv.visitIntInsn(Opcodes.LLOAD, varIdx)
              case LengineDouble => mv.visitIntInsn(Opcodes.DLOAD, varIdx)
              case _ => mv.visitIntInsn(Opcodes.ALOAD, varIdx)
            }

            finalCast.foreach(storedType.cast(_))
        }
      }
    case l@LispClause(_) => new LispClauseWriter(mv, l).writeValue(finalCast)
    case LispValueDef(symbol, value) =>
      new LispValueAsmWriter(mv, value).writeValue(None)
      value.resolveType.map(varType => {
        val varIdx = LengineEnv.callLastWithLabel(symbol.name, varType, new LispValueDefWriter(mv, symbol, value).writeValue)
        varType match {
          case LengineChar => mv.visitIntInsn(Opcodes.ISTORE, varIdx)
          case LengineInteger => mv.visitIntInsn(Opcodes.LSTORE, varIdx)
          case LengineDouble => mv.visitIntInsn(Opcodes.DSTORE, varIdx)
          case _ => mv.visitIntInsn(Opcodes.ASTORE, varIdx)
        }
      })
    case f: LispFuncDef =>
      new LispFnAsmWriter(mv, f).writeValue()
  }


  private def declareSequence(body: List[LispValue]): Unit = {
    mv.visitTypeInsn(Opcodes.NEW, "lengine/runtime/Sequence")
    mv.visitInsn(Opcodes.DUP)
    mv.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      "lengine/runtime/Sequence",
      "<init>",
      Type.getMethodDescriptor(Type.getType(java.lang.Void.TYPE)),
      false
    )
    val seqIdx = allocateVariable
    mv.visitIntInsn(Opcodes.ASTORE, seqIdx)
    body.foreach(value => {
      new LispValueAsmWriter(mv, value).writeValue()
      value.resolveType.map(_.getCommands)
        .getOrElse((Opcodes.ASTORE, Opcodes.ALOAD)) match { case (store, load) =>
        val idx = allocateVariable
        mv.visitIntInsn(store, idx)
        mv.visitIntInsn(Opcodes.ALOAD, seqIdx)
        mv.visitIntInsn(load, idx)
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
          "lengine/runtime/Sequence",
          "add",
          Type.getMethodDescriptor(
            Type.getType(java.lang.Void.TYPE),
            Type.getType(value.resolveType
              .map(_.getJvmNativeType)
              .getOrElse(classOf[java.lang.Object]))
          ),
          false
        )
      }
    })
    mv.visitIntInsn(Opcodes.ALOAD, seqIdx)
  }

}