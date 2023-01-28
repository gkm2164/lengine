package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.statements.{LispFuncDef, LispValueDef}
import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispValue}
import co.gyeongmin.lisp.types._
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

class LispValueAsmWriter(value: LispValue)(implicit runtimeEnv: LengineRuntimeEnvironment) {

  import LengineTypeSystem._

  val mv: MethodVisitor = runtimeEnv.methodVisitor
  private def boxing(boxedType: Class[_ <: Object], primitiveType: Class[_ <: Object]): Unit = {
    mv.visitMethodInsn(
      Opcodes.INVOKESTATIC,
      Type.getType(boxedType).getInternalName,
      "valueOf",
      Type.getMethodDescriptor(
        Type.getType(boxedType),
        Type.getType(primitiveType)
      ),
      false
    )
  }

  def writeValue(finalCast: Option[LengineType] = None): Unit = value match {
    case LispChar(ch) =>
      mv.visitLdcInsn(ch)
      boxing(classOf[Character], Character.TYPE)
    case IntegerNumber(n) =>
      mv.visitLdcInsn(n)
      boxing(classOf[java.lang.Long], java.lang.Long.TYPE)
    case FloatNumber(n) =>
      mv.visitLdcInsn(n)
      boxing(classOf[java.lang.Double], java.lang.Double.TYPE)
    case LispString(str) =>
      mv.visitLdcInsn(str)
    case LispList(body) =>
      declareSequence(body)
      finalCast.foreach(LengineList.cast)
    case ref: EagerSymbol =>
      if (runtimeEnv.hasVar(ref)) {
        runtimeEnv.getVar(ref).foreach(varLoc => mv.visitIntInsn(Opcodes.ALOAD, varLoc))
      } else {
        throw new RuntimeException(s"Unexpected exception: no capture found: $ref")
      }
    case l@LispClause(_) => new LispClauseWriter(l).writeValue()
    case LispValueDef(symbol, value) =>
      new LispValueAsmWriter(value).writeValue(None)
      value.resolveType.map(varType => {
        val varIdx = LengineEnv.callLastWithLabel(symbol.name, varType, new LispValueDefWriter(symbol, value).writeValue)(runtimeEnv)
        mv.visitIntInsn(Opcodes.ASTORE, varIdx)
        runtimeEnv.registerVariable(symbol, varIdx)
      })
    case f: LispFuncDef =>
      new LispFnAsmWriter(f).writeValue()
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
    val seqIdx = runtimeEnv.allocateNextVar
    mv.visitIntInsn(Opcodes.ASTORE, seqIdx)
    body.foreach(value => {
      new LispValueAsmWriter(value).writeValue()
      val idx = runtimeEnv.allocateNextVar
      mv.visitIntInsn(Opcodes.ASTORE, idx)
      mv.visitIntInsn(Opcodes.ALOAD, seqIdx)
      mv.visitIntInsn(Opcodes.ALOAD, idx)
      mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
        "lengine/runtime/Sequence",
        "add",
        Type.getMethodDescriptor(
          Type.getType(java.lang.Void.TYPE),
          Type.getType(classOf[java.lang.Object])
        ),
        false
      )
    })
    mv.visitIntInsn(Opcodes.ALOAD, seqIdx)
  }

}