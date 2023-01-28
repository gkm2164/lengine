package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.statements.{LispFuncDef, LispLoopStmt, LispValueDef}
import co.gyeongmin.lisp.lexer.tokens.LispLambda
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, ObjectReferSymbol}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispObject, LispValue}
import lengine.runtime.{LengineMap, Sequence}
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

  def declareMap(map: Map[ObjectReferSymbol, LispValue]): Unit = {
    mv.visitMethodInsn(
      Opcodes.INVOKESTATIC,
      Type.getType(classOf[LengineMap]).getInternalName,
      "create",
      Type.getMethodDescriptor(
        Type.getType(classOf[LengineMap])
      ),
      false
    )
    val mapIdx = runtimeEnv.allocateNextVar
    mv.visitIntInsn(Opcodes.ASTORE, mapIdx)
    map.foreach {
      case (ObjectReferSymbol(name), value) =>
        val keyIdx = runtimeEnv.allocateNextVar
        val valIdx = runtimeEnv.allocateNextVar
        mv.visitLdcInsn(name)
        mv.visitIntInsn(Opcodes.ASTORE, keyIdx)

        new LispValueAsmWriter(value).writeValue()
        mv.visitIntInsn(Opcodes.ASTORE, valIdx)

        mv.visitIntInsn(Opcodes.ALOAD, mapIdx)
        mv.visitIntInsn(Opcodes.ALOAD, keyIdx)
        mv.visitIntInsn(Opcodes.ALOAD, valIdx)

        mv.visitMethodInsn(
          Opcodes.INVOKEVIRTUAL,
          Type.getType(classOf[LengineMap]).getInternalName,
          "put",
          Type.getMethodDescriptor(
            Type.getType(Void.TYPE),
            Type.getType(classOf[Object]),
            Type.getType(classOf[Object]),
          ),
          false
        )
    }
    mv.visitIntInsn(Opcodes.ALOAD, mapIdx)
  }

  def writeValue(finalCast: Option[LengineType] = None): Unit = value match {
    case LispTrue =>
      mv.visitLdcInsn(true)
      boxing(classOf[java.lang.Boolean], java.lang.Boolean.TYPE)
    case LispFalse =>
      mv.visitLdcInsn(false)
      boxing(classOf[java.lang.Boolean], java.lang.Boolean.TYPE)
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
    case LispObject(kv) =>
      declareMap(kv)
      finalCast.foreach(LengineList.cast)
    case LispLoopStmt(forStmts, body) =>
      new LispLoopAsmWriter(forStmts, body).writeValue()
    case ref: EagerSymbol =>
      if (runtimeEnv.hasVar(ref)) {
        runtimeEnv.getVar(ref).foreach(varLoc => mv.visitIntInsn(Opcodes.ALOAD, varLoc))
      } else {
        throw new RuntimeException(s"Unexpected exception: no capture found: $ref")
      }
    case l@LispClause(_) => new LispClauseWriter(l).writeValue()
    case LispValueDef(symbol, value) =>
      new LispValueAsmWriter(value).writeValue(None)
      value.resolveType match {
        case Left(err) =>
          new RuntimeException(s"Unable to resolve the type for $symbol: $err")
        case Right(varType) =>
          val varIdx = LengineEnv.callLastWithLabel(symbol.name, varType, new LispValueDefWriter(symbol, value).writeValue)(runtimeEnv)
          mv.visitIntInsn(Opcodes.ASTORE, varIdx)
          runtimeEnv.registerVariable(symbol, varIdx)

      }
    case LispFuncDef(symbol, funcDef) =>
      val fnName = new LispFnAsmWriter(funcDef).writeValue()
      runtimeEnv.mapFnName(symbol, fnName)
    case genDef: GeneralLispFunc =>
      new LispFnAsmWriter(genDef).writeValue()
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
        Type.getType(classOf[Sequence]).getInternalName,
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