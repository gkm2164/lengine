package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.lexer.statements._
import co.gyeongmin.lisp.lexer.values.boolean.{LispFalse, LispTrue}
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol, ObjectReferSymbol}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispObject, LispValue}
import lengine.runtime.{LengineMap, LengineMapKey, Sequence}
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

import scala.annotation.tailrec

class LispValueAsmWriter(value: LispValue)(implicit runtimeEnv: LengineRuntimeEnvironment) {
  import LengineTypeSystem._

  val mv: MethodVisitor = runtimeEnv.methodVisitor
  private def boxing(boxedType: Class[_ <: Object], primitiveType: Class[_ <: Object]): Unit = {
    mv.visitStaticMethodCall(
      boxedType,
      "valueOf",
      boxedType,
      primitiveType
    )
  }

  private def declareMap(map: Map[ObjectReferSymbol, LispValue]): Unit = {
    mv.visitStaticMethodCall(
      classOf[LengineMap],
      "create",
      classOf[LengineMap]
    )
    val mapIdx = runtimeEnv.allocateNextVar
    mv.visitIntInsn(Opcodes.ASTORE, mapIdx)
    map.foreach {
      case (symbol, value) =>
        mv.visitALoad(mapIdx)
        mv.visitLispValue(symbol, needReturn = true)
        mv.visitLispValue(value, needReturn = true)
        mv.visitMethodCall(
          classOf[LengineMap],
          "put",
          Void.TYPE,
          classOf[LengineMapKey], classOf[Object]
        )
    }
    mv.visitALoad(mapIdx)
  }

  def visitForValue(finalCast: Option[LengineType] = None, tailRecReference: Option[(LispSymbol, Label)] = None, needReturn: Boolean): Unit = value match {
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
    case ObjectReferSymbol(key) =>
      mv.visitLdcInsn(key)
      mv.visitStaticMethodCall(
        classOf[LengineMapKey],
        "create",
        classOf[LengineMapKey],
        classOf[String]
      )
    case LispLetDef(name, value, body) =>
      val newEnv = runtimeEnv.createChild()
      mv.visitLabel(new Label())
      val idx = newEnv.allocateNextVar
      new LispValueAsmWriter(value)(newEnv).visitForValue(needReturn = true)
      mv.visitAStore(idx)
      newEnv.registerVariable(name, idx)
      new LispValueAsmWriter(body)(newEnv).visitForValue(needReturn = true, tailRecReference = tailRecReference)
      val used = newEnv.getLastVarIdx
      runtimeEnv.overrideUsedVar(used)
      mv.visitLabel(new Label())
    case LispImportDef(path) =>
      new LispValueAsmWriter(
        LispClause(EagerSymbol("import") :: path :: Nil)
      ).visitForValue(needReturn = false)
    case LispLoopStmt(forStmts, body) =>
      new LispLoopAsmWriter(forStmts, body, tailRecReference = tailRecReference).writeValue()
    case LispDoStmt(body) =>
      visitDoBody(body, tailRecReference = tailRecReference)
    case ref: EagerSymbol =>
      if (runtimeEnv.hasVar(ref)) {
        runtimeEnv.getVar(ref).foreach(varLoc => mv.visitIntInsn(Opcodes.ALOAD, varLoc))
      } else {
        throw new RuntimeException(s"Unexpected exception: no capture found: $ref")
      }
    case l@LispClause(_) => new LispClauseWriter(l).visitForValue(needReturn, tailRecReference = tailRecReference)
    case LispValueDef(symbol, value) =>
      new LispValueAsmWriter(value).visitForValue(None, needReturn = true, tailRecReference = tailRecReference)
      value.resolveType match {
        case Left(err) =>
          new RuntimeException(s"Unable to resolve the type for $symbol: $err")
        case Right(varType) =>
          val varIdx = LengineEnv.callLastWithLabel(symbol.name, varType, new LispValueDefWriter(symbol, value).writeValue)(runtimeEnv)
          mv.visitAStore(varIdx)
          runtimeEnv.registerVariable(symbol, varIdx)

      }
    case LispFuncDef(symbol, funcDef) =>
      new LispFnAsmWriter(funcDef).writeValue(Some(symbol))
      val fnIdx = runtimeEnv.allocateNextVar
      mv.visitAStore(fnIdx)
      runtimeEnv.registerVariable(symbol, fnIdx)
    case genDef: GeneralLispFunc =>
      new LispFnAsmWriter(genDef).writeValue()
  }


  private def declareSequence(body: List[LispValue]): Unit = {
    val arrayLoc = mv.allocateNewArray(classOf[Object], body.length)
    mv.visitArrayAssignWithLispValues(body, arrayLoc)
    mv.visitALoad(arrayLoc)
    mv.visitStaticMethodCall(
      classOf[Sequence],
      "create",
      classOf[Sequence],
      classOf[Array[Object]]
    )
  }

  @tailrec
  private def visitDoBody(body: List[LispValue], tailRecReference: Option[(LispSymbol, Label)]): Unit = body match {
    case Nil =>
      throw new RuntimeException("unexpected error: do statement can't be empty")
    case v :: Nil =>
      mv.visitLispValue(v, needReturn = true, tailRecReference = tailRecReference)
    case v :: tail =>
      mv.visitLispValue(v)
      visitDoBody(tail, tailRecReference = tailRecReference)
  }
}