package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.compile.asmwriter.LengineType.{
  CharacterClass,
  CharacterPrimitive,
  ClassClass,
  DoubleClass,
  DoublePrimitive,
  LongClass,
  LongPrimitive,
  ObjectClass,
  PreludeClass
}
import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.lexer.statements.{ LispLetDef, LispLoopStmt }
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{ FloatNumber, IntegerNumber }
import co.gyeongmin.lisp.lexer.values.seq.{ LispSeq, LispString }
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import co.gyeongmin.lisp.lexer.values.{ LispChar, LispClause, LispObject, LispValue }
import org.objectweb.asm.Type

object LengineTypeSystem {
  implicit class TypeCastor(lengineType: LengineType) {
    def boxing(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
      val (boxed, primitive) = lengineType match {
        case LengineChar    => (CharacterClass, CharacterPrimitive)
        case LengineInteger => (LongClass, LongPrimitive)
        case LengineDouble  => (DoubleClass, DoublePrimitive)
        case LengineString  => return
      }
      val mv = runtimeEnvironment.methodVisitor

      mv.visitStaticMethodCall(
        boxed,
        "valueOf",
        boxed,
        primitive
      )
    }

    def cast(toType: LengineType)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
      val mv = runtimeEnvironment.methodVisitor
      if (lengineType == toType) {
        return
      }
      mv.visitLdcInsn(Type.getType(toType.getJvmType))
      mv.visitStaticMethodCall(
        PreludeClass,
        "cast",
        ObjectClass,
        ObjectClass,
        ClassClass
      )
    }
  }

  implicit class LispValueTypeExtension(lispValue: LispValue) {
    def resolveType: Either[EvalError, LengineType] = lispValue match {
      case _: LispChar        => Right(LengineChar)
      case _: IntegerNumber   => Right(LengineInteger)
      case _: FloatNumber     => Right(LengineDouble)
      case _: LispString      => Right(LengineString)
      case _: LispClause      => Right(LengineAny)
      case _: LispLetDef      => Right(LengineAny)
      case _: LispSymbol      => Right(LengineAny)
      case _: LispSeq         => Right(LengineList)
      case _: LispObject      => Right(LengineObject)
      case _: LispBoolean     => Right(LengineBoolean)
      case _: GeneralLispFunc => Right(LengineAny)
      case _: LispLoopStmt    => Right(LengineAny)
    }
  }
}
