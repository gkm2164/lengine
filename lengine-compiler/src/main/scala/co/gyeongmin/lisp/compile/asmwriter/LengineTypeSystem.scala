package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.lexer.values.boolean.LispBoolean
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.numbers.{FloatNumber, IntegerNumber}
import co.gyeongmin.lisp.lexer.values.seq.{LispSeq, LispString}
import co.gyeongmin.lisp.lexer.values.{LispChar, LispClause, LispObject, LispValue}
import lengine.Prelude
import org.objectweb.asm.MethodVisitor

object LengineTypeSystem {
  implicit class TypeCastor(lengineType: LengineType) {
    def boxing(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
      val (boxed, primitive) = lengineType match {
        case LengineChar    => (classOf[java.lang.Character], java.lang.Character.TYPE)
        case LengineInteger => (classOf[java.lang.Long], java.lang.Long.TYPE)
        case LengineDouble  => (classOf[java.lang.Double], java.lang.Double.TYPE)
        case LengineString  => return
      }
      val mv = runtimeEnvironment.methodVisitor

      mv.visitStaticMethodCall(
        boxed,
        "valueOf",
        boxed,
        primitive :: Nil
      )
    }

    def cast(toType: LengineType)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
      val mv = runtimeEnvironment.methodVisitor
      if (lengineType == toType) {
        return
      }
      mv.visitLdcInsn(toType.getJvmType.getName)
      mv.visitStaticMethodCall(
        classOf[Class[_]],
        "forName",
        classOf[Class[_]],
        List(classOf[String])
      )

      mv.visitStaticMethodCall(
        classOf[Prelude],
        "cast",
        classOf[Object],
        List(
          classOf[Object],
          classOf[Class[_]]
        )
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
      case _: LispSeq         => Right(LengineList)
      case _: LispObject      => Right(LengineObject)
      case _: LispBoolean     => Right(LengineBoolean)
      case _: GeneralLispFunc => Right(LengineFunction)
    }
  }
}
