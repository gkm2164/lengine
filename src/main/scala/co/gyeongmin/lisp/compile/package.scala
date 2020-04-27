package co.gyeongmin.lisp

import co.gyeongmin.lisp.errors._
import co.gyeongmin.lisp.execution.LispEnvironment
import co.gyeongmin.lisp.lexer._
import org.objectweb.asm._

package object compile {
  type LispCompileEnv = Map[LispValue, String]
  implicit class LispCompileValue(v: LispValue) {
    def compile(env: LispEnvironment, cw: ClassWriter): Either[EvalError, LispEnvironment] = v match {
      case LispNamespace(LispString(namespace)) =>
        Right(env.updated(EagerSymbol("$$NAMESPACE$$"), LispString(namespace)))
      case LispFuncDef(symbol, fn) =>
        val mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, symbol.toString,
          s"""(${fn.placeHolders.mkString(",")})Object""", null, null)
        mv.visitCode()
        fn.writeMethod(env, mv)
        mv.visitEnd()
        cw.visitEnd()
        Right(env)
      case _: LispLetDef => Right(env)
      case _: LispValueDef => Right(env)
      case _: LispDoStmt => Right(env)
      case _: LispLoopStmt => Right(env)
      case LispImportDef(LispString(_)) => Right(env)
      case _: LazySymbol => Right(env)
      case _: LispSymbol => Right(env)
      case LispClause(_ :: _) => Right(env)
      case m: SpecialToken => Right(env)
      case n: LispNumber => Right(env)
      case LispObject(_) | LispChar(_) | LispString(_) | LispList(_) | LispUnit | LispTrue | LispFalse => Right(env)
      case f: GeneralLispFunc => Right(env)
      case value => Right(env)
    }
  }

  implicit class GeneralLispFuncWriter(fn: GeneralLispFunc) {
    def writeMethod(env: LispEnvironment, mv: MethodVisitor): Unit = fn.body match {
      case LispClause(EagerSymbol("+") :: IntegerNumber(a) :: IntegerNumber(b) :: Nil) =>
        mv.visitInsn(Opcodes.ICONST_3)
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "co/gyeongmin/lisp/Runtime", s"add", "", false)
      case _ =>
    }
  }
}
