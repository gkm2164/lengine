package co.gyeongmin.lisp

import co.gyeongmin.lisp.lexer.{EagerSymbol, LispSymbol}
import org.objectweb.asm._

package object asm {

  trait LengineASMError

  type ByteWriter = MethodVisitor => Either[LengineASMError, Unit]

  type LispASMEnvironment = Map[LispSymbol, ByteWriter]

  def runtime: LispASMEnvironment = Map(
    EagerSymbol("+") -> { mv =>
      Right(mv.visitEnd())
    }
  )
}
