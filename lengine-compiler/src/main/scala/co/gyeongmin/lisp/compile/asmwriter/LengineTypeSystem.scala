package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.lexer.values.numbers.IntegerNumber
import co.gyeongmin.lisp.types.{LengineChar, LengineDouble, LengineInteger, LengineNumber, LengineString, LengineType}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

object LengineTypeSystem {

  implicit class TypeCastor(lengineType: LengineType) {

    def cast(toType: LengineType)(implicit mv: MethodVisitor): Unit = {
      val originType  = Type.getType(lengineType.getJvmNativeType)
      val castingType = Type.getType(toType.getJvmNativeType)

      if (lengineType == toType) {
        return
      }

      toType match {
        case LengineString =>
          mv.visitMethodInsn(
            Opcodes.INVOKESTATIC,
            castingType.getInternalName,
            "valueOf",
            Type.getMethodDescriptor(
              castingType,
              originType
            ),
            false
          )
        case LengineDouble => lengineType match {
          case LengineChar => mv.visitInsn(Opcodes.I2D)
          case LengineInteger => mv.visitInsn(Opcodes.L2D)
          case _ => throw new IllegalArgumentException("Unsupported casting")
        }
      }
    }
  }
}
