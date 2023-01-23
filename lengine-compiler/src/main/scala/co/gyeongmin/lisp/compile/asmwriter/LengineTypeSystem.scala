package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.types.{LengineString, LengineType}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

object LengineTypeSystem {

  implicit class TypeCastor(lengineType: LengineType) {

    def cast(toType: LengineType)(implicit mv: MethodVisitor): Unit = {
      val originType  = Type.getType(lengineType.getJvmNativeType)
      val castingType = Type.getType(toType.getJvmNativeType)

      if (lengineType == toType) {
        return
      }

      if (toType == LengineString) {
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
      } else {
        val boxedType = Type.getType(lengineType.getBoxedType)
        mv.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          boxedType.getInternalName,
          "valueOf",
          Type.getMethodDescriptor(
            boxedType,
            originType
          ),
          false
        )

        if (toType == LengineString) {
          mv.visitMethodInsn(
            Opcodes.INVOKEVIRTUAL,
            boxedType.getInternalName,
            "toString",
            Type.getMethodDescriptor(
              Type.getType(toType.getJvmNativeType),
              boxedType
            ),
            false
          )
        }
      }
    }
  }
}
