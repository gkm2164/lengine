package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.values.LispUnit.ResolveHelper
import co.gyeongmin.lisp.types.{LengineChar, LengineDouble, LengineInteger, LengineString, LengineType}
import lengine.Prelude
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}

object LengineTypeSystem {
  implicit class TypeCastor(lengineType: LengineType) {
    def boxing(implicit mv: MethodVisitor): Unit = {
      val (boxed, primitive) = lengineType match {
        case LengineChar => (classOf[java.lang.Character], java.lang.Character.TYPE)
        case LengineInteger => (classOf[java.lang.Long], java.lang.Long.TYPE)
        case LengineDouble => (classOf[java.lang.Double], java.lang.Double.TYPE)
        case LengineString => return
      }

      mv.visitMethodInsn(
        Opcodes.INVOKESTATIC,
        Type.getInternalName(boxed),
        "valueOf",
        Type.getMethodDescriptor(
          Type.getType(boxed),
          Type.getType(primitive)
        ),
        false
      )
    }

    def cast(toType: LengineType)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
      val mv = runtimeEnvironment.methodVisitor
      if (lengineType == toType) {
        return
      }
      mv.visitLdcInsn(toType.getJvmType.getName)
      mv.visitMethodInsn(
        Opcodes.INVOKESTATIC,
        Type.getType(classOf[Class[_]]).getInternalName,
        "forName",
        Type.getMethodDescriptor(
          Type.getType(classOf[Class[_]]),
          Type.getType(classOf[String])
        ),
        false
      )
      mv.visitMethodInsn(
        Opcodes.INVOKESTATIC,
        Type.getType(classOf[Prelude]).getInternalName,
        "cast",
        Type.getMethodDescriptor(
          Type.getType(classOf[Object]),
          Type.getType(classOf[Object]),
          Type.getType(classOf[Class[_]])
        ),
        false
      )
    }
  }

  implicit val resolveHelper: ResolveHelper = name => LengineEnv.getVarInfo(name).map(_.storedType)
}
