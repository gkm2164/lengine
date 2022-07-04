package co.gyeongmin.lisp

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

package object compile {

  def createClass(): Array[Byte] = {
    val cw = new ClassWriter(0)
    cw.visit(
      49,
      ACC_PUBLIC + ACC_SUPER,
      "Hello",
      null,
      "java/lang/Object",
      null
    )
//    val init = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
//    init.visitVarInsn(ALOAD, 0)
//    init.visitMethodInsn(
//      INVOKESPECIAL,
//      "java/lang/Object",
//      "<init>",
//      "()V"
//    )
//    init.visitInsn(RETURN)
//    init.visitMaxs(1, 1)
//    init.visitEnd()

    val main = cw.visitMethod(
      ACC_PUBLIC + ACC_STATIC,
      "main",
      "([Ljava/lang/String;)V",
      null,
      null
    )

    main.visitFieldInsn(
      GETSTATIC,
      "java/lang/System",
      "out",
      "Ljava/io/PrintStream;"
    )
    main.visitLdcInsn("hello")
    main.visitMethodInsn(
      INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      "(Ljava/lang/String;)V"
    )
    main.visitInsn(RETURN)
    main.visitMaxs(2, 1)
    main.visitEnd()

    cw.visitEnd()
    cw.toByteArray
  }
}
