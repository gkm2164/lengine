package co.gyeongmin.lisp

import lengine.runtime.{ClassPrinter, LengineObjectMain}
import org.objectweb.asm.{ClassReader, ClassWriter}
import org.objectweb.asm.Opcodes._

import java.io.FileInputStream

package object compile {
  val main = new LengineObjectMain

  def printRuntime() = {
    val classReader = new ClassReader(
      new FileInputStream(
        "./lengine-runtime/target/scala-2.12/classes/lengine/runtime/LengineObjectMain.class"
      )
    )
    classReader.accept(new ClassPrinter, 0)
  }

  def createClass(): Array[Byte] = {
    printRuntime()

    val cr = new ClassReader(
      new FileInputStream(
        "./lengine-runtime/target/scala-2.12/classes/lengine/runtime/LengineObjectMain.class"
      )
    )
    val cw = new ClassWriter(cr, 0)
    cw.visit(
      49,
      ACC_PUBLIC + ACC_SUPER,
      "LengineObjectMain",
      null,
      "java/lang/Object",
      null
    )

    val field = cw.visitField(
      ACC_STATIC,
      "runtime",
      "Llengine/runtime/LengineRuntime;",
      null,
      null
    )

    field.visitEnd()

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
      "(Ljava/lang/String;)V",
      false
    )

    main.visitInsn(RETURN)
    main.visitMaxs(2, 1)
    main.visitEnd()

    cw.visitEnd()
    cw.toByteArray
  }
}
