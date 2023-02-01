package co.gyeongmin.lisp

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{ObjectClass, StringClass, VoidPrimitive}
import co.gyeongmin.lisp.compile.asmwriter.{AsmHelper, LengineRuntimeEnvironment, LispValueAsmWriter, LispValueDefWriter}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label, Type}

import scala.collection.mutable

package object compile {

  def writeClass(clsName: String, statements: List[LispValue]): Array[Byte] = {
    val cw = new ClassWriter(AsmHelper.GLOBAL_CONFIG)
    cw.visit(V1_8, ACC_PUBLIC, clsName, null, "java/lang/Object", null)
    writeCsInitMethod(cw, clsName)
    writeInitMethod(cw)
    writeMain(cw, statements, clsName)
    writeExportMethod(cw, clsName)
    writeImportMethod(cw, clsName)

    cw.toByteArray
  }

  private def writeMain(cw: ClassWriter, statements: List[LispValue], className: String): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC | ACC_STATIC,
                            "main",
                            Type.getMethodDescriptor(
                              Type.getType(java.lang.Void.TYPE),
                              Type.getType(classOf[Array[String]])
                            ),
                            null,
                            null)
    mv.visitCode()
    val startLabel: Label = new Label()
    val endLabel: Label = new Label()
    mv.visitLabel(startLabel)
    implicit val mainRuntimeEnv: LengineRuntimeEnvironment =
      new LengineRuntimeEnvironment(cw, mv, mutable.Map(), className, 1)

    statements.foreach(stmt => new LispValueAsmWriter(stmt, ObjectClass).visitForValue(needReturn = false))
    mv.visitLabel(endLabel)
    mv.visitInsn(RETURN)
    // Need to give hint to assembly generator for helping decide frame size
    new LispValueDefWriter(EagerSymbol("__PADDING__"))
      .writeValue(startLabel, endLabel, mainRuntimeEnv.getLastVarIdx)
    mv.visitMaxs(0, 0)
    mv.visitEnd()
  }

  private def writeInitMethod(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  private def writeCsInitMethod(cw: ClassWriter, className: String): Unit = {
    val fieldVisit = cw.visitField(
      ACC_PUBLIC | ACC_STATIC,
      "exportMap",
      Type.getType(classOf[java.util.Map[String, Object]]).getDescriptor,
      null,
      null
    )
    fieldVisit.visitEnd()

    val mv = cw.visitMethod(ACC_STATIC,
                            "<clinit>",
                            Type.getMethodDescriptor(
                              Type.getType(Void.TYPE)
                            ),
                            null,
                            null)
    mv.visitCode()
    mv.visitTypeInsn(NEW, "java/util/HashMap")
    mv.visitInsn(DUP)
    mv.visitMethodInsn(
      INVOKESPECIAL,
      "java/util/HashMap",
      "<init>",
      "()V",
      false
    )
    mv.visitFieldInsn(
      PUTSTATIC,
      className,
      "exportMap",
      "Ljava/util/Map;"
    )
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  private def writeExportMethod(cw: ClassWriter, clsName: String): Unit = {
    val mv = cw.visitMethod(
      ACC_PUBLIC | ACC_STATIC,
      "export",
      Type.getMethodDescriptor(
        Type.getType(VoidPrimitive),
        Type.getType(StringClass),
        Type.getType(ObjectClass)
      ),
      null,
      null
    )

    mv.visitFieldInsn(GETSTATIC, clsName, "exportMap", "Ljava/util/Map;")
    mv.visitIntInsn(ALOAD, 0)
    mv.visitIntInsn(ALOAD, 1)
    mv.visitMethodInsn(
      INVOKEINTERFACE,
      "java/util/Map",
      "put",
      Type.getMethodDescriptor(
        Type.getType(ObjectClass),
        Type.getType(ObjectClass),
        Type.getType(ObjectClass)
      ),
      true
    )
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }

  private def writeImportMethod(cw: ClassWriter, clsName: String): Unit = {
    val mv = cw.visitMethod(
      ACC_PUBLIC | ACC_STATIC,
      "importSymbol",
      Type.getMethodDescriptor(
        Type.getType(ObjectClass),
        Type.getType(StringClass)
      ),
      null,
      null
    )

    mv.visitFieldInsn(GETSTATIC, clsName, "exportMap", "Ljava/util/Map;")
    mv.visitIntInsn(ALOAD, 0)
    mv.visitMethodInsn(
      INVOKEINTERFACE,
      "java/util/Map",
      "get",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[Object]),
      ),
      true
    )
    mv.visitInsn(ARETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
  }
}
