package co.gyeongmin.lisp

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorWrapper
import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorWrapper.MethodVisitorWrapperExt
import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.compile.asmwriter.{AsmHelper, CompileException, LengineRuntimeEnvironment, LispValueAsmWriter}
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label, Type}

import scala.collection.mutable

package object compile {

  def writeClass(sourceFileName: String, pkgName: String, clsName: String, statements: List[LispValue]): Array[Byte] = {
    val cw = new ClassWriter(AsmHelper.GlobalConfig)
    val qualifiedName = if (pkgName.nonEmpty) {
      s"${pkgName.replaceAllLiterally(".", "/")}/$clsName"
    } else {
      clsName
    }
    cw.visit(V1_8, ACC_PUBLIC, qualifiedName, null, "java/lang/Object", null)
    cw.visitSource(sourceFileName, null)
    writeCsInitMethod(cw, qualifiedName)
    writeInitMethod(cw)
    writeMain(sourceFileName, cw, statements, qualifiedName)
    writeExportMethod(cw, qualifiedName)
    writeImportMethod(cw, qualifiedName)

    cw.toByteArray
  }

  private def writeMain(sourceFileName: String,
                        cw: ClassWriter,
                        statements: List[LispValue],
                        className: String): Unit = {
    val mv = cw
      .visitMethod(ACC_PUBLIC | ACC_STATIC,
                   "main",
                   Type.getMethodDescriptor(
                     Type.getType(VoidPrimitive),
                     Type.getType(StringArrayClass)
                   ),
                   null,
                   null)
      .wrap()
    mv.visitCode()
    val startLabel: Label = new Label()
    val endLabel: Label   = new Label()
    mv.visitLabel(startLabel)
    implicit val mainRuntimeEnv: LengineRuntimeEnvironment =
      new LengineRuntimeEnvironment(cw, mv, mutable.Map(), className, sourceFileName, 1)

    statements.foreach(stmt => {
      val thisLabel = new Label
      mv.visitLabel(thisLabel)
      new LispValueAsmWriter(stmt, ObjectClass).visitForValue()
      stmt match {
        case LispClause(EagerSymbol("export") :: _) =>
        case _                                      => mv.visitPop()
      }
      if (mv.stackSizeTrace.get() > 0) {
        throw CompileException(s"somewhere, stack leaking is happening",
                               filename = mainRuntimeEnv.fileName,
                               location = stmt.tokenLocation)
      }
    })
    mv.visitLabel(endLabel)
    mv.visitReturn()
    // Need to give hint to assembly generator for helping decide frame size
    mv.visitLocalVariable("__PADDING__",
      Type.getType(ObjectClass).getDescriptor,
      null,
      startLabel,
      endLabel,
      mainRuntimeEnv.getLastVarIdx
    )
    mv.visitMaxs()
    mv.visitEnd()
  }

  private def writeInitMethod(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null).wrap()
    mv.visitCode()
    mv.visitALoad(0)
    mv.visitSpecialMethodCall(ObjectClass, "<init>", VoidPrimitive)
    mv.visitReturn()
    mv.visitMaxs()
    mv.visitEnd()
  }

  private def writeCsInitMethod(cw: ClassWriter, className: String): Unit = {
    val fieldVisit = cw.visitField(
      ACC_PRIVATE | ACC_STATIC,
      "exportMap",
      Type.getType(JavaMapClass).getDescriptor,
      null,
      null
    )
    fieldVisit.visitEnd()

    val mv: MethodVisitorWrapper = cw
      .visitMethod(ACC_STATIC,
                   "<clinit>",
                   Type.getMethodDescriptor(
                     Type.getType(VoidPrimitive)
                   ),
                   null,
                   null)
      .wrap()

    mv.visitCode()
    mv.visitNew(JavaHashMapClass)
    mv.visitDup()
    mv.visitSpecialMethodCall(
      JavaHashMapClass,
      "<init>",
      VoidPrimitive
    )
    mv.visitPutStatic(
      className,
      "exportMap",
      JavaMapClass
    )

    mv.visitReturn()
    mv.visitMaxs()
    mv.visitEnd()
  }

  private def writeExportMethod(cw: ClassWriter, clsName: String): Unit = {
    val mv = cw
      .visitMethod(
        ACC_PRIVATE | ACC_STATIC,
        "export",
        Type.getMethodDescriptor(
          Type.getType(VoidPrimitive),
          Type.getType(StringClass),
          Type.getType(ObjectClass)
        ),
        null,
        null
      )
      .wrap()

    mv.visitGetStatic(clsName, "exportMap", JavaMapClass)
    mv.visitALoad(0)
    mv.visitALoad(1)
    mv.visitInterfaceMethodCall(
      JavaMapClass,
      "put",
      ObjectClass,
      ObjectClass,
      ObjectClass
    )
    mv.visitReturn()
    mv.visitMaxs()
    mv.visitEnd()
  }

  private def writeImportMethod(cw: ClassWriter, clsName: String): Unit = {
    val mv = cw
      .visitMethod(
        ACC_PUBLIC | ACC_STATIC,
        "importSymbol",
        Type.getMethodDescriptor(
          Type.getType(ObjectClass),
          Type.getType(StringClass)
        ),
        null,
        null
      )
      .wrap()

    mv.visitGetStatic(clsName, "exportMap", JavaMapClass)
    mv.visitALoad(0)
    mv.visitInterfaceMethodCall(
      JavaMapClass,
      "get",
      ObjectClass,
      ObjectClass
    )
    mv.visitAReturn()
    mv.visitMaxs()
    mv.visitEnd()
  }
}
