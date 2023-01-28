package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.values.{LispClause, LispValue}
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol, ObjectReferSymbol}
import lengine.runtime.{LengineFn, LengineMap}
import org.objectweb.asm.{MethodVisitor, Opcodes, Type}
import org.objectweb.asm.Opcodes._

import scala.collection.mutable

class LispClauseWriter(clause: LispClause)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {

  import AsmHelper._

  val mv: MethodVisitor = runtimeEnvironment.methodVisitor

  private def declareObjectRefer(key: String, operands: List[LispValue]): Unit = {
    val map :: _ = operands
    mv.visitLdcInsn(key)
    val keyIdx = runtimeEnvironment.allocateNextVar
    mv.visitIntInsn(Opcodes.ASTORE, keyIdx)
    new LispValueAsmWriter(map).writeValue()
    mv.visitIntInsn(Opcodes.ALOAD, keyIdx)
    mv.visitMethodInsn(
      INVOKEVIRTUAL,
      Type.getType(classOf[LengineMap]).getInternalName,
      "get",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[Object]),
      ),
      false
    )
  }

  def writeValue(): Unit = {
    val operation :: operands = clause.body
    operation match {
      case ObjectReferSymbol(key) => declareObjectRefer(key, operands)
      case s if RuntimeMethodVisitor.supportOperation(s) => RuntimeMethodVisitor.handle(clause.body)
      case s: EagerSymbol if runtimeEnvironment.hasFn(s) =>
        runtimeEnvironment.getFn(s).foreach(realFn => {
          LengineEnv.getFn(realFn.name).foreach(fn => {
            val popThis = mutable.ListBuffer[Int]()
            (0 until fn.args).zip(operands).foreach { case (_, value) =>
              new LispValueAsmWriter(value).writeValue()
              val loc = runtimeEnvironment.allocateNextVar
              mv.visitIntInsn(Opcodes.ASTORE, loc)
              popThis += loc
            }

            fn.fnEnv.captureVariables.foreach(_.getRequestedCaptures.foreach(symbol => {
              val location = runtimeEnvironment.getVar(symbol)
              if (location.isEmpty) {
                throw new RuntimeException(s"Unable to capture variable: $symbol")
              }

              val Some(locationAddress) = location
              popThis += locationAddress
            }))

            val argCount = fn.args + fn.fnEnv.captureVariables.map(_.getRequestedCaptures.size).getOrElse(0)

            popThis.foreach(mv.visitIntInsn(ALOAD, _))
            mv.visitMethodInsn(
              INVOKESTATIC,
              runtimeEnvironment.className,
              realFn.name.name,
              getFnDescriptor(classOf[Object], argCount),
              false
            )
          })
        })
      case s: EagerSymbol if runtimeEnvironment.hasVar(s) =>
        runtimeEnvironment.getVar(s).foreach(suspectFn => {
          val mv = runtimeEnvironment.methodVisitor
          val argLoc = runtimeEnvironment.allocateNextVar
          runtimeEnvironment.allocateNewArray(classOf[Object], operands.size, argLoc)
          val tmpLoc = runtimeEnvironment.allocateNextVar
          operands.zipWithIndex.foreach {
            case (value, idx) =>
              new LispValueAsmWriter(value).writeValue()
              mv.visitIntInsn(Opcodes.ASTORE, tmpLoc)
              mv.visitIntInsn(Opcodes.ALOAD, argLoc)
              mv.visitLdcInsn(idx)
              mv.visitIntInsn(Opcodes.ALOAD, tmpLoc)
              mv.visitInsn(AASTORE)
          }

          mv.visitIntInsn(Opcodes.ALOAD, suspectFn)
          mv.visitTypeInsn(Opcodes.CHECKCAST,
            Type.getType(classOf[LengineFn]).getInternalName)
          mv.visitIntInsn(Opcodes.ALOAD, argLoc)
          mv.visitMethodInsn(
            INVOKEVIRTUAL,
            Type.getType(classOf[LengineFn]).getInternalName,
            "invoke",
            Type.getMethodDescriptor(
              Type.getType(classOf[Object]),
              Type.getType(classOf[Array[Object]])
            ),
            false
          )
        })
      case _ =>
    }
  }
}
