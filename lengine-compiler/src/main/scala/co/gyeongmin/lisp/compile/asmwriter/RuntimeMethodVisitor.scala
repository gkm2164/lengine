package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.{MethodVisitorExtension, getFnDescriptor}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}
import lengine.Prelude
import lengine.functions.{LengineLambda1, LengineLambda2}
import lengine.runtime._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{Label, MethodVisitor, Type}

object RuntimeMethodVisitor {
  private val supportedOps = Set(
    "export",
    "import",
    "str",
    "int",
    "double",
    "char",
    "len",
    "+",
    "-",
    "*",
    "/",
    "take",
    "drop",
    "take-while",
    "drop-while",
    "split-at",
    "len",
    "filter",
    "flatten",
    "println",
    "read-line",
    "not",
    "if",
    "range",
    "assert",
    "fold",
    "seq",
    "head",
    "tail",
    "entry"
  )

  private val compareOpMap = Map(
    "<" -> "lt",
    "<=" -> "le",
    ">" -> "gt",
    ">=" -> "ge",
    "=" -> "eq",
    "/=" -> "neq",
    "and" -> "and",
    "or" -> "or"
  )

  private val PreludeClass: Type = Type.getType(classOf[Prelude])
  private val BooleanClass = classOf[java.lang.Boolean]
  private val BooleanClassType: Type = Type.getType(BooleanClass)

  private val ObjectClass: Type = Type.getType(classOf[Object])

  def supportOperation(operation: LispValue): Boolean = operation match {
    case EagerSymbol(op) => supportedOps.contains(op) || compareOpMap.contains(op)
    case _ => false
  }

  private def visitCreateEntry(
                                operands: List[LispValue]
                              )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: value :: Nil = operands
    val mv = runtimeEnvironment.methodVisitor

    new LispValueAsmWriter(key).visitForValue(needReturn = true)
    new LispValueAsmWriter(value).visitForValue(needReturn = true)
    mv.visitStaticMethodCall(
      classOf[LengineMapEntry],
      "create",
      classOf[LengineMapEntry],
      List(classOf[Object], classOf[Object])
    )
  }

  def handle(body: List[LispValue], needReturn: Boolean)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operation :: operands = body
    operation match {
      case EagerSymbol(op) =>
        op match {
          case "+" => visitCalc("add", operands)
          case "-" => visitCalc("sub", operands)
          case "*" => visitCalc("mult", operands)
          case "/" => visitCalc("div", operands)
          case "if" => visitIfStmt(operands)
          case "not" => visitNotOps(operands)
          case "len" => visitLenOp(operands)
          case "take" | "drop" => visitSeqOpN(op, operands)
          case "filter" | "take-while" | "drop-while" | "split-at" => visitSeqOpFn(op, operands)
          case "head" | "tail" => visitSeqUOps(op, operands)
          case "flatten" => visitFlatten(operands)
          case "println" => visitPrintln(operands, needReturn)
          case "read-line" => visitReadLine
          case "str" | "int" | "double" | "char" | "seq" => visitTypeCast(op, operands)
          case "assert" => visitAssert(operands)
          case "range" => visitRange(operands)
          case "fold" => visitFold(operands)
          case "export" => visitExport(operands)
          case "import" => visitImport(operands)
          case "entry" => visitCreateEntry(operands)
          case _ if compareOpMap.contains(op) => visitCompareOps(op, operands)
        }

    }
  }

  private def visitSeqOpFn(operationName: String,
                           operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val fn :: seq :: _ = operands

    val mv = runtimeEnvironment.methodVisitor

    new LispValueAsmWriter(fn).visitForValue(needReturn = true)
    mv.visitCheckCast(classOf[LengineLambda1[java.lang.Boolean, _]])
    new LispValueAsmWriter(seq).visitForValue(needReturn = true)
    mv.visitCheckCast(classOf[CreateIterator])
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      operationName match {
        case "filter" => "filter"
        case "take-while" => "takeWhile"
        case "drop-while" => "dropWhile"
        case "split-at" => "splitAt"
      },
      Type.getMethodDescriptor(
        Type.getType(classOf[Sequence]),
        Type.getType(classOf[LengineLambda1[java.lang.Boolean, _]]),
        Type.getType(classOf[CreateIterator])
      ),
      false
    )
  }

  private def visitLenOp(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    new LispValueAsmWriter(seq).visitForValue(needReturn = true)
    val mv = runtimeEnvironment.methodVisitor
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      "len",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[Object])
      ),
      false
    )
  }

  private def visitSeqOpN(operationName: String,
                          operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val number :: seq :: _ = operands
    new LispValueAsmWriter(number).visitForValue(needReturn = true)
    new LispValueAsmWriter(seq).visitForValue(needReturn = true)
    val mv = runtimeEnvironment.methodVisitor
    mv.visitCheckCast(classOf[CreateIterator])
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      operationName,
      Type.getMethodDescriptor(
        Type.getType(classOf[Sequence]),
        Type.getType(classOf[java.lang.Long]),
        Type.getType(classOf[CreateIterator])
      ),
      false
    )
  }

  private def visitSeqUOps(op: String,
                           operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    new LispValueAsmWriter(seq).visitForValue(needReturn = true)
    val mv = runtimeEnvironment.methodVisitor
    mv.visitCheckCast(classOf[CreateIterator])
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      op,
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[CreateIterator])
      ),
      false
    )
  }

  private def visitFlatten(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    new LispValueAsmWriter(seq).visitForValue(needReturn = true)
    val mv = runtimeEnvironment.methodVisitor
    mv.visitCheckCast(classOf[Sequence])
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      "flatten",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[Sequence])
      ),
      false
    )
  }

  private def visitCalc(operation: String,
                        operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(None, needReturn = true))
    val mv = runtimeEnvironment.methodVisitor
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      operation,
      getFnDescriptor(classOf[Object], 2),
      false
    )
  }

  private def visitPrintln(operands: List[LispValue], needReturn: Boolean)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(Some(LengineString), needReturn = true))
    mv.visitMethodInsn(INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      Type.getMethodDescriptor(
        Type.getType(Void.TYPE),
        Type.getType(classOf[Object])
      ),
      false)
    if (needReturn) {
      mv.visitUnit()
    }
  }

  private def visitTypeCast(op: String,
                            operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operand :: _ = operands

    new LispValueAsmWriter(operand).visitForValue(needReturn = true)

    val mv = runtimeEnvironment.methodVisitor

    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      s"cast_$op",
      Type.getMethodDescriptor(
        ObjectClass,
        ObjectClass
      ),
      false
    )
  }

  private def visitReadLine(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    mv.visitMethodInsn(INVOKESTATIC,
      PreludeClass.getInternalName,
      "readLine",
      Type.getMethodDescriptor(
        Type.getType(classOf[String])
      ),
      false)
  }

  private def visitCompareOps(op: String, operands: List[LispValue])(
    implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(needReturn = true))
    compareOpMap
      .get(op)
      .foreach(
        name =>
          mv.visitStaticMethodCall(
            classOf[Prelude],
            name,
            classOf[java.lang.Boolean],
            List(classOf[Object], classOf[Object])
          )
      )
  }

  private def visitNotOps(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    operands.foreach(v => mv.visitLispValue(v, needReturn = true))
    mv.visitMethodInsn(
      INVOKESTATIC,
      PreludeClass.getInternalName,
      "not",
      Type.getMethodDescriptor(
        BooleanClassType,
        ObjectClass
      ),
      false
    )
  }

  private def visitIfStmt(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val condition :: ifmatch :: elsematch :: Nil = operands
    val mv = runtimeEnvironment.methodVisitor

    new LispValueAsmWriter(condition).visitForValue(needReturn = true)
    mv.visitMethodCall(
      BooleanClass,
      "booleanValue",
      java.lang.Boolean.TYPE
    )

    val tLabel = new Label()
    val fLabel = new Label()
    val next = new Label()

    mv.visitJumpInsn(IFNE, tLabel)
    mv.visitLabel(fLabel)
    mv.visitLispValue(elsematch, needReturn = true)
    mv.visitJumpInsn(GOTO, next)

    mv.visitLabel(tLabel)
    mv.visitLispValue(ifmatch, needReturn = true)
    mv.visitLabel(next)
  }

  private def visitRange(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val from :: to :: Nil = operands

    val fromIdx = runtimeEnvironment.allocateNextVar
    val toIdx = runtimeEnvironment.allocateNextVar

    val mv = runtimeEnvironment.methodVisitor

    mv.visitLispValue(from, needReturn = true)
    mv.visitAStore(fromIdx)
    mv.visitLispValue(to, needReturn = true)
    mv.visitAStore(toIdx)

    mv.visitALoad(fromIdx)
    mv.visitALoad(toIdx)
    mv.visitStaticMethodCall(
      classOf[RangeSequence],
      "create",
      classOf[RangeSequence],
      List(classOf[java.lang.Long], classOf[java.lang.Long])
    )
  }

  private def visitAssert(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    val message :: v :: Nil = operands

    val messageLoc = mv.visitStoreLispValue(message)
    val vLoc = mv.visitStoreLispValue(v)

    mv.visitALoad(messageLoc)
    mv.visitALoad(vLoc)
    mv.visitStaticMethodCall(
      classOf[Prelude],
      "assertTrue",
      Void.TYPE,
      List(classOf[Object], classOf[Object])
    )
  }

  // (fold <seq> <init> <lambda>)
  private def visitFold(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val startLoop = new Label()
    val endLoop = new Label()

    val seq :: init :: lambda :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(lambda, needReturn = true)
    val fnLoc = runtimeEnvironment.allocateNextVar
    mv.visitAStore(fnLoc)

    val accLoc = runtimeEnvironment.allocateNextVar

    mv.visitLispValue(init, needReturn = true)
    mv.visitAStore(accLoc)
    mv.visitLispValue(seq, needReturn = true)
    mv.visitCheckCast(classOf[CreateIterator])
    mv.visitInterfaceMethodCall(
      classOf[CreateIterator],
      "iterator",
      classOf[LengineIterator]
    )

    val seqItLoc = runtimeEnvironment.allocateNextVar
    mv.visitAStore(seqItLoc)

    mv.visitLabel(startLoop)
    mv.visitALoad(seqItLoc)
    mv.visitInterfaceMethodCall(
      classOf[LengineIterator],
      "hasNext",
      java.lang.Boolean.TYPE
    )
    mv.visitJumpInsn(IFEQ, endLoop)

    mv.visitALoad(seqItLoc)
    mv.visitInterfaceMethodCall(
      classOf[LengineIterator],
      "next",
      classOf[Object],
      Nil,
    )
    val elemLoc = runtimeEnvironment.allocateNextVar
    mv.visitAStore(elemLoc)

    visitLambdaInvoke(fnLoc, accLoc, elemLoc)
    mv.visitAStore(accLoc)

    mv.visitJumpInsn(GOTO, startLoop)
    mv.visitLabel(endLoop)
    mv.visitALoad(accLoc)
  }

  private def visitLambdaInvoke(lambdaLoc: Int, accLoc: Int, elemLoc: Int)(
    implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    mv.visitALoad(lambdaLoc)
    mv.visitCheckCast(classOf[LengineLambda2[_, _, _]])
    mv.visitALoad(accLoc)
    mv.visitALoad(elemLoc)
    mv.visitInterfaceMethodCall(
      classOf[LengineLambda2[_, _, _]],
      "invoke",
      classOf[Object],
      List(classOf[Object], classOf[Object])
    )
  }

  private def visitExport(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val symbol :: value :: Nil = operands
    val symbolLoc = runtimeEnvironment.allocateNextVar

    val nameOfSymbol = symbol.asInstanceOf[LispSymbol].name
    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(value, finalCast = None, needReturn = true)
    mv.visitAStore(symbolLoc)

    mv.visitLdcInsn(nameOfSymbol)
    mv.visitALoad(symbolLoc)
    mv.visitStaticMethodCallStringOwner(
      runtimeEnvironment.className,
      "export",
      Void.TYPE,
      List(classOf[Object], classOf[Object])
    )
  }

  private def visitImport(operands: List[LispValue])(implicit runtimeMethodVisitor: LengineRuntimeEnvironment): Unit = {

    val importNameSymbol :: Nil = operands
    val mv = runtimeMethodVisitor.methodVisitor
    val symbolNameComb = importNameSymbol.asInstanceOf[LispSymbol]
    val separated = symbolNameComb.name.split("\\.").toList
    val clsName = separated.dropRight(1).mkString(".")
    val importName = separated.last

    mv.visitLdcInsn(clsName)
    mv.visitStaticMethodCall(
      classOf[Prelude],
      "loadClass",
      Void.TYPE,
      List(classOf[String])
    )

    mv.visitLdcInsn(importName)
    mv.visitMethodInsn(
      INVOKESTATIC,
      clsName,
      "importSymbol",
      Type.getMethodDescriptor(
        Type.getType(classOf[Object]),
        Type.getType(classOf[Object])
      ),
      false
    )

    val varLoc = runtimeMethodVisitor.allocateNextVar
    mv.visitAStore(varLoc)

    runtimeMethodVisitor.registerVariable(EagerSymbol(importName), varLoc)
  }
}
