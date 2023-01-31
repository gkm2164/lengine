package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }
import lengine.Prelude
import lengine.functions.{ LengineLambda1, LengineLambda2 }
import lengine.runtime._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ Label, Type }

import java.io.PrintStream

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
    "=range",
    "assert",
    "fold",
    "seq",
    "head",
    "tail",
    "entry",
    "keys",
    "key"
  )

  private val compareOpMap = Map(
    "<"   -> "lt",
    "<="  -> "le",
    ">"   -> "gt",
    ">="  -> "ge",
    "="   -> "eq",
    "/="  -> "neq",
    "and" -> "and",
    "or"  -> "or"
  )

  private val PreludeClass: Class[Prelude]               = classOf[Prelude]
  private val VoidPrimitive: Class[Void]                 = java.lang.Void.TYPE
  private val BooleanClass: Class[java.lang.Boolean]     = classOf[java.lang.Boolean]
  private val BooleanPrimitive: Class[java.lang.Boolean] = java.lang.Boolean.TYPE
  private val ObjectClass: Class[Object]                 = classOf[Object]
  private val LongClass: Class[java.lang.Long]           = classOf[java.lang.Long]
  private val StringClass: Class[java.lang.String]       = classOf[java.lang.String]

  def supportOperation(operation: LispValue): Boolean = operation match {
    case EagerSymbol(op) => supportedOps.contains(op) || compareOpMap.contains(op)
    case _               => false
  }

  private def visitCreateEntry(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: value :: Nil = operands
    val mv                  = runtimeEnvironment.methodVisitor
    mv.visitLispValue(key, needReturn = true)
    mv.visitCheckCast(classOf[LengineMapKey])
    mv.visitLispValue(value, needReturn = true)
    mv.visitStaticMethodCall(
      classOf[LengineMapEntry],
      "create",
      classOf[LengineMapEntry],
      classOf[LengineMapKey],
      classOf[Object]
    )
  }

  private def visitMapKeys(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mapObj :: Nil = operands
    val mv            = runtimeEnvironment.methodVisitor
    mv.visitLispValue(mapObj)
    mv.visitCheckCast(classOf[LengineMap])
    mv.visitMethodCall(
      classOf[LengineMap],
      "keys",
      classOf[Sequence]
    )
  }

  private def visitMapKeyCreate(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: Nil = operands
    val mv         = runtimeEnvironment.methodVisitor
    mv.visitLispValue(key)
    mv.visitCheckCast(classOf[String])
    mv.visitStaticMethodCall(
      classOf[LengineMapKey],
      "create",
      classOf[LengineMapKey],
      classOf[String]
    )
  }

  def handle(body: List[LispValue],
             needReturn: Boolean)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operation :: operands = body
    operation match {
      case EagerSymbol(op) =>
        op match {
          case "+"                                                 => visitCalc("add", operands)
          case "-"                                                 => visitCalc("sub", operands)
          case "*"                                                 => visitCalc("mult", operands)
          case "/"                                                 => visitCalc("div", operands)
          case "if"                                                => visitIfStmt(operands)
          case "not"                                               => visitNotOps(operands)
          case "len"                                               => visitLenOp(operands)
          case "take" | "drop"                                     => visitSeqOpN(op, operands)
          case "filter" | "take-while" | "drop-while" | "split-at" => visitSeqOpFn(op, operands)
          case "head" | "tail"                                     => visitSeqUOps(op, operands)
          case "flatten"                                           => visitFlatten(operands)
          case "println"                                           => visitPrintln(operands, needReturn)
          case "read-line"                                         => visitReadLine
          case "str" | "int" | "double" | "char" | "seq"           => visitTypeCast(op, operands)
          case "assert"                                            => visitAssert(operands)
          case "range" | "=range"                                  => visitRange(op, operands)
          case "fold"                                              => visitFold(operands)
          case "export"                                            => visitExport(operands)
          case "import"                                            => visitImport(operands)
          case "entry"                                             => visitCreateEntry(operands)
          case "key"                                               => visitMapKeyCreate(operands)
          case "keys"                                              => visitMapKeys(operands)
          case _ if compareOpMap.contains(op)                      => visitCompareOps(op, operands)
        }

    }
  }

  private def visitSeqOpFn(operationName: String,
                           operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val fn :: seq :: _ = operands

    val mv = runtimeEnvironment.methodVisitor

    mv.visitLispValue(fn, needReturn = true)
    mv.visitCheckCast(classOf[LengineLambda1[java.lang.Boolean, _]])
    mv.visitLispValue(seq, needReturn = true)
    mv.visitCheckCast(classOf[CreateIterator])
    mv.visitStaticMethodCall(
      PreludeClass,
      operationName match {
        case "filter"     => "filter"
        case "take-while" => "takeWhile"
        case "drop-while" => "dropWhile"
        case "split-at"   => "splitAt"
      },
      classOf[Sequence],
      classOf[LengineLambda1[java.lang.Boolean, _]],
      classOf[CreateIterator]
    )
  }

  private def visitLenOp(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    val mv       = runtimeEnvironment.methodVisitor
    mv.visitLispValue(seq, needReturn = true)
    mv.visitStaticMethodCall(
      PreludeClass,
      "len",
      ObjectClass,
      ObjectClass
    )
  }

  private def visitSeqOpN(operationName: String,
                          operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val number :: seq :: _ = operands
    val mv                 = runtimeEnvironment.methodVisitor
    mv.visitLispValue(number, needReturn = true)
    mv.visitLispValue(seq, needReturn = true)
    mv.visitCheckCast(classOf[CreateIterator])
    mv.visitStaticMethodCall(
      PreludeClass,
      operationName,
      classOf[Sequence],
      classOf[java.lang.Long],
      classOf[CreateIterator]
    )
  }

  private def visitSeqUOps(op: String,
                           operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    val mv       = runtimeEnvironment.methodVisitor
    mv.visitLispValue(seq, needReturn = true)
    mv.visitCheckCast(classOf[CreateIterator])
    mv.visitStaticMethodCall(
      PreludeClass,
      op,
      classOf[Object],
      classOf[CreateIterator]
    )
  }

  private def visitFlatten(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    new LispValueAsmWriter(seq).visitForValue(needReturn = true)
    val mv = runtimeEnvironment.methodVisitor
    mv.visitCheckCast(classOf[Sequence])
    mv.visitStaticMethodCall(
      PreludeClass,
      "flatten",
      classOf[Object],
      classOf[Sequence]
    )
  }

  private def visitCalc(operation: String,
                        operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(None, needReturn = true))
    val mv = runtimeEnvironment.methodVisitor
    mv.visitStaticMethodCall(
      PreludeClass,
      operation,
      classOf[Object],
      classOf[Object],
      classOf[Object]
    )
  }

  private def visitPrintln(operands: List[LispValue],
                           needReturn: Boolean)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    mv.visitFieldInsn(GETSTATIC,
                      Type.getType(classOf[System]).getInternalName,
                      "out",
                      Type.getType(classOf[PrintStream]).getDescriptor)
    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(Some(LengineString), needReturn = true))
    mv.visitMethodCall(
      classOf[PrintStream],
      "println",
      Void.TYPE,
      ObjectClass
    )
    if (needReturn) {
      mv.visitUnit()
    }
  }

  private def visitTypeCast(op: String,
                            operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val operand :: _ = operands

    new LispValueAsmWriter(operand).visitForValue(needReturn = true)

    val mv = runtimeEnvironment.methodVisitor

    mv.visitStaticMethodCall(
      PreludeClass,
      s"cast_$op",
      ObjectClass,
      ObjectClass
    )
  }

  private def visitReadLine(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    mv.visitStaticMethodCall(PreludeClass, "readLine", classOf[String])
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
            classOf[Object],
            classOf[Object]
        )
      )
  }

  private def visitNotOps(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    operands.foreach(v => mv.visitLispValue(v, needReturn = true))
    mv.visitStaticMethodCall(
      PreludeClass,
      "not",
      BooleanClass,
      ObjectClass
    )
  }

  private def visitIfStmt(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val condition :: ifmatch :: elsematch :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(condition, needReturn = true)
    mv.visitCheckCast(BooleanClass)
    mv.visitMethodCall(
      BooleanClass,
      "booleanValue",
      BooleanPrimitive
    )

    val tLabel = new Label()
    val fLabel = new Label()
    val next   = new Label()

    mv.visitJumpInsn(IFNE, tLabel)
    mv.visitLabel(fLabel)
    mv.visitLispValue(elsematch, needReturn = true)
    mv.visitJumpInsn(GOTO, next)

    mv.visitLabel(tLabel)
    mv.visitLispValue(ifmatch, needReturn = true)
    mv.visitLabel(next)
  }

  private def visitRange(op: String,
                         operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val from :: to :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(from, needReturn = true)
    mv.visitCheckCast(LongClass)
    mv.visitLispValue(to, needReturn = true)
    mv.visitCheckCast(LongClass)
    mv.visitStaticMethodCall(
      classOf[RangeSequence],
      op match {
        case "range"  => "createRange"
        case "=range" => "createInclusiveRange"
      },
      classOf[RangeSequence],
      LongClass,
      LongClass
    )
  }

  private def visitAssert(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val message :: v :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(message, needReturn = true)
    mv.visitLispValue(v, needReturn = true)
    mv.visitStaticMethodCall(
      PreludeClass,
      "assertTrue",
      VoidPrimitive,
      ObjectClass,
      ObjectClass
    )
  }

  // (fold <seq> <init> <lambda>)
  private def visitFold(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val startLoop = new Label()
    val endLoop   = new Label()

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
      BooleanPrimitive
    )
    mv.visitJumpInsn(IFEQ, endLoop)

    mv.visitALoad(seqItLoc)
    mv.visitInterfaceMethodCall(
      classOf[LengineIterator],
      "next",
      ObjectClass
    )
    val elemLoc = runtimeEnvironment.allocateNextVar
    mv.visitAStore(elemLoc)

    visitLambdaFn2Invoke(fnLoc, accLoc, elemLoc)
    mv.visitAStore(accLoc)

    mv.visitJumpInsn(GOTO, startLoop)
    mv.visitLabel(endLoop)
    mv.visitALoad(accLoc)
  }

  private def visitLambdaFn2Invoke(lambdaLoc: Int, accLoc: Int, elemLoc: Int)(
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
      ObjectClass,
      ObjectClass,
      ObjectClass
    )
  }

  private def visitExport(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val symbol :: value :: Nil = operands
    val symbolLoc              = runtimeEnvironment.allocateNextVar

    val nameOfSymbol = symbol.asInstanceOf[LispSymbol].name
    val mv           = runtimeEnvironment.methodVisitor
    mv.visitLispValue(value, finalCast = None, needReturn = true)
    mv.visitAStore(symbolLoc)

    mv.visitLdcInsn(nameOfSymbol)
    mv.visitALoad(symbolLoc)
    mv.visitStaticMethodCallStringOwner(
      runtimeEnvironment.className,
      "export",
      VoidPrimitive,
      ObjectClass,
      ObjectClass
    )
  }

  private def visitImport(operands: List[LispValue])(implicit runtimeMethodVisitor: LengineRuntimeEnvironment): Unit = {

    val importNameSymbol :: Nil = operands
    val mv                      = runtimeMethodVisitor.methodVisitor
    val symbolNameComb          = importNameSymbol.asInstanceOf[LispSymbol]
    val separated               = symbolNameComb.name.split("\\.").toList
    val clsName                 = separated.dropRight(1).mkString(".")
    val importName              = separated.last

    mv.visitLdcInsn(clsName)
    mv.visitStaticMethodCall(
      PreludeClass,
      "loadClass",
      VoidPrimitive,
      StringClass,
    )

    mv.visitLdcInsn(importName)
    mv.visitStaticMethodCallStringOwner(
      clsName,
      "importSymbol",
      ObjectClass,
      ObjectClass
    )

    val varLoc = runtimeMethodVisitor.allocateNextVar
    mv.visitAStore(varLoc)

    runtimeMethodVisitor.registerVariable(EagerSymbol(importName), varLoc)
  }
}
