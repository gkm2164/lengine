package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.AsmHelper.MethodVisitorExtension
import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.{ EagerSymbol, LispSymbol }
import lengine.runtime._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ Label, Type }

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
    "read-eof",
    "read-file",
    "read-file-seq",
    "and",
    "or",
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
    "get",
    "key",
    "bool?",
    "char?",
    "int?",
    "double?",
    "string?",
    "seq?",
    "object?",
    "now"
  )

  private val compareOpMap = Map(
    "<"  -> "lt",
    "<=" -> "le",
    ">"  -> "gt",
    ">=" -> "ge",
    "="  -> "eq",
    "/=" -> "neq",
  )

  def supportOperation(operation: LispValue): Boolean = operation match {
    case EagerSymbol(op) => supportedOps.contains(op) || compareOpMap.contains(op)
    case _               => false
  }

  private def visitTypeCheck(op: String, operands: List[LispValue])(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val t = op.dropRight(1)
    val cls = t match {
      case "bool"   => BooleanClass
      case "char"   => CharacterClass
      case "int"    => LongClass
      case "double" => DoubleClass
      case "string" => StringClass
      case "seq"    => SequenceClass
      case "object" => LengineMapClass
    }

    val value :: Nil = operands
    val mv           = runtimeEnvironment.methodVisitor

    mv.visitLispValue(value)
    mv.visitInstanceOf(cls)
    mv.visitBoxing(BooleanClass, BooleanPrimitive)
  }

  private def visitNow()(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    mv.visitStaticMethodCall(
      SystemClass,
      "currentTimeMillis",
      LongPrimitive
    )
    mv.visitBoxing(LongClass, LongPrimitive)
  }

  def handle(body: List[LispValue], needReturn: Boolean, tailRecReference: Option[(LispSymbol, Label)])(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val operation :: operands = body
    operation match {
      case EagerSymbol(op) =>
        op match {
          case "+"                                                                     => visitCalc("add", operands)
          case "-"                                                                     => visitCalc("sub", operands)
          case "*"                                                                     => visitCalc("mult", operands)
          case "/"                                                                     => visitCalc("div", operands)
          case "if"                                                                    => visitIfStmt(operands, tailRecReference)
          case "and" | "or"                                                            => visit2BoolOps(op, operands)
          case "not"                                                                   => visitNotOps(operands)
          case "len"                                                                   => visitLenOp(operands)
          case "take" | "drop"                                                         => visitSeqOpN(op, operands)
          case "filter" | "take-while" | "drop-while" | "split-at"                     => visitSeqOpFn(op, operands)
          case "head" | "tail"                                                         => visitSeqUOps(op, operands)
          case "flatten"                                                               => visitFlatten(operands)
          case "println"                                                               => visitPrintln(operands, needReturn)
          case "read-line"                                                             => visitReadLine
          case "read-eof"                                                              => visitReadEof
          case "read-file"                                                             => visitReadFile(operands)
          case "read-file-seq"                                                         => visitReadFileSeq(operands)
          case "str" | "int" | "double" | "char" | "seq"                               => visitTypeCast(op, operands)
          case "assert"                                                                => visitAssert(operands)
          case "range" | "=range"                                                      => visitRange(op, operands)
          case "fold"                                                                  => visitFold(operands)
          case "export"                                                                => visitExport(operands)
          case "import"                                                                => visitImport(operands)
          case "entry"                                                                 => visitCreateEntry(operands)
          case "key"                                                                   => visitMapKeyCreate(operands)
          case "get"                                                                   => visitGetKeyName(operands)
          case "keys"                                                                  => visitMapKeys(operands)
          case "bool?" | "char?" | "int?" | "double?" | "string?" | "object?" | "seq?" => visitTypeCheck(op, operands)
          case "now"                                                                   => visitNow()
          case _ if compareOpMap.contains(op)                                          => visitCompareOps(op, operands)
        }

    }
  }

  private def visitCreateEntry(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: value :: Nil = operands
    val mv                  = runtimeEnvironment.methodVisitor
    mv.visitLispValue(key, needReturn = true)
    mv.visitCheckCast(LengineMapKeyClass)
    mv.visitLispValue(value, needReturn = true)
    mv.visitStaticMethodCall(
      LengineMapEntryClass,
      "create",
      LengineMapEntryClass,
      LengineMapKeyClass,
      ObjectClass
    )
  }

  private def visitMapKeys(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mapObj :: Nil = operands
    val mv            = runtimeEnvironment.methodVisitor
    mv.visitLispValue(mapObj)
    mv.visitCheckCast(LengineMapClass)
    mv.visitMethodCall(
      LengineMapClass,
      "keys",
      SequenceClass
    )
  }

  private def visitMapKeyCreate(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: Nil = operands
    val mv         = runtimeEnvironment.methodVisitor
    mv.visitLispValue(key)
    mv.visitCheckCast(StringClass)
    mv.visitStaticMethodCall(
      LengineMapKeyClass,
      "create",
      LengineMapKeyClass,
      StringClass
    )
  }

  private def visitGetKeyName(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val key :: Nil = operands
    val mv         = runtimeEnvironment.methodVisitor
    mv.visitLispValue(key)
    mv.visitCheckCast(LengineMapKeyClass)
    mv.visitMethodCall(
      LengineMapKeyClass,
      "getKey",
      StringClass
    )
  }

  private def visitSeqOpFn(operationName: String,
                           operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val fn :: seq :: _ = operands

    val mv = runtimeEnvironment.methodVisitor

    mv.visitLispValue(fn, needReturn = true)
    mv.visitCheckCast(LengineLambdaClass(1))
    mv.visitLispValue(seq, needReturn = true)
    mv.visitCheckCast(CreateIteratorClass)
    mv.visitStaticMethodCall(
      PreludeClass,
      operationName match {
        case "filter"     => "filter"
        case "take-while" => "takeWhile"
        case "drop-while" => "dropWhile"
        case "split-at"   => "splitAt"
      },
      SequenceClass,
      LengineLambdaClass(1),
      CreateIteratorClass
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
    mv.visitCheckCast(CreateIteratorClass)
    mv.visitStaticMethodCall(
      PreludeClass,
      operationName,
      SequenceClass,
      LongClass,
      CreateIteratorClass
    )
  }

  private def visitSeqUOps(op: String,
                           operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    val mv       = runtimeEnvironment.methodVisitor
    mv.visitLispValue(seq, needReturn = true)
    mv.visitCheckCast(CreateIteratorClass)
    mv.visitStaticMethodCall(
      PreludeClass,
      op,
      ObjectClass,
      CreateIteratorClass
    )
  }

  private def visitFlatten(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val seq :: _ = operands
    new LispValueAsmWriter(seq).visitForValue(needReturn = true)
    val mv = runtimeEnvironment.methodVisitor
    mv.visitCheckCast(SequenceClass)
    mv.visitStaticMethodCall(
      PreludeClass,
      "flatten",
      ObjectClass,
      SequenceClass
    )
  }

  private def visitCalc(operation: String,
                        operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(None, needReturn = true))
    val mv = runtimeEnvironment.methodVisitor
    mv.visitStaticMethodCall(
      PreludeClass,
      operation,
      ObjectClass,
      ObjectClass,
      ObjectClass
    )
  }

  private def visitPrintln(operands: List[LispValue],
                           needReturn: Boolean)(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    mv.visitFieldInsn(GETSTATIC,
                      Type.getType(SystemClass).getInternalName,
                      "out",
                      Type.getType(PrintStreamClass).getDescriptor)
    operands.foreach(v => new LispValueAsmWriter(v).visitForValue(needReturn = true))
    mv.visitMethodCall(
      PrintStreamClass,
      "println",
      VoidPrimitive,
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

    mv.visitStaticMethodCall(PreludeClass, "readLine", StringClass)
  }

  private def visitReadEof(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor

    mv.visitStaticMethodCall(PreludeClass, "readEof", StringClass)
  }

  private def visitReadFile(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val filename :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor

    mv.visitLispValue(filename, needReturn = true)
    mv.visitCheckCast(StringClass)
    mv.visitStaticMethodCall(PreludeClass, "readFile", StringClass, StringClass)
  }

  private def visitReadFileSeq(
      operands: List[LispValue]
  )(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val filename :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor

    mv.visitLispValue(filename, needReturn = true)
    mv.visitCheckCast(StringClass)
    mv.visitStaticMethodCall(PreludeClass, "readFileSeq", FileSequenceClass, StringClass)
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
            PreludeClass,
            name,
            BooleanClass,
            ObjectClass,
            ObjectClass
        )
      )
  }

  private def visit2BoolOps(op: String,
                            operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val a :: b :: Nil = operands
    val mv            = runtimeEnvironment.methodVisitor
    mv.visitLispValue(a, needReturn = true)
    mv.visitCheckCast(BooleanClass)
    mv.visitLispValue(b, needReturn = true)
    mv.visitCheckCast(BooleanClass)
    mv.visitStaticMethodCall(
      PreludeClass,
      op,
      BooleanClass,
      BooleanClass,
      BooleanClass
    )
  }

  private def visitNotOps(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    operands.foreach(v => mv.visitLispValue(v, needReturn = true))
    mv.visitStaticMethodCall(
      PreludeClass,
      "not",
      BooleanClass,
      BooleanClass
    )
  }

  private def visitIfStmt(operands: List[LispValue], tailRecReference: Option[(LispSymbol, Label)])(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
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
    mv.visitLispValue(elsematch, needReturn = true, tailRecReference = tailRecReference)
    mv.visitJumpInsn(GOTO, next)

    mv.visitLabel(tLabel)
    mv.visitLispValue(ifmatch, needReturn = true, tailRecReference = tailRecReference)
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
      RangeSequenceClass,
      op match {
        case "range"  => "createRange"
        case "=range" => "createInclusiveRange"
      },
      RangeSequenceClass,
      LongClass,
      LongClass
    )
  }

  private def visitAssert(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val message :: v :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(message, needReturn = true)
    mv.visitLispValue(v, needReturn = true)
    mv.visitCheckCast(BooleanClass)
    mv.visitStaticMethodCall(
      PreludeClass,
      "assertTrue",
      VoidPrimitive,
      ObjectClass,
      BooleanClass
    )
  }

  // (fold <seq> <init> <lambda>)
  private def visitFold(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val startLoop = new Label()
    val endLoop   = new Label()

    val seq :: init :: lambda :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(lambda, needReturn = true)
    mv.visitCheckCast(LengineLambdaClass(2))
    val fnLoc = runtimeEnvironment.allocateNextVar
    mv.visitAStore(fnLoc)

    val accLoc = runtimeEnvironment.allocateNextVar

    mv.visitLispValue(init, needReturn = true)
    mv.visitAStore(accLoc)
    mv.visitLispValue(seq, needReturn = true)
    mv.visitCheckCast(CreateIteratorClass)
    mv.visitInterfaceMethodCall(
      CreateIteratorClass,
      "iterator",
      LengineIteratorClass
    )

    mv.visitLabel(startLoop)
    mv.visitInsn(DUP)
    mv.visitInterfaceMethodCall(
      LengineIteratorClass,
      "hasNext",
      BooleanPrimitive
    )
    mv.visitJumpInsn(IFEQ, endLoop)

    mv.visitInsn(DUP)
    mv.visitInterfaceMethodCall(
      classOf[LengineIterator],
      "next",
      ObjectClass
    )

    mv.visitIntInsn(ALOAD, fnLoc)
    mv.visitInsn(SWAP)
    mv.visitIntInsn(ALOAD, accLoc)
    mv.visitInsn(SWAP)
    visitLambdaFn2Invoke()
    mv.visitAStore(accLoc)

    mv.visitJumpInsn(GOTO, startLoop)
    mv.visitLabel(endLoop)
    mv.visitInsn(POP)
    mv.visitALoad(accLoc)
  }

  private def visitLambdaFn2Invoke()(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    mv.visitInterfaceMethodCall(
      LengineLambdaClass(2),
      "invoke",
      ObjectClass,
      ObjectClass,
      ObjectClass
    )
  }

  private def visitExport(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val symbol :: value :: Nil = operands
    val nameOfSymbol           = symbol.asInstanceOf[LispSymbol].name
    val mv                     = runtimeEnvironment.methodVisitor
    mv.visitLdcInsn(nameOfSymbol)
    mv.visitLispValue(value, needReturn = true)
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
