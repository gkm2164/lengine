package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.InteroperabilityHelper.ReservedKeywordFunctions
import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.lexer.tokens.{LispFn, LispVar}
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.functions.GeneralLispFunc
import co.gyeongmin.lisp.lexer.values.symbol.{EagerSymbol, LispSymbol}
import org.objectweb.asm.Label

object RuntimeMethodVisitor {
  private val supportedOps = Set(
    "export",
    "import",
    "native",
    "if",
    "exit",
    "lazy",
    "force",
  )

  def supportOperation(operation: LispValue): Boolean = operation match {
    case EagerSymbol(op) => supportedOps.contains(op)
    case _               => false
  }

  def handle(body: List[LispValue], requestedType: Class[_], tailRecReference: Option[(LispSymbol, Label)])(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val operation :: operands = body
    operation match {
      case EagerSymbol(op) =>
        op match {
          case "export" => visitExport(operands)
          case "import" => visitImport(operands)
          case "native" => visitNative(operands)
          case "if"     => visitIfStmt(operands, requestedType, tailRecReference)
          case "exit"   => visitQuit(operands)
          case "lazy"   => visitLazy(operands)
          case "force"  => visitForce(operands)
          case _        => new RuntimeException("Unsupported operation: " + op)
        }

    }
  }

  private def visitIfStmt(operands: List[LispValue],
                          requestedType: Class[_],
                          tailRecReference: Option[(LispSymbol, Label)])(
      implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val condition :: ifmatch :: elsematch :: Nil = operands

    val mv = runtimeEnvironment.methodVisitor
    mv.visitLispValue(condition, BooleanClass)
    mv.visitMethodCall(
      BooleanClass,
      "booleanValue",
      BooleanPrimitive
    )

    val tLabel = new Label()
    val fLabel = new Label()
    val next   = new Label()

    mv.visitIfNe(tLabel)
    mv.visitLabel(fLabel)
    mv.visitLispValue(elsematch, requestedType, tailRecReference = tailRecReference)
    mv.visitGoto(next)

    mv.visitLabel(tLabel)
    mv.visitLispValue(ifmatch, requestedType, tailRecReference = tailRecReference)
    mv.visitLabel(next)

    mv.stackSizeTrace.decrementAndGet()
  }

  private def visitExport(operands: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val symbol :: value = operands
    val nameOfSymbol    = symbol.asInstanceOf[LispSymbol].name
    val mv              = runtimeEnvironment.methodVisitor

    if (ReservedKeywordFunctions.contains(symbol.asInstanceOf[LispSymbol])) {
      throw CompileException(s"You can't override function $nameOfSymbol",
                             runtimeEnvironment.fileName,
                             symbol.tokenLocation)
    }

    if (value == Nil) {
      mv.visitString(nameOfSymbol)
      mv.visitLispValue(symbol, ObjectClass)
      mv.visitStaticMethodCall(
        runtimeEnvironment.className,
        "export",
        VoidPrimitive,
        LengineStringClass,
        ObjectClass
      )
    } else {
      mv.visitLispValue(value.head, ObjectClass) // [V]
      mv.visitDup()                              // [V V]
      mv.visitString(nameOfSymbol)              // [V V S]
      mv.visitSwap()                             // [V S V]
      mv.visitStaticMethodCall( // [V]
                               runtimeEnvironment.className,
                               "export",
                               VoidPrimitive,
                               LengineStringClass,
                               ObjectClass)
      val loc = runtimeEnvironment.allocateNextVar
      mv.visitAStore(loc) // []
      runtimeEnvironment.registerVariable(symbol.asInstanceOf[LispSymbol], loc, ObjectClass)
      runtimeEnvironment.writeLaterAllScope(symbol.asInstanceOf[LispSymbol], ObjectClass, loc)
    }
  }

  private def visitImport(operands: List[LispValue])(implicit runtimeMethodVisitor: LengineRuntimeEnvironment): Unit = {

    val importNameSymbol :: Nil = operands
    val mv                      = runtimeMethodVisitor.methodVisitor
    val symbolNameComb          = importNameSymbol.asInstanceOf[LispSymbol]

    mv.visitString(symbolNameComb.name)
    mv.visitStaticMethodCall(
      LengineClassLoaderClass,
      "importSymbol",
      ObjectClass,
      LengineStringClass
    )
    mv.visitDup()
    val varLoc = runtimeMethodVisitor.allocateNextVar
    mv.visitAStore(varLoc)

    val symbolToBeRegistered = EagerSymbol(symbolNameComb.name.split('.').last)

    runtimeMethodVisitor.registerVariable(symbolToBeRegistered, varLoc, ObjectClass)
    runtimeMethodVisitor.writeLaterAllScope(symbolToBeRegistered, ObjectClass, varLoc)
  }

  private def visitLazy(values: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val body :: Nil = values
    val mv          = runtimeEnvironment.methodVisitor

    mv.visitLispValue(GeneralLispFunc(Nil, body), typeToBe = LengineLambdaClass.head)
    mv.visitStaticMethodCall(
      LengineLazyValueClass,
      "create",
      LengineLazyValueClass,
      LengineLambdaClass.head
    )
  }

  private def visitForce(values: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val symbol :: Nil = values
    val mv            = runtimeEnvironment.methodVisitor
    mv.visitLispValue(symbol, typeToBe = LengineLazyValueClass)
    mv.visitMethodCall(
      LengineLazyValueClass,
      "force",
      ObjectClass,
    )
  }


  private def visitQuit(body: List[LispValue])(implicit runtimeEnvironment: LengineRuntimeEnvironment): Unit = {
    val mv = runtimeEnvironment.methodVisitor
    val operand :: Nil = body

    mv.visitLispValue(operand, LongClass)
    mv.visitDup()
    mv.visitMethodCall(
      LongClass,
      "intValue",
      Integer.TYPE,
    )
    mv.visitStaticMethodCall(
      classOf[System],
      "exit",
      VoidPrimitive,
      Integer.TYPE
    )
  }

  private def visitNative(operands: List[LispValue])(
    implicit runtimeEnvironment: LengineRuntimeEnvironment
  ): Unit = {
    val symbol :: objectType :: Nil = operands
    val symbolName = symbol.asInstanceOf[LispSymbol].name

    val expectingType = objectType match {
      case LispFn() => LengineLambdaCommonClass
      case LispVar() => ObjectClass
    }

    val mv = runtimeEnvironment.methodVisitor

    mv.visitGetStatic(
      PreludeClass,
      symbolName,
      expectingType
    )
  }
}
