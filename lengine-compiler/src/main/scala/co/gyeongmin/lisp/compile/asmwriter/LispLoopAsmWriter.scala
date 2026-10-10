package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType._
import co.gyeongmin.lisp.lexer.ast.LispForStmt
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Label

class LispLoopAsmWriter(
  forStmts: List[LispForStmt],
  body: LispValue,
  requestedType: Class[_],
  tailRecReference: Option[(LispSymbol, Label)] = None
)(implicit env: LengineRuntimeEnvironment) {
  def writeValue(): Unit =
    visitForStmt(forStmts, body)

  private def visitForStmt(
    forStmts: List[LispForStmt],
    body: LispValue,
    retAddr: Option[Int] = None
  ): Unit =
    forStmts match {
      case Nil if retAddr.isEmpty =>
        throw CompileException(
          "unable to determine returning sequence.",
          env.fileName,
          body.tokenLocation
        )
      case Nil =>
        val mv = env.methodVisitor
        mv.visitALoad(retAddr.get) // [S']
        mv.visitCheckCast(CollectionBuilderClass) // [S']
        mv.visitLispValue(body, requestedType) // [S' E'], never a tail position
        mv.visitInterfaceMethodCall(
          CollectionBuilderClass,
          "ADD",
          VoidPrimitive,
          ObjectClass
        )
      case LispForStmt(symbol, seq) :: tail =>
        val varIdx = env.allocateNextVar

        val startLoop = new Label()
        val endLoop = new Label()

        val mv = env.methodVisitor
        // The source is evaluated before the loop variable exists, so it sees any outer binding.
        mv.visitLispValue(seq, LengineIterableClass) // [S]
        val outer = env.saveBindings(Seq(symbol))
        env.registerVariable(symbol, varIdx, ObjectClass)

        mv.visitDup() // [S S]
        mv.visitCheckCast(BuildableClass)
        mv.visitInterfaceMethodCall(
          BuildableClass,
          "BUILDER",
          CollectionBuilderClass
        ) // [S S(Nil)]
        val accLoc = env.allocateNextVar
        mv.visitAStore(accLoc) // [S], [S(Nil)]

        mv.visitInterfaceMethodCall(
          LengineIterableClass,
          "iterator",
          LengineIteratorClass
        ) // [I<S>]
        mv.visitLabel(startLoop)
        mv.visitDup() // [I<S>, I<S>]
        mv.visitInterfaceMethodCall(
          LengineIteratorClass,
          "hasNext",
          BooleanPrimitive
        ) // [I<S>, Z]
        mv.visitIfEq(endLoop)
        // [I<S>]

        mv.visitDup()
        // [I<S> I<S>]
        mv.visitInterfaceMethodCall(
          LengineIteratorClass,
          "next",
          ObjectClass
        )
        // [I<S> E]
        mv.visitAStore(varIdx)
        // [I<S>]

        visitForStmt(tail, body, Some(accLoc))
        // [I<S>]

        env.restoreBindings(outer)
        mv.visitGoto(startLoop)
        mv.visitLabel(endLoop)
        mv.visitPop()
        // []

        if (retAddr.isDefined) {
          mv.visitALoad(retAddr.get)
        }
        mv.visitALoad(accLoc)
        mv.visitCheckCast(CollectionBuilderClass)
        mv.visitInterfaceMethodCall(
          CollectionBuilderClass,
          "BUILD",
          LengineIterableClass
        )
        if (retAddr.isDefined) {
          mv.visitInterfaceMethodCall(
            CollectionBuilderClass,
            "ADD",
            VoidPrimitive,
            ObjectClass
          )
        }
    }
}
