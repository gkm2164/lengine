package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{AddableClass, BooleanPrimitive, BuildableClass, CollectionBuilderClass, ConsClass, LengineIterableClass, LengineIteratorClass, LengineListClass, NillableClass, ObjectClass, VoidPrimitive, WrapClass}
import co.gyeongmin.lisp.lexer.statements.LispForStmt
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Label

class LispLoopAsmWriter(forStmts: List[LispForStmt],
                        body: LispValue,
                        requestedType: Class[_],
                        tailRecReference: Option[(LispSymbol, Label)] = None)(implicit env: LengineRuntimeEnvironment) {
  def writeValue(): Unit =
    visitForStmt(forStmts, body)

  private def visitForStmt(forStmts: List[LispForStmt], body: LispValue, retAddr: Option[Int] = None): Unit =
    forStmts match {
      case Nil if retAddr.isEmpty =>
        throw CompileException("unable to determine returning sequence.", env.fileName, body.tokenLocation)
      case Nil =>
        val mv = env.methodVisitor
        mv.visitALoad(retAddr.get)                               // [S']
        mv.visitCheckCast(CollectionBuilderClass)                // [S']
        mv.visitLispValue(body, requestedType, tailRecReference) // [S' E']
        mv.visitInterfaceMethodCall(
          CollectionBuilderClass,
          "ADD",
          VoidPrimitive,
          ObjectClass
        )
      case LispForStmt(symbol, seq) :: tail =>
        val varIdx = env.allocateNextVar

        env.registerVariable(symbol, varIdx, ObjectClass)

        val startLoop = new Label()
        val endLoop   = new Label()

        val mv = env.methodVisitor
        mv.visitLispValue(seq, LengineIterableClass) // [S]

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
        //[I<S>]

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

        env.deregisterVariable(symbol)
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
