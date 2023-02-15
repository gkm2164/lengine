package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{AddableClass, BooleanPrimitive, ConsClass, CreateIteratorClass, LengineIteratorClass, LengineListClass, NillableClass, ObjectClass, WrapClass}
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

  def declareSequence(): Unit = {
    val mv = env.methodVisitor

    mv.visitStaticMethodCall(
      LengineListClass,
      "nil",
      LengineListClass
    )
  }

  private def visitForStmt(forStmts: List[LispForStmt], body: LispValue, retAddr: Option[Int] = None): Unit =
    forStmts match {
      case Nil if retAddr.isEmpty =>
        throw CompileException("unable to determine returning sequence.", env.fileName, body.tokenLocation)
      case Nil =>
        val mv = env.methodVisitor
        mv.visitALoad(retAddr.get) // [S']
        mv.visitCheckCast(AddableClass) // [S']
        mv.visitLispValue(body, requestedType, tailRecReference) // [S' E']
        mv.visitInterfaceMethodCall(
          AddableClass,
          "ADD",
          CreateIteratorClass,
          ObjectClass
        ) // [S'']
      case LispForStmt(symbol, seq) :: tail =>
        val varIdx = env.allocateNextVar

        env.registerVariable(symbol, varIdx, ObjectClass)

        val startLoop = new Label()
        val endLoop   = new Label()

        val accLoc = env.allocateNextVar

        val mv = env.methodVisitor
        mv.visitLispValue(seq, CreateIteratorClass) // [S]
        mv.visitDup() // [S S]
        mv.visitCheckCast(NillableClass)
        mv.visitInterfaceMethodCall(
          NillableClass,
          "NIL",
          CreateIteratorClass
        )  // [S S(Nil)]
        mv.visitAStore(accLoc) // [S], [S(Nil)]

        mv.visitInterfaceMethodCall(
          CreateIteratorClass,
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
        // [I<S> S'']

        mv.visitAStore(accLoc)
        // [I<S>]

        env.deregisterVariable(symbol)
        mv.visitGoto(startLoop)
        mv.visitLabel(endLoop)
        mv.visitPop()
        // []

        mv.visitALoad(accLoc)
        mv.visitCheckCast(WrapClass)
        mv.visitInterfaceMethodCall(
          WrapClass,
          "WRAP",
          CreateIteratorClass
        )
    }
}
