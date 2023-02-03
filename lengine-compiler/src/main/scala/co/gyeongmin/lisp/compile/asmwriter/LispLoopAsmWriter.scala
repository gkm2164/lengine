package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.asmwriter.LengineType.{BooleanPrimitive, ConsClass, CreateIteratorClass, LengineListClass, ObjectClass}
import co.gyeongmin.lisp.lexer.statements.LispForStmt
import co.gyeongmin.lisp.lexer.values.LispValue
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import lengine.runtime.{CreateIterator, LengineIterator}
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
        mv.visitLispValue(body, requestedType, tailRecReference) // [E']
        mv.visitALoad(retAddr.get)                               // [E' S']
        mv.visitStaticMethodCall(
          LengineListClass,
          "cons",
          ConsClass,
          ObjectClass,
          LengineListClass
        ) // [S'']
      case LispForStmt(symbol, seq) :: tail =>
        val varIdx = env.allocateNextVar

        env.registerVariable(symbol, varIdx, ObjectClass)

        val startLoop = new Label()
        val endLoop   = new Label()

        val mv = env.methodVisitor
        val newSeqLoc = retAddr match {
          case Some(value) => value
          case None =>
            val nextVar = env.allocateNextVar
            declareSequence()
            // [S']
            mv.visitAStore(nextVar)
            nextVar
        } // []

        mv.visitLispValue(seq, CreateIteratorClass) // [S]
        mv.visitInterfaceMethodCall(
          classOf[CreateIterator],
          "iterator",
          classOf[LengineIterator]
        ) // [I<S>]
        mv.visitLabel(startLoop)
        mv.visitDup() // [I<S>, I<S>]
        mv.visitInterfaceMethodCall(
          classOf[LengineIterator],
          "hasNext",
          BooleanPrimitive
        ) // [I<S>, Z]
        mv.visitIfEq(endLoop)
        //[I<S>]

        mv.visitDup()
        // [I<S> I<S>]
        mv.visitInterfaceMethodCall(
          classOf[LengineIterator],
          "next",
          classOf[Object]
        )
        // [I<S> E]
        mv.visitAStore(varIdx)
        // [I<S>]

        visitForStmt(tail, body, Some(newSeqLoc))
        // [I<S> S'']

        mv.visitAStore(newSeqLoc)
        // [I<S>]

        env.deregisterVariable(symbol)
        mv.visitGoto(startLoop)
        mv.visitLabel(endLoop)
        mv.visitPop()
        // []

        mv.visitALoad(newSeqLoc)
    }
}
