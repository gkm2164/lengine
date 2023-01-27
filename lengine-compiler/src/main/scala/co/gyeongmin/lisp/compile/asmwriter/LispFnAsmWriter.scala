package co.gyeongmin.lisp.compile.asmwriter

import co.gyeongmin.lisp.compile.LengineEnv
import co.gyeongmin.lisp.lexer.statements.LispFuncDef
import co.gyeongmin.lisp.lexer.values.LispUnit.traverse
import co.gyeongmin.lisp.lexer.values.symbol.LispSymbol
import org.objectweb.asm.Label

class LispFnAsmWriter(f: LispFuncDef)(implicit runtimeEnvironment: LengineRuntimeEnvironment) {
  def writeValue(): Unit = {
    val fnLabel = new Label
    LengineEnv.defineFn(f.symbol.name, fnLabel, f.fn.placeHolders.size, (mv, args) => {
      mv.visitCode()
      mv.visitLabel(fnLabel)

      val argmap = traverse(f.fn.placeHolders
        .map(holder => holder.as[LispSymbol]))
        .map(_.map(_.name).zip(args)) match {
        case Left(err) => throw new RuntimeException(s"unexpected error: $err")
        case Right(value) => value.toMap
      }

      val newRuntimeEnvironment: LengineRuntimeEnvironment = new LengineRuntimeEnvironment(
        argmap,
        runtimeEnvironment.className,
        f.fn.placeHolders.size
      )
      new LispValueAsmWriter(mv, f.fn.body)(newRuntimeEnvironment).writeValue(None)
      newRuntimeEnvironment.getLastVarIdx
    })
  }
}
