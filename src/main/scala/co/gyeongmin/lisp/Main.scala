package co.gyeongmin.lisp

import co.gyeongmin.lisp.builtin.Builtin
import co.gyeongmin.lisp.execution.{replLoop, runFile}

object Main {
  def main(args: Array[String]): Unit = {
    val env = Builtin.symbols

    if (args.nonEmpty) {
      runFile(args.head, env)
    } else {
      replLoop(env)
    }
  }
}
