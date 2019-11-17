package co.gyeongmin.lisp

package object asm {
  case class ProgramWriter(queue: Vector[MachineCode])

  sealed trait PrintableCode {
    def print(): String
  }

  sealed trait MachineCode extends PrintableCode {

  }

  sealed trait Instance extends PrintableCode {

  }

  case class Memory(addr: Long) extends Instance {
    override def print(): String = ???
  }

  case class Register(name: String) extends Instance {
    override def print(): String = ???
  }

  case class Add(x: Instance, y: Instance) extends MachineCode {
    override def print(): String = s"ADD ${x.print()} ${y.print()}"
  }

  case object InitCode extends MachineCode {
    override def print(): String = StringBuilder.newBuilder
      .append(".global main").append("\n")
      .append(".text").append("\n")
      .append("_main:").append("\n")
      .append("  CALL main").append("\n")
      .toString()
  }
}
