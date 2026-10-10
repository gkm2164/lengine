package co.gyeongmin.lisp.compile

/** Thrown by the test exit handler in place of System.exit. */
final case class ExitException(status: Int) extends RuntimeException(s"exit($status)")
