package co.gyeongmin.lisp.monad

import co.gyeongmin.lisp.errors.parser.ParseError
import org.scalatest.{FlatSpec, Matchers}

class packageTest extends FlatSpec with Matchers {

  behavior of "packageTest"

  case object TestingError extends ParseError {
    override def message: String = "this is error"
  }

  it should "lispTokenStateMonad" in {
    val f: String => LispTokenState[Either[String, String]] = {
      case "xxxxx" => LispTokenState.apply(Right("xxxxx"))
      case v       => LispTokenState.apply(Left("x" + v))
    }

    val g: String => LispTokenState[Either[String, String]] = {
      case "xxxxx" => LispTokenState.error(TestingError)
      case v       => LispTokenState.apply(Left("x" + v))
    }

    lispTokenStateMonad.tailRecM("x")(f)(Stream()) should be(
      Right(("xxxxx", Stream()))
    )

    lispTokenStateMonad.tailRecM("x")(g)(Stream()) should be(
      Left(TestingError)
    )
  }
}
