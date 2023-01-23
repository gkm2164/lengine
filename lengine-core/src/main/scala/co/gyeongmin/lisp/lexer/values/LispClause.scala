package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.errors.eval.EvalError
import co.gyeongmin.lisp.lexer.values.symbol.EagerSymbol
import co.gyeongmin.lisp.types.LengineType

case class LispClause(body: List[LispValue]) extends LispValue {
  private def resolveTypeWithOperands(operator: LispValue, operands: Seq[LengineType]): Either[EvalError, LengineType] =
    operator match {
      case EagerSymbol("+") =>
        operands.foldLeft[Either[EvalError, LengineType]](Right(LengineType.Zero))(
          (acc, cur) =>
            acc match {
              case Left(e)            => Left(e)
              case Right(lengineType) => lengineType + cur
          }
        )
      case EagerSymbol("-") =>
        operands.foldLeft[Either[EvalError, LengineType]](Right(LengineType.Zero))(
          (acc, cur) =>
            acc match {
              case Left(e)            => Left(e)
              case Right(lengineType) => lengineType - cur
          }
        )
      case EagerSymbol("*") =>
        operands.foldLeft[Either[EvalError, LengineType]](Right(LengineType.Zero))(
          (acc, cur) =>
            acc match {
              case Left(e)            => Left(e)
              case Right(lengineType) => lengineType * cur
          }
        )
      case EagerSymbol("/") =>
        operands.foldLeft[Either[EvalError, LengineType]](Right(LengineType.Zero))(
          (acc, cur) =>
            acc match {
              case Left(e)            => Left(e)
              case Right(lengineType) => lengineType / cur
          }
        )
    }
  override def resolveType(implicit resolveHelper: ResolveHelper): Either[EvalError, LengineType] = body match {
    case operation :: operands =>
      for {
        operandsTypes <- traverse(operands.map(_.resolveType))
        resolvedType  <- resolveTypeWithOperands(operation, operandsTypes)
      } yield resolvedType
  }
}
