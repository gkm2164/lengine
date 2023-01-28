package co.gyeongmin.lisp.lexer.values

import co.gyeongmin.lisp.errors.eval.{EvalError, KeyIsNotReferSymbolError, ObjectKeyNotExistError}
import co.gyeongmin.lisp.lexer.values.symbol.ObjectReferSymbol
import co.gyeongmin.lisp.types.{LengineObject, LengineType}

case class LispObject(kv: Map[ObjectReferSymbol, LispValue]) extends LispValue {
  override def resolveType(implicit rh: ResolveHelper): Either[EvalError, LengineType] = Right(LengineObject)
  def refer(args: List[LispValue]): Either[EvalError, LispValue] = {
    if (args.length != 1) Left(KeyIsNotReferSymbolError)
    else
      args.head match {
        case ors: ObjectReferSymbol =>
          kv.get(ors).toRight(ObjectKeyNotExistError(ors.name))
        case _ => Left(KeyIsNotReferSymbolError)
      }
  }
}
