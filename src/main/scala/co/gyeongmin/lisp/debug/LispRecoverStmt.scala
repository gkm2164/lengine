package co.gyeongmin.lisp.debug

import co.gyeongmin.lisp.lexer.statements._
import co.gyeongmin.lisp.lexer.tokens.SpecialToken
import co.gyeongmin.lisp.lexer.values.functions.{
  BuiltinLispFunc,
  GeneralLispFunc,
  LispFunc,
  OverridableFunc
}
import co.gyeongmin.lisp.lexer.values.numbers.{
  ComplexNumber,
  FloatNumber,
  IntegerNumber,
  RatioNumber
}
import co.gyeongmin.lisp.lexer.values.seq.{LispList, LispString}
import co.gyeongmin.lisp.lexer.values.symbol.{
  EagerSymbol,
  LazySymbol,
  ListSymbol,
  ObjectReferSymbol
}
import co.gyeongmin.lisp.lexer.values.{
  LispChar,
  LispClause,
  LispObject,
  LispUnit,
  LispValue
}

object LispRecoverStmt {
  def recoverStmtWith(value: LispValue): String = value match {
    case LispUnit      => "()"
    case LispChar(chs) => s"#\\$chs"
    case LispObject(kv) =>
      kv.map { case (key, value) => s"${key.recoverStmt} ${value.recoverStmt}" }
        .mkString("{", " ", "}")
    case LispClause(body) => body.map(_.recoverStmt).mkString("(", " ", ")")
    case LispDoStmt(body) =>
      body.map(_.recoverStmt).mkString("(do ", " ", ")")
    case SpecialToken(body)  => s"#$body"
    case LispImportDef(path) => s"(import ${path.recoverStmt})"
    case LispFuncDef(symbol, fn) =>
      s"(fn ${symbol.recoverStmt} ${fn.recoverStmt})"
    case LispForStmt(symbol, seq) =>
      s"for ${symbol.recoverStmt} in ${seq.recoverStmt}"
    case LispNamespace(namespace) =>
      s"(ns ${namespace.recoverStmt})"
    case OverridableFunc(funcList) =>
      funcList.map(_.recoverStmt).mkString("\n")
    case LispValueDef(symbol, value) =>
      s"(def ${symbol.recoverStmt} ${value.recoverStmt})"
    case LispLoopStmt(forStmts, body) =>
      s"(loop ${forStmts.map(_.recoverStmt).mkString(" ")} ${body.recoverStmt})"
    case LispLetDef(name, value, body) =>
      s"(let ${name.recoverStmt} ${value.recoverStmt} ${body.recoverStmt})"
    case ObjectReferSymbol(name) => s":$name"
    case ListSymbol(name)        => s"$name"
    case LispString(value)       => s""""$value""""
    case EagerSymbol(name)       => s"$name"
    case LazySymbol(name)        => s"$name"
    case LispList(values) =>
      values.map(_.recoverStmt).mkString("(list ", " ", ")")
    case GeneralLispFunc(placeHolders, body) =>
      s"${placeHolders.map(_.recoverStmt).mkString("(", " ", ")")} ${body.recoverStmt}"
    case f: BuiltinLispFunc =>
      s"(lambda ${f.placeHolders.map(_.recoverStmt).mkString("(", " ", ")")} #native)"
    case f: LispFunc =>
      s"(lambda ${f.placeHolders.map(_.recoverStmt).mkString("(", " ", ")")} #native)"
    case IntegerNumber(value)     => value.toString
    case FloatNumber(value)       => value.toString
    case RatioNumber(over, under) => s"$over/$under"
    case ComplexNumber(real, imagine) =>
      s"#C(${real.recoverStmt} ${imagine.recoverStmt})"
  }

  implicit class LispValueExt(v: LispValue) {
    def recoverStmt: String = recoverStmtWith(v)
  }
}
