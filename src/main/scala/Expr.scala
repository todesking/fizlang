package com.todesking.fizlang
sealed abstract class Expr
object Expr {
  case class Lit(value: Any) extends Expr
  case class IntPlus(l: Expr, r: Expr) extends Expr
  case class IntMinus(l: Expr, r: Expr) extends Expr
  case class IntMul(l: Expr, r: Expr) extends Expr
  case class IntDiv(l: Expr, r: Expr) extends Expr
  case class IntMod(l: Expr, r: Expr) extends Expr
  case class Eq(l: Expr, r: Expr) extends Expr
  case class Gt(l: Expr, r: Expr) extends Expr
  case class Ge(l: Expr, r: Expr) extends Expr
  case class Le(l: Expr, r: Expr) extends Expr
  case class Lt(l: Expr, r: Expr) extends Expr
  case class Not(expr: Expr) extends Expr
  case class If(cond: Expr, th: Expr, el: Expr) extends Expr
  case class Fun(param: String, body: Expr) extends Expr
  case class Ref(param: String) extends Expr
  case class App(fun: Expr, arg: Expr) extends Expr
  case class Block(body: Seq[Expr]) extends Expr
  case class LetRec(defs: Seq[(String, Fun)], body: Expr) extends Expr
  case class Patmat(expr: Expr, clauses: Seq[(Pat, Expr)]) extends Expr

  sealed abstract class Pat
  case class PLit(value: Any) extends Pat
  case class PAny(name: String) extends Pat
}
