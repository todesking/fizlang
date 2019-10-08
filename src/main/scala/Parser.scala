package com.todesking.fizlang

import scala.util.parsing.combinator.RegexParsers
object Parser extends RegexParsers {
  def parseExpr(src: String): Either[String, Expr] =
    translate(parseAll(expr, src))
  def parseToplevel(src: String): Either[String, Seq[(String, Boolean, Expr)]] =
    translate(parseAll(toplevel, src))

  private[this] def translate[A](a: ParseResult[A]): Either[String, A] =
    a match {
      case Success(e, _)    => Right(e)
      case NoSuccess(e, in) => Left(e + "\n" + in.pos.longString)
    }

  val keywords = Set(
    "fun",
    "if",
    "then",
    "else",
    "let",
    "in",
    "rec"
  )

  val E = Expr

  def toplevel: Parser[Seq[(String, Boolean, Expr)]] = term.+

  def term = top_let

  def top_let = ("let" ~> "rec".?) ~ name ~ name.* ~ ("=" ~> expr) <~ ";" ^? {
    case Some(_) ~ name ~ (params @ _ :: _) ~ body =>
      (
        name,
        true,
        params.foldRight(body) { case (p, e) => E.Fun(p, e) }
      )
    case None ~ name ~ params ~ body =>
      (
        name,
        false,
        params.foldRight(body) { case (p, e) => E.Fun(p, e) }
      )
  }

  private[this] def opSyntax(
      sep: Parser[String]
  )(
      handle: PartialFunction[(Expr, String, Expr), Expr]
  ): Parser[Expr] => Parser[Expr] = { x: Parser[Expr] =>
    x ~ (sep ~ x).* ^^ {
      case x ~ xs =>
        new ~(x, xs.map { case b ~ a => (b, a) })
    } ^^ {
      case x ~ xs =>
        xs.foldLeft(x) {
          case (l, (op, r)) =>
            handle((l, op, r))
        }
    }
  }

  def expr: Parser[Expr] = fun | ifelse | guard | let | let_rec | expr1
  def name = "[a-z][a-z_]*".r ^? { case n if !keywords.contains(n) => n }
  def fun = ("fun" ~> name) ~ ("=>" ~> expr) ^^ {
    case param ~ body => E.Fun(param, body)
  }
  def ifelse = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
    case cond ~ th ~ el => E.If(cond, th, el)
  }
  def guard =
    ("if" ~ "|") ~> rep1sep((expr <~ "then") ~ expr, "|") ~ (("|" ~ "else") ~> expr) ^^ {
      case ifs ~ el =>
        ifs.foldRight(el) {
          case (cond ~ body, rest) =>
            E.If(cond, body, rest)
        }
    }
  def let = ("let" ~> name) ~ name.* ~ ("=" ~> expr) ~ ("in" ~> expr) ^^ {
    case n ~ params ~ e ~ body =>
      val value = params.foldRight(e) {
        case (p, e) =>
          E.Fun(p, e)
      }
      E.App(E.Fun(n, body), value)
  }
  def let_rec =
    ("let" ~ "rec") ~> rep1sep(name ~ name.+ ~ ("=" ~> expr), ";") ~ ("in" ~> expr) ^^ {
      case defs ~ body =>
        E.LetRec(
          defs.map {
            case (name ~ params ~ expr) =>
              name -> params
                .foldRight(expr) {
                  case (p, e) =>
                    E.Fun(p, e)
                }
                .asInstanceOf[Expr.Fun]
          },
          body
        )
    }

  def atom: Parser[Expr] = paren | block | lit_int | lit_str | lit_char | ref
  def expr1 =
    Seq(
      opSyntax("$") {
        case (l, op, r) =>
          E.App(l, r)
      },
      opSyntax("==|!=|<=|>=|<|>".r) {
        case (l, "==", r) =>
          E.Eq(l, r)
        case (l, "!=", r) =>
          E.Not(E.Eq(l, r))
        case (l, "<", r) =>
          E.Lt(l, r)
        case (l, ">", r) =>
          E.Gt(l, r)
        case (l, "<=", r) =>
          E.Le(l, r)
        case (l, ">=", r) =>
          E.Ge(l, r)
      },
      opSyntax("[-+]".r) {
        case (l, "+", r) =>
          E.IntPlus(l, r)
        case (l, "-", r) =>
          E.IntMinus(l, r)
      },
      opSyntax("[*/%]".r) {
        case (l, "*", r) =>
          E.IntMul(l, r)
        case (l, "/", r) =>
          E.IntDiv(l, r)
        case (l, "%", r) =>
          E.IntMod(l, r)
      },
      opSyntax(".") {
        case (l, ".", r) =>
          E.Fun("$x", E.App(l, E.App(r, E.Ref("$x"))))
      }, { next: Parser[Expr] =>
        next ~ rep(next) ^^ {
          case x ~ xs =>
            xs.foldLeft(x) { case (l, r) => E.App(l, r) }
        }
      }
    ).foldRight(atom) { case (f, next) => f(next) }

  def paren = ("(" ~> expr) <~ ")"
  def block = ("{" ~> rep1sep(expr, ";")) <~ "}" ^^ { body =>
    E.Block(body)
  }
  def lit_int = "[0-9]+".r ^^ { x =>
    E.Lit(x.toInt)
  }
  def lit_str = ("\"" ~> """[^"]*""".r) <~ "\"" ^^ { x =>
    E.Lit(x)
  }
  def lit_char = ("'" ~> ".".r) <~ "'" ^^ { c =>
    E.Lit(c.head)
  }
  def ref = name ^^ { x =>
    E.Ref(x)
  }
}
