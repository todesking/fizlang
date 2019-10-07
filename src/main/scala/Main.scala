package com.todesking.fizlang
import scala.util.parsing.combinator.RegexParsers

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
}

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

object Interpreter {
  var debug: Boolean = false
  val E = Expr
  type Env = Map[String, Any]
  class Error(msg: String) extends RuntimeException(msg)

  case class FunData(param: String, body: Expr, var env: Env)
  case class Instinct(name: String, f: Any => Any)

  val defaultEnv = Seq(
    "println" -> { x: Any =>
      println(x)
    },
    "char_to_int" -> { x: Any =>
      x.asInstanceOf[Char].toInt
    },
    "int_to_char" -> { x: Any =>
      x.asInstanceOf[Int].toChar
    },
    "char_to_string" -> { x: Any =>
      x.asInstanceOf[Char].toString
    },
    "string_concat" -> { x: Any =>
      Instinct("string_concat_1", { y: Any =>
        x.asInstanceOf[String] + y.asInstanceOf[String]
      })
    }
  ).map { case (k, v) => k -> Instinct(k, v) }.toMap ++ Map(
    "true" -> true,
    "false" -> false,
    "unit" -> ()
  )

  def runExpr(src: String, env: Env = defaultEnv): Any = {
    Parser.parseExpr(src) match {
      case Left(msg)   => throw new Error("Parse error: " + msg)
      case Right(expr) => eval(expr, env)
    }
  }
  def runScript(src: String, env: Env = defaultEnv): Env =
    Parser.parseToplevel(src) match {
      case Left(msg) => throw new Error(s"Parse error: $msg")
      case Right(xs) =>
        val finalEnv =
          xs.foldLeft(env) {
            case (env, (name, rec, expr)) =>
              val v = eval(expr, env)
              env + (name -> v)
          }
        xs.filter(_._2)
          .foreach {
            case (name, rec, expr) =>
              finalEnv(name).asInstanceOf[FunData].env = finalEnv
          }
        finalEnv
    }
  def runMain(src: String, env: Env = defaultEnv): Any = {
    val e = runScript(src, env)
    val main = evalFun(E.Ref("main"), e)
    eval(main.body, e + (main.param -> ()))
  }

  def eval(expr: Expr, env: Env = defaultEnv): Any = expr match {
    case E.Lit(value) => value
    case E.IntPlus(l, r) =>
      evalInt(l, env) + evalInt(r, env)
    case E.IntMinus(l, r) =>
      evalInt(l, env) - evalInt(r, env)
    case E.IntMul(l, r) =>
      evalInt(l, env) * evalInt(r, env)
    case E.IntDiv(l, r) =>
      evalInt(l, env) / evalInt(r, env)
    case E.IntMod(l, r) =>
      evalInt(l, env) % evalInt(r, env)
    case E.Eq(l, r) =>
      eval(l, env) == eval(r, env)
    case E.Gt(l, r) =>
      evalInt(l, env) > evalInt(r, env)
    case E.Ge(l, r) =>
      evalInt(l, env) >= evalInt(r, env)
    case E.Le(l, r) =>
      evalInt(l, env) <= evalInt(r, env)
    case E.Lt(l, r) =>
      evalInt(l, env) < evalInt(r, env)
    case E.Not(expr) =>
      !evalBool(expr, env)
    case E.If(cond, th, el) =>
      if (evalBool(cond, env)) eval(th, env)
      else eval(el, env)
    case E.App(f, a) =>
      val fun = eval(f, env)
      val arg = eval(a, env)
      fun match {
        case FunData(param, body, env) =>
          eval(body, env + (param -> arg))
        case Instinct(_, f) =>
          f(arg)
      }
    case E.Fun(param, body) =>
      FunData(param, body, env)
    case E.Ref(name) =>
      if (debug) {
        try {
          println(s"Deref $name => ${env.get(name)}")
        } catch {
          case e: StackOverflowError =>
        }
      }
      env.get(name).getOrElse {
        throw new Error(s"Name not found: $name")
      }
    case E.Block(body) =>
      body.foldLeft((): Any) {
        case (_, expr) =>
          eval(expr, env)
      }
    case E.LetRec(defs, body) =>
      val funs = defs.map {
        case (name, fun) =>
          name -> FunData(fun.param, fun.body, null)
      }
      val newEnv = env ++ funs
      funs.foreach {
        case (k, fun) =>
          fun.env = newEnv
      }
      eval(body, newEnv)
  }
  private[this] def typeError(expected: String, value: Any, expr: Expr) = {
    val tpe = if (value == null) "null" else value.getClass
    new Error(s"Expected $expected: $value: ${tpe}, location: $expr")
  }
  def evalInt(expr: Expr, env: Env = defaultEnv): Int =
    eval(expr, env) match {
      case x: Int => x
      case unk    => throw typeError("int", unk, expr)
    }
  def evalBool(expr: Expr, env: Env = defaultEnv): Boolean =
    eval(expr, env) match {
      case x: Boolean => x
      case unk        => throw typeError("bool", unk, expr)
    }
  def evalFun(expr: Expr, env: Env = defaultEnv): FunData =
    eval(expr, env) match {
      case f: FunData => f
      case unk        => throw typeError("fun", unk, expr)
    }
}

object Main {
  def test(s: String, expected: Any): Unit = {
    println(s"> $s")
    val result = try {
      val e = Interpreter.runExpr(s)
      if (e == expected) e.toString
      else throw new AssertionError(s"[Unexpected] $e != $expected")
    } catch {
      case e: Interpreter.Error => "[Error] " + e.getMessage
      case e: AssertionError    => e.getMessage
    }
    println(s"=> $result")
  }
  def testScript(s: String, expected: Any): Unit = {
    println(s"> $s")
    val result = try {
      val e = Interpreter.runMain(s)
      if (e == expected) e.toString
      else throw new AssertionError(s"[Unexpected] $e != $expected")
    } catch {
      case e: Interpreter.Error => "[Error] " + e.getMessage
      case e: AssertionError    => e.getMessage
    }
    println(s"=> $result")
  }
  def main(args: Array[String]): Unit = {
    test("1", 1)
    test(""""Hello world!"""", "Hello world!")
    test("1 + 2", 3)
    test("1 * 2", 2)
    test("1 / 2", 0)
    test("1 % 2", 1)
    test("(fun x => x + 1) 10", 11)
    test("println(1 + 2)", ())
    test("if true then 1 else 2", 1)
    test("if false then 1 else 2", 2)
    test("char_to_string . int_to_char $ 42", "*")
    test("if | false then 1 | true then 2 | else 3", 2)
    test("1 == 2", false)
    test("1 != 2", true)
    test("{ 1; println 2; 3 }", 3)
    test("1 >= 1", true)
    test("1 > 2", false)
    test("1 <= 1", true)
    test("1 < 2", true)
    test("unit", ())
    test("let x = 10 in let y = 20 in let x = 3 in x + y", 23)
    test("let div x y = x / y in div 10 3", 3)
    testScript("""
      let a = 1;
      let b = 2;
      let add x y = x + y ;
      let main x = add a b ;
    """, 3)
    // Interpreter.debug = true
    test(
      """
      let num_to_str n =
        let last_digit n =
            char_to_string (int_to_char $ char_to_int '0' + n % 10) in
        let rec impl n s =
          let s = string_concat $ last_digit n $ s in
          if n < 10 then s
          else impl $ n / 10 $ s in
        impl n "" in
      num_to_str 123
  """,
      "123"
    )
    testScript(
      """
      let rec is_even n =
        if n == 0 then true
        else is_odd $ n - 1 ;
      let rec is_odd n =
        if n == 0 then false
        else is_even $ n - 1 ;
      let main x = is_even 13 ;
    """,
      false
    )
    testScript(
      """
      let main args =
        let rec
          is_even n =
            if n == 0 then true
            else is_odd $ n - 1 ;
          is_odd n =
            if n == 0 then false
            else is_even $ n - 1 in
        is_even 14 ;
    """,
      true
    )

    // Interpreter.debug = true
    testScript(
      """
      let main x =
        foreach 1 30 $ println . fizzbuzz_str ;

      let num_to_str n =
        let last_digit n =
            char_to_string (int_to_char $ char_to_int '0' + n % 10) in
        let rec impl n s =
          let s = string_concat $ last_digit n $ s in
          if n < 10 then s
          else impl $ n / 10 $ s in
        impl n "" ;

      let fizzbuzz_str n = if
          | n % 15 == 0 then "FizzBuzz"
          | n % 3 == 0 then "Fizz"
          | n % 5 == 0 then "Buzz"
          | else num_to_str n ;

      let rec foreach from to f = if
          | from <= to then { f from; foreach $ from + 1 $ to $ f }
          | else unit
          ;
    """,
      ()
    )
  }
}
