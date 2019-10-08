package com.todesking.fizlang

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
