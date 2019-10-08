package com.todesking.fizlang

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
