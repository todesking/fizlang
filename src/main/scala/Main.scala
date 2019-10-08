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
