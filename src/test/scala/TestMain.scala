import org.scalatest.FunSpec
import com.todesking.fizlang.TrampolineInterpreter
import com.todesking.fizlang.NaiveInterpreter
class TestMain extends FunSpec {
  private[this] def test(
      src: String,
      expected: Any,
      testNaive: Boolean = true,
      testTrampoline: Boolean = true
  ) = {
    val desc =
      src.replaceAll("\\s+", " ").replaceAll("\n", " ").take(20)
    describe(s"Expr $desc") {
      if (testNaive) {
        it(s"should evaluate to $expected by NaiveInterpreter") {
          val actual = NaiveInterpreter.runExpr(src)
          assert(actual == expected)
        }
      }
      if (testTrampoline) {
        it(s"should evaluate to $expected by TrampolineInterpreter") {
          val actual = TrampolineInterpreter.runExpr(src)
          assert(actual == expected)
        }
      }
    }
  }
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
  test("let rec f x = g x; g x = x in f 123", 123)
  test("let rec f x y = if x == 0 then y else f (x - 1) y in f 1 123", 123)
  test(
    "let rec sum from to  = if from == to then from else from + (sum (from + 1) to) in sum 1 1000",
    500500,
    testNaive = false,
    testTrampoline = true
  )
  test("""match 123 | 123 => 456 | x => x + 1""", 456)
  test("""match 456 | 123 => 456 | x => x + 1""", 457)
  test("""(1, 2)""", (1, 2))
  test("""match (1, 2) | (a, b) => a + b""", 3, testTrampoline = false)
  test("""match (1, (2, 3)) | (1, (a, 3)) => a""", 2, testTrampoline = false)
  test(
    """match (10, (20, 30)) | (10, (a, 20)) => 1 | (10, (a, 30)) => 2""",
    2,
    testTrampoline = false
  )

  private[this] def testScript(src: String, expected: Any) = {
    val desc =
      src.replaceAll("\\s+", " ").replaceAll("\n", " ").take(20)
    describe(
      s"Script $desc..."
    ) {
      it(s"should evaluate to $expected by NaiveInterpreter") {
        val actual = NaiveInterpreter.runMain(src)
        assert(actual == expected)
      }
      it(s"should evaluate to $expected by TrampolineInterpreter") {
        val actual = TrampolineInterpreter.runMain(src)
        assert(actual == expected)
      }
    }
  }

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
}
