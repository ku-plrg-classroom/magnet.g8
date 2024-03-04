package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // -------------------------------------------------------------------------
  // Problem #1: eval
  // -------------------------------------------------------------------------
  test(eval("undefined"), "undefined")
  test(eval("42"), "42")
  test(eval("true"), "true")
  test(eval("false"), "false")
  test(eval("-(-42)"), "42")
  test(eval("1 + 2"), "3")
  test(eval("19 - 2"), "17")
  test(eval("2 * 3"), "6")
  test(eval("17 / 5"), "3")
  testExc(eval("3 / 0"))
  test(eval("17 % 5"), "2")
  testExc(eval("3 % 0"))
  test(eval("!true"), "false")
  test(eval("!false"), "true")
  test(eval("true && true"), "true")
  test(eval("true && false"), "false")
  test(eval("true && 42"), "42")
  test(eval("false && true"), "false")
  test(eval("false && false"), "false")
  test(eval("true || true"), "true")
  test(eval("true || false"), "true")
  test(eval("false || 42"), "42")
  test(eval("false || true"), "true")
  test(eval("false || false"), "false")
  test(eval("1 == 1"), "true")
  test(eval("1 == 2"), "false")
  test(eval("true == true"), "true")
  test(eval("true == 42"), "false")
  test(eval("undefined == undefined"), "true")
  test(eval("1 != 1"), "false")
  test(eval("1 < 2"), "true")
  test(eval("2 >= 5"), "false")
  test(eval("1 > 1"), "false")
  test(eval("1 <= 1"), "true")
  test(eval("var x = 1; x++ <= x++; x"), "3")
  testExc(eval("1 < true"))
  testExc(eval("x"))
  test(eval("if (3 < 5) 10 else 20"), "10")
  test(eval("if (3 > 5) 10 else 20"), "20")
  test(eval("if (3 < 5) 10 else 1 + true"), "10")
  test(eval("if (3 > 5) 1 + true else 20"), "20")
  testExc(eval("if (3) 10 else 20"))
  test(eval("var x = 1; x"), "1")
  test(eval("var x = 2; x = 3; x"), "3")
  test(eval("var x = 2; (x = x + 1) + x"), "6")
  test(eval("x => x"), "<function>")
  test(eval("(x => x + 1)(2)"), "3")
  test(eval("((x, y) => x - y)(2, 5, 8)"), "-3")
  test(eval("(x => x)()"), "undefined")
  test(eval("(x => { return x; 1 })(2)"), "2", weight = 3)
  test(eval("function f(x) { return x - 1; 42 } f(5)"), "4", weight = 3)
  testExc(eval("return 0"), weight = 3)
  testExc(eval("1(2)"))
  test(eval("while (false) 42"), "undefined", weight = 3)
  test(eval("var x = 0; while (x < 3) x += 1; x"), "3", weight = 3)
  test(eval("var x = 0; while(true) { if (x > 1) break else x += 1 }; x"),
    "2", weight = 3)
  test(eval("var x = 0; while(x < 3) { x += 1; continue; return 0 }; x"),
    "3", weight = 3)
  testExc(eval("while (42) 42"), weight = 3)
  testExc(eval("var f = () => break; while (true) f()"), weight = 3)
  testExc(eval("var f = () => continue; while (true) f()"), weight = 3)
  test(eval("try 1 catch (e) e"), "1", weight = 3)
  test(eval("try (throw 1) + 2 catch (e) e"), "1", weight = 3)
  testExc(eval("throw 1"), weight = 3)
  test(eval("var f = x => throw x + 1; try f(1) * 5 catch (e) e"),
    "2", weight = 3)
  test(eval("x =>* x"), "<generator>", weight = 3)
  test(eval("(x =>* x)(42)"), "<iterator>", weight = 3)
  test(eval("(x =>* x)(42).next()"),
    "{ value: 42, done: true }", weight = 3)
  test(eval("(x =>* yield x)(42).next()"),
    "{ value: 42, done: false }", weight = 3)
  test(eval("(x =>* x)(42).next().value"), "42", weight = 3)
  test(eval("(x =>* x)(42).next().done"), "true", weight = 3)
  test(eval("(x =>* { return 1; 2 })().next()"),
    "{ value: 1, done: true }", weight = 3)
  test(eval("(x =>* { throw 42; 2 })().next()"),
    "{ value: 42, done: true }", weight = 3)
  test(eval("(x =>* (yield x + 1) * 3)(2).next()"),
    "{ value: 3, done: false }", weight = 3)
  test(eval("var i = (x =>* yield(yield 1))(); i.next(); i.next(7).value"),
    "7", weight = 3)
  test(eval("var i = (x =>* x)(); i.next(); i.next()"),
    "{ value: undefined, done: true }", weight = 3)

  val expr1 = """
    var x = 10;
    var sum = 0;
    while (x > 0) { sum += x; x -= 1; };
    sum;
  """
  test(eval(expr1), "55", weight = 5) // 10 + 9 + ... + 1 = 55

  val expr2 = """
    function factorial(n) {
      var result = n;
      while (--n > 0) result *= n;
      return result;
    }
    factorial(5);
  """
  test(eval(expr2), "120", weight = 5) // 5 * 4 * 3 * 2 * 1 = 120

  val expr3 = """
    function collatzCount(n) {
      var count = 0;
      while (n > 1) {
        if (n % 2 == 0) n /= 2;
        else n = 3 * n + 1;
        count++;
      };
      return count;
    }
    collatzCount(27);
  """
  test(eval(expr3), "111", weight = 5) // 27 -> 82 -> ... -> 2 -> 1 (111 steps)

  val expr4 = """
    function* allNatural() {
      var n = 0;
      while (true) yield n++;
    }
    var iter = allNatural();
    iter.next();
    iter.next();
    iter.next();
  """
  test(eval(expr4), "{ value: 2, done: false }", weight = 5)

  val expr5 = """
    function f() { 42; }
    var x = 3;
    while (x < 5) {
      f();
      break;
      x++;
    };
    x
  """
  test(eval(expr5), "3", weight = 5)

  val expr6 = """
    function* allNatural() {
      var n = 0;
      while (true) yield n++;
    }
    var sum = 0;
    for (x of allNatural()) {
      if (x > 5) break;
      else sum += x;
    };
    sum
  """
  test(eval(expr6), "15", weight = 10) // 0 + 1 + 2 + 3 + 4 + 5 = 15

  val expr7 = """
    function* allNatural() {
      var n = 0;
      while (true) yield n++;
    }
    var iter1 = allNatural();
    var iter2 = iter1;
    iter1.next();
    iter2.next();
    iter1.next();
    iter2.next();
  """
  test(eval(expr7), "{ value: 3, done: false }", weight = 10)

  val expr8 = """
    function* range(from, until) {
      if (until == undefined) { until = from; from = 0; }
      else undefined;
      while (from < until) yield from++;
    }
    function sum(iter) {
      var result = 0;
      for (x of iter) result += x;
      return result;
    }
    sum(range(6, 15));
  """
  test(eval(expr8), "90", weight = 10) // 6 + 7 + 8 + ... + 13 + 14 = 90

  val expr9 = """
    function* range(from, until) {
      if (until == undefined) { until = from; from = 0; }
      else undefined;
      while (from < until) yield from++;
    }
    function* fibonacci(n) {
      var a = 0;
      var b = 1;
      for (x of range(n)) {
        var tmp = a;
        a = b;
        b += tmp;
        yield a;
      }
    }
    function sum(iter) {
      var result = 0;
      for (x of iter) result += x;
      return result;
    }
    sum(fibonacci(10));
  """
  test(eval(expr9), "143", weight = 10) // 1 + 1 + 2 + 3 + ... + 34 + 55 = 143

  val expr10 = """
    function* range(from, until) {
      if (until == undefined) { until = from; from = 0; }
      else undefined;
      while (from < until) yield from++;
    }
    function isPrime(n) {
      for (x of range(2, n)) {
        if (n % x == 0) return false;
        else if (x * x > n) return true;
        else undefined;
      };
      true
    }
    var getAllPrimes = () =>* {
      var n = 1;
      while (n++; true) {
        if (isPrime(n)) yield n;
        else continue;
      }
    };
    var take = (iter, n) =>* {
      var i = 0;
      for (x of iter) {
        if (i >= n) break;
        else undefined;
        yield x;
        i++;
      }
    };
    function sum(iter) {
      var result = 0;
      for (x of iter) result += x;
      return result;
    }
    sum(take(getAllPrimes(), 10));
  """
  test(eval(expr10), "129", weight = 10) // 2 + 3 + 5 + 7 + ... + 23 + 29 = 129

  // -------------------------------------------------------------------------
  // Problem #2: squareSumExpr
  // -------------------------------------------------------------------------
  // (3 * 3) + (4 * 4) + (5 * 5) = 50
  test(if (passAll) eval(squareSumExpr(3, 5)) else "fail", "50", weight = 10)

  // test case generator for squareSumExpr
  def squareSum(n: Int, m: Int): Int = (n to m).map(x => x * x).sum
  def pass = (0 until 50).forall {
    case k => eval(squareSumExpr(k/2+1, k)) == squareSum(k/2+1, k).toString
  }
  test(passAll && pass, true, weight = 40)

  /* Write your own tests */
}
