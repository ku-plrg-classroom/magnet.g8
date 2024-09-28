package kuplrg

trait Template {

  import Inst.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def eval(str: String, debug: Boolean = false): String =
    def aux(st: State): Value =
      if (debug) println("\n" + "-" * 40 + "\n" + st.str)
      reduce(st) match
        case State(Nil, List(v), _, _) =>
          if (debug) println("\n" + "-" * 40 + "\n" + "Result: " + v.str)
          v
        case st => aux(st)
    val initSt = State(IEval(Map(), Expr(str)) :: Nil, Nil, Map(), Map())
    aux(initSt).str

  def reduce(st: State): State

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def squareSumExpr(n: Int, m: Int): String = s"""
    function* squares(from, to) {
      $bodyOfSquares
    }
    var sum = 0;
    for (x of squares($n, $m)) { sum += x; };
    sum
  """

  def bodyOfSquares: String
}
