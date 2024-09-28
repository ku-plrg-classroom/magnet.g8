package kuplrg

import scala.util.parsing.combinator.*

// expressions
enum Expr:
  // undefined
  case EUndef
  // number
  case ENum(number: BigInt)
  // boolean
  case EBool(bool: Boolean)
  // addition
  case EAdd(left: Expr, right: Expr)
  // multiplication
  case EMul(left: Expr, right: Expr)
  // division
  case EDiv(left: Expr, right: Expr)
  // modulo
  case EMod(left: Expr, right: Expr)
  // equal-to
  case EEq(left: Expr, right: Expr)
  // less-than
  case ELt(left: Expr, right: Expr)
  // mutable variable definition
  case EVar(name: String, init: Expr, body: Expr)
  // identifier lookup
  case EId(name: String)
  // variable assignment
  case EAssign(name: String, expr: Expr)
  // sequence
  case ESeq(left: Expr, right: Expr)
  // conditional
  case EIf(cond: Expr, thenExpr: Expr, elseExpr: Expr)
  // while loop
  case EWhile(cond: Expr, body: Expr)
  // break
  case EBreak
  // continue
  case EContinue
  // anonymous (lambda) function
  case EFun(params: List[String], body: Expr)
  // function application
  case EApp(fun: Expr, args: List[Expr])
  // return
  case EReturn(expr: Expr)
  // try-catch
  case ETry(body: Expr, catchParam: String, catchExpr: Expr)
  // throw
  case EThrow(expr: Expr)
  // generator
  case EGen(params: List[String], body: Expr)
  // iterator next
  case EIterNext(iter: Expr, arg: Option[Expr])
  // generator yield
  case EYield(expr: Expr)
  // value field of iterator result
  case EValueField(result: Expr)
  // done field of iterator result
  case EDoneField(result: Expr)

  // the string form of an expression
  def str: String = this match
    case EUndef          => "undefined"
    case ENum(n)         => n.toString
    case EBool(b)        => b.toString
    case EAdd(l, r)      => s"(${l.str} + ${r.str})"
    case EMul(l, r)      => s"(${l.str} * ${r.str})"
    case EDiv(l, r)      => s"(${l.str} / ${r.str})"
    case EMod(l, r)      => s"(${l.str} % ${r.str})"
    case EEq(l, r)       => s"(${l.str} == ${r.str})"
    case ELt(l, r)       => s"(${l.str} < ${r.str})"
    case EId(x)          => x
    case EVar(x, i, b)   => s"var $x = (${i.str}); ${b.str}"
    case EAssign(x, e)   => s"$x = (${e.str})"
    case ESeq(l, r)      => s"${l.str}; ${r.str}"
    case EFun(ps, b)     => s"(${ps.mkString(", ")}) => { ${b.str} }"
    case EApp(f, es)     => s"${f.str}(${es.map(_.str).mkString(", ")})"
    case EReturn(e)      => s"return (${e.str})"
    case EIf(c, t, e)    => s"if (${c.str}) { ${t.str} } else { ${e.str} }"
    case EWhile(c, b)    => s"while (${c.str}) { ${b.str} }"
    case EBreak          => "break"
    case EContinue       => "continue"
    case ETry(b, x, c)   => s"try { ${b.str} } catch ($x) { ${c.str} }"
    case EThrow(e)       => s"throw (${e.str})"
    case EGen(ps, b)     => s"(${ps.mkString(", ")}) =>* { ${b.str} }"
    case EIterNext(i, a) => s"${i.str}.next(${a.fold("")(_.str)})"
    case EYield(e)       => s"yield (${e.str})"
    case EValueField(r)  => s"${r.str}.value"
    case EDoneField(r)   => s"${r.str}.done"

// states
case class State(cont: Cont, stack: Stack, handler: Handler, mem: Mem):
  // string form of a state
  def str: String =
    val memList = mem.toList.sortBy(_._1)
    val hdlList = handler.toList.sortBy(_._1.str)
    s"""
    |* cont   : ${(cont.map(_.str) :+ "[]").mkString(" :: ")}
    |* stack  : ${(stack.map(_.str) :+ "[]").mkString(" :: ")}
    |* handler: ${hdlList
      .map((c, kv) =>
        s"\n  - ${c.str} -> ${(kv.cont.map(_.str) :+ "[]").mkString(" :: ")}",
      )
      .mkString}
    |* mem    : [${memList.map((k, v) => s"#$k -> ${v.str}").mkString(", ")}]
    """.trim.stripMargin

// continuations
type Cont = List[Inst]

// instructions
enum Inst:
  // expression evaluation
  case IEval(env: Env, expr: Expr)
  // addition
  case IAdd
  // multiplication
  case IMul
  // division
  case IDiv
  // modulo
  case IMod
  // equal-to
  case IEq
  // less-than
  case ILt
  // identifier definition
  case IDef(xs: List[String], env: Env, body: Expr)
  // address write
  case IWrite(addr: Addr)
  // pop and discard
  case IPop
  // conditional jump
  case IJmpIf(kv: KValue)
  // unconditional jump
  case IJmp(control: Control)
  // function call
  case ICall(argSize: Int)
  // function return
  case IReturn
  // iterator next
  case INext
  // yield
  case IYield
  // value field of iterator result
  case IValueField
  // done field of iterator result
  case IDoneField

  // the string form of a continuation
  def str: String = this match
    case IEval(_, e)    => s"eval[_ |- ${e.str}]"
    case IAdd           => s"(+)"
    case IMul           => s"(*)"
    case IDiv           => s"(/)"
    case IMod           => s"(%)"
    case IEq            => s"(==)"
    case ILt            => s"(<)"
    case IDef(xs, _, b) => s"var[${xs.mkString(", ")}][_ |- ${b.str}]"
    case IWrite(a)      => s"write[#$a]"
    case IPop           => s"pop"
    case IJmpIf(_)      => s"jmp-if[_]"
    case IJmp(c)        => s"jmp[${c.str}]"
    case ICall(n)       => s"call[$n]"
    case IReturn        => s"return"
    case INext          => s"next"
    case IYield         => s"yield"
    case IValueField    => s"value"
    case IDoneField     => s"done"

// value stacks
type Stack = List[Value]

// values
enum Value:
  // undefined value
  case UndefV
  // number value
  case NumV(number: BigInt)
  // boolean value
  case BoolV(bool: Boolean)
  // closure value
  case CloV(params: List[String], body: Expr, env: Env)
  // continuation value
  case ContV(pair: KValue)
  // generator value
  case GenV(params: List[String], body: Expr, env: Env)
  // iterator value
  case IterV(next: Addr)
  // iterator result value
  case ResultV(value: Value, done: Boolean)

  // the string form of a value
  def str: String = this match
    case UndefV        => "undefined"
    case NumV(n)       => n.toString
    case BoolV(b)      => b.toString
    case CloV(_, _, _) => s"<function>"
    case ContV(_)      => s"<continuation>"
    case GenV(_, _, _) => s"<generator>"
    case IterV(_)      => s"<iterator>"
    case ResultV(v, b) => s"{ value: ${v.str}, done: $b }"

// control handlers
type Handler = Map[Control, KValue]

// control operators
enum Control:
  case Return
  case Break
  case Continue
  case Throw
  case Finally
  case Yield

  // the string form of a control operator
  def str: String = this match
    case Return   => "return"
    case Break    => "break"
    case Continue => "continue"
    case Throw    => "throw"
    case Finally  => "finally"
    case Yield    => "yield"

// continuation values
case class KValue(cont: Cont, stack: Stack, handler: Handler)

// memories
type Mem = Map[Addr, Value]

// environments
type Env = Map[String, Addr]

// addresses
type Addr = Int

// -----------------------------------------------------------------------------
// Parsers
// -----------------------------------------------------------------------------
object Expr extends Parser.From(Parser.expr)
object Parser extends RegexParsers with PackratParsers {
  import Expr.*
  type P[+T] = PackratParser[T]
  class From[T](p: Parser[T]) {
    def apply(s: String): T =
      init
      parseAll(p, s).getOrElse(error("parse error"))
  }
  private val keywords = Set(
    "break",
    "catch",
    "continue",
    "else",
    "false",
    "for",
    "function",
    "if",
    "of",
    "return",
    "throw",
    "true",
    "try",
    "undefined",
    "var",
    "while",
    "yield",
  )
  private val d: String = "0-9"
  private val w: String = "_a-zA-Z"
  private lazy val num: P[BigInt] = s"-?[$d]+".r ^^ BigInt.apply
  private lazy val bool: P[Boolean] = "true" ^^^ true | "false" ^^^ false
  private lazy val id: P[String] = s"[$w][$w$d]*".r.withFilter(!keywords(_))
  lazy val expr: P[Expr] =
    import PostfixOps.*
    lazy val e0: P[Expr] =
      rep1sep(e1, ";") <~ opt(";") ^^ { _.reduceLeft(ESeq.apply) }
    lazy val e1: P[Expr] =
      (id <~ "=") ~ e1 ^^ { case x ~ e => EAssign(x, e) } |
      (id <~ "+=") ~ e1 ^^ { case x ~ e => EAddAssign(x, e) } |
      (id <~ "-=") ~ e1 ^^ { case x ~ e => ESubAssign(x, e) } |
      (id <~ "*=") ~ e1 ^^ { case x ~ e => EMulAssign(x, e) } |
      (id <~ "/=") ~ e1 ^^ { case x ~ e => EDivAssign(x, e) } |
      (id <~ "%=") ~ e1 ^^ { case x ~ e => EModAssign(x, e) } |
      ("var" ~> id <~ "=") ~ e1 ~ (";" ~> e0) ^^ {
        case x ~ e ~ b => EVar(x, e, b)
      } |
      e2

    lazy val e2: P[Expr] = rep1sep(e3, "||") ^^ (_.reduceLeft(EOr))
    lazy val e3: P[Expr] = rep1sep(e4, "&&") ^^ (_.reduceLeft(EAnd))
    lazy val e4: P[Expr] = e5 ~ rep(("==" | "!=") ~ e5) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (l, "==" ~ r) => EEq(l, r)
          case (l, _ ~ r)    => ENe(l, r)
        }
    }
    lazy val e5: P[Expr] = e6 ~ rep(("<=" | "<" | ">=" | ">") ~ e6) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (l, "<" ~ r)  => ELt(l, r)
          case (l, "<=" ~ r) => ELe(l, r)
          case (l, ">" ~ r)  => EGt(l, r)
          case (l, _ ~ r)    => EGe(l, r)
        }
    }
    lazy val e6: P[Expr] = e7 ~ rep(("+" | "-") ~ e7) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (l, "+" ~ r) => EAdd(l, r)
          case (l, _ ~ r)   => ESub(l, r)
        }
    }
    lazy val e7: P[Expr] = e8 ~ rep(("*" | "/" | "%") ~ e8) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (l, "*" ~ r) => EMul(l, r)
          case (l, "/" ~ r) => EDiv(l, r)
          case (l, _ ~ r)   => EMod(l, r)
        }
    }
    lazy val e8: P[Expr] =
      "++" ~> id ^^ EPreInc.apply |
      "--" ~> id ^^ EPreDec.apply |
      "-" ~> e8 ^^ ENeg |
      "!" ~> e8 ^^ ENot |
      e9

    lazy val e9: P[Expr] = e10 ~ rep(
      "(" ~> repsep(e0, ",") <~ ")" ^^ PApp.apply |
      "." ~ "next" ~ ("(" ~> opt(e0) <~ ")") ^^ { case _ ~ a => PNext(a) } |
      "." ~ "value" ^^^ PValue |
      "." ~ "done" ^^^ PDone,
    ) ^^ {
      case e ~ es =>
        es.foldLeft(e) {
          case (f, PApp(as)) => EApp(f, as)
          case (i, PNext(a)) => EIterNext(i, a)
          case (r, PValue)   => EValueField(r)
          case (r, PDone)    => EDoneField(r)
        }
    }
    lazy val e10: P[Expr] = (
      (id <~ "++") ^^ EPostInc.apply |
        (id <~ "--") ^^ EPostDec.apply |
        (id <~ "=>") ~ e1 ^^ { case p ~ b => EFun(List(p), b) } |
        (id <~ "=>*") ~ e1 ^^ { case p ~ b => EGen(List(p), b) } |
        params ~ ("=>" ~> e1) ^^ { case ps ~ b => EFun(ps, b) } |
        params ~ ("=>*" ~> e1) ^^ { case ps ~ b => EGen(ps, b) } |
        "undefined" ^^^ EUndef |
        "(" ~> e0 <~ ")" |
        "{" ~> e0 <~ "}" |
        num ^^ ENum.apply |
        bool ^^ EBool.apply |
        "if" ~> ("(" ~> e0 <~ ")") ~ e0 ~ ("else" ~> e1) ^^ {
          case c ~ t ~ e => EIf(c, t, e)
        } |
        "function" ~ "*" ~> id ~ params ~ ("{" ~> e0 <~ "}") ~ e0 ^^ {
          case n ~ ps ~ b ~ t => ENamedGen(n, ps, b, t)
        } |
        "function" ~> id ~ params ~ ("{" ~> e0 <~ "}") ~ e0 ^^ {
          case n ~ ps ~ b ~ t => ENamedFun(n, ps, b, t)
        } |
        "while" ~> ("(" ~> e0 <~ ")") ~ e1 ^^ { case c ~ b => EWhile(c, b) } |
        "for" ~> ("(" ~> id <~ "of") ~ (e0 <~ ")") ~ e1 ^^ {
          case x ~ i ~ b => EForOf(x, i, b)
        } |
        "return" ~> e1 ^^ EReturn.apply |
        "break" ^^^ EBreak |
        "continue" ^^^ EContinue |
        ("try" ~> e1) ~ ("catch" ~> ("(" ~> id <~ ")")) ~ e1 ^^ {
          case t ~ x ~ c => ETry(t, x, c)
        } |
        "throw" ~> e1 ^^ EThrow.apply |
        "yield" ~> e1 ^^ EYield.apply |
        id ^^ EId.apply
    )
    e0

  // parameters
  private lazy val params: P[List[String]] =
    "(" ~> repsep(id, ",") <~ ")" ^^ { dupCheck(_, "parameter") }

  // postfix operators
  private enum PostfixOps:
    case PApp(args: List[Expr])
    case PNext(arg: Option[Expr])
    case PValue
    case PDone

  // desugaring rules
  private val T: Expr = EBool(true)
  private val F: Expr = EBool(false)
  private def ENeg(e: Expr): Expr = EMul(e, ENum(-1))
  private def ESub(l: Expr, r: Expr): Expr = EAdd(l, ENeg(r))
  private def EAnd(l: Expr, r: Expr): Expr = EIf(l, r, F)
  private def EOr(l: Expr, r: Expr): Expr = EIf(l, T, r)
  private def ENot(e: Expr): Expr = EIf(e, F, T)
  private def ENe(l: Expr, r: Expr): Expr = ENot(EEq(l, r))
  private def ELe(l: Expr, r: Expr): Expr =
    val (x, y) = (tid, tid)
    EVar(x, l, EVar(y, r, EOr(EEq(EId(x), EId(y)), ELt(EId(x), EId(y)))))
  private def EGt(l: Expr, r: Expr): Expr = ENot(ELe(l, r))
  private def EGe(l: Expr, r: Expr): Expr = ENot(ELt(l, r))
  private def EAddAssign(x: String, e: Expr): Expr = EAssign(x, EAdd(EId(x), e))
  private def ESubAssign(x: String, e: Expr): Expr = EAssign(x, ESub(EId(x), e))
  private def EMulAssign(x: String, e: Expr): Expr = EAssign(x, EMul(EId(x), e))
  private def EDivAssign(x: String, e: Expr): Expr = EAssign(x, EDiv(EId(x), e))
  private def EModAssign(x: String, e: Expr): Expr = EAssign(x, EMod(EId(x), e))
  private def ENamedFun(f: String, ps: List[String], b: Expr, t: Expr): Expr =
    EVar(f, EFun(ps, b), t)
  private def ENamedGen(f: String, ps: List[String], b: Expr, t: Expr): Expr =
    EVar(f, EGen(ps, b), t)
  private def EPreInc(x: String): Expr =
    ESeq(EAddAssign(x, ENum(1)), EId(x))
  private def EPreDec(x: String): Expr =
    ESeq(ESubAssign(x, ENum(1)), EId(x))
  private def EPostInc(x: String): Expr =
    val y = tid
    EVar(y, EId(x), ESeq(EAddAssign(x, ENum(1)), EId(y)))
  private def EPostDec(x: String): Expr =
    val y = tid
    EVar(y, EId(x), ESeq(ESubAssign(x, ENum(1)), EId(y)))
  private def EForOf(x: String, e: Expr, b: Expr): Expr =
    val (iter, result) = (tid, tid)
    EVar(
      iter,
      e,
      EVar(
        result,
        EIterNext(EId(iter), None),
        EWhile(
          ENot(EDoneField(EId(result))),
          EVar(
            x,
            EValueField(EId(result)),
            ESeq(b, EAssign(result, EIterNext(EId(iter), None))),
          ),
        ),
      ),
    )

  private def init = tidCount = -1
  private var tidCount = -1
  private def tid: String = { tidCount += 1; s"$$t$tidCount" }

  // duplicate check
  private def dupCheck[T](
    names: List[T],
    kind: String,
    f: T => String = (x: T) => x.toString,
  ): List[T] =
    if (names.distinct.length != names.length)
      error(s"duplicate $kind: ${names.mkString(", ")}")
    names
}
