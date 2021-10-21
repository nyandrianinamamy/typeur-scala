/**
 * Lambda calculus definition
 * M ::= x | lambda x.M | M N
 */
trait Term

/**
 * Variable x
 *
 * @param name x
 */
case class Var(name: String)
  extends Term

/**
 * Application M N
 *
 * @param term1 M
 * @param term2 N
 */
case class App(term1: Term, term2: Term)
  extends Term

/**
 * Abstraction lambda x.M
 *
 * @param arg  x
 * @param body M
 */
case class Abs(arg: Var, body: Term)
  extends Term

case class N(i: Int) extends Term :
  override def toString: String = s"${i}"

case class Add(a: Term, b: Term) extends Term :
  override def toString: String =
    s"${a.toString()} + ${b.toString()}"

case class Subs(a: Term, b: Term) extends Term :
  override def toString: String =
    s"${a.toString()} - ${b.toString()}"

case class Nil() extends Term

/**
 * Native List with ordered sequence of element
 *
 */
case class Lst(current: Term, next: Lst | Nil) extends Term :
  override def toString: String = Lst(current, next) match
    case Lst(_, _: Nil) => s"${current.toString},Nil"
    case _ => s"${current.toString},${next.toString()}"

  def head(): Term =
    current

  def tail(): Term = this match
    case Lst(_, Nil()) => current
    case Lst(_, nxt: Lst) => nxt.tail()

/**
 * Contains last variable used for alpha conversion
 */
object Var {
  var last: Int = 1
  val `0` = Var("x0")

  def fresh_var(): Var =
    Var.last = Var.last + 1
    return Var(s"x${Var.last}")

  def last_var(): Var =
    return Var(s"x${Var.last}")
}

/**
 * Print term to human readable form
 *
 * @param term term to print
 */
def print_term(term: Term): String =
  def print_term(term: Term): String = term match
    case Var(name) => name
    case App(term1, term2) => s"(${print_term(term1)} ${print_term(term2)})"
    case Abs(arg, body) => s"(fun ${print_term(arg)} -> ${print_term(body)})"

  val term_to_string = print_term(term)

  println(term_to_string)
  term_to_string

/**
 * Alpha convert a term according to Barendregt convention.
 *
 * @param l term to alpha-convert
 * @return term alpha-converted
 */
def alpha_conversion(l: Term): Term =
  def barendregt(l: Term, remp: Map[Var, Var]): Term = l match
    case Var(x) =>
      remp get Var(x) getOrElse Var(x)

    case Abs(arg, body) =>
      var new_var: Var = remp get arg getOrElse Var.fresh_var()
      Abs(new_var, barendregt(body, remp + (arg -> new_var)))

    case App(term1, term2) =>
      App(barendregt(term1, remp), barendregt(term2, remp))

  barendregt(l, Map())
