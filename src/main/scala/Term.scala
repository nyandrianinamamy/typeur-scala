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

/**
 * Contains last variable used for alpha conversion
 */
object Var {
  var last: Int = 1

  def fresh_var(): Var =
    Var.last = Var.last + 1
    return Var("V" + Var.last)

  def last_var(): Var =
    return Var("V" + Var.last)
}

/**
 * Print term to human readable form
 *
 * @param term term to print
 */
def print_term(term: Term): Unit =
  def to_string(term: Term): String = term match
    case Var(name) => name
    case App(term1, term2) => "(" + to_string(term1) + " " + to_string(term2) + ")"
    case Abs(arg, body) => "(fun " + to_string(arg) + " -> " + to_string(body) + ")"

  println(to_string(term))

/**
 * Alpha convert a term according to Barendregt convention.
 *
 * @param l term to alpha-convert
 * @return term alpha-converted
 */
def barendregt(l: Term): Term =
  def _barendregt(l: Term, remp: Map[Var, Var]): Term = l match
    case Var(x) =>
      remp get Var(x) getOrElse Var(x)

    case Abs(arg, body) =>
      var new_var: Var = remp get arg getOrElse Var.fresh_var()
      Abs(new_var, _barendregt(body, remp + (arg -> new_var)))

    case App(term1, term2) =>
      App(_barendregt(term1, remp), _barendregt(term2, remp))

  _barendregt(l, Map())