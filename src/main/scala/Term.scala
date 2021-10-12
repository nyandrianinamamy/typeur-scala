trait Term

case class Var(name: String)
  extends Term

case class App(term1: Term, term2: Term)
  extends Term

case class Abs(arg: Var, body: Term)
  extends Term

object Var {
  var last: Int = 1

  def fresh_var(): Var =
    Var.last = Var.last + 1
    return Var("V" + Var.last)

  def last_var(): Var =
    return Var("V" + Var.last)
}

def print_term(term: Term): Unit =
  def to_string(term: Term): String = term match
    case Var(name) => name
    case App(term1, term2) => "(" + to_string(term1) + " " + to_string(term2) + ")"
    case Abs(arg, body) => "(fun " + to_string(arg) + " -> " + to_string(body) + ")"

  println(to_string(term))

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