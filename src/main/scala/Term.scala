trait Term

case class Var(name: String)
  extends Term

case class App(term1: Term, term2: Term)
  extends Term

case class Abs(arg: String, body: Term)
  extends Term

def print_term(term: Term): Unit =
  def to_string(term: Term): String = term match
    case Var(name) => name
    case App(term1, term2) => "(" + to_string(term1) + " " + to_string(term2) + ")"
    case Abs(arg, body) => "(fun " + arg + " -> " + to_string(body) + ")"

  println(to_string(term))