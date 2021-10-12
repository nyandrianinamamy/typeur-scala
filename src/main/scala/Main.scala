@main def hello: Unit =
  val x = Var("x")
  val y = Var("y")
  val app = App(x, y)
  val arr = App(x, y)

  var I = Abs("x", app)

  print_term(I)

  I = barendregt(I) match
    case Abs(arg, body) => Abs(arg, body)

  print_term(I)

def barendregt(l: Term): Term =
  def _barendregt(l: Term, remp: Map[String, Var]): Term = l match
    case Var(x) =>
      if (!remp.contains(x)) Var(x) else remp(x)

    case Abs(arg, body) =>
      if (!remp.contains(arg))
        var new_var = Var.fresh_var()
        Abs(new_var.name, _barendregt(body, remp + (arg -> new_var)))
      else
        Abs(remp(arg).name, _barendregt(body, remp))

    case App(term1, term2) =>
      var new_term1 = _barendregt(term1, remp)
      var new_term2 = _barendregt(term2, remp)
      App(new_term1, new_term2)

  _barendregt(l, Map())

