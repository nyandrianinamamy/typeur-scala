object Typeur {
  def infer(term: Term, env: Map[Var, Type] = Map()): String =
    val eqs = generate_equation(term, TVar.t0, env)
    try {
      unification_etape(eqs) match
        case List(Eq(ltype, rtype)) => s"Typable avec le type $rtype"
        case _ => throw Error()
    } catch
      case _ => "Non typable"
}