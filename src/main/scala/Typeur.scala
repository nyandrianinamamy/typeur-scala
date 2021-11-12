object Typeur {
  def infer(term: Term, env: Map[Var, Type] = Map()): Type =
    val eqs = generate_equation(term, TVar.t0, env)
    try {
      unification_etape(eqs) match
        case List(Eq(_, rtype: Type)) => rtype
        case _ => throw Error("Non typable")
    } catch
      case _ => throw Error("Non typable")
}