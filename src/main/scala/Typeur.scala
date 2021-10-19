object Typeur {
  def typeur(term: Term): String =
    val env: Map[Var, Type] = Map()
    val eqs = generate_equation(term, TVar.t0, env)
    try {
      unification_etape(eqs) match
        case List(Eq(ltype, rtype)) => s"Typable avec le type $rtype"
    } catch
      case _ => "Non typable"
}