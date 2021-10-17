object Typeur {
  def typeur(term: Term): String =
    val env: Map[Var, Type] = Map()
    val eqs = generate_equation(term, TVar.t0, env)
    unification_etape(eqs) match
      case List(Eq(ltype, rtype)) => s"Typable avec le type $rtype"
      case Nil | _ => "Non typable"
}