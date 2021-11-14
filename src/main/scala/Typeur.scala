import Solveur._

object Typeur {
  def infer(term: Term, env: Map[Var, Type] = Map()): Type =
    val eqs = generate_equation(term, TVar.t0, env)
    try {
      val unified_eqs = unification_etape(eqs)
      solve(unified_eqs)
    } catch
      case e: Error =>
        println(e.getMessage())
        throw Error("Non typable")
}