import Solveur._
import scala.util.{Try,Success,Failure}

object Typeur {
  def infer(term: Term, env: Map[Var, Type] = Map()): Try[Type] =
    val eqs = generate_equation(term, TVar.t0, env)
    Try {
      val unified_eqs = unification_etape(eqs)
      solve(unified_eqs)
    }
}