import SolveurExceptions._

object Solveur {
  def solve(eqs: List[Eq], env: Map[Type, Type] = Map()): Type =
    eqs match
      case Nil =>
        env get (TVar.t0) getOrElse {
          throw GoalNotFoundException()
        }
      case h :: t =>
        h match
          case Eq(ltype, rtype) =>
            env get ltype match
              case Some(x) if !x.equals(rtype) =>
                throw ClashTypeException(TVar.t0, x, rtype)

              case Some(x) =>
                solve(t, env)

              case None =>
                solve(t, env + (ltype -> rtype))
}
