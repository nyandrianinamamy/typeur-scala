object Solveur {
  def solve(eqs: List[Eq], env: Map[Type, Type] = Map()): Type =
    eqs match {
      case Nil =>
        env get (TVar.t0) getOrElse {
          throw new Error(s"${TVar.t0} not found")
        }
      case h :: t =>
        h match {
          case Eq(ltype, rtype) =>
            env get ltype match {
              case Some(x) if !x.equals(rtype) =>
                throw new Error(s"${TVar.t0} has conflicting types ${x} and ${rtype}")

              case Some(x) =>
                solve(t, env)
              case None =>
                solve(t, env + (ltype -> rtype))
            }
        }
    }
}
