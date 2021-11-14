object Solveur {
  def solve(eqs: List[Eq], env: Map[Type, Type] = Map()): Type =
    eqs match {
      case h :: Nil => h match {
        case Eq(TVar.t0, rtype) => rtype
        case _ => throw Error("t0 not found")
      }
      case h :: t =>
        h match {
          case Eq(ltype, rtype) =>
            if (!env.contains(ltype))
            then
              solve(t, env + (ltype -> rtype))
            if (env.get(ltype) == rtype)
            then
              solve(t, env)
            else
              throw new Error(s"${TVar.t0} has conflicting types ${env.get(ltype)} and ${rtype}")
        }
      case _ => throw Error("Solving failed")
    }
}
