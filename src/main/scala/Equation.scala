
case class Equation(ltype: Type, rtype: Type)

def generate_equation(term: Term, stype: Type, env: Map[Var, Type]): List[Equation] =
  term match
    case x: Var => List(Equation(env(x), stype))
    case _ =>
      println("No more match for generate equation")
      List()
