
case class Equation(ltype: Type, rtype: Type)

def equation_to_string(eq: Equation): String = eq match
  case Equation(ltype, rtype) => s"${type_to_string(ltype)} = ${type_to_string(rtype)}"

def generate_equation(term: Term, t0: Type, env: Map[Var, Type]): List[Equation] =
  term match
    case x: Var =>
      List(Equation(t0, env(x)))

    case Abs(arg, body) =>
      val t1 = TVar(Var.fresh_var())
      val t2 = TVar(Var.fresh_var())
      val `t1->t2` = Arrow(t1, t2)
      Equation(t0, `t1->t2`) :: generate_equation(body, t2, env + (arg -> t1))