import java.util.NoSuchElementException
import scala.collection.mutable.Map

case class Equation(ltype: Type, rtype: Type)

def equation_to_string(eq: Equation): String = eq match
  case Equation(ltype, rtype) => s"${type_to_string(ltype)} = ${type_to_string(rtype)}"

def generate_equation(term: Term): List[Equation] =
  def generate_equation(term: Term, t0: Type, env: Map[Var, Type]): List[Equation] =
    term match
      case x: Var =>
        env get x match {
          case Some(a) => Equation(t0, a) :: List()
          case None =>
            println(env)
            throw NoSuchElementException(s"Var $x not found in environment")
        }

      case Abs(arg, body) =>
        val t1 = TVar(Var.fresh_var())
        val t2 = TVar(Var.fresh_var())
        val `t1->t2` = Arrow(t1, t2)
        env(arg) = t1
        Equation(t0, `t1->t2`) :: generate_equation(body, t2, env)

      case App(term1, term2) =>
        val t1 = TVar(Var.fresh_var())
        val `t1->t0` = Arrow(t1, t0)

        val eq1 = generate_equation(term1, `t1->t0`, env)
        val eq2 = generate_equation(term2, t1, env)

        eq1 ::: eq2

  var env: Map[Var, Type] = Map() // Mutable Map
  val `0` = Var("x0")
  val t0 = TVar(`0`)

  generate_equation(term, t0, env)