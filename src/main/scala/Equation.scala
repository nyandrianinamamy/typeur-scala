import java.util.NoSuchElementException
import scala.collection.mutable.Map

case class Eq(ltype: Type, rtype: Type):
  override def toString(): String =
    s"${ltype.toString()} = ${rtype.toString()}"

def generate_equation(term: Term): List[Eq] =
  def generate_equation(term: Term, t0: Type, env: Map[Var, Type]): List[Eq] =
    term match
      case x: Var =>
        env get x match {
          case Some(a) => Eq(t0, a) :: List()
          case None =>
            throw NoSuchElementException(s"Var $x not found in environment")
        }

      case Abs(arg, body) =>
        val t1 = TVar(Var.fresh_var())
        val t2 = TVar(Var.fresh_var())
        val `t1->t2` = Arrow(t1, t2)
        env(arg) = t1
        Eq(t0, `t1->t2`) :: generate_equation(body, t2, env)

      case App(term1, term2) =>
        val t1 = TVar(Var.fresh_var())
        val `t1->t0` = Arrow(t1, t0)

        generate_equation(term1, `t1->t0`, env) ::: generate_equation(term2, t1, env)

  var env: Map[Var, Type] = Map() // Mutable Map
  val t0 = TVar(Var("x0")) // Type goal alias T0

  generate_equation(term, t0, env)