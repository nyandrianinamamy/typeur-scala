import java.util.NoSuchElementException

case class Eq(ltype: Type, rtype: Type):
  override def toString(): String =
    s"${ltype.toString()} = ${rtype.toString()}"

def generate_equation(term: Term, t0: Type, env: ENV): List[Eq] =
  term match
    case x: Var =>
      env get x match {
        case Some(a) => Eq(t0, a) :: List()
        case None =>
          throw NoSuchElementException(s"Var $x not found in environment")
      }

    case n: Nat =>
      Eq(t0, N()) :: List()

    case Abs(arg, body) =>
      val t1 = TVar(Var.fresh_var())
      val t2 = TVar(Var.fresh_var())
      val `t1->t2` = Arrow(t1, t2)
      generate_equation(body, t2, env + (arg -> t1)) ::: Eq(t0, `t1->t2`) :: List()

    case App(term1, term2) =>
      val t1 = TVar(Var.fresh_var())
      val `t1->t0` = Arrow(t1, t0)

      generate_equation(term1, `t1->t0`, env) ::: generate_equation(term2, t1, env)

    case Add(a, b) =>
      val ta = TVar(Var("a"))
      val tb = TVar(Var("b"))

      generate_equation(a, ta, env) match {
        case List(Eq(la, ra)) =>
          generate_equation(b, tb, env) match {
            case List(Eq(lb, rb)) =>
              if (!ra.equals(rb))
                throw Error("Type addition différent")

              val `ta->ta` = Arrow(ra, ra);
              val `ta->ta->ta` = Arrow(ra, `ta->ta`)
              Eq(t0, `ta->ta->ta`) :: List()

            case _ => throw Error("Type addition différent")
          }
        case _ => throw Error("Type addition différent")
      }
