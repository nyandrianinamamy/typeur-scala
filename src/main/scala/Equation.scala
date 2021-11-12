import java.util.NoSuchElementException
import Typeur._

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
      generate_equation(a, N(), env) match
        case List(Eq(_, ta)) =>

          generate_equation(b, N(), env) match
            case List(Eq(_, tb)) =>
              val `ta->tb` = Arrow(ta, tb);
              val `ta->tb->N` = Arrow(`ta->tb`, N())

              Eq(t0, `ta->tb->N`) :: List()

            case _ => throw Error("Addition impossible")
        case _ => throw Error("Addition impossible")

    case Head(lst) =>
      var X = TVar(Var("X"))
      var `[X]` = TLst(X)
      var `∀X.[X]` = Forall(X, `[X]`)
      var `∀X.[X]->X` = Arrow(`∀X.[X]`, X)

      Eq(t0, `∀X.[X]->X`) :: List()

    case Tail(lst) =>
      var X = TVar(Var("X"))
      var `[X]` = TLst(X)
      var `∀X.[X]` = Forall(X, `[X]`)
      var `∀X.[X] -> ∀X.[X]` = Arrow(`∀X.[X]`, `∀X.[X]`)

      Eq(t0, `∀X.[X] -> ∀X.[X]`) :: List()

    case Letin(x, e1, e2) =>
      val t1 = infer(e1, env)
      val newenv = getFreeVar(t1, env)
      val tx = generalise(newenv, t1)
      generate_equation(e2, t0, env + (x -> tx))

/**
 * Generalize a type with all free vars not in env
 * ∀x1...xk.t
 */
def generalise(newEnv: List[TVar], t: Type): Type =
  newEnv.foldLeft(t) {
    case (acc, tv) => Forall(tv, acc)
  }

def getFreeVar(t: Type, env: ENV): List[TVar] =
  t match
    case n: N => List()
    case tv@TVar(x) =>
      if (!env.contains(x))
      then tv :: List()
      else List()
    case Arrow(tl, tr) => getFreeVar(tl, env)


