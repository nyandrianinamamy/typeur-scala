import java.util.NoSuchElementException
import Typeur._

case class Eq(ltype: Type, rtype: Type):
  override def toString(): String =
    s"${ltype.toString()} = ${rtype.toString()}"

def generate_equation(term: Term, t0: Type, env: ENV): List[Eq] =
  term match

    case n: Nat =>
      Eq(t0, N()) :: List()

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
      generate_equation(body, t2, env + (arg -> t1)) ::: Eq(t0, `t1->t2`) :: List()

    case App(term1, term2) =>
      val t1 = TVar(Var.fresh_var())
      val `t1->t0` = Arrow(t1, t0)

      generate_equation(term2, t1, env) ::: generate_equation(term1, `t1->t0`, env)

    case Add(t1, t2) =>
       generate_equation(t1, N(), env) ::: generate_equation(t2, N(), env) ::: Eq(t0, N()) :: List()

    case Diff(t1, t2) =>
      generate_equation(t1, N(), env) ::: generate_equation(t2, N(), env) ::: Eq(t0, N()) :: List()

    case lst: EOL =>
      throw Error("Empty list not typable")

    // List of all terms
    // Check if all terms have the same type
    case cons@Cons(term: Term, lst: Lst) =>
      val t1 = infer(term, env)
      cons match {
        case Cons(_, EOL()) => Eq(t0, TLst(t1)) :: List()
        case Cons(t, lst: Lst) =>
          val t2 = infer(t, env)
          if (!t2.equals(t1))
          then
            throw Error(s"Term of type ${t1} expected, but ${t2} given")
          generate_equation(lst, t0, env)
        case null => throw Error("List term not supported")
      }

    case Head(lst) =>
      val tlst = infer(lst, env)
      tlst match
        case TLst(t) => Eq(t0, t) :: List()
        case _ => throw Error("Term given to Head has invalid type")

    case Tail(lst) =>
      lst match {
        case Cons(_, list) => generate_equation(list, t0, env)
      }

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


