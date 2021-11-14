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
      List()

    case cons@Cons(term: Term, lst: Lst) =>
      val x = Var("x")
      val X = TVar(x)
      val tlst = TLst(X)
      generate_equation(lst, tlst, env) ::: generate_equation(term, X, env) ::: Eq(t0, tlst) :: List()

    case Head(lst) =>
      val x = Var("x")
      val X = TVar(x)
      val `[X]` = TLst(X)
      val `[X] -> X` = Arrow(`[X]`, X)
      val `Forall X.[X] -> X` = Forall(X, `[X] -> X`)
      generate_equation(lst, X, env + (x -> `Forall X.[X] -> X`)) ::: Eq(t0, X) :: List()


    case Tail(lst) =>
      val x = Var("x")
      val X = TVar(x)
      val `[X]` = TLst(X)
      val `[X] -> [X]` = Arrow(`[X]`, `[X]`)
      val `Forall X.[X] -> [X]` = Forall(X, `[X] -> [X]`)

      val eq1 = generate_equation(lst, X, env + (x -> `Forall X.[X] -> [X]`))
      val eq2 = Eq(t0, `[X]`)
      eq1 ::: eq2 :: List()


    case Letin(x, e1, e2) =>
      val t1 = infer(e1, env)
      val newenv = getFreeVar(t1, env)
      val tx = generalise(newenv, t1)
      generate_equation(e2, t0, env + (x -> tx))

    case Izte(nat, term1, term2) =>
      val eq1 = generate_equation(nat, N(), env)
      val eq2 = generate_equation(term1, t0, env)
      val eq3 = generate_equation(term2, t0, env)
      eq1 ::: eq2 ::: eq3 ::: List()

    case Iete(lst, term1, term2) =>
      val x = Var("x")
      val X = TVar(x)
      val `[X]` = TLst(X)
      val `Forall X.[X]` = Forall(X, `[X]`)

      val eq1 = generate_equation(lst, `[X]`, env + (x -> `Forall X.[X]`))
      val eq2 = generate_equation(term1, t0, env)
      val eq3 = generate_equation(term2, t0, env)
      eq1 ::: eq2 ::: eq3 ::: List()

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
    case TLst(t) => getFreeVar(t, env)


