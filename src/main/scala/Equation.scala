import EquationExceptions._

import java.util.NoSuchElementException
import scala.util.{Failure, Success, Try}
import Typeur.*

case class Eq(ltype: Type, rtype: Type):
  override def toString(): String =
    s"${ltype.toString()} = ${rtype.toString()}"

def generate_equation(term: Term, t0: Type, env: ENV): List[Eq] =
  term match

    case n: Nat =>
      Eq(t0, N()) :: List()

    case x: Var =>
      env get x match
        case Some(a) => Eq(t0, a) :: List()
        case None =>
          throw VarNotFoundInEnvException(x)

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
      Eq(t0, EmptyLst()) :: List()

    case cons@Cons(term: Term, lst: Lst) =>
      val x = Var.fresh_var()
      val X = TVar(x)
      val tlst = TLst(X)

      val eq1 = generate_equation(term, X, env)
      val eq2 = Eq(t0, tlst)
      eq1 ::: eq2 :: List()

    case Head(lst) =>
      val X = TVar(Var.fresh_var())
      val `[X]` = TLst(X)
      val `[X] -> X` = Arrow(`[X]`, X)

      val `[t0]` = TLst(t0)
      val `[t0] -> t0` = Arrow(`[t0]`, t0)

      val eq1 = generate_equation(lst, `[X]`, env)
      val eq2 = List(Eq(`[t0] -> t0`, `[X] -> X`))

      eq1 ::: eq2 ::: List()

    case Tail(lst) =>
      val X = TVar(Var.fresh_var())

      val eq1 = generate_equation(lst, X, env)
      val eq2 = Eq(t0, X)

      eq1 ::: eq2 :: List()

    case Letin(x, e1, e2) =>
      val t1 = infer(e1, env)
      t1 match
        case Success(infered_type: Type) =>
          val newenv = free_type_var(infered_type, env)
          val tx = generalise(newenv, infered_type)
          generate_equation(e2, t0, env + (x -> tx))

        case Failure(exception) =>
          throw LetInferenceException(x, e1)


    case Izte(nat, term1, term2) =>
      val eq1 = generate_equation(nat, N(), env)
      val eq2 = generate_equation(term1, t0, env)
      val eq3 = generate_equation(term2, t0, env)
      eq1 ::: eq2 ::: eq3 ::: List()

    case Iete(lst, term1, term2) =>
      val x = Var.fresh_var()
      val X = TVar(x)
      val `[X]` = TLst(X)
      val `Forall X.[X]` = Forall(X, `[X]`)

      val eq1 = generate_equation(lst, `Forall X.[X]`, env)
      val eq2 = generate_equation(term1, t0, env)
      val eq3 = generate_equation(term2, t0, env)
      eq1 ::: eq2 ::: eq3 ::: List()

    case Fix(phi, m) =>
      val x = Var.fresh_var()
      val tf = TVar(x)
      val `tf -> tf` = Arrow(tf, tf)

      val eq1 = generate_equation(m, tf, env)
      val eq2 = generate_equation(phi, Forall(tf, `tf -> tf`), env)
      val eq3 = List(Eq(Arrow(t0, t0), `tf -> tf`))

      eq2 ::: eq1 ::: eq3

    case Void() =>
      Eq(t0, TVoid()) :: List()

    case Ref(e) =>
      val X = TVar(Var.fresh_var())
      val eq1 = generate_equation(e, X, env)
      val eq2 = Eq(t0, TRef(X))

      eq1 ::: eq2 :: List()

    case Deref(e) =>
      val X = TVar(Var.fresh_var())
      val R = TRef(X)
      val eq1 = generate_equation(e, R, env)
      val eq2 = Eq(t0, X)

      eq1 ::: eq2 :: List()

    case Assign(e1, e2) =>
      val x = Var.fresh_var()
      val tx = TVar(x)

      val tr = TRef(tx)
      val eq1 = generate_equation(e1, tr, env)
      val eq2 = generate_equation(e2, tx, env)
      val eq3 = Eq(t0, TVoid())

      eq1 ::: eq2 ::: eq3 :: List()


/**
 * Generalize a type with all free vars not in env
 * ∀x1...xk.t
 */
def generalise(free_type_vars: List[TVar], t: Type): Type =
  free_type_vars.foldLeft(t) {
    case (acc, tv) => Forall(tv, acc)
  }

def free_type_var(t: Type, env: ENV): List[TVar] =
  t match
    case n: N => List()
    case tv@TVar(x) =>
      if env.contains(x)
      then List()
      else
        tv :: List()
    case Arrow(tl, tr) =>
      val in_left = free_type_var(tl, env)
      val in_right = free_type_var(tr, env)
      val combined = in_left ::: in_right

      combined.distinct
    case TLst(t) => free_type_var(t, env)
    case Forall(a, b) => free_type_var(b, env).filter(p => !p.equals(a))


