/**
 * Checks if a variable x is present in the type t
 *
 * @param x
 * @param t
 * @return
 */
def occur_check(x: Var, t: Type): Boolean =
  t match
    case TVar(y) => x.equals(y)
    case Arrow(arg, res) => occur_check(x, arg) || occur_check(x, res)
    case Forall(a, b) => occur_check(x, a) || occur_check(x, b)
    case n: N => false
    case TLst(t) => occur_check(x, t)
    case TRef(r) => occur_check(x, r)
    case TVoid() => false
    case EmptyLst() => false

/**
 * Substitute type t with type s for all occurrences of var x in type t
 *
 * @param x
 * @param s
 * @param t
 * @return
 */
def substitue(x: Var, s: Type, t: Type): Type =
  t match
    case n: N => n
    case TLst(y) => TLst(substitue(x, s, y))
    case TVar(y) if y.equals(x) => s
    case TVar(y) if !(y.equals(x)) => TVar(y)
    case Arrow(arg, res) => Arrow(substitue(x, s, arg), substitue(x, s, res))
    case Forall(l, r) => Forall(l, substitue(x, s, r))
    case TRef(y) => TRef(substitue(x, s, y))
    case TVoid() => t

def substitue_partout(x: Var, s: Type, eqs: List[Eq]): List[Eq] =
  eqs map {
    case Eq(ltype, rtype) => Eq(substitue(x, s, ltype), substitue(x, s, rtype))
  }

/**
 * Removes the forall surrounding a type expression
 */
def open_forall(f: Type): Type =
  f match
    case Forall(c: TVar, d: Forall) => open_forall(d)
    case Forall(c: TVar, d: Type) => d
    case _ => throw new Error("Not type Forall")

/**
 * Simple equation unifier.
 *
 * Suppose t0 is always on the left side.
 *
 * Suppose the equation containing t0 is always in the bottom of the list.
 */
def unification_etape(eqs: List[Eq]): List[Eq] =
  eqs match

    case Nil => eqs

    case h :: t => h match

      // Equation with goal, skip
      case Eq(TVar(x), r) if x.equals(Var.`0`) =>
        h :: unification_etape(t)

      // t1 = t2, skip
      case Eq(l, r) if l equals r =>
        unification_etape(t)

      // v1 = v2, substitute v1 with v2
      case Eq(TVar(x), TVar(y)) =>
        unification_etape(substitue_partout(x, TVar(y), t))

      // a -> b = c -> d, create a = c and b = d
      case Eq(Arrow(arg, res), Arrow(arg1, res1)) =>
        unification_etape(Eq(arg, arg1) :: Eq(res, res1) :: t)

      // [T] = [X], T = X
      case Eq(TLst(l), TLst(r)) =>
        unification_etape(Eq(l, r) :: t)

      // [T] = [Nil], T = [Nil]
      case Eq(TLst(l), EmptyLst()) =>
        unification_etape(Eq(l, EmptyLst()) :: t)

      // Ref a = Ref b, a = b
      case Eq(TRef(a), TRef(b)) =>
        unification_etape(Eq(a, b) :: t)

      // t = VX.T, t = barendregt(VX,T)
      case Eq(l, Forall(a, b)) =>
        val alpha_converted = alpha_conversion_type(Forall(a, b))
        unification_etape(Eq(l, alpha_converted) :: t)

      // VX.T = t, barendregt(VX,T) = t
      case Eq(Forall(a, b), r) =>
        val alpha_converted = alpha_conversion_type(Forall(a, b))
        unification_etape(Eq(alpha_converted, r) :: t)

      // var x = t, and x not in t, substitute all var in t with x
      case eq@Eq(TVar(x), r) =>
        if r contains x
        then throw new Error(s"${eq.toString} non unifiable, $r contains $x")
        unification_etape(substitue_partout(x, r, t))

      // t = var x, and x not in t, substitute all var in t with x
      case eq@Eq(l, TVar(x)) =>
        if l contains x
        then throw new Error(s"${eq.toString} non unifiable, $l contains $x")
        unification_etape(substitue_partout(x, l, t))

      // Arrow on left but not on right, throw
      case Eq(ar@Arrow(arg, res), r) =>
        throw new Error(s"${ar.toString} not unifiable with ${r.toString}")

      // Arrow on right but not on left, throw
      case Eq(l, ar@Arrow(arg, res)) =>
        throw new Error(s"${ar.toString} not unifiable with ${l.toString}")

      case _ => throw new Error(s"Case ${h.toString} non unifiable")