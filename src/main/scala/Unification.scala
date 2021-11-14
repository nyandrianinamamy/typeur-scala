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
    case Forall(a, b) => substitue(x, s, a) match
      case tv: TVar => Forall(tv, substitue(x, s, b))

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

    case h :: t => h match // extract to independent function

      // Removes an eq if ltype = rtype
      case Eq(l, r) if l equals r =>
        unification_etape(t)

      case Eq(tv: TVar, r) if tv.equals(TVar.t0) =>
        h :: unification_etape(t)

      // Open right forall, barendregt type
      case Eq(l, Forall(a, b)) =>
        val alpha_converted = alpha_conversion_type(Forall(a, b))
        unification_etape(Eq(l, open_forall(alpha_converted)) :: t)

      // Open left forall, barendregt type
      case Eq(Forall(a, b), r) =>
        val alpha_converted = alpha_conversion_type(Forall(a, b))
        unification_etape(Eq(open_forall(alpha_converted), r) :: t)

      // Remove X = Td and eqs[X/Td] if X not in Td and X is not t0
      case Eq(TVar(x), r) if (!(r contains x) && !(x == Var.`0`)) =>
        unification_etape(substitue_partout(x, r, t))

      // Remove Td = X and eqs[X/Td] if X not in Td
      case Eq(l, TVar(x)) if (!(l contains x) && !(l contains Var.`0`)) =>
        unification_etape(substitue_partout(x, l, t))

      // Remove a -> b = c -> d and replace with a = c and b = d
      case Eq(Arrow(arg, res), Arrow(arg1, res1)) =>
        unification_etape(Eq(arg, arg1) :: Eq(res, res1) :: t)

      case Eq(TLst(a), TLst(b)) if !a.equals(b) =>
        throw new Error("Type constructor different")

      case Eq(TLst(x), r) =>
        unification_etape(Eq(x, r) :: t)

      case Eq(l, TLst(x)) =>
        unification_etape(Eq(l, x) :: t)

      case _ => eqs