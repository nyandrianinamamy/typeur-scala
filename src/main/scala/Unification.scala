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
    case TVar(y) if y.equals(x) => s
    case TVar(y) if !(y.equals(x)) => TVar(y)
    case Arrow(arg, res) => Arrow(substitue(x, s, arg), substitue(x, s, res))

def substitue_partout(x: Var, s: Type, eqs: List[Eq]): List[Eq] =
  eqs map {
    case Eq(ltype, rtype) => Eq(substitue(x, s, ltype), substitue(x, s, rtype))
  }

/**
 * Simple equation unifier.
 *
 * Suppose t0 is always on the left side.
 *
 * Suppose the equation containing t0 is always in the bottom of the list.
 */
def unification_etape(eqs: List[Eq]): List[Eq] =
  eqs match

    case Nil => throw Error("No more equations in list")

    case h :: t => h match // extract to independent function

      case eq@Eq(TVar.t0, r: Type) if t.isEmpty =>
        eq :: List()

      // Removes an eq if ltype = rtype
      case Eq(l, r) if l.equals(r) =>
        unification_etape(t)

      // Remove X = Td and eqs[X/Td] if X not in Td
      case Eq(TVar(x), r) if !occur_check(x, r) =>
        unification_etape(substitue_partout(x, r, t))

      // Remove Td = X and eqs[X/Td] if X not in Td
      case Eq(l, TVar(x)) if !occur_check(x, l) =>
        unification_etape(substitue_partout(x, l, t))

      // Remove a -> b = c -> d and replace with a = c and b = d
      case Eq(Arrow(arg, res), Arrow(arg1, res1)) =>
        unification_etape(Eq(arg, arg1) :: Eq(res, res1) :: t)

      case _ => throw Error("Cannot unify")
