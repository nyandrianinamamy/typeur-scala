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
 * First step of unification
 *
 * - Removes an eq if ltype = rtype
 *
 * - Remove X = Td and eqs[X/Td] if X not in Td
 *
 * - Remove a -> b = c -> d and replace with a = c and b = d
 *
 * If x is t0 and only t0 do not touch left side, replace only right side
 *
 * @param eqs
 * @param i
 * @return
 */
def unification_etape(eqs: List[Eq], i: Int): List[Eq] =
  eqs match

    case Nil => throw Error("No more equations in list")

    case h :: t => h match // extract to independent function

      // Found solution
      case eq@Eq(l: TVar, r: Type) if l.equals(TVar.t0) =>
        eq :: List()

      case Eq(l, r) if l.equals(r) =>
        unification_etape(t, i + 1)

      case Eq(TVar(x), r) if !occur_check(x, r) =>
        unification_etape(substitue_partout(x, r, t), i + 1)

      case Eq(l, TVar(x)) if !occur_check(x, l) =>
        unification_etape(substitue_partout(x, l, t), i + 1)

      case Eq(Arrow(arg, res), Arrow(arg1, res1)) =>
        unification_etape(Eq(arg, arg1) :: Eq(res, res1) :: t, i + 1)

      case _ => throw Error("Cannot unify")
