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
 * - Removes an eq if ltype = rtype
 * - Remove X = Td and eqs[X/Td] if X not in Td
 *
 * @param eqs
 * @param i
 * @return
 */
def unification_etape(eqs: List[Eq], i: Int): List[Eq] =
  eqs match
    case Nil => List()

    case h :: t => h match

      case Eq(l, r) if l.equals(r) =>
        unification_etape(t, i + 1)

      case Eq(TVar(x), r) if !occur_check(x, r) =>
        unification_etape(substitue_partout(x, r, t), i + 1)

      case _ => h :: t
