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