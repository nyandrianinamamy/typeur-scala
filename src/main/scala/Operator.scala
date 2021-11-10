/**
 * If zero Then term1 Else term2
 *
 */
case class Izte(nat: Nat, term1: Term, term2: Term):
  override def toString: String = s"If ${nat.toString} then ${term1.toString} else ${term2.toString}"

/**
 * If empty Then term1 Else term2
 *
 */
case class Iete(lst: Lst, term1: Term, term2: Term):
  override def toString: String = s"If ${lst.toString} then ${term1.toString} else ${term2.toString}"

/**
 * Fix point operator
 */
class Fix(phi: Term, M: Term):
  override def toString: String = s"fix (${phi.toString}, ${M.toString})"

/**
 * Head operator
 */
case class Head(lst: Lst):
  override def toString: String = s"head ${lst.toString}"

/**
 * Tail operator
 */
case class Tail(lst: Lst):
  override def toString: String = s"tail ${lst.toString}"

/**
 * Addition operator
 */
case class Add(a: Term, b: Term) extends Term:
  override def toString: String =
    s"${a.toString()} + ${b.toString()}"