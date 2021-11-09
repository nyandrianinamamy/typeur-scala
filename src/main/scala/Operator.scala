/**
 * If zero Then term1 Else term2
 *
 */
class Izte(nat: N, term1: => Term, term2: => Term):
  override def toString: String = s"If ${nat.toString} then ${term1.toString} else ${term2.toString}"

/**
 * If empty Then term1 Else term2
 *
 */
class Iete(lst: Lst, term1: => Term, term2: => Term):
  override def toString: String = s"If ${lst.toString} then ${term1.toString} else ${term2.toString}"
