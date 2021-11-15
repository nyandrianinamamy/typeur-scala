/**
 * If zero Then term1 Else term2
 *
 */
case class Izte(nat: Nat, term1: Term, term2: Term) extends Term :
  override def toString: String = s"If ${nat.toString} then ${term1.toString} else ${term2.toString}"

/**
 * If empty Then term1 Else term2
 *
 */
case class Iete(lst: Lst, term1: Term, term2: Term) extends Term :
  override def toString: String = s"If ${lst.toString} then ${term1.toString} else ${term2.toString}"

/**
 * Fix point operator
 */
case class Fix(phi: Term, M: Term) extends Term :
  override def toString: String = s"fix (${phi.toString}, ${M.toString})"

/**
 * Head operator
 */
case class Head(lst: Lst) extends Term :
  override def toString: String = s"head ${lst.toString}"

/**
 * Tail operator
 */
case class Tail(lst: Lst) extends Term :
  override def toString: String = s"tail ${lst.toString}"

/**
 * Addition operator
 */
case class Add(a: Term, b: Term) extends Term :
  override def toString: String =
    s"${a.toString()} + ${b.toString()}"

/**
 * Substraction operator
 */
case class Diff(a: Term, b: Term) extends Term :
  override def toString: String =
    s"${a.toString()} - ${b.toString()}"

/**
 * Dereferencing
 */
case class Deref(e: Term) extends Term :
  override def toString: String =
    s"!${e.toString}"

/**
 * Assignment
 */
case class Assign(e1: Term, e2: Term) extends Term :
  override def toString: String =
    s"!${e1.toString} := ${e1.toString}"

/**
 * Referencing
 */
case class Ref(e: Term) extends Term :
  override def toString: String =
    s"ref ${e.toString}"

case class Void() extends Term :
  override def toString: String = "unit"