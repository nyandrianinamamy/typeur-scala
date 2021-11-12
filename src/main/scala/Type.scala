trait Type {
  def contains(that: Var): Boolean = occur_check(that, this)
}

type ENV = Map[Var, Type]

case class TVar(x: Var) extends Type :
  override def toString(): String =
    x.name

case class Arrow(arg: Type, res: Type) extends Type :
  override def toString(): String =
    s"(${arg.toString()} -> ${res.toString()})"

/**
 * Natural number type
 */
case class N() extends Type :
  override def toString: String =
    "N"

/**
 * List type
 */
case class TLst(T: Type) extends Type :
  override def toString: String =
    s"[${T.toString}]"

/**
 * Empty list type
 */
case class Empty() extends Type:
  override def toString: String = "[Nil]"

/**
 * Type constructor forall X.T
 */
case class Forall(X: TVar, T: Type) extends Type:
  override def toString: String = s"forall ${X.toString}.${T.toString}"

object TVar {
  val t0 = TVar(Var.`0`)
}

/**
 * Compare two types
 *
 * @param ltype left type
 * @param rtype right type
 * @return true if left equals right
 */
def sequal_type(ltype: Type, rtype: Type): Boolean =
  ltype.equals(rtype)