trait Type

case class TVar(x: Var) extends Type :
  override def toString(): String =
    x.name

case class Arrow(arg: Type, res: Type) extends Type :
  override def toString(): String =
    s"(${arg.toString()} -> ${res.toString()})"

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