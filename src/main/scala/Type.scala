import scala.language.postfixOps

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

case class EmptyLst() extends Type :
  override def toString: String = "[Nil]"

/**
 * Type constructor forall X.T
 */
case class Forall(X: TVar, T: Type) extends Type:
  override def toString: String = s"forall ${X.toString}.${T.toString}"

case class TVoid() extends Type:
  override def toString: String = "Unit"

case class TRef(t: Type) extends Type:
  override def toString: String = s"Ref ${t.toString}"

object TVar {
  var last: Int = 1
  val t0 = TVar(Var.`0`)

  def fresh_tvar(): TVar =
    TVar.last = TVar.last + 1
    return TVar(Var(s"t${TVar.last}"))

  def last_tvar(): TVar =
    return TVar(Var(s"t${TVar.last}"))
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

def alpha_conversion_type(f: Forall, remp: Map[TVar, TVar] = Map()): Type =
  def barendregt(t: Type, remp: Map[TVar, TVar]): Type = t match
    case N() => N()

    case TVar(x) =>
      remp get TVar(x) getOrElse TVar(x)

    case TLst(x) =>
      TLst(barendregt(x, remp))

    case Arrow(arg, res) =>
      Arrow(barendregt(arg, remp), barendregt(res, remp))

  f match {
    case Forall(a: TVar, b: Forall) =>
      alpha_conversion_type(b, Map(a -> TVar.fresh_tvar()))
    case Forall(a: TVar, b) => barendregt(b, Map(a -> TVar.fresh_tvar()))
  }