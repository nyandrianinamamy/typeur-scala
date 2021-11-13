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

/**
 * Type constructor forall X.T
 */
case class Forall(X: TVar, T: Type) extends Type:
  override def toString: String = s"forall ${X.toString}.${T.toString}"

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

def alpha_conversion_type(t: Type): Type =
  def barendregt(t: Type, remp: Map[TVar, TVar]): Type = t match
    case TVar(x) =>
      remp get TVar(x) getOrElse TVar(x)

    case Arrow(arg: TVar, res) =>
      var new_tvar: TVar = remp get arg getOrElse TVar.fresh_tvar()
      Arrow(new_tvar, barendregt(res, remp + (arg -> new_tvar)))

    case Arrow(arg, res: TVar) =>
      var new_tvar: TVar = remp get res getOrElse TVar.fresh_tvar()
      Arrow(barendregt(arg, remp + (res -> new_tvar)), new_tvar)

    case Forall(a, f) =>
      var new_tvar = remp get a getOrElse TVar.fresh_tvar()
      Forall(new_tvar, barendregt(f, remp + (a -> new_tvar)))

    case _ => throw Error("Barendregt failed")

  barendregt(t, Map())
