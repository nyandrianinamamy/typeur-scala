trait Type

case class TVar(x: Var)
  extends Type

case class Arrow(arg: Type, res: Type)
  extends Type

def type_to_string(t: Type): String = t match
  case TVar(x) => x.name
  case Arrow(arg, res) => s"(${type_to_string(arg)} -> ${type_to_string(res)})"

/**
 * Compare two types
 *
 * @param ltype left type
 * @param rtype right type
 * @return true if left equals right
 */
def sequal_type(ltype: Type, rtype: Type): Boolean =
  ltype.equals(rtype)