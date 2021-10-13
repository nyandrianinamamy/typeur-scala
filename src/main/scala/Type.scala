trait Type

case class TVar(x: Var)
  extends Type

case class Arrow(arg: Type, res: Type)
  extends Type

def print_type(t: Type): String =
  def print_type(t: Type): String = t match
    case TVar(x) => x.name
    case Arrow(arg, res) => s"(${print_type(arg)} -> ${print_type(res)})"

  val printed_type = print_type(t)

  println(printed_type)

  printed_type