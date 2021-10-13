import org.junit.Test
import org.junit.Assert.*

class EquationTest:
  @Test def should_gen_eq_var(): Unit =
    val x = Var("x")
    val tx = TVar(x)
    val tv = TVar(Var.fresh_var()) // New env for tx

    val expected = Equation(tv, tx)

    generate_equation(x, tx, Map(x -> tv)) match
      case l: List[Equation] => assertEquals(expected, l.head)
