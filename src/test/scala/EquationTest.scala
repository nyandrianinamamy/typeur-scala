import org.junit.Test
import org.junit.Assert.*
import org.junit.Before

class EquationTest:

  @Before def initialize(): Unit =
    Var.last = 1

  @Test def should_get_var_env(): Unit =
    val x = Var("x")
    val t0 = TVar(Var("x0"))

    generate_equation(x, t0, Map(x -> t0)) match
      case l: List[Equation] => l foreach {
        case eq: Equation => assertEquals("x0 = x0", equation_to_string(eq))
      }

  @Test def should_gen_eq_abs(): Unit =
    val x = Var("x")
    val I = Abs(x, x)

    var `0` = Var("x0")
    var t0 = TVar(`0`)

    var expected = List(Equation(t0, t0))

    generate_equation(I, t0, Map()) match
      case l: List[Equation] =>
        l foreach {
          case Equation(lterm: TVar, rterm: TVar) =>
            assertEquals(equation_to_string(Equation(lterm, rterm)), "x3 = x2")
          case Equation(lterm: TVar, rterm: Arrow) =>
            assertEquals(equation_to_string(Equation(lterm, rterm)), "x0 = (x2 -> x3)")
        }
