import org.junit.Test
import org.junit.Assert.*
import org.junit.Before

class EquationTest:

  @Before def initialize(): Unit =
    Var.last = 1

  /**
   * lamba x.x
   */
  @Test def should_gen_eq_abs(): Unit =
    val x = Var("x")
    val I = Abs(x, x)

    generate_equation(I) match
      case l: List[Equation] =>
        l foreach {
          case Equation(lterm: TVar, rterm: TVar) =>
            assertEquals(equation_to_string(Equation(lterm, rterm)), "x3 = x2")
          case Equation(lterm: TVar, rterm: Arrow) =>
            assertEquals(equation_to_string(Equation(lterm, rterm)), "x0 = (x2 -> x3)")
        }

  /**
   * lambda x.x x
   */
  @Test def should_gen_eq_app(): Unit =
    val x = Var("x")
    val I = Abs(x, x)
    val `I x` = App(I, x)

    val `0` = Var("x0")
    val t0 = TVar(`0`)

    val expected = "(x2 -> x0) = (x3 -> x4)" :: "x4 = x3" :: "x2 = x3" :: List()

    generate_equation(`I x`) match {
      case l: List[Equation] =>
        l foreach {
          case eq: Equation => assertTrue(expected exists (str => str == equation_to_string(eq)))
        }
    }
