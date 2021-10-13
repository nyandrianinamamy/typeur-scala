import org.junit.Test
import org.junit.Assert.*
import org.junit.Before

class EqTest:

  @Before def initialize(): Unit =
    Var.last = 1

  /**
   * lamba x.x
   */
  @Test def should_gen_eq_abs(): Unit =
    val x = Var("x")
    val I = Abs(x, x)

    generate_equation(I) match
      case l: List[Eq] =>
        l foreach {
          case Eq(lterm: TVar, rterm: TVar) =>
            assertEquals(Eq(lterm, rterm).toString(), "x3 = x2")
          case Eq(lterm: TVar, rterm: Arrow) =>
            assertEquals(Eq(lterm, rterm).toString(), "x0 = (x2 -> x3)")
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
      case l: List[Eq] =>
        l foreach {
          case eq: Eq => assertTrue(expected exists (str => str == eq.toString()))
        }
    }
