import org.junit.Test
import org.junit.Assert.*
import org.junit.Before

class EqTest:

  @Before def initialize(): Unit =
    Var.last = 1

  @Test def `should generate equation on TVar`(): Unit =
    val x = Var("x")
    val but = Var("x0")
    val Tbut = TVar(but)
    val env: Map[Var, Type] = Map(x -> Tbut)

    generate_equation(x, Tbut, env) match
      case h :: q => assertEquals("x0 = x0", h.toString())

  /**
   * lamba x.x
   */
  @Test def should_gen_eq_abs(): Unit =
    val x = Var("x")
    val I = Abs(x, x)

    val `0` = Var("x0")
    val t0 = TVar(`0`)

    val env: Map[Var, Type] = Map(x -> t0)
    generate_equation(I, t0, env) match
      case l: List[Eq] =>
        l foreach {
          case Eq(lterm: TVar, rterm: TVar) =>
            assertEquals("x3 = x2", Eq(lterm, rterm).toString())
          case Eq(lterm: TVar, rterm: Arrow) =>
            assertEquals("x0 = (x2 -> x3)", Eq(lterm, rterm).toString())
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

    val expected = "(x2 -> x0) = (x3 -> x4)" :: "x4 = x3" :: "x2 = x0" :: List()

    val env: Map[Var, Type] = Map(x -> t0)
    generate_equation(`I x`, t0, env) match
      case l: List[Eq] =>
        l foreach {
          case eq: Eq => assertTrue(expected exists (str => str == eq.toString()))
        }