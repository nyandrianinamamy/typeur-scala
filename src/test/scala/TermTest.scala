import org.junit.Test
import org.junit.Assert.*

class TermTest:
  @Test def should_generate_different_var(): Unit =
    val v1: Var = Var.fresh_var()
    val v2: Var = Var.fresh_var()
    assertNotEquals(v1.name, v2.name)

  /**
   * x => x
   */
  @Test def should_alpha_convert_var(): Unit =
    var old: Var = Var("x")
    var new_var: Var = barendregt(old) match
      case Var(x) => Var(x)

    assertEquals(old, new_var)

  /**
   * lambda x.x => lambda V2.V2
   */
  @Test def should_alpha_convert_abs_with_single_bound_var(): Unit =
    var x = Var("x")
    var abs = Abs("x", x)

    var new_abs: Abs = barendregt(abs) match
      case Abs(arg, body) => Abs(arg, body)

    assertNotEquals(abs, new_abs)
    assertNotEquals(abs.arg, new_abs.arg)
    assertNotEquals(abs.body, new_abs.body)

  /**
   * lambda x.y x => lambda V2.y V2
   */
  @Test def should_alpha_convert_abs_with_single_bound_var_and_single_free_var(): Unit =
    var x = Var("x")
    var y = Var("y")
    var app = App(y, x)
    var abs = Abs("x", app)

    barendregt(abs) match
      case Abs(arg, body) =>
        assertEquals(Var.last_var().name, arg)
        assertEquals(y, body match {
          case App(term1, term2) => term1
        })

  /**
   * lambda y.y x => lambda V2.V2 x
   */
  @Test def should_alpha_convert_abs_with_single_bound_var_and_single_free_var_1(): Unit =
    var x = Var("x")
    var y = Var("y")
    var app = App(y, x)
    var abs = Abs("y", app)

    barendregt(abs) match
      case Abs(arg, body) =>
        assertEquals(Var.last_var().name, arg)
        assertEquals(x, body match {
          case App(term1, term2) => term2
        })

  /**
   * lambda x.x lambda x.x => lambda a.a lambda b.b
   */
  @Test def shouldAlphaConvertApp(): Unit =
    var x = Var("x")
    var I = Abs("x", x)
    var I_2 = Abs("x", x)

    var app = App(I, I_2)

    barendregt(app) match
      case App(term1, term2) =>

        term1 match
          case Abs(arg, body) => assertEquals(Var.last_var().name, arg)

        term2 match
          case Abs(arg, body) => assertEquals(Var.last_var().name, arg)