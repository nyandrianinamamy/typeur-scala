import org.junit.Test
import org.junit.Assert.*

class TermTest:
  @Test def should_generate_different_var(): Unit =
    assertNotEquals(Var.fresh_var(), Var.fresh_var())

  /**
   * x => x
   */
  @Test def should_alpha_convert_var(): Unit =
    val x = Var("x")
    alpha_conversion(x) match
      case y: Var => assertEquals(x, y)
      case _ => assertTrue(false)

  /**
   * lambda x.x => lambda V2.V2
   */
  @Test def should_alpha_convert_abs_with_single_bound_var(): Unit =
    val x = Var("x")
    val abs = Abs(x, x)

    alpha_conversion(abs) match
      case new_abs: Abs =>
        assertNotEquals(abs, new_abs)
        assertNotEquals(abs.arg, new_abs.arg)
        assertNotEquals(abs.body, new_abs.body)

      case _ => assertTrue(false)

  /**
   * lambda x.y x => lambda V2.y V2
   */
  @Test def should_alpha_convert_abs_with_single_bound_var_and_single_free_var(): Unit =
    var x = Var("x")
    var y = Var("y")
    var app = App(y, x)
    var abs = Abs(x, app)

    alpha_conversion(abs) match
      case Abs(arg, body) =>
        assertEquals(Var.last_var(), arg)
        assertEquals(y, body match
          case App(term1, term2) => term1
        )

  /**
   * lambda y.y x => lambda V2.V2 x
   */
  @Test def should_alpha_convert_abs_with_single_bound_var_and_single_free_var_inverted(): Unit =
    var x = Var("x")
    var y = Var("y")
    var app = App(y, x)
    var abs = Abs(y, app)

    alpha_conversion(abs) match
      case Abs(arg, body) =>
        assertEquals(Var.last_var(), arg)
        assertEquals(x, body match
          case App(term1, term2) => term2
        )

  /**
   * lambda x.x lambda x.x => lambda a.a lambda b.b
   */
  @Test def shouldAlphaConvertApp(): Unit =
    var x = Var("x")
    var I = Abs(x, x)
    var I_2 = Abs(x, x)

    var app = App(I, I_2)

    alpha_conversion(app) match
      case App(term1, term2) =>

        term1 match
          case abs: Abs =>
            assertNotEquals(I, abs)

        term2 match
          case abs: Abs =>
            assertNotEquals(I_2, abs)

        assertNotEquals(term1, term2)

      case _ => assertTrue(false)

  @Test def alpha_convert_KII_to_motivate_myself(): Unit =
    val x = Var("x")
    val y = Var("y")
    val K = Abs(x, Abs(y, x))
    val I = Abs(x, x)

    var KII = App(App(K, I), I)

    print_term(KII)

    KII = alpha_conversion(KII) match
      case App(term1, term2) => App(term1, term2)
      case _ => assert(false)

    print_term(KII)

  @Test def should_print_term(): Unit =
    val x = Var("x")
    val abs = Abs(x, x)
    val app = App(x, x)

    val x_string = print_term(x)
    val abs_string = print_term(abs)
    val app_string = print_term(app)

    assertEquals(x.name, x_string)
    assertEquals(s"(fun ${abs.arg.name} -> ${
      abs.body match
        case Var(x) => x
    })", abs_string)
    assertEquals(s"(${
      app.term1 match
        case Var(x) => x
    } ${
      app.term2 match
        case Var(x) => x
    })", app_string)

  @Test def should_print_nat(): Unit =
    val n = Nat(0)

    assertEquals("0", n.toString())

  @Test def should_print_lst(): Unit =
    val l = Cons(Nat(0), Cons(Nat(1), EOL()))

    assertEquals(s"(0,(1,Nil))", l.toString)

  @Test def should_print_let_poly(): Unit =
    val x = Var("x")
    val one = Nat(1)
    val two = Nat(2)
    val t1 = Add(x, one);

    val let = Letin(x, two, t1);

    assertEquals("let x = (2) in (x + 1)", let.toString)
