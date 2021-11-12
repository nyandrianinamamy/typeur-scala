import org.junit.Test
import org.junit.Assert.*
import org.junit.Before

class EqTest:

  @Before def initialize(): Unit =
    Var.last = 1

  @Test def should_gen_eq_on_TVar(): Unit =
    val x = Var("x")
    val tx = TVar(x)

    val env: ENV = Map(x -> tx) // Suppose x is type tx

    generate_equation(x, TVar.t0, env) match
      case h :: q => assertEquals("x0 = x", h.toString)

  /**
   * lamba x.x
   */
  @Test def should_gen_eq_abs(): Unit =
    val x = Var("x")
    val I = Abs(x, x)
    val tx = TVar(x)

    val env: ENV = Map(x -> tx) // Suppose x is type tx
    generate_equation(I, TVar.t0, env) match
      case l: List[Eq] =>
        l foreach {
          case Eq(lterm: TVar, rterm: TVar) =>
            assertEquals("x3 = x2", Eq(lterm, rterm).toString)
          case Eq(lterm: TVar, rterm: Arrow) =>
            assertEquals("x0 = (x2 -> x3)", Eq(lterm, rterm).toString)
        }

  /**
   * lambda x.x x
   */
  @Test def should_gen_eq_app(): Unit =
    val x = Var("x")
    val tx = TVar(x)
    val I = Abs(x, x)
    val `I x` = App(I, x)

    val expected = "(x2 -> x0) = (x3 -> x4)" :: "x4 = x3" :: "x2 = x" :: List()

    val env: ENV = Map(x -> tx) // Suppose x is type tx
    generate_equation(`I x`, TVar.t0, env) match
      case l: List[Eq] =>
        l foreach {
          case eq: Eq => assertTrue(expected exists (str => str == eq.toString))
        }

  @Test def should_gen_eq_nat(): Unit =
    val x = Nat(1)

    val env: ENV = Map()
    generate_equation(x, TVar.t0, env) match
      case h :: q => assertEquals(s"x0 = $N", h.toString)

  @Test def should_gen_eq_add_nat(): Unit =
    val x = Nat(1)
    val y = Nat(2)
    val add = Add(x, y)

    val env: ENV = Map()
    generate_equation(add, TVar.t0, env) match
      case h :: q => assertEquals(s"x0 = (($N -> $N) -> $N)", h.toString)

  @Test def should_gen_eq_add_var(): Unit =
    val y = Nat(1)
    val x = Var("x")
    val add = Add(x, y)

    val env: ENV = Map(x -> N())
    generate_equation(add, TVar.t0, env) match
      case h :: q => assertEquals(s"x0 = (($N -> $N) -> $N)", h.toString)

  @Test def should_gen_eq_head(): Unit =
    val lst = Cons(Nat(0), Empty())
    val hd = Head(lst)

    val env: ENV = Map()
    generate_equation(hd, TVar.t0, env) match
      case h :: q => assertEquals(s"x0 = (forall X.[X] -> X)", h.toString)

  @Test def should_gen_eq_tail(): Unit =
    val lst = Cons(Nat(0), Empty())
    val t = Tail(lst)

    val env: ENV = Map()
    generate_equation(t, TVar.t0, env) match
      case h :: q => assertEquals(s"x0 = (forall X.[X] -> forall X.[X])", h.toString)

  @Test def should_gen_eq_let_add(): Unit =
    val x = Var("x")
    val t1 = Nat(2)
    val t2 = Add(x, Nat(1))
    val let = Letin(x, t1, t2)

    val env: ENV = Map()
    generate_equation(let, TVar.t0, env) match
      case h :: q => assertEquals(s"x0 = ((N -> N) -> N)", h.toString)

  @Test def should_gen_eq_let_abs(): Unit =
    val n = Nat(1)
    val x = Var("x")

    val f = Abs(x, x)
    val t2 = App(f, n)
    val let = Letin(x, f, t2)

    val env: ENV = Map()
    assertEquals("List(x6 = x5, (x4 -> x0) = (x5 -> x6), x4 = N)", generate_equation(let, TVar.t0, env).toString())