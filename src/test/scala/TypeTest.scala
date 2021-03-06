import org.junit.{Before, Test}
import org.junit.Assert.*

class TypeTest:
  @Before def initialize(): Unit =
    Var.last = 1
    TVar.last = 1

  @Test def should_type_to_string(): Unit =
    val x = Var("x")
    val tx = TVar(x)
    val tarrow = Arrow(tx, tx)

    assertEquals("x", tx.toString())

    assertEquals("(x -> x)", tarrow.toString())

  @Test def should_compare_type(): Unit =
    val x = Var("x")
    val y = Var("y")
    val tx = TVar(x)
    val ty = TVar(y)
    val txarrow = Arrow(tx, tx)
    val tyarrow = Arrow(ty, ty)

    assertFalse(sequal_type(tx, ty))
    assertTrue(sequal_type(ty, ty))
    assertTrue(sequal_type(tx, tx))
    assertTrue(sequal_type(txarrow, txarrow))
    assertFalse(sequal_type(txarrow, tyarrow))

  @Test def should_print_type_nat(): Unit =
    val t = N()
    assertEquals("N", t.toString())

  @Test def should_print_type_list(): Unit =
    val tx = TVar(Var("x"))
    val tl = TLst(tx)
    assertEquals("[x]", tl.toString)

  @Test def should_print_type_forall(): Unit =
    val X = TVar(Var("X"))
    val T = Arrow(X, X)
    val f = Forall(X, T)

    assertEquals("forall X.(X -> X)", f.toString)

  @Test def should_alpha_convert_type(): Unit =
    val X = TVar(Var("X"))
    val T = Arrow(X, X)
    val lt = Arrow(Arrow(X, X), X)
    val rt = Arrow(X, Arrow(X, X))

    assertEquals("(t2 -> t2)", alpha_conversion_type(Forall(X, T)).toString)
    assertEquals("((t3 -> t3) -> t3)", alpha_conversion_type(Forall(X, lt)).toString)
    assertEquals("(t4 -> (t4 -> t4))", alpha_conversion_type(Forall(X, rt)).toString)
