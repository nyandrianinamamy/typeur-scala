import org.junit.Test
import org.junit.Assert.*

class TypeTest:
  @Test def should_print_type(): Unit =
    val x = Var("x")
    val tx = TVar(x)
    val tarrow = Arrow(tx, tx)

    print_type(tx) match
      case str: String => assertEquals("x", str)

    print_type(tarrow) match
      case str: String => assertEquals("(x -> x)", str)

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