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
