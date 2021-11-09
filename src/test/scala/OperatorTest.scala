import org.junit.Assert.assertEquals
import org.junit.Test

class OperatorTest:
  @Test def should_print_izte(): Unit =
    val x = Var("x")
    val y = Var("y")
    val n = N(0)
    val term1 = Abs(x, x)
    val term2 = Abs(y, y)

    val eval = Izte(n, term1, term2)

    assertEquals("If 0 then Abs(Var(x),Var(x)) else Abs(Var(y),Var(y))", eval.toString)

  @Test def should_print_iete(): Unit =
    val x = Var("x")
    val y = Var("y")
    val lst = Lst(x, EOL())
    val term1 = Abs(x, x)
    val term2 = Abs(y, y)

    val eval = Iete(lst, term1, term2)

    assertEquals("If Var(x),EOL then Abs(Var(x),Var(x)) else Abs(Var(y),Var(y))", eval.toString)

