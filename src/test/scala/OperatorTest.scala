import org.junit.Assert.assertEquals
import org.junit.{Before, Test}

class OperatorTest:
  @Before def initialize(): Unit =
    Var.last = 1
    TVar.last = 1

  @Test def should_print_izte(): Unit =
    val x = Var("x")
    val y = Var("y")
    val n = Nat(0)
    val term1 = Abs(x, x)
    val term2 = Abs(y, y)

    val eval = Izte(n, term1, term2)

    assertEquals("If 0 then fun (x) -> x else fun (y) -> y", eval.toString)

  @Test def should_print_iete(): Unit =
    val x = Var("x")
    val y = Var("y")
    val lst = EOL()
    val term1 = Abs(x, x)
    val term2 = Abs(y, y)

    val eval = Iete(lst, term1, term2)

    assertEquals("If Nil then fun (x) -> x else fun (y) -> y", eval.toString)

  @Test def should_print_fix(): Unit =
    val x = Var("x")
    val y = Var("y")
    val f = Var("f")
    val term = Abs(x, App(f, x))
    val eval = Fix(f, term)

    assertEquals("fix (f, fun (x) -> (f x))", eval.toString)

