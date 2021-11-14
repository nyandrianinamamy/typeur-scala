import org.junit.Assert.assertEquals
import org.junit.{Before, Test}
import Solveur.solve

class SolveurTest:
  @Before def initialize(): Unit =
    Var.last = 1

  @Test def should_return_solution(): Unit =
    val X = TVar(Var("X"))
    val eqs = Eq(TVar.t0, X) :: List()

    assertEquals(X, solve(eqs))