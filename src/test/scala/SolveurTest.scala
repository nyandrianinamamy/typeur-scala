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


  @Test def should_fail_on_conflict(): Unit =
    val X = TVar(Var("X"))
    val Y = TVar(Var("Y"))

    val eqs = Eq(TVar.t0, X) :: Eq(TVar.t0, Y) :: List()

    try {
      solve(eqs)
    } catch {
      case e: Error => assertEquals(s"${TVar.t0} has conflicting types $X and $Y", e.getMessage())
    }