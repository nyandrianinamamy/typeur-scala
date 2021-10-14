import org.junit.Test
import org.junit.Assert.*

class UnificationTest:
  @Test def occurence_in_var(): Unit =
    val x = Var("x")
    val t = TVar(x)

    assertTrue(occur_check(x, t))