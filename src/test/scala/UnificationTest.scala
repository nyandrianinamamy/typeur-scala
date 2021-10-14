import org.junit.Test
import org.junit.Assert.*

class UnificationTest:
  @Test def occurence_in_tvar(): Unit =
    val x = Var("x")
    val t = TVar(x)

    assertTrue(occur_check(x, t))

  @Test def occurence_in_arrow(): Unit =
    val x = Var("x")
    val y = Var("y")
    val tx = TVar(x)
    val ty = TVar(y)
    val `tx->ty` = Arrow(tx, ty)

    assertTrue(occur_check(x, tx))
    assertTrue(occur_check(y, ty))
    assertFalse(occur_check(x, ty))
    assertFalse(occur_check(y, tx))
    assertTrue(occur_check(x, `tx->ty`))
    assertTrue(occur_check(y, `tx->ty`))