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

  @Test def substitue_in_tvar(): Unit =
    val x = Var("x")
    val y = Var("y")

    val tx = TVar(x)
    val ty = TVar(y)

    substitue(x, ty, tx) match
      case v: TVar => v.equals(ty)
      case _ => assert(false)

  @Test def substitue_in_arrow(): Unit =
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val tx = TVar(x)
    val ty = TVar(y)
    val tz = TVar(z)

    val `tx->ty` = Arrow(tx, ty)

    substitue(x, tz, `tx->ty`) match
      case Arrow(arg, res) =>
        assertTrue(arg.equals(tz))

        substitue(y, tz, Arrow(arg, res)) match
          case Arrow(arg1, res1) => assertTrue(res1.equals(tz))


  @Test def should_remove_same_type_in_eq(): Unit =
    val x = Var("x")
    val y = Var("y")
    val tx = TVar(x)
    val ty = TVar(y)
    val eq = Eq(tx, tx)
    val eq1 = Eq(tx, tx)
    val neq = Eq(tx, ty)

    val l = eq :: eq1 :: neq :: List[Eq]()

    unification_etape(l, 0) match {
      case h :: t => assertTrue(h.equals(neq))
      case nil => assertTrue(false)
    }