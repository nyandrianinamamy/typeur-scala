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

  @Test def substitue_in_equations(): Unit =
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    val tx = TVar(x)
    val ty = TVar(y)
    val tz = TVar(z)

    val l: List[Eq] = Eq(tx, ty) :: Eq(ty, tx) :: List()

    substitue_partout(x, tz, l) match {
      case r: List[Eq] => assertEquals(Eq(tz, ty) :: Eq(ty, tz) :: List(), r)
    }

    substitue_partout(y, tz, l) match {
      case r: List[Eq] => assertEquals(Eq(tx, tz) :: Eq(tz, tx) :: List(), r)
    }

  @Test def `should_remove_X_=_Y_in_eqs`(): Unit =
    val x = Var("x")
    val y = Var("y")
    val tx = TVar(x)
    val ty = TVar(y)
    val eq = Eq(tx, tx)
    val eq1 = Eq(tx, tx)
    val neq = Eq(tx, ty)

    val l = eq :: eq1 :: neq :: List[Eq]()

    try {
      unification_etape(l)
    } catch {
      case e: Error => assertEquals("No more equations in list", e.getMessage)
    }

  @Test def `should_remove_X_=_Td_and_eqs[X/Td]`(): Unit =
    val X = Var("X")
    val d = Var("d")
    val Z = Var("Z")
    val TX = TVar(X)
    val Td = TVar(d)
    val TZ = TVar(Z)
    val `TX -> TX` = Arrow(TX, TX)
    val `TZ -> TZ` = Arrow(TZ, TZ)
    val `Td -> Td` = Arrow(Td, Td)

    val eqs: List[Eq] = Eq(TX, Td) :: Eq(`TX -> TX`, `TZ -> TZ`) :: List()

    assertFalse(occur_check(X, Td)) // X should not appear in Td

    try {
      unification_etape(eqs)
    } catch {
      case e: Error => assertEquals("No more equations in list", e.getMessage)
    }


  @Test def `should_throw_on X = X -> X`(): Unit =
    val X = Var("X")
    val TX = TVar(X)
    val `TX -> TX` = Arrow(TX, TX)

    val eqs: List[Eq] = Eq(TX, `TX -> TX`) :: List()

    try {
      unification_etape(eqs)
    } catch {
      case e: Error => assertEquals("Cannot unify", e.getMessage)
    }

  @Test def should_return_solution(): Unit =
    val x = Var("x")
    val tx = TVar(x)

    val eqs: List[Eq] = Eq(TVar.t0, tx) :: List()

    unification_etape(eqs) match
      case l: List[Eq] => assertEquals(Eq(TVar.t0, tx) :: List(), l)
