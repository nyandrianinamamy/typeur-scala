import org.junit.{Before, Test}
import org.junit.Assert.*


class UnificationTest:
  @Before def initialize(): Unit =
    Var.last = 1
    TVar.last = 1

  @Test def should_skip_t0: Unit =
    val X = TVar(Var("X"))
    val Y = TVar(Var("Y"))
    val eqs = Eq(TVar.t0, X) :: Eq(TVar.t0, Y) :: List()

    val unified = unification_etape(eqs)

    assertEquals("List(x0 = X, x0 = Y)", unified.toString)

  @Test def should_remove_equal_types: Unit =
    val X = TVar(Var("X"))
    val tlst = TLst(X)
    val eqs = Eq(X, X) :: Eq(tlst, tlst) :: List()

    val unified = unification_etape(eqs)

    assertTrue("Equal types should be removed", unified.isEmpty)

  @Test def should_substitute_X_by_Td: Unit =
    val X = TVar(Var("X"))
    val Y = TVar(Var("Y"))
    val Z = TVar(Var("Z"))
    val A = TVar(Var("A"))
    val `X -> X` = Arrow(X, X)
    val eqs = Eq(X, Y) :: Eq(`X -> X`, `X -> X`) :: List()

    val unified = unification_etape(eqs)

    assertEquals("List()", unified.toString)

  @Test def should_not_substitute_X_by_Td: Unit =
    val X = TVar(Var("X"))
    val Y = TVar(Var("Y"))
    val `X -> X` = Arrow(X, X)
    val `Y -> X` = Arrow(Y, X)
    val eqs = Eq(X, `Y -> X`) :: List()

    try {
      val unified = unification_etape(eqs)
    } catch {
      case e: Error => assertEquals("X = (Y -> X) non unifiable, (Y -> X) contains X", e.getMessage())
    }


  @Test def should_remove_arrow_types_and_create_equal_types: Unit =
    val A = TVar(Var("A"))
    val B = TVar(Var("B"))

    val `X -> Y` = Arrow(TVar.t0, TVar.t0) // To avoid removal of X = A after
    val `A -> B` = Arrow(A, B)

    val eqs = Eq(`X -> Y`, `A -> B`) :: List()

    val unified = unification_etape(eqs)

    assertEquals("List(x0 = A, x0 = B)", unified.toString)


  @Test def should_unify_forall(): Unit =
    val X = Var("X")
    val tx = TVar(X)
    val t = TVar(Var("t"))
    val s = TVar(Var("s"))
    val forall = Forall(TVar(X), TVar(X))

    val eqs: List[Eq] = Eq(forall, s) :: Eq(t, forall) :: Eq(TVar.t0, tx) :: Nil;

    assertEquals("List(x0 = X)", unification_etape(eqs).toString)

  @Test def occurence_in_tvar(): Unit =
    val x = Var("x")
    val t = TVar(x)

    assertTrue(t contains x)

  @Test def occurence_in_arrow(): Unit =
    val x = Var("x")
    val y = Var("y")
    val tx = TVar(x)
    val ty = TVar(y)
    val `tx->ty` = Arrow(tx, ty)

    assertTrue(tx contains x)
    assertTrue(ty contains y)
    assertFalse(ty contains x)
    assertFalse(tx contains y)
    assertTrue(`tx->ty` contains x)
    assertTrue(`tx->ty` contains y)

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