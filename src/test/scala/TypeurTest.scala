import org.junit.{Before, Test}
import org.junit.Assert.*
import scala.util.{Try, Success, Failure}
import Typeur.*

class TypeurTest:
  @Before def initialize(): Unit =
    Var.last = 1
    TVar.last = 1

    println("=============================================")

  @Test def `lambda xyz.(xz)(yz) : ('a -> ('b -> 'c)) -> (('a -> 'b) -> ('a -> 'c))`: Unit =
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val term = Abs(x, Abs(y, Abs(z, App(App(x, z), App(y, z)))))

    assertPass("((x6 -> (x8 -> x7)) -> ((x6 -> x8) -> (x6 -> x7)))", term)

  @Test def `lambda x.xx : Not Typable`: Unit =
    val x = Var("x")
    val term = Abs(x, App(x, x))

    assertFail("(x2 -> x3) is not unifiable with x2", term)

  @Test def `let f = lambda x.x in f f: 'a->'a`: Unit =
    val x = Var("x")
    val f = Var("f")
    val abs = Abs(x, x)
    val app = App(f, f)

    val term = Letin(f, abs, app)

    assertPass("(t2 -> t2)", term)

  @Test def `let f = (lambda x.x) in (f 1): N`: Unit =
    val x = Var("x")
    val f = Var("f")
    val `1` = Nat(1)
    val app = App(f, `1`)
    val abs = Abs(x, x)

    val term = Letin(f, abs, app)

    assertPass("N", term)

  @Test def `let f = (lambda x.x) in let g = (lambda xy.x) in g (f 1) (f t): N`: Unit =
    val x = Var("x")
    val f = Var("f")
    val t = Var("t")
    val g = Var("g")
    val y = Var("y")
    val T = TVar(t)
    val `1` = Nat(1)
    val `f 1` = App(f, `1`)
    val `lambda x.x` = Abs(x, x)
    val `lambda xy.x` = Abs(x, Abs(y, x))
    val `f t` = App(f, t)
    val `g (f 1) (f t)` = App(App(g, `f 1`), `f t`)

    val `let g` = Letin(g, `lambda xy.x`, `g (f 1) (f t)`)
    val term = Letin(f, `lambda x.x`, `let g`)

    val env: ENV = Map(t -> T)

    assertPass("N", term, env)

  /**
   * A constant debate, whether or not Head [Nil] should be typable or not
   * Here it is of type [Nil] because we also need EmptyList to be typable
   * in the case of If [Nil] Then Else
   */
  @Test def `Head [Nil]: [Nil]`: Unit =
    val term = Head(EOL())

    assertPass("[Nil]", term)

  @Test def `Head [1, Nil]: N`: Unit =
    val lst = Cons(Nat(1), EOL())
    val term = Head(lst)

    assertPass("N", term)

  @Test def `Tail [1, [2, Nil]]: [N]`: Unit =
    val lst = Cons(Nat(1), Cons(Nat(2), EOL()))
    val term = Tail(lst)

    assertPass("[N]", term)

  @Test def `[]: [Nil]`: Unit =
    val term = EOL()

    assertPass("[Nil]", term)

  @Test def `[1, Nil]: [N]`: Unit =
    val term = Cons(Nat(1), EOL())

    assertPass("[N]", term)

  @Test def `[1, 2, Nil]: [N]`: Unit =
    val term = Cons(Nat(1), Cons(Nat(2), EOL()))

    assertPass("[N]", term)

  @Test def `[lambda x.x, lambda x.x, Nil]: [(x3 -> x3)]`: Unit =
    val abs = Abs(Var("x"), Var("x"))
    val term = Cons(abs, Cons(abs, EOL()))

    assertPass("[(x3 -> x3)]", term)

  @Test def `1 + 1: N`: Unit =
    val term = Add(Nat(1), Nat(1))

    assertPass("N", term)

  @Test def `1 - 1: N`: Unit =
    val term = Diff(Nat(1), Nat(1))

    assertPass("N", term)

  @Test def `if 0 then 1 else 2: N`: Unit =
    val term = Izte(Nat(0), Nat(1), Nat(2))

    assertPass("N", term)

  @Test def `if 0 then T else X: Non Typable`: Unit =
    val x = Var("x")
    val X = TVar(x)
    val t = Var("t")
    val T = TVar(t)
    val term = Izte(Nat(0), t, x)

    val env: ENV = Map(t -> T, x -> X)

    assertFail("x0 has conflicting types t and x", term, env)

  @Test def `if [Nil] then 1 else 2: N`: Unit =
    val term = Iete(EOL(), Nat(1), Nat(2))

    assertPass("N", term)

  @Test def `if [1, Nil] then 1 else 2: N`: Unit =
    val lst = Cons(Nat(1), EOL())
    val term = Iete(lst, Nat(1), Nat(2))

    assertPass("N", term)

  @Test def `if [1, Nil] then Head [2, Nil] else Head [3, Nil]: N`: Unit =
    val lst1 = Cons(Nat(1), EOL())
    val lst2 = Cons(Nat(2), EOL())
    val lst3 = Cons(Nat(3), EOL())

    val term = Iete(lst1, Head(lst2), Head(lst3))

    assertPass("N", term)

  @Test def `if [1, Nil] then Tail [2, [3, Nil]] else Tail [3, [4, Nil]]: [N]`: Unit =
    val lst1 = Cons(Nat(1), EOL())
    val lst2 = Cons(Nat(2), Cons(Nat(3), EOL()))
    val lst3 = Cons(Nat(3), Cons(Nat(4), EOL()))

    val term = Iete(lst1, Tail(lst2), Tail(lst3))

    assertPass("[N]", term)

  @Test def `fix (f: F -> F, 1: N): N`: Unit =
    val f = Var("f")

    val `F -> F` = Arrow(N(), N())
    val term = Fix(f, Nat(1))

    val env: ENV = Map(f -> `F -> F`)

    assertPass("N", term, env)

  @Test def `fix (lambda x.x, t: T): T`: Unit =
    val x = Var("x")
    val abs = Abs(x, x)

    val t = Var("t")
    val T = TVar(Var("T"))

    val term = Fix(abs, t)

    val env: ENV = Map(t -> T)

    assertPass("T", term, env)

  @Test def `fix (lambda f. lambda n. if 0 then 1 else Add(n, App(f, Add(n, 1)), 1: N): N`: Unit =
    val n = Var("n")
    val f = Var("f")
    val `n+1` = Add(n, Nat(1))
    val `f n+1` = App(f, `n+1`)
    val `n + (f n+1)` = Add(n, `f n+1`)
    val cond = Izte(Nat(0), Nat(1), `n + (f n+1)`)
    val `lambda n. cond` = Abs(n, cond)
    val factorial = Abs(f, `lambda n. cond`)

    val term = Fix(factorial, Nat(1))

    assertPass("N", term)

  @Test def `unit : Unit`: Unit =
    val term = Void()

    assertPass("Unit", term)

  @Test def `Ref x: Ref x`: Unit =
    val x = Var("x")
    val tx = TVar(x)

    val term = Ref(x)

    val env: ENV = Map(x -> tx)

    assertPass("Ref x", term, env)

  @Test def `Deref(Ref x): x`: Unit =
    val x = Var("x")
    val tx = TVar(x)

    val ref = Ref(x)
    val term = Deref(ref)

    val env: ENV = Map(x -> tx)

    assertPass("x", term, env)

  @Test def `Deref(x): Not typable`: Unit =
    val x = Var("x")
    val tx = TVar(x)

    val term = Deref(x)

    val env: ENV = Map(x -> tx)

    assertFail("Ref x2 is not unifiable with x", term, env)

  @Test def `Ref x := y : Unit`: Unit =
    val x = Var("x")
    val tx = TVar(x)
    val y = Var("y")
    val ty = TVar(y)

    val ref = Ref(x)
    val term = Assign(ref, y)

    val env: ENV = Map(x -> tx, y -> ty)

    assertPass("Unit", term, env)

  @Test def `let x = lambda y.y in x : Vx2.(x2 -> x2)`: Unit =
    val y = Var("y")
    val abs = Abs(y, y)
    val x = Var("x")

    val term = Letin(x, abs, x)

    assertPass("forall x2.(x2 -> x2)", term)

  def assertPass(expected_type: String, term: Term, env: ENV = Map()): Unit =
    println(s"Term: $term")
    infer(term, env) match
      case Success(infered_type: Type) =>
        println(s"Infered type: $infered_type")
        assertEquals(expected_type, infered_type.toString)

      case Failure(exception: Exception) =>
        fail(exception.getMessage())

      case Failure(_) =>
        fail()

  def assertFail(expected_failure: String, term: Term, env: ENV = Map()): Unit =
    println(s"Term: $term")
    infer(term, env) match
      case Success(infered_type: Type) =>
        fail(s"$term should not be typable")

      case Failure(exception: Exception) =>
        println(s"Infered type: Not typable")
        println(s"Reason: $expected_failure")
        assertEquals(expected_failure, exception.getMessage())

      case Failure(_) =>
        fail()