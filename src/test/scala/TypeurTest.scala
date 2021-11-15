import org.junit.{Before, Test}
import org.junit.Assert.*
import Typeur.*

class TypeurTest:
  @Before def initialize(): Unit =
    Var.last = 1
    TVar.last  = 1

  @Test def `lambda xyz.(xz)(yz)`(): Unit =
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val term = Abs(x, Abs(y, Abs(z, App(App(x, z), App(y, z)))))

    assertEquals("((x6 -> (x8 -> x7)) -> ((x6 -> x8) -> (x6 -> x7)))", infer(term).toString)


  @Test def `lambda x.xx`(): Unit =
    val x = Var("x")
    val term = Abs(x, App(x, x))

    try
      infer(term)
    catch
      case e: Error => assertEquals("Non typable", e.getMessage())

  @Test def `let f = (lambda x.x) in (f 1): N`(): Unit =
    val x = Var("x")
    val f = Var("f")
    val `1` = Nat(1)
    val app = App(f, `1`)
    val abs = Abs(x, x)

    val term = Letin(f, abs, app)

    assertEquals("N", infer(term).toString)

  @Test def `let f = (lambda x.x) in let g = (lambda xy.x) in g (f 1) (f t): N`(): Unit =
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
    val `let f` = Letin(f, `lambda x.x`, `let g`)

    val env: ENV = Map(t -> T)

    assertEquals("N", infer(`let f`, env).toString)

  @Test def `Head [Nil]: Not typable`: Unit =
    val head = Head(EOL())

    try {
      infer(head)
    } catch {
      case e: Error => assertEquals("Empty list not typable", e.getMessage())
    }

  @Test def `Head [1, Nil]: N`: Unit =
    val lst = Cons(Nat(1), EOL())
    val head = Head(lst)

    assertEquals("N", infer(head).toString)


  @Test def `Tail [1, [2, Nil]]: [N]`: Unit =
    val lst = Cons(Nat(1), Cons(Nat(2), EOL()))
    val tail = Tail(lst)

    assertEquals("[N]", infer(tail).toString)

  @Test def `Nil: Not typable`: Unit =
    val lst = EOL()

    try {
      infer(lst)
    } catch {
      case e: Error => assertEquals("Non typable", e.getMessage())
    }


  @Test def `[1, Nil]: [N]`: Unit =
    val lst = Cons(Nat(1), EOL())

    val env: ENV = Map()
    assertEquals("[N]", infer(lst).toString)

  @Test def `[1, 2, Nil]: [N]`: Unit =
    val lst = Cons(Nat(1), Cons(Nat(2), EOL()))

    val env: ENV = Map()
    assertEquals("[N]", infer(lst).toString)

  @Test def `[lambda x.x, lambda x.x, Nil]: [(x4 -> x4)]`: Unit =
    val abs = Abs(Var("x"), Var("x"))
    val lst = Cons(abs, Cons(abs, EOL()))

    val env: ENV = Map()
    assertEquals("[(x3 -> x3)]", infer(lst).toString)

  @Test def `1 op 1: N`: Unit =
    val add = Add(Nat(1), Nat(1))
    val diff = Diff(Nat(1), Nat(1))

    assertEquals("N", infer(add).toString);
    assertEquals("N", infer(diff).toString);


  @Test def `if 0 then 1 else 2: N`: Unit =
    val cond = Izte(Nat(0), Nat(1), Nat(2))

    assertEquals("N", infer(cond).toString)

  @Test def `if 0 then T else X: Non Typable`: Unit =
    val x = Var("x")
    val X = TVar(x)
    val t = Var("t")
    val T = TVar(t)
    val cond = Izte(Nat(0), t, x)

    val env: ENV = Map(t -> T, x -> X)

    try {
      infer(cond, env)
    } catch {
      case e: Error => assertEquals("Non typable", e.getMessage())
    }

  @Test def `if Nil then 1 else 2: N`: Unit =
    val cond = Iete(EOL(), Nat(1), Nat(2))

    assertEquals("N", infer(cond).toString)

  @Test def `if [1, Nil] then 1 else 2: N`: Unit =
    val lst = Cons(Nat(1), EOL())
    val cond = Iete(lst, Nat(1), Nat(2))

    assertEquals("N", infer(cond).toString)

  @Test def `if [1, Nil] then Head [2, Nil] else Head [3, Nil]: N`: Unit =
    val lst1 = Cons(Nat(1), EOL())
    val lst2 = Cons(Nat(2), EOL())
    val lst3 = Cons(Nat(3), EOL())

    val cond = Iete(lst1, Head(lst2), Head(lst3))

    assertEquals("N", infer(cond).toString)

  @Test def `if [1, Nil] then Tail [2, [3, Nil]] else Tail [3, [4, Nil]]: [N]`: Unit =
    val lst1 = Cons(Nat(1), EOL())
    val lst2 = Cons(Nat(2), Cons(Nat(3), EOL()))
    val lst3 = Cons(Nat(3), Cons(Nat(4), EOL()))

    val cond = Iete(lst1, Tail(lst2), Tail(lst3))

    assertEquals("[N]", infer(cond).toString)

  @Test def `fix (f: F -> F, 1: N): N`: Unit =
    val f = Var("f")

    val `F -> F` = Arrow(N(), N())
    val fix = Fix(f, Nat(1))

    val env: ENV = Map(f -> `F -> F`)
    assertEquals("N", infer(fix, env).toString)

  @Test def `fix (g: G, 1: N): Non typable`: Unit =
    val g = Var("g")
    val G = TVar(g)
    val fix = Fix(g, Nat(1))

    val env: ENV = Map(g -> G)

    try {
      infer(fix, env)
    } catch {
      case e: Error => assertEquals("Non typable", e.getMessage())
    }

  @Test def `fix (lambda x.x, t: T): T`: Unit =
    val x = Var("x")
    val abs = Abs(x, x)

    val t = Var("t")
    val T = TVar(Var("T"))

    val fix = Fix(abs, t)

    val env: ENV = Map(t -> T)

    assertEquals("T", infer(fix, env).toString)


  @Test def `unit : Unit`: Unit =
    val u = Void()

    assertEquals("Unit", infer(u).toString)

  @Test def `Ref x: Ref x`: Unit =
    val x = Var("x")
    val tx = TVar(x)

    val r = Ref(x)

    val env: ENV = Map(x -> tx)
    assertEquals("Ref x", infer(r, env).toString)

  @Test def `Deref(Ref x): x`: Unit =
    val x = Var("x")
    val tx = TVar(x)

    val r = Ref(x)
    val dr = Deref(r)

    val env: ENV = Map(x -> tx)
    assertEquals("x", infer(dr, env).toString)

  @Test def `x := y : Unit`: Unit =
    val x = Var("x")
    val tx = TVar(x)
    val y = Var("y")
    val ty = TVar(y)

    val r = Ref(x)
    val u = Assign(r, y)

    val env: ENV = Map(x -> tx, y -> ty)
    assertEquals("Unit", infer(u, env).toString)