import org.junit.{Before, Test}
import org.junit.Assert.*
import Typeur.*

class TypeurTest:
  @Before def initialize(): Unit =
    Var.last = 1

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

  @Test def `let f = (lambda x.x) in (f 1) : N`(): Unit =
    val x = Var("x")
    val f = Var("f")
    val `1` = Nat(1)
    val app = App(f, `1`)
    val abs = Abs(x, x)

    val term = Letin(f, abs, app)

    assertEquals("N", infer(term).toString)

  @Test def `let f = (lambda x.x) in let g = (lambda xy.x) in g (f 1) (f t) : N`(): Unit =
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
      case e: Error => assertEquals("Empty list not typable", e.getMessage())
    }

  @Test def `[1, Nil]: [N]`: Unit =
    val lst = Cons(Nat(1), EOL())

    val env: ENV = Map()
    assertEquals("[N]", infer(lst).toString)

  @Test def `[1, 2, Nil]: [N]`: Unit =
    val lst = Cons(Nat(1), Cons(Nat(2), EOL()))

    val env: ENV = Map()
    assertEquals("[N]", infer(lst).toString)