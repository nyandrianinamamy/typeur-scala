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

  @Test def `let f = (lambda x.x) in (f 1)`(): Unit =
    val x = Var("x")
    val f = Var("f")
    val `1` = Nat(1)
    val app = App(f, `1`)
    val abs = Abs(x, x)

    val term = Letin(f, abs, app)

    assertEquals("N", infer(term).toString)