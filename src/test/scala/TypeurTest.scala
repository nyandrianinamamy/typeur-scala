import org.junit.Test
import org.junit.Assert.*
import Typeur._

class TypeurTest:
  @Test def `lambda xyz.(xz)(yz)`(): Unit =
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val term = Abs(x, Abs(y, Abs(z, App(App(x, z), App(y, z)))))

    assertEquals("Typable avec le type ((x6 -> (x8 -> x7)) -> ((x6 -> x8) -> (x6 -> x7)))", infer(term))


  @Test def `lambda x.xx`(): Unit =
    val x = Var("x")

    val term = Abs(x, App(x, x))

    assertEquals("Non typable", infer(term))