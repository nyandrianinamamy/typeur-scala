import org.junit.Test
import org.junit.Assert.*

class TermTest:
  @Test def shouldGenerateDifferentVar(): Unit =
    val v1: Var = Var.fresh_var()
    val v2: Var = Var.fresh_var()
    assertNotEquals(v1.name, v2.name)

  @Test def shouldAlphaConvertVar(): Unit =
    var old: Var = Var("x")
    var new_var: Var = barendregt(old) match
      case Var(x) => Var(x)

    assertNotEquals(old.name, new_var.name)
    assertNotEquals(old, new_var)

  @Test def shouldAlphaConvertAbs(): Unit =
    var x = Var("x")
    var abs = Abs("x", x)

    var new_abs: Abs = barendregt(abs) match
      case Abs(arg, body) => Abs(arg, body)

    assertNotEquals(abs, new_abs)
    assertNotEquals(abs.arg, new_abs.arg)
    assertNotEquals(abs.body, new_abs.body)

  @Test def shouldAlphaConvertApp(): Unit =
    var x = Var("x")
    var y = Var("y")
    var app = App(x, y)

    var new_app: App = barendregt(app) match
      case App(term1, term2) => App(term1, term2)

    var new_var: Var = new_app.term2 match
      case Var(x) => Var(x)

    assertNotEquals(app, new_app)
    assertNotEquals(app.term2, new_app.term2)
    assertNotEquals(y.name, new_var.name)

