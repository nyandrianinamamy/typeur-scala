import Typeur._

@main def hello: Unit =
  val x = Var("x")
  val I = Abs(x, x)

  println(typeur(I))

