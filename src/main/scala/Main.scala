import Typeur._

@main def hello: Unit =
  val x = Var("x")
  val y = Var("y")
  val z = Var("z")

  val term = Abs(x, Abs(y, Abs(z, App(App(x, z), App(y, z)))))

  println(typeur(term))

