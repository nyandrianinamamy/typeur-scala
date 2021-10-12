@main def hello: Unit =
  val x = Var("x")
  val y = Var("y")
  val I = Abs(x, x)
  val K = Abs(x, Abs(y, x))

  val KII = App(App(K, I), I)

  print_term(KII)

  alpha_conversion(KII) match
    case app: App => print_term(app)