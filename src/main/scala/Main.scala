@main def hello: Unit =
  val x = Var("x")
  val y = Var("y")
  val app = App(x, y)
  val abs = Abs("arg", x)

  print_term(x)
  print_term(y)
  print_term(app)
  print_term(abs)

