@main def hello: Unit =
  val x = Var("x")
  val y = Var("y")
  val I = Abs(x, x)
  val K = Abs(x, Abs(y, x))

  val KII = App(App(K, I), I)

  print_term(KII)

  alpha_conversion(KII) match
    case app: App => print_term(app)

  val t0 = TVar(Var("x0"))

  val env: Map[Var, Type] = Map(x -> t0)

  generate_equation(KII, t0, env) match
    case l: List[Eq] => l foreach {
      case eq: Eq => println(eq.toString())
    }