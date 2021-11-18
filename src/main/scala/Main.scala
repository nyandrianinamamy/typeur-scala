import Typeur._
import scala.language.postfixOps

@main def hello: Unit =
  println("++++++++++++++")
  println("Hello !")
  println("You can write expressions in the Main file if you want to try out the type inference system.")
  println("However, if you just want to see how everything is supposed to be done")
  println("I invite you to see and run the TypeurTest located at src/test/scala/ file which demonstrates all of the type system capabilities")
  println("++++++++++++++")
  println("$ sbt")
  println("> testOnly TypeurTest")
  println("++++++++++++++")
  println("Have a nice day")
