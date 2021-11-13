package example

object Hello extends App:
  // main
  import Ast.*

  println(Interpreter().interpret((int(10) * int(2)) + int(5)))
