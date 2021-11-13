package example

object Hello extends App:
  // main
  import Ast.*

  val expr = block(
    ident("x") := int(1),
    ident("y") := int(2),
    ident("z") := ident("x") + ident("y"),
    ident("z") := ident("z") * int(2),
    `if`(ident("z") > int(5), int(1), int(0))
  )
  println(Interpreter().interpret(expr))
