package example

object Hello extends App:
  // main
  import Ast.*

  val p = program(
    defun(
      "main",
      Seq(),
      block(
        ident("a") := int(5),
        call("factorial", ident("a"))
      )
    ),
    defun("triple", Seq("x"), ident("x") * int(3)),
    defun(
      "factorial",
      Seq("x"),
      block(
        `if`(
          ident("x") === int(1),
          int(1),
          ident("x") * call("factorial", ident("x") - int(1))
        )
      )
    )
  )

  println(Interpreter().callMain(p))
