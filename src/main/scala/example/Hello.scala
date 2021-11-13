package example

object Hello extends App:
  // main
  import Ast.*

  val p = program(
    defun(
      "main",
      Seq(),
      block(
        ident("x") := int(12),
        ident("y") := int(6),
        ident("z") := int(0),
        call("tarai", ident("x"), ident("y"), ident("z"))
      )
    ),
    defun(
      "tarai",
      Seq("x", "y", "z"),
      block(
        `if`(
          ident("x") <= ident("y"),
          ident("y"),
          block(
            ident("a") := call(
              "tarai",
              ident("x") - int(1),
              ident("y"),
              ident("z")
            ),
            ident("b") := call(
              "tarai",
              ident("y") - int(1),
              ident("z"),
              ident("x")
            ),
            ident("c") := call(
              "tarai",
              ident("z") - int(1),
              ident("x"),
              ident("y")
            ),
            call("tarai", ident("a"), ident("b"), ident("c"))
          )
        )
      )
    ),
    defun("infiniteLoop", Seq(), call("infiniteLoop")),
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
