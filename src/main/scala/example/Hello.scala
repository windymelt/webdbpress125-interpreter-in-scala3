package example

object Hello extends App:
  // main
  import Ast.*

  val p = program(
    defun(
      "main",
      Seq(),
      block(
        $("x") := int(12),
        $("y") := int(6),
        $("z") := int(0),
        call("tarai", $("x"), $("y"), $("z"))
      )
    ),
    defun(
      "tarai",
      Seq("x", "y", "z"),
      `if`(
        $("x") <= $("y"),
        $("y"),
        block(
          $("a") := call("tarai", $("x") - int(1), $("y"), $("z")),
          $("b") := call("tarai", $("y") - int(1), $("z"), $("x")),
          $("c") := call("tarai", $("z") - int(1), $("x"), $("y")),
          call("tarai", $("a"), $("b"), $("c"))
        )
      )
    ),
    defun("infiniteLoop", Seq(), call("infiniteLoop")),
    defun("triple", Seq("x"), $("x") * int(3)),
    defun(
      "factorial",
      Seq("x"),
      block(
        `if`(
          $("x") === int(1),
          int(1),
          $("x") * call("factorial", $("x") - int(1))
        )
      )
    )
  )

  println(Interpreter().callMain(p))
