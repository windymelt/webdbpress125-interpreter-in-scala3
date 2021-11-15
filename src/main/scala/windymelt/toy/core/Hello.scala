package windymelt.toy.core

import windymelt.toy.parser.TopLevelDefinition

object Hello extends App:
  // main
  import Ast.*

  import windymelt.toy.parser.ASTParser
  val code = """
  define main() {
    println(1 + 2)
  }
  """.strip
  val p: Option[Seq[TopLevelDefinition]] = ASTParser(code)
  println(p)
  import scala.language.implicitConversions
  import windymelt.toy.core.ParserASTConverter.given
  val pp: Option[Program] = p.map { given_Conversion_Seq_Program }
  println(pp)

  p match {
    case Some(prog) =>
      Interpreter().callMain(prog)
    case None => println("parse error")
  }
//Interpreter().callMain(p)

// val p = program(
//   defun(
//     "main",
//     Seq(),
//     block(
//       $("x") := int(6),
//       $("y") := int(3),
//       $("z") := int(0),
//       call("tarai", $("x"), $("y"), $("z"))
//     )
//   ),
//   defun(
//     "tarai",
//     Seq("x", "y", "z"),
//     `if`(
//       $("x") <= $("y"),
//       $("y"),
//       block(
//         $("a") := call("tarai", $("x") - int(1), $("y"), $("z")),
//         $("b") := call("tarai", $("y") - int(1), $("z"), $("x")),
//         $("c") := call("tarai", $("z") - int(1), $("x"), $("y")),
//         call("tarai", $("a"), $("b"), $("c"))
//       )
//     )
//   ),
//   defun("infiniteLoop", Seq(), call("infiniteLoop")),
//   defun("triple", Seq("x"), $("x") * int(3)),
//   defun(
//     "factorial",
//     Seq("x"),
//     block(
//       `if`(
//         $("x") === int(1),
//         int(1),
//         $("x") * call("factorial", $("x") - int(1))
//       )
//     )
//   )
// )

//println(Interpreter().callMain(p))
