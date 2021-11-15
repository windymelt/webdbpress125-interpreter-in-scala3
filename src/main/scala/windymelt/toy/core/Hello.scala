package windymelt.toy.core

import windymelt.toy.parser.TopLevelDefinition

object Hello extends App:
  // main
  import Ast.*

  import windymelt.toy.parser.ASTParser
  val code = """
  define main() {
    println(tarai(12,6,0))
  }
  define tarai(x, y, z) {
    if (x <= y) { y; }
    else {
      tarai(tarai(x - 1, y, z),
            tarai(y - 1, z, x),
            tarai(z - 1, x, y));
    }
  }
  """.strip
  val p: Option[Seq[TopLevelDefinition]] = ASTParser(code)
  println(p)
  import scala.language.implicitConversions
  import windymelt.toy.core.ParserASTConverter.given
  val pp: Option[Program] = p.map { given_Conversion_Seq_Program }
  println(pp)

  println("*** running program ***")

  p match
    case Some(prog) =>
      Interpreter().callMain(prog)
    case None => println("parse error")
