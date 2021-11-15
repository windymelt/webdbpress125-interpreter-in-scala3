package windymelt.toy.parser

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import scala.util.Success
import scala.util.Try

class ASTParserSpec extends AnyFlatSpec with should.Matchers {
  "identifier" should "parse alpha" in {
    val input = "foo"
    val result = new ASTParser(input).identifier.run().toOption
    result should be(defined)
  }
  // function
  "functionCall" should "parse f(x)" in {
    val input = "f(x)"
    val result = new ASTParser(input).functionCall.run().toOption
    result should be(defined)
  }
  it should "parse f(x,y)" in {
    val input = "f(x,y)"
    val result = new ASTParser(input).functionCall.run().toOption
    result should be(defined)
  }
  it should "parse f(x, y)" in {
    val input = "f(x, y)"
    val result = new ASTParser(input).functionCall.run().toOption
    result should be(defined)
  }
  it should "parse f(1, 2, 3)" in {
    val input = "f(1, 2, 3)"
    val result = new ASTParser(input).functionCall.run().toOption
    result should be(defined)
  }

  "integer" should "parse 42" in {
    val input = "42"
    val result = new ASTParser(input).integer.run().toOption
    result should be(defined)
    result.get should be(Integer(42))
  }
  it should "parse -42" in {
    val input = "-42"
    val result = new ASTParser(input).integer.run().toOption
    result should be(defined)
    result.get should be(Integer(-42))
  }

  "unary" should "parse -f(x)" in {
    val input = "-f(x)"
    val result = new ASTParser(input).unary.run().toOption
    result should be(defined)
  }

  "multiplicative" should "parse 1 * 2 * 3" in {
    val input = "1 * 2 * 3"
    val result = new ASTParser(input).multiplicative.run().toOption
    result should be(defined)
  }

  "additive" should "parse 1 + 2 * 3" in {
    val input = "1 + 2 * 3"
    val result = new ASTParser(input).additive.run().toOption
    result should be(defined)
  }

  "comparative" should "parse 1 < 2" in {
    val input = "1 < 2"
    val result = new ASTParser(input).comparative.run().toOption
    result should be(defined)
  }
  it should "parse x <= y" in {
    val input = "x <= y"
    val result = new ASTParser(input).comparative.run().toOption
    result should be(defined)
  }

  "expression" should "parse (3 * 2 * 1) + 4 > 10" in {
    val input = "(3 * 2 * 1) + 4 > 10"
    val result = new ASTParser(input).expression.run().toOption
    result should be(defined)
  }

  "assignment" should "parse x = 1" in {
    val input = "x = 1;"
    val result = new ASTParser(input).assignment.run().toOption
    result should be(defined)
  }

  "blockExpression" should "parse { x = 1; }" in {
    val input = "{ x = 1; }"
    val result = new ASTParser(input).blockExpression.run().toOption
    result should be(defined)
  }

  "ifExpression" should "parse if (x == 0) { println(0) } else { println(-x) }" in {
    val input = "if (x == 0) { println(0) } else { println(-x) }"
    val result = new ASTParser(input).ifExpression.run().toOption
    result should be(defined)
  }

  "whileExpression" should "parse while (x > 0) { x = 1; }" in {
    val input = "while (x > 0) { x = 1; }"
    val result = new ASTParser(input).whileExpression.run().toOption
    result should be(defined)
  }
  it should "parse while (1 == 1) {}" in {
    val input = "while (1 == 1) {}"
    val result = new ASTParser(input).whileExpression.run().toOption
    result should be(defined)
  }

  "globalVariableDefinition" should "parse global x = 1" in {
    val input = "global x = 1"
    val result = new ASTParser(input).globalVariableDefinition.run().toOption
    result should be(defined)
  }

  "functionDefinition" should "parse define tak" in {
    val input = """
    define tak(x, y, z) {
        if (x < y) {
            z = x;
            tak(y, z, x);
        } else {
            z = y;
        }
    }
    """.strip
    val result = new ASTParser(input).functionDefinition.run().toOption
    result should be(defined)
  }
  it should "parse define f() { 1; }" in {
    val input = """
    define f() { 1; }
    """.strip
    val result = new ASTParser(input).functionDefinition.run().toOption
    result should be(defined)
  }
}
