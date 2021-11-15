package windymelt.toy.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import windymelt.toy.parser.Comparative

class ParserASTConverterSpec extends AnyFlatSpec with Matchers {
  import windymelt.toy.parser
  import Ast.*
  "Primary Conversion" should "convert identity" in {
    val ast = ParserASTConverter.given_Conversion_Primary_Expression(
      parser.Identifier("x")
    )
    ast shouldBe ident("x")
  }
  it should "convert integer literal" in {
    val ast = ParserASTConverter.given_Conversion_Primary_Expression(
      parser.Integer(42)
    )
    ast shouldBe int(42)
  }

  "Unary Conversion" should "convert ConcreteUnary" in {
    val ast = ParserASTConverter.given_Conversion_Unary_Expression(
      parser.ConcreteUnary(parser.Integer(42))
    )
    ast shouldBe unary(int(42))
  }
  it should "convert negative" in {
    val ast = ParserASTConverter.given_Conversion_Unary_Expression(
      parser.Negate(parser.Integer(42))
    )
    ast shouldBe unary(
      Ops.Neg,
      int(42)
    )
  }

  "Multiplicative Conversion" should "convert multiplicative" in {
    val ast = ParserASTConverter.given_Conversion_Multiplicative_Expression(
      parser.Multiplicative(
        parser.ConcreteUnary(parser.Integer(42)),
        Seq(parser.Op("*") -> parser.ConcreteUnary(parser.Integer(42)))
      )
    )
    ast shouldBe Expression.BinaryExpression(
      Ops.Mul,
      unary(int(42)),
      unary(int(42))
    )
  }
  it should "convert lone multiplicative" in {
    val ast = ParserASTConverter.given_Conversion_Multiplicative_Expression(
      parser.Multiplicative(
        parser.ConcreteUnary(parser.Integer(42))
      )
    )
    ast shouldBe unary(int(42))
  }

  "Additive Conversion" should "convert additive" in {
    val ast = ParserASTConverter.given_Conversion_Additive_Expression(
      parser.Additive(
        parser.Multiplicative(parser.ConcreteUnary(parser.Integer(42))),
        Seq(
          parser.Op("+") -> parser.Multiplicative(
            parser.ConcreteUnary(parser.Integer(42))
          )
        )
      )
    )
    ast shouldBe Expression.BinaryExpression(
      Ops.Add,
      unary(int(42)),
      unary(int(42))
    )
  }
  it should "convert lone additive" in {
    val ast = ParserASTConverter.given_Conversion_Additive_Expression(
      parser.Additive(
        parser.Multiplicative(parser.ConcreteUnary(parser.Integer(42)))
      )
    )
    ast shouldBe unary(int(42))
  }

  "Comparative Conversion" should "convert comparative" in {
    val ast = ParserASTConverter.given_Conversion_Comparative_Expression(
      parser.Comparative(
        parser.Additive(
          parser.Multiplicative(parser.ConcreteUnary(parser.Integer(42)))
        ),
        Seq(
          parser.Op("<") -> parser.Additive(
            parser.Multiplicative(parser.ConcreteUnary(parser.Integer(42)))
          )
        )
      )
    )
    ast shouldBe Expression.BinaryExpression(
      Ops.LT,
      unary(int(42)),
      unary(int(42))
    )
  }

  "Assignment Conversion" should "convert assignment" in {
    val ast = ParserASTConverter.given_Conversion_Assignment_Expression(
      parser.Assignment(
        parser.Identifier("x"),
        parser.Expression(
          parser.Comparative(
            parser.Additive(
              parser.Multiplicative(parser.ConcreteUnary(parser.Integer(42)))
            )
          )
        )
      )
    )
    ast shouldBe Expression.Assignment(
      "x",
      unary(int(42))
    )
  }

  "Function call Converter" should "convert function call" in {
    val ast = ParserASTConverter.given_Conversion_Primary_Expression(
      parser.FunctionCall(
        parser.Identifier("f"),
        Seq()
      )
    )
    ast shouldBe call("f")
  }
  it should "convert function call with argument" in {
    val comparative: Comparative =
      parser.Comparative(
        parser.Additive(
          parser.Multiplicative(parser.ConcreteUnary(parser.Integer(42)))
        )
      )
    val ast = ParserASTConverter.given_Conversion_Primary_Expression(
      parser.FunctionCall(
        parser.Identifier("f"),
        Seq(parser.Expression(comparative))
      )
    )
    ast shouldBe call("f", unary(int(42)))
  }
}
