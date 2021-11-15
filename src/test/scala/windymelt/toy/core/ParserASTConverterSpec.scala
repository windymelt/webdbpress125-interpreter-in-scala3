package windymelt.toy.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
    ast shouldBe Expression.UnaryExpression(None, int(42))
  }
  it should "convert negative" in {
    val ast = ParserASTConverter.given_Conversion_Unary_Expression(
      parser.Negate(parser.Integer(42))
    )
    ast shouldBe Expression.UnaryExpression(
      Some(Ops.Neg),
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
      int(42),
      int(42)
    )
  }
  it should "convert lone multiplicative" in {
    val ast = ParserASTConverter.given_Conversion_Multiplicative_Expression(
      parser.Multiplicative(
        parser.ConcreteUnary(parser.Integer(42)),
        Seq.empty
      )
    )
    ast shouldBe int(42)
  }
  // it should "convert function call" in {
  //   val ast = ParserASTConverter.given_Conversion_Primary_Expression(
  //     parser.FunctionCall(
  //       parser.Identifier("f"),
  //       Seq(
  //         parser.Expression(
  //           parser.Comparative(
  //             parser.Additive(
  //               parser.Multiplicative(
  //                 parser.ConcreteUnary(parser.Integer(42)),
  //                 Seq()
  //               ),
  //               Seq()
  //             )
  //           )
  //         )
  //       )
  //     )
  //   )
  //   ast shouldBe call(ident("f"), Seq(int(42)))
  // }
}
