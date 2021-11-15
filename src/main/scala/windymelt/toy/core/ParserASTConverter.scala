package windymelt.toy.core

import windymelt.toy.parser
import windymelt.toy.core.Ast._
import windymelt.toy.parser.Identifier

object ParserASTConverter {
  import scala.language.implicitConversions

  given Conversion[Seq[parser.TopLevelDefinition], Program] =
    (t: Seq[parser.TopLevelDefinition]) => {
      Program(t.map(_ match {
        case g: parser.GlobalVariableDefinition =>
          given_Conversion_GlobalVariableDefinition_GlobalValiableDefinition(g)
        case f: parser.FunctionDefinition =>
          given_Conversion_FunctionDefinition_FunctionDefinition(f)
      }))
    }
  given Conversion[
    parser.GlobalVariableDefinition,
    TopLevel.GlobalValiableDefinition
  ] =
    (ast: parser.GlobalVariableDefinition) => {
      TopLevel.GlobalValiableDefinition(
        ast.name.name,
        ast.value
      )
    }
  given Conversion[parser.FunctionDefinition, TopLevel.FunctionDefinition] with
    def apply(ast: parser.FunctionDefinition) = {
      windymelt.toy.core.Ast.TopLevel.FunctionDefinition(
        ast.name.name,
        ast.args.map(_.name),
        ast.body
      )
    }
  given Conversion[parser.Op, Ops] = (ast: parser.Op) => {
    val canonicalOp = ast.op match {
      case "+"  => Ops.Add
      case "-"  => Ops.Sub
      case "*"  => Ops.Mul
      case "/"  => Ops.Div
      case "==" => Ops.EQ
      case "!=" => Ops.NEQ
      case "<"  => Ops.LT
      case "<=" => Ops.LEQ
      case ">"  => Ops.GT
      case ">=" => Ops.GEQ
    }
    canonicalOp
  }
  given Conversion[parser.Identifier, Expression] = (ast: parser.Identifier) =>
    ident(ast.name)
  given Conversion[parser.Assignment, Expression] = (ast: parser.Assignment) =>
    assign(ast.identifier.name, ast.expr)
  given Conversion[parser.Comparative, Expression] =
    (ast: parser.Comparative) =>
      ast match {
        case parser.Comparative(additive, next) =>
          next match {
            case Seq() => additive
            case seq =>
              seq.foldLeft(additive: Expression)((lhs, moreNext) =>
                Expression.BinaryExpression(moreNext._1, lhs, moreNext._2)
              )
          }
      }
  given Conversion[parser.Additive, Expression] = (ast: parser.Additive) =>
    ast match {
      case parser.Additive(multiplicative, next) =>
        next match {
          case Seq() => multiplicative
          case seq =>
            seq.foldLeft(multiplicative: Expression)((lhs, moreNext) =>
              Expression.BinaryExpression(moreNext._1, lhs, moreNext._2)
            )
        }
    }
  given Conversion[parser.Multiplicative, Expression] =
    (ast: parser.Multiplicative) =>
      ast match {
        case parser.Multiplicative(unary, next) =>
          next match {
            case Seq() => unary
            case seq =>
              seq.foldLeft(unary: Expression)((lhs, moreNext) =>
                Expression.BinaryExpression(moreNext._1, lhs, moreNext._2)
              )
          }
      }
  given Conversion[parser.Unary, Expression] = (ast: parser.Unary) =>
    ast match {
      case parser.ConcreteUnary(primary) =>
        Expression.UnaryExpression(None, primary)
      case parser.Negate(primary) =>
        Expression.UnaryExpression(Some(Ops.Neg), primary)
    }
  given Conversion[parser.Primary, Expression] = (ast: parser.Primary) =>
    ast match {
      case parser.Expression(expr) =>
        expr
      case parser.Integer(value) =>
        Expression.IntegerLiteral(value)
      case parser.FunctionCall(name, args) =>
        val argsConverted: Seq[Expression] = args.map(identity)
        Expression.FunctionCall(name.name, argsConverted)
      case Identifier(name) =>
        ident(name)
    }
  given Conversion[parser.Line, Expression] = (ast: parser.Line) =>
    ast match {
      case parser.Println(expr) =>
        Expression.Println(expr)
      case parser.Assignment(identifier, expr) =>
        Expression.Assignment(identifier.name, expr)
      case parser.ExpressionLine(expr) => expr
      case parser.WhileExpression(condition, body) =>
        Expression.While(condition, body)
      case parser.IfExpression(condition, body, elseBody) =>
        val convertedElseBody: Option[Expression] = elseBody.map(identity)
        Expression.If(condition, body, convertedElseBody)
      case parser.BlockExpression(expressions) =>
        val linestoSeqExp: Seq[Expression] = expressions.map(identity)
        Expression.Block(linestoSeqExp)
    }
}
