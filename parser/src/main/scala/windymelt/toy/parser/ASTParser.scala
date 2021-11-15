package windymelt.toy.parser

import org.parboiled2._
import scala.util.Failure
import scala.util.Success

object ASTParser {
  def apply(input: ParserInput) = {
    val parser = new ASTParser(input)
    val result = parser.topLevelRule.run()
    result match {
      case Failure(exception) => {
        exception match {
          case p: ParseError => {
            println(
              s"Parse error at line ${p.position.line} column ${p.position.column}"
            )
            println(input.getLine(p.position.line))
            println(s"${" " * (p.position.column - 1)}^")
            println(p.traces.head.terminal)
            None
          }
          case _ => {
            println(s"Parse error: ${exception}")
            None
          }
        }
      }
      case Success(value) => Some(value)
    }
  }
}

sealed trait TopLevelDefinition
case class GlobalVariableDefinition(name: Identifier, value: Expression)
    extends TopLevelDefinition
case class FunctionDefinition(
    name: Identifier,
    args: Seq[Identifier],
    body: BlockExpression
) extends TopLevelDefinition
case class Op(op: String)
case class Comparative(
    additive: Additive,
    next: Seq[(Op, Additive)] = Seq.empty
) {
  override def toString = {
    val sb = new StringBuilder
    sb.append(additive)
    next.foreach { case (op, additive) =>
      sb.append(op.op)
      sb.append(additive)
    }
    sb.toString()
  }
}
case class Additive(
    multiplicative: Multiplicative,
    next: Seq[(Op, Multiplicative)] = Seq.empty
) {
  override def toString = {
    val sb = new StringBuilder
    sb.append(multiplicative)
    next.foreach { case (op, multiplicative) =>
      sb.append(op.op)
      sb.append(multiplicative)
    }
    sb.toString()
  }
}
case class Multiplicative(unary: Unary, next: Seq[(Op, Unary)] = Seq.empty) {
  override def toString = {
    val sb = new StringBuilder
    sb.append(unary)
    next.foreach { case (op, unary) =>
      sb.append(op.op)
      sb.append(unary)
    }
    sb.toString()
  }
}

sealed trait Unary
case class ConcreteUnary(primary: Primary) extends Unary {
  override def toString = primary.toString
}
case class Negate(expression: Primary) extends Unary {
  override def toString = s"-($expression)"
}

sealed trait Primary
case class Identifier(name: String) extends Primary {
  override def toString = s"#$name"
}
case class Integer(value: Int) extends Primary
case class FunctionCall(name: Identifier, args: Seq[Expression]) extends Primary
case class Expression(comparative: Comparative) extends Primary {
  override def toString = comparative.toString
}

sealed trait Line
case class Println(expr: Expression) extends Line
case class WhileExpression(expr: Expression, body: BlockExpression) extends Line
case class BlockExpression(expressions: Seq[Line]) extends Line {
  override def toString = expressions.mkString("{", "\n", "}")
}
case class IfExpression(
    expr: Expression,
    body: BlockExpression,
    elsebody: Option[BlockExpression]
) extends Line
case class Assignment(identifier: Identifier, expr: Expression) extends Line
case class ExpressionLine(expr: Expression) extends Line {
  override def toString = expr.toString + ";"
}

class ASTParser(val input: ParserInput) extends Parser {
  def ws: Rule0 = rule { zeroOrMore(anyOf(" \t\n")) }
  def topLevelRule = rule {
    ws ~ program ~ EOI
  }
  def program = rule {
    zeroOrMore(topLevelDefinition)
  }

  def lines: Rule1[Seq[Line]] = rule {
    zeroOrMore(line)
  }

  def topLevelDefinition = rule {
    globalVariableDefinition | functionDefinition
  }

  def functionDefinition: Rule1[FunctionDefinition] = rule {
    "define" ~ ws ~ identifier ~ "(" ~ ws ~
      optional(identifier ~ zeroOrMore("," ~ ws ~ identifier) ~> { (a, b) =>
        a +: b
      }) ~> { args =>
        args match {
          case None       => Seq()
          case Some(args) => args
        }
      } ~ ")" ~ ws ~ blockExpression ~> { (id, args, body) =>
        FunctionDefinition(id, args, body)
      }
  }

  def globalVariableDefinition: Rule1[GlobalVariableDefinition] = rule {
    "global" ~ ws ~ identifier ~ "=" ~ ws ~ expression ~> { (id, expr) =>
      GlobalVariableDefinition(id, expr)
    }
  }

  def line: Rule1[Line] = rule {
    println | whileExpression | ifExpression | assignment | expressionLine | blockExpression
  }

  def println: Rule1[Println] = rule {
    "println" ~ ws ~ "(" ~ ws ~ expression ~ ")" ~ ws ~> { (e) => Println(e) }
  }

  def whileExpression: Rule1[WhileExpression] = rule {
    "while" ~ ws ~ "(" ~ ws ~ expression ~ ")" ~ ws ~ blockExpression ~> {
      (e, b) =>
        WhileExpression(e, b)
    }
  }

  def blockExpression: Rule1[BlockExpression] = rule {
    "{" ~ ws ~ lines ~ "}" ~ ws ~> { (l) => BlockExpression(l) }
  }

  def ifExpression: Rule1[IfExpression] = rule {
    "if" ~ ws ~ "(" ~ ws ~ expression ~ ")" ~ ws ~ blockExpression ~ optional(
      "else" ~ ws ~ blockExpression
    ) ~> { (e, b, eb) =>
      IfExpression(e, b, eb)
    }
  }

  def assignment: Rule1[Assignment] = rule {
    identifier ~ "=" ~ ws ~ expression ~ ";" ~ ws ~> { (i, e) =>
      Assignment(i, e)
    }
  }

  def expressionLine: Rule1[ExpressionLine] = rule {
    expression ~ ";" ~ ws ~> { (e) => ExpressionLine(e) }
  }

  def expression: Rule1[Expression] = rule {
    comparative ~> { (c) => Expression(c) }
  }

  def comparativeOp: Rule1[Op] = rule {
    // caveat: there is symbol precedence in PEG: write "<=" before "<"
    capture("<=" | "<" | ">=" | ">" | "==" | "!=") ~ ws ~> { op => Op(op) }
  }

  def comparative: Rule1[Comparative] = rule {
    additive ~ zeroOrMore(
      comparativeOp ~ additive ~> { (l, r) => l -> r }
    ) ~> { (a, ops) =>
      Comparative(a, ops)
    }
  }

  def additiveOp: Rule1[Op] = rule {
    capture("+" | "-") ~ ws ~> { op => Op(op) }
  }

  def additive: Rule1[Additive] = rule {
    multiplicative ~ zeroOrMore(
      additiveOp ~ multiplicative ~> { (l, r) => l -> r }
    ) ~> { (m, ops) =>
      Additive(m, ops)
    }
  }

  def multiOp: Rule1[Op] = rule {
    capture("*" | "/") ~ ws ~> { op => Op(op) }
  }

  def multiplicative: Rule1[Multiplicative] = rule {
    unary ~ zeroOrMore(
      multiOp ~ unary ~> { (l, r) => l -> r }
    ) ~> { (u, r) =>
      Multiplicative(u, r)
    }
  }

  def negateFlag: Rule1[Boolean] = rule {
    optional(capture('-') ~ ws ~> { _ => true }) ~ ws ~> {
      (b: Option[Boolean]) =>
        b.getOrElse(false)
    }
  }

  def unary: Rule1[Unary] = rule {
    negateFlag ~ primary ~> { (sign: Boolean, p: Primary) =>
      if (sign) Negate(p) else ConcreteUnary(p)
    }
  }

  def primary: Rule1[Primary] = rule {
    "(" ~ ws ~ expression ~ ")" ~ ws | integer | functionCall | identifier
  }

  def integer: Rule1[Integer] = rule {
    negateFlag ~ capture(
      oneOrMore(CharPredicate.Digit)
    ) ~ ws ~> { (flg: Boolean, s: String) =>
      flg match {
        case true  => Integer(-s.toInt)
        case false => Integer(s.toInt)
      }
    }
  }

  def functionCall: Rule1[FunctionCall] = rule {
    identifier ~ "(" ~ ws ~ zeroOrMore(
      (expression ~ zeroOrMore("," ~ ws ~ expression) ~> { (l, r) => r.:+(l) })
    ) ~ ws ~> { (l, r) => FunctionCall(l, r.flatten) } ~ ")" ~ ws
  }

  def identifier: Rule1[Identifier] = rule {
    capture(oneOrMore(CharPredicate.Alpha)) ~ ws ~> { cap: String =>
      Identifier(cap.toString)
    }
  }
}
