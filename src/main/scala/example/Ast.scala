package example

object Ast:
  case class Program(definitions: Seq[TopLevel])
  enum TopLevel:
    case GlobalValiableDefinition(name: String, value: Expression)
    case FunctionDefinition(
        name: String,
        parameters: Seq[String],
        body: Expression
    )
  enum Expression:
    case BinaryExpression(operator: Ops, lhs: Expression, rhs: Expression)
    case IntegerLiteral(value: Int)
    case Assignment(name: String, expression: Expression)
    case Identifier(name: String)
    case Block(exps: Seq[Expression])
    case While(cond: Expression, body: Expression)
    case If(
        cond: Expression,
        `then`: Expression,
        `else`: Option[Expression] = None
    )
    case FunctionCall(name: String, arguments: Seq[Expression])

  case class Env(bindings: Map[String, Int], next: Option[Env]):
    def findBinding(name: String): Option[Map[String, Int]] =
      bindings.isDefinedAt(name) match {
        case true  => Some(bindings)
        case false => next.flatMap(_.findBinding(name))
      }

  enum Ops(sym: String):
    case Add extends Ops("+")
    case Sub extends Ops("-")
    case Mul extends Ops("*")
    case Div extends Ops("/")
    case LT extends Ops("<")
    case LEQ extends Ops("<=")
    case GT extends Ops(">")
    case GEQ extends Ops(">=")
    case EQ extends Ops("=")
    case NEQ extends Ops("!=")

  import Ast.{Expression => E}
  import E.*
  def add(lhs: E, rhs: E): E =
    BinaryExpression(Ops.Add, lhs, rhs)
  def sub(lhs: E, rhs: E): E =
    BinaryExpression(Ops.Sub, lhs, rhs)
  def mul(lhs: E, rhs: E): E =
    BinaryExpression(Ops.Mul, lhs, rhs)
  def div(lhs: E, rhs: E): E =
    BinaryExpression(Ops.Div, lhs, rhs)
  def lt(lhs: E, rhs: E): E = BinaryExpression(Ops.LT, lhs, rhs)
  def leq(lhs: E, rhs: E): E = BinaryExpression(Ops.LEQ, lhs, rhs)
  def gt(lhs: E, rhs: E): E = BinaryExpression(Ops.GT, lhs, rhs)
  def geq(lhs: E, rhs: E): E = BinaryExpression(Ops.GEQ, lhs, rhs)
  def eq(lhs: E, rhs: E): E = BinaryExpression(Ops.EQ, lhs, rhs)
  def neq(lhs: E, rhs: E): E = BinaryExpression(Ops.NEQ, lhs, rhs)
  def int(i: Int): E = IntegerLiteral(i)
  def assign(name: String, exp: E): E = Assignment(name, exp)
  def ident(name: String) = Identifier(name)
  def block(exps: E*): E = Block(exps)
  def `while`(cond: E, body: E): E = While(cond, body)
  def `if`(cond: E, `then`: E, `else`: E): E = If(cond, `then`, Some(`else`))
  def `if`(cond: E, `then`: E): E = If(cond, `then`)
  def defun(name: String, params: Seq[String], body: E): TopLevel =
    TopLevel.FunctionDefinition(name, params, body)
  def defvar(name: String, value: E): TopLevel =
    TopLevel.GlobalValiableDefinition(name, value)
  def program(defs: TopLevel*): Program = Program(defs)
  def call(name: String, args: E*): E = FunctionCall(name, args)

  // Syntax sugar
  extension (self: E)
    def +(rhs: E): E = add(self, rhs)
    def -(rhs: E): E = sub(self, rhs)
    def *(rhs: E): E = mul(self, rhs)
    def /(rhs: E): E = div(self, rhs)
    def <(rhs: E): E = lt(self, rhs)
    def <=(rhs: E): E = leq(self, rhs)
    def >(rhs: E): E = gt(self, rhs)
    def >=(rhs: E): E = geq(self, rhs)
    def ===(rhs: E): E = eq(self, rhs)
    def !=(rhs: E): E = neq(self, rhs)
    def :=(rhs: E): E = self match {
      case Identifier(name) => assign(name, rhs)
      case _                => sys.error("Invalid lhs")
    }
