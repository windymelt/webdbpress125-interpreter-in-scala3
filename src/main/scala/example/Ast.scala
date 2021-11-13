package example

object Ast:
  enum Expression:
    case BinaryExpression(operator: Ops, lhs: Expression, rhs: Expression)
    case IntegerLiteral(value: Int)
    case Assignment(name: String, expression: Expression)
    case Identifier(name: String)

  enum Ops(sym: String):
    case Add extends Ops("+")
    case Sub extends Ops("-")
    case Mul extends Ops("*")
    case Div extends Ops("/")

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
  def int(i: Int): E = IntegerLiteral(i)
  def assign(name: String, exp: E): E = Assignment(name, exp)
  def ident(name: String): E = Identifier(name)

  // Syntax sugar
  extension (self: E)
    def +(rhs: E): E = add(self, rhs)
    def -(rhs: E): E = sub(self, rhs)
    def *(rhs: E): E = mul(self, rhs)
    def /(rhs: E): E = div(self, rhs)
