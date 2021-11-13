package example

class Interpreter:
  import Ast.Expression
  import Ast.Expression.*
  val env = scala.collection.mutable.Map[String, Int]()
  def interpret(expression: Expression): Int = expression match
    case BinaryExpression(op, lhs, rhs) => {
      val lhsInterpreted: Int = interpret(lhs)
      val rhsInterpreted: Int = interpret(rhs)
      import Ast.Ops.*
      op match
        case Add => lhsInterpreted + rhsInterpreted
        case Sub => lhsInterpreted - rhsInterpreted
        case Mul => lhsInterpreted * rhsInterpreted
        case Div => lhsInterpreted / rhsInterpreted
    }
    case IntegerLiteral(lit) => lit
    case Assignment(name, value) => {
      val intval = interpret(value)
      env(name) = intval
      intval
    }
    case Identifier(name) => env(name)