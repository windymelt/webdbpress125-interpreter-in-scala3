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
        case LT  => if (lhsInterpreted < rhsInterpreted) 1 else 0
        case LEQ => if (lhsInterpreted <= rhsInterpreted) 1 else 0
        case GT  => if (lhsInterpreted > rhsInterpreted) 1 else 0
        case GEQ => if (lhsInterpreted >= rhsInterpreted) 1 else 0
        case EQ  => if (lhsInterpreted == rhsInterpreted) 1 else 0
        case NEQ => if (lhsInterpreted != rhsInterpreted) 1 else 0
    }
    case IntegerLiteral(lit) => lit
    case Assignment(name, value) => {
      val intval = interpret(value)
      env(name) = intval
      intval
    }
    case Identifier(name) => env(name)
    case If(cond, thenExpr, elseExpr) => {
      val condInterpreted = interpret(cond)
      if (condInterpreted != 0) interpret(thenExpr)
      else elseExpr.map(interpret).getOrElse(1)
    }
    case While(cond, body) => {
      var condInterpreted = interpret(cond)
      while (condInterpreted != 0) {
        interpret(body)
        condInterpreted = interpret(cond)
      }
      1
    }
    case Block(exprs) => exprs.map(interpret).last
