package windymelt.toy.core

class Interpreter:
  import Ast.Expression
  import Ast.Expression.*
  import Ast.TopLevel.*
  import Ast.Env
  val StackMaxCount = 10000
  var stackCounter = 0
  var env = Env(Map(), None)
  val functionEnvironment =
    scala.collection.mutable.Map[String, FunctionDefinition]()

  def callMain(program: Ast.Program): Int = {
    val toplevels = program.definitions
    for (toplevel <- toplevels) {
      toplevel match
        case fd @ FunctionDefinition(name, parameters, body) =>
          functionEnvironment.put(name, fd)
        case GlobalValiableDefinition(name, value) =>
          env = Env(env.bindings.updated(name, interpret(value)), None)
    }
    functionEnvironment.get("main") match
      case Some(fd) => interpret(fd.body)
      case None     => throw new Exception("main function not found")
  }

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
        case Neg =>
          throw new Exception(
            "not implemented"
          ) // Neg cannot be appeared in the BinaryExpression
    }
    case UnaryExpression(op, rhs) => {
      val rhsInterpreted: Int = interpret(rhs)
      import Ast.Ops.Neg
      op match
        case Some(Neg) => -rhsInterpreted
        case None      => rhsInterpreted
        case Some(_) =>
          throw new Exception(
            "not implemented"
          ) // Other than Neg cannot be appeared in the UnaryExpression
    }
    case IntegerLiteral(lit) => lit
    case Assignment(name, value) => {
      val intval = interpret(value)
      env = env.copy(bindings = env.bindings.updated(name, intval))
      intval
    }
    case Identifier(name) =>
      env.findBinding(name) match
        case Some(binding) => binding(name)
        case None          => throw new Exception("undefined variable: " + name)
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
    case FunctionCall(name, args) => {
      val fd = functionEnvironment.get(name)
      fd.match
        case Some(fd) => {
          val actualParams = args
          val formalParams = fd.parameters
          val body = fd.body
          val parameterMap: Map[String, Int] = formalParams
            .zip(actualParams)
            .map { case (formalName, actual) =>
              formalName -> interpret(actual)
            }
            .toMap
          parameterMap.foreach { case (name, value) =>
          //print(" " + name + ": " + value)
          }
          val backupEnv = env
          env = Env(parameterMap, Some(env))
          if (stackCounter > StackMaxCount) {
            println("stack overflow")
            sys.exit(1)
          }
          stackCounter += 1
          //println(" stackCounter: " + stackCounter)
          val result = interpret(body)
          stackCounter -= 1
          env = backupEnv
          result
        }
        case None => throw new Exception("function not found")
    }
    case Println(expr) => {
      val intval = interpret(expr)
      println(intval)
      intval
    }
