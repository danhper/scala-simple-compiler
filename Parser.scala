object Parser {
  import Expression._
  import Lexer._
  import Statement._

  class ParseException(err: String) extends Exception {
    def printError = println("parse error : " + err)
  }

  class Pars(lexer: Lex) {
    var token: Token = Eof
    var eval: Boolean = false

    def advance = token = lexer.getToken
    def eat(t: Token) = token match {
      case t if token == t => advance
      case _ => error("error at token " + t)
    }
    def eatId: Id = token match {
      case id: Id => advance; id
      case _ => error("missing id at token " + token)
    }

    def error(s: String) = throw new ParseException(s)

    def parse(s: String): Exp = {
      lexer.init(s)
      advance
      T.lastInput = parseExpression
      token match {
        case Eof => T.lastInput
        case n => throw new ParseException("wrong token " + token + " at end of line")
      }
    }

    def parseStatement: Stmt = token match {
      case KeyWord(s) => s match {
        case "fun" => parseFunctionDef
        case _ => throw new ParseException("Wrong keyword " + s)
      }
      case _ => ExprStmt(parseExpression)
    }

    def parseFunctionDef: FuncDef = {
      FuncDef("", List(), List())
    }

    def parseExpression: Exp = {
      def parseRightExpression(exp: Exp): Exp = token match {
        case Op("+") => advance; parseRightExpression(exp + parseTerm)
        case Op("-") => advance; parseRightExpression(exp - parseTerm)
        case _ => exp
      }
      parseRightExpression(parseTerm)
    }

    def parseTerm: Exp = {
      def parseRightTerm(exp: Exp): Exp = token match {
        case Op("*") => advance; parseRightTerm(exp * parsePow)
        case Op("/") => advance; parseRightTerm(exp / parsePow)
        case _ => exp
      }
      parseRightTerm(parsePow)
    }

    def parsePow: Exp = {
      def parseRightPow(exp: Exp): Exp = token match {
        case Op("^") => advance; parseRightPow(exp ** parseFactor)
        case _ => exp
      }
      parseRightPow(parseFactor)
    }

    def parseFactor: Exp = token match {
      case IntTok(n) => advance; IntNum(n)
      case DoubleTok(n) => advance; DoubleNum(n)
      case One('(') =>
        advance
      val exp = parseExpression
      eat(One(')')); exp
      case Op("+") => advance; parseFactor
      case Op("-") => advance; -parseFactor
      case Id(s) => advance; token match {
          case Op("=") => advance; Var(s) := parseExpression
          case One('(') => callFunction(s)
          case _ => Var(s)
        }
      case KeyWord(s) => s match {
        case "fun" => error("test")
      }
      case One('%') => advance; T.lastInput
      case One(')') => error("missing left bracket")
      case _ => error("missing operand at token " + token)
    }

    private def callFunction(s: String) = {
      advance
      val exp = parseExpression
      val n = BuiltIns.table.get(s) match {
        case None => throw new ParseException("Unknown function " + s)
        case Some(f) => f(exp.eval)
      }
      eat(One(')'))
      n
    }
  }
}

