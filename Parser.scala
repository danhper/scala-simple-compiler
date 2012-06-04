object Parser {
  import Expression._
  import Lexer._

  class ParseException(err: String) extends Exception {
    def printError = println("parse error : " + err)
  }

  class Pars {
    var lexer: Lex = null
    var token: Token = Eof
    var eval: Boolean = false

    def advance = token = lexer.getToken
    def eat(t: Token) = if (token == t) advance else error("error at token " + t)
    def eatId: Id = if(token.isInstanceOf[Id]) {
      val tmp = token.asInstanceOf[Id]
      advance; tmp
    } else error("missing id at token " + token) 

    def error(s: String) = throw new ParseException(s)
    
    def parse(s: String): Exp = {
      lexer = new Lex(s)
      advance
      T.lastInput = parseExpression
      token match {
        case Eof => T.lastInput
        case n => throw new ParseException("wrong token " + token + " at end of line")
      }
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
          case Op("=") => assignValue(s)
          case One('(') => callFunction(s)
          case _ => Var(s)
        }
      case KeyWord(s) => s match {
        case "Fun" => error("test")
      }
      case One('%') => advance; T.lastInput
      case One(')') => error("missing left bracket")
      case _ => error("missing operand at token " + token)
    }

    private def assignValue(s: String ) = {
      advance
      val exp = parseExpression
      T.addVar(s, exp.eval)
      Var(s)
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

