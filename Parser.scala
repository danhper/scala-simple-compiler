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

    def advance = token = lexer getToken
    def eat(t: Token) = token match {
      case t if token == t => advance
      case _ => error("error at token " + token + " expected " + t)
    }

    def error(s: String) = throw new ParseException(s)

    def parse(s: String): Exp = {
      lexer init(s)
      advance
      T.lastInput = parseExpression
      token match {
        case Eof => T.lastInput
        case n => error("wrong token " + token + " at end of line")
      }
    }

    def parseStatements: StmtList = {
      def parseAll(stmts: List[Stmt]): StmtList = token match {
        case Eof => StmtList(stmts)
        case _ => {
          val stmt = parseStatement
          parseAll(stmts :+ stmt)
        }
      }
      parseAll(List())
    }

    def parseStatement: Stmt = token match {
      case KeyWord(s) => s match {
        case "fun" => advance; parseFunctionDef
        case _ => error("Wrong keyword " + s)
      }
      case _ => ExprStmt(parseExpression)
    }

    def parseFunctionDef: FuncDef = {
      val name = token match {
        case Id(s) => s
        case _ => error("missing function name" + token)
      }
      val varList = parseFormalParameters
      val stmts = parseBlock
      FuncDef(name, varList, stmts)
    }
    
    def parseBlock: StmtList = {
      eat(One(':'))
      val stmts = parseStatements
      eat(KeyWord("end"))
      stmts
    }

    def parseFormalParameters: List[Var] = {
      val parametersList = parseParameters
      parametersList map { 
        case v: Var => v
        case x => error("Bad formal parameter " + x)
      }
    }

    def parseParameters: List[Exp] = {
      def parseAllParams(params: List[Exp]): List[Exp] = token match {
        case One(')') => advance; params
        case _ => {
          eat(One(','))
          parseAllParams(params :+ parseExpression)
        }
      }
      eat(One('('))
      token match {
        case One(')') => advance; Nil
        case _ => parseAllParams(List(parseExpression))
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
      case One('(') => {
        advance
        val exp = parseExpression
        eat(One(')')); exp
      }
      case Op("+") => advance; parseFactor
      case Op("-") => advance; -parseFactor
      case Id(s) => advance; token match {
        case Op("=") => advance; Var(s) := parseExpression
        case One('(') => FunCall(s, parseParameters)
        case _ => Var(s)
      }
      case One('%') => advance; T.lastInput
      case One(')') => error("missing left bracket")
      case _ => error("missing operand at token " + token)
    }
  }
}

