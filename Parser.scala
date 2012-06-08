object Parser {
  import Expression._
  import Lexer._
  import Statement._

  class ParseException(err: String) extends Exception {
    def printError = println("parse error : " + err)
  }
  
  class EofException extends Exception

  class Pars(lexer: Lex) {
    var token: Token = null

    def advance = token = lexer getToken
    def eat(t: Token) = {
      if(t == token) advance 
      else error("error at token " + token + " expected " + t)
    }

    def error(s: String) = {
      while(token != NewLine && token != Eof) {
        advance
      }
      throw new ParseException(s)
    }

    def interactiveParse: Stmt = {
      advance
      token match {
        case NewLine => EmptyStmt
        case Eof => throw new EofException
        case _ =>  {
          val stmt = parseStatement
          token match {
            case NewLine => stmt
            case Eof => throw new EofException
            case n => error("wrong token " + token + " at end of line")
          }
        }
      }
    }

    def parseStatements: StmtList = {
      def parseAll(stmts: List[Stmt]): StmtList = {
          
        token match {
          case Eof => StmtList(stmts)
          case KeyWord("end") => StmtList(stmts)
          case NewLine => advance; parseAll(stmts)
          case _ => {
            val stmt = parseStatement
            eat(NewLine)
            parseAll(stmts :+ stmt)
          }
        }
      }
      parseAll(List())
    }

    def parseStatement: Stmt = token match {
      case KeyWord(s) => s match {
        case "fun" => advance; parseFunctionDef
        case _ => error("Wrong keyword " + s)
      }
      case _ =>  {
        T.lastInput = parseExpression
        ExprStmt(T.lastInput)
      }
    }

    def parseFunctionDef: FuncDef = {
      val name = token match {
        case Id(s) => advance; s
        case _ => error("missing function name" + token)
      }
      val varList = parseFormalParameters
      val stmts = parseBlock
      FuncDef(name, varList, stmts)
    }
    
    def parseBlock: StmtList = {
      eat(One(':'))
      eat(NewLine)
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

