/**
 * Parses the tokens returned by the [[Lexer]] passed
 * @constructor creates a new parser with the given lexer
 * @param lexer
 */
class Parser(lexer: Lexer) {
  var token: Token = null
  var interactiveMode = false
  var indentDepth = 0

  /**
   * Returns the next token
   * @return Token
   */
  def advance = token = lexer getToken
  /**
   * Checks the next token and returns it
   * @param t token to compare the next token to
   * @return Token
   */
  def eat(t: Token) = {
    if(t == token) advance 
    else error("error at token " + token + " expected " + t)
  }

  /**
   * Empties the buffer and throws an exception
   * @throws ParseException
   */
  def error(s: String) = {
    while(token != NewLine && token != Eof) {
      advance
    }
    throw new ParseException(s)
  }

  /**
   * Parses a statement in interactive mode
   * @throws EofException
   * @return Stmt
   */
  def interactiveParse: Stmt = {
    interactiveMode = true
    advance
    token match {
      case NewLine => EmptyStmt
      case Eof => throw new EofException
      case _ =>  {
        val stmt = parseStatement
        stmt match {
          case e: ExprStmt => StackFrame.lastInput = e
          case _ => ()
        }
        token match {
          case NewLine => stmt
          case Eof => throw new EofException
          case n => error("wrong token " + token + " at end of line")
        }
      }
    }
  }

  /**
   * Prints a prompt when writing a block in interactive mode
   */
  def printIndent = {
    if(interactiveMode && token != Eof && 
       (token != KeyWord("end") || indentDepth != 0))
      print(".. " + " " * indentDepth)
  }
  
  /**
   * Parses a list of statement separated by a new line
   * @return StmtList the list of statements parsed
   */
  def parseStatements: StmtList = {
    def parseAll(stmts: List[Stmt]): StmtList = {
      token match {
        case Eof => StmtList(stmts)
        case KeyWord("end") => StmtList(stmts)
        case NewLine => printIndent; advance; parseAll(stmts)
        case _ => {
          val stmt = parseStatement
          printIndent
          eat(NewLine)
          parseAll(stmts :+ stmt)
        }
      }
    }
    parseAll(Nil)
  }

  /**
   * Parses a single statement
   * @return Stmt the parsed statement
   */
  def parseStatement: Stmt = token match {
    case KeyWord(s) => s match {
      case "fun" => advance; parseFunctionDef
//      case "if" => advance; parseIf
      case _ => error("Wrong keyword " + s)
    }
    case _ =>  {
      ExprStmt(parseExpression)
    }
  }

  /**
   * Parses a function definition
   * @return FuncDef the parsed function
   */
  def parseFunctionDef: FuncDef = {
    val name = token match {
      case Id(s) => advance; s
      case _ => error("missing function name" + token)
    }
    val varList = parseFormalParameters
    val stmts = parseBlock
    if(stmts isEmpty)
      error("A function body cannot be empty")
    FuncDef(name, varList, stmts)
  }

  /**
   * Parses a block begining by a ':' and ending by
   * the keyword 'end'
   * @return StmtList the statements in the block
   */
  def parseBlock: StmtList = {
    indentDepth += 3
    eat(One(':'))
    printIndent
    eat(NewLine)
    val stmts = parseStatements
    indentDepth -= 3
    eat(KeyWord("end"))
    stmts
  }

  /**
   * Parses the formal parameters in a function definition
   * @return List[Var] the list of formal parameters
   */
  def parseFormalParameters: List[Var] = {
    val parametersList = parseParameters
    parametersList map { 
      case v: Var => v
      case x => error("Bad formal parameter " + x)
    }
  }

  /**
   * Parses the parameters for a function definition or call
   * @return List[Exp] the parameters as expressions
   */
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

  /**
   * Parses an expression of the form
   * term (+ expression)*
   * term (- expression)*
   * @return Exp
   */
  def parseExpression: Exp = {
    def parseRightExpression(exp: Exp): Exp = token match {
      case Op("+") => advance; parseRightExpression(exp + parseTerm)
      case Op("-") => advance; parseRightExpression(exp - parseTerm)
      case _ => exp
    }
    parseRightExpression(parseTerm)
  }
  
  /**
   * Parses a term of the form
   * pow (* term)*
   * pow (/ term)*
   * @return Exp
   */
  def parseTerm: Exp = {
    def parseRightTerm(exp: Exp): Exp = token match {
      case Op("*") => advance; parseRightTerm(exp * parsePow)
      case Op("/") => advance; parseRightTerm(exp / parsePow)
      case _ => exp
    }
    parseRightTerm(parsePow)
  }

  /**
   * Parses a pow of the form
   * factor (^ pow)*
   * @return Exp
   */
  def parsePow: Exp = {
    def parseRightPow(exp: Exp): Exp = token match {
      case Op("^") => advance; parseRightPow(exp ** parseFactor)
      case _ => exp
    }
    parseRightPow(parseFactor)
  }

  /**
   * Parses a factor of the form
   * integer
   * double
   * id
   * (exp)
   * -factor
   * id = exp
   * id(params)
   * %    ->last input
   * @return Exp
   */
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
      case One('(') => FunCall(Var(s), parseParameters)
      case _ => Var(s)
    }
    case One('%') => advance; StackFrame.lastInput match {
      case ExprStmt(exp) => exp
      case _ => error("last input is not an expression")
    }
    case One(')') => error("missing left bracket")
    case _ => error("missing operand at token " + token)
  }
}


