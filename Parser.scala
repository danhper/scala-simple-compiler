/**
 * Parses the tokens returned by the [[Lexer]] passed
 * @constructor creates a new parser with the given lexer
 * @param lexer
 */
class Parser(lexer: Lexer) {
  var token: Token = null
  var interactiveMode = false
  var indentDepth = 0

  val nextTokens = new collection.mutable.Queue[Token]

  /**
   * Returns the next token
   * @return Token
   */
  def advance = token = {
    if(nextTokens isEmpty) lexer getToken
    else nextTokens dequeue
  }
  
  def lookAhead = {
    val tok = lexer.getToken
    nextTokens enqueue(tok)
    tok
  }

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
    indentDepth = 0
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
        val toCheck = if(nextTokens isEmpty) token else nextTokens.last
        toCheck match {
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
        case KeyWord("end") | KeyWord("elif") | KeyWord("else")=> StmtList(stmts)
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
      case "print" => advance; parsePrint
      case "if" => advance; parseIf
      case _ => error("Wrong keyword " + s)
    }
    case Id(s) => lookAhead match {
      case AssignOp(a) => {
        advance; advance
        parseAssign(s, a)
      }
      case _ => ExprStmt(parseExpressionOrTest)
    }
    case _ => ExprStmt(parseExpressionOrTest)
  }

  def parseIf: Stmt =  {
    def parseElif(ifs: List[IfStmt]): List[IfStmt] = token match {
      case KeyWord("elif") => {
        advance
        val cond = parseOrTest(None)
        val stmts = parseBlock(true)
        parseElif(ifs :+ IfStmt(cond, stmts, Nil, None))
      }
      case _ => ifs
    }

    val cond = parseOrTest(None)
    val stmts = parseBlock(true)
    val ifs = parseElif(Nil)
    val elseStmt = token match {
      case KeyWord("else") => advance; Some(parseBlock(false))
      case KeyWord("end") => advance; None
      case _ => error("missing 'end' at end of if")
    }
    IfStmt(cond, stmts, ifs, elseStmt)
  }


  def parseAssign(s: String, a: String): Stmt = a match {
    case "=" => AssignStmt(Var(s), parseExpression)
    case _ => {
      val v = Var(s)
      a match {
        case "++" => AssignStmt(v, v + IntNum(1))
        case "--" => AssignStmt(v, v - IntNum(1))
        case "+=" => AssignStmt(v, v + parseExpression)
        case "-=" => AssignStmt(v, v - parseExpression)
        case "*=" => AssignStmt(v, v * parseExpression)
        case "/=" => AssignStmt(v, v / parseExpression)
        case "%=" => AssignStmt(v, v % parseExpression)
        case "^=" => AssignStmt(v, v ** parseExpression)
        case _ => error("wrong assignment operator " + a)
      }
    } 
  }

  /**
   * Parses a print statement of the form
   * print(exp)
   * @return Stmt
   */
  def parsePrint: Stmt = {
    eat(One('('))
    val stmt = PrintStmt(parseExpression)
    eat(One(')'))
    stmt
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
    val stmts = parseBlock(false)
    if(stmts isEmpty)
      error("A function body cannot be empty")
    FuncDef(name, varList, stmts)
  }

  /**
   * Parses a block begining by a ':' and ending by
   * the keyword 'end'
   * @return StmtList the statements in the block
   */
  def parseBlock(isIf: Boolean): StmtList = {
    indentDepth += 3
    eat(One(':'))
    printIndent
    eat(NewLine)
    val stmts = parseStatements
    indentDepth -= 3
    if(!isIf)
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

  def parseExpressionOrTest: Exp = {
    parseOrTest(None)
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
   * pow (% term)*
   * @return Exp
   */
  def parseTerm: Exp = {
    def parseRightTerm(exp: Exp): Exp = token match {
      case Op("*") => advance; parseRightTerm(exp * parsePow)
      case Op("/") => advance; parseRightTerm(exp / parsePow)
      case Op("%") => advance; parseRightTerm(exp % parsePow)
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
      val exp = parseExpressionOrTest
      eat(One(')')); exp
    }
    case Op("+") => advance; parseFactor
    case Op("-") => advance; -parseFactor
    case Id(s) => advance; token match {
      case One('(') => FunCall(Var(s), parseParameters)
      case _ => Var(s)
    }
    case One('$') => advance; StackFrame.lastInput match {
      case ExprStmt(exp) => exp
      case _ => error("last input is not an expression")
    }
    case One(')') => error("missing left bracket")
    case _ => error("missing operand at token " + token)
  }

  def parseOrTest(e: Option[Exp]): Exp =  {
    def parseRightTest(exp: Exp): Exp = token match {
      case LogicOp("||") => {
        advance 
        parseRightTest(LogicExp(Or, exp, Some(parseAndTest(None))))
      }
      case _ => exp
    }
    parseRightTest(parseAndTest(e))
  }

  def parseAndTest(e: Option[Exp]): Exp =  {
    def parseRightTest(exp: Exp): Exp = token match {
      case LogicOp("&&") => advance; parseRightTest(LogicExp(And, exp, Some(parseNotTest(None))))
      case _ => exp
    }
    parseRightTest(parseNotTest(e))
  }

  def parseNotTest(e: Option[Exp]): Exp = token match {
    case LogicOp("!") => advance; LogicExp(Not, parseCompTest(e), None)
    case _ => parseCompTest(e)
  }

  def parseCompTest(e: Option[Exp]): Exp = {
    def parseRightTest(exp: Exp): Exp = token match {
      case CompOp(s) => {
        advance
        val test = s match {
          case "==" => Test(Eq, exp, parseExpression)
          case "!=" => Test(Neq, exp, parseExpression)
          case "<" => Test(Lt, exp, parseExpression)
          case "<=" => Test(Lte, exp, parseExpression)
          case ">=" => Test(Gt, exp, parseExpression)
          case ">" => Test(Gte, exp, parseExpression)
          case _ => error("unknown comparison " + s)
        }
        parseRightTest(test)
      }
      case _ => exp
    }
    e match {
      case Some(n) => parseRightTest(n)
      case None => parseRightTest(parseExpression)
    }
  }
}



