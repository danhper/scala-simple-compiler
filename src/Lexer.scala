/** Represents tokens used in [[Lexer]] */
abstract class Token
/** Represents an integer */
case class IntTok(v: BigInt) extends Token
/** Represents a floating number */
case class DoubleTok(v: BigDecimal) extends Token
/** Represents an identifier */
case class Id(s: String) extends Token
/** Represents a string */
case class StrTok(s: String) extends Token
/** Represents an operator */
case class Op(s: String) extends Token
/** Represents an assignment operator */
case class AssignOp(s: String) extends Token
/** Represents a comparison operator */
case class CompOp(s: String) extends Token
/** Represents a logic operator */
case class LogicOp(s: String) extends Token
/** Represents a single symbol */
case class One(c: Char) extends Token
/** Represents a keyword */
case class KeyWord(s: String) extends Token
/** Represents the end of file */
case object Eof extends Token
/** Represents a new line */
case object NewLine extends Token

/**
 * Companion object for Lexer
 * Contains a list of the keywords
 */
object Lexer {
  val KeyWords = List("fun", "end", "if", "print", "elif", "else", "for", "in", "to", "while", "import")
  val operators = "-+*/%^"
  val logicOperators = "!&|"
  val compOperators = "<>=!"
  def isOperator(c: Char): Boolean = isOperator(c toString)
  def isOperator(s: String): Boolean = s matches("[" + operators + "]")
  def isCompOperator(c: Char): Boolean = isCompOperator(c toString)
  def isCompOperator(s: String): Boolean = s matches("[" + compOperators + "]")
  def isLogicOperator(c: Char): Boolean = isLogicOperator(c toString)
  def isLogicOperator(s: String): Boolean = s matches("[" + logicOperators + "]")
}

/**
 * Reads a stream from a [[scala.collection.BuferedIterator]] and divides it into tokens
 * @constructor create a lexer with the given buffer
 * @param buffer stream to process
 */
class Lexer(buffer: BufferedIterator[Char]) {
  /**
   * Reads the next char
   * @return Char 
   */
  def advance = buffer next
  /**
   * Returns the next char as a [[scala.Option]] if it exists
   * @return Option[Char]
   */
  def lookAhead: Option[Char] = if(buffer hasNext) Some(buffer head) else None

  /**
   * Parses an integer or a floating number
   * @throws BadTokenException if the number has more than one point
   * @throws NumberFormatException on overflow
   * @return Token 
   */
  def parseNumber = {
    def getIntString(s: String, decPart: Boolean): String = lookAhead match {
      case None => s
      case Some(c) => c match {
        case c if c isDigit => advance; getIntString(s + c, decPart)
        case '.' if decPart => throw new BadTokenException("multiple points in number")
        case _ => s
      }
    }
    val intPart = getIntString("", false)
    lookAhead match {
      case None => IntTok(BigInt(intPart))
      case Some(c) => c match {
        case '.' => advance; DoubleTok(BigDecimal(intPart + c + getIntString("", true)))
        case _ => IntTok(BigInt(intPart))
      }
    }
  }
  
  /**
   * Skips white spaces to get to the next token and returns it
   * @return Token
   */
  def getToken: Token = nativeToken match {
    case One(' ') | One('\t') => getToken
    case One('\r') => lookAhead match {
      case None => One('\r')
      case Some(c) => c match {
        case '\n' => advance; NewLine
        case _ => getToken
      }
    }
    case One('\n') => NewLine
    case One('#') => skipLine
    case token => token
  }

  def skipLine: Token = nativeToken match {
    case One('\n') => NewLine
    case _ => skipLine
  }

  /**
   * Reads a string from the buffer and returns it
   * Returns a KeyWord if the string is a keyword, else returns an Id
   * @return Token
   */ 
  def getId(s: String): Token = lookAhead match {
    case None => if(Lexer.KeyWords contains(s)) KeyWord(s) else Id(s)
    case Some(c) => c match {
      case c if c.toString matches("[_0-9a-zA-Z]") => advance; getId(s + c)
      case _ => if(Lexer.KeyWords contains(s)) KeyWord(s) else Id(s)
    }
  }

  /**
   * Checks for the next char after the given one
   * and returns the appropriate token
   * (normal or assignment operator )
   * @param c the previous char
   * @return Token
   */
  def getOperator(c: Char): Token = lookAhead match {
    case None => Op(c toString)
    case Some(x) => x match {
      case '=' => advance; AssignOp(c + "=")
      case '+' if c == '+' => advance; AssignOp("++")
      case '-' if c == '-' => advance; AssignOp("--")
      case _ => Op(c toString)
    }
  }
  
  /**
   * Checks for the next operator and returns
   * the appropriate logic, assignment or comparison
   * operator
   * @param c the previous char
   * @return Token
   */
  def getCompOperator(c: Char): Token = lookAhead match {
    case None => CompOp(c toString)
    case Some(x) => x match {
      case '=' => advance; CompOp(c + "=")
      case _ => c match {
        case '!' => LogicOp(c toString)
        case '=' => AssignOp(c toString)
        case _ => CompOp(c toString)
      }
    }
  }

  /**
   * Gets the logic operator (!, &&, ||) or throws
   * an exception if could not find it
   * @throws BadTokenException
   * @return Token
   */
  def getLogicOperator(c: Char): Token = lookAhead match {
    case None => throw new BadTokenException("Bad token " + c)
    case Some(x) => (c, x) match {
      case ('!', _) => LogicOp("!")
      case ('&', '&') => advance; LogicOp("&&")
      case ('|', '|') => advance; LogicOp("||")
      case n => throw new BadTokenException("Bad token " + c + n)
    }
  }

  /**
   * Parses a string ended with the char `c`
   * TODO: support for escape character
   * @return Token
   */
  def parseString(c: Char): Token = {
    def getString(s: String): String = lookAhead match {
      case None => throw new BadTokenException("Unclosed string")
      case Some(x) => x match {
        case '\n' => advance; throw new BadTokenException("Unclosed string")
        case x if c == x => advance; s
        case _ => advance; getString(s + x)
      }
    }
    StrTok(getString(""))
  }
  
  /**
   * Evaluates the first character found and get the token
   * @return Token
   */
  def nativeToken: Token = lookAhead match {
    case None => Eof
    case Some(c) => c match {
      case c if c isDigit => parseNumber
      case c if c.toString matches("[_0-9a-zA-Z]") => getId("")
      case c if Lexer.isOperator(c) => advance; getOperator(c)
      case c if Lexer.isCompOperator(c) => advance; getCompOperator(c)
      case c if Lexer.isLogicOperator(c) => advance; getLogicOperator(c)
      case '\'' | '"' => advance; parseString(c)
      case _ => advance; One(c)
    }
  }
}

