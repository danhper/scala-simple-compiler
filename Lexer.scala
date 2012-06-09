/** Represents tokens used in [[Lexer]] */
abstract class Token
/** Represents an integer */
case class IntTok(v: Int) extends Token
/** Represents a floating number */
case class DoubleTok(v: Double) extends Token
/** Represents an identifier */
case class Id(s: String) extends Token
/** Represents an operator */
case class Op(s: String) extends Token
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
  val KeyWords = List("fun", "end")
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
      case None => IntTok(intPart toInt)
      case Some(c) => c match {
        case '.' => advance; DoubleTok((intPart + c + getIntString("", true)) toDouble)
        case _ => IntTok(intPart toInt)
      }
    }
  }

  /**
   * Skips white spaces to get to the next token and returns it
   * @return Token
   */
  def getToken: Token = nativeToken match {
    case One(' ') | One('\t') => getToken
    case One('\n') => NewLine
    case token => token
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
   * Evaluates the first character found and get the token
   * @return Token
   */
  def nativeToken: Token = lookAhead match {
    case None => Eof
    case Some(c) => c match {
      case c if c isDigit => parseNumber
      case c if c.toString matches("[_0-9a-zA-Z]") => getId("")
      case c if c.toString matches("[=+/*'^-]") => advance; Op(c toString)
      case _ => advance; One(c)
    }
  }
}

