/**
 * Defines a pretty print for exceptions/errors
 */
abstract trait Printable {
  def printError: Unit
}

/**
 * Exception thrown when incompatible types are found (eg: fun + int)
 */
class TypeException(a: Object, b: Option[Object]) extends Exception  with Printable {
  def this(a: Object) = this(a, None)
  def printError = println("incompatible types for " + a + {if(b != None) " and " + b.get else ""})
}


/**
 * General exception for problems during parsing
 */
class ParseException(err: String) extends Exception with Printable {
  def printError = println("parse error : " + err)
}

/**
 * General exception for problems when splitting up into tokens
 */
class BadTokenException(err: String) extends Exception with Printable {
  def printError = println(err)
}

/**
 * General exception for calculations problem (eg: fact(-5))
 */
class CalcException(err: String) extends Exception with Printable {
  def printError = println(err)
}

/**
 * General exception for evalutaion problems
 */
class EvalException(err: String) extends Exception with Printable {
  def printError = println(err)
}

/**
 * Exception thrown when the input is over
 */
class EofException extends Exception

/**
 * Exception thrown when reference to an undefined variable/function
 */
class UndefinedException(key: Var) extends Exception with Printable {
  def printError = println(key + " is undefined")
}
