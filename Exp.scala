/**
 * Companion object of Exp
 * Contains factory methods to create Exp
 */
object Exp {
  /**
   * Creates a unary expression
   * @param f the operator to apply
   * @param exp the operand
   * @return Exp
   */
  def makeUnaryApp(f: Operator, exp: Exp): Exp = App(f, exp, None)
  /**
   * Creates a binary expression
   * @param f the operator to apply
   * @param left the left operand
   * @param right the right operand
   * @return Exp
   */
  def makeBinaryApp(f: Operator, left: Exp, right: Exp): Exp = App(f, left, Some(right))
}


/**
 * Represents an expression
 * Contains operators overloading and abstract evaluation method
 */
abstract class Exp {
  def eval: Object
  def +(other: Exp): Exp = Exp.makeBinaryApp(Add, this, other)
  def -(other: Exp): Exp = Exp.makeBinaryApp(Sub, this, other)
  def *(other: Exp): Exp = Exp.makeBinaryApp(Mul, this, other)
  def /(other: Exp): Exp = Exp.makeBinaryApp(Div, this, other)
  def **(other: Exp): Exp = Exp.makeBinaryApp(Pow, this, other)
  def unary_- = Exp.makeUnaryApp(UnarySub, this)
}

/**
 * Represents a function call
 * @constructor creates an instance to represent the call
 * @param funcName the name of the function
 * @param params the arguments of the call
 */
case class FunCall(funcName: Var, params: List[Exp]) extends Exp {
  /**
   * Executes the function it represents with the given arguments
   * @throws EvalExcpetion if the object with the function name is not callable
   * @return Object
   */
  def eval = {
    val value = StackFrame.getValue(funcName) match {
      case f: Fun => f.execute(params)
      case _ => throw new EvalException(funcName + " is not callable")
    }
    value
  }
}

/**
 * Represents an identifier
 * @constructor creates an instance for the given string
 * @param s the name of the identifier
 */
case class Var(s: String) extends Exp {
  override def toString = s
  def :=(other: Exp): Exp = Exp.makeBinaryApp(DefVar, this, other)
  /**
   * Looks up for the variable in the current stack frame
   * @return Object
   */
  def eval = StackFrame.getValue(this)
}

/**
 * Represents an application of an operator and one or two operands
 * @constructor creates the application
 * @param fun the operator
 * @param exp the first operand
 * @param other the second operand if it exists
 */
case class App(fun: Operator, exp: Exp, other: Option[Exp]) extends Exp {
  /**
   * Recursively evaluates the expression
   * @return Object
   */
  def eval = fun match {
    case un: Unary => un f(exp eval)
    case bin: Binary => bin f(exp eval, other.get eval)
    case d @ DefVar => exp match {
      case v: Var => d.assign(v, other get)
      case _ => throw new EvalException("Wrong assignement")
    }
  }
}

