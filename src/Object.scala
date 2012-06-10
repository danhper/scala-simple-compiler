import BuiltinFunctions._

/**
 * Represents an object (integer, double, function)
 */
abstract class Object extends Exp { 
  def eval = this
}

abstract class Value extends Object

/**
 * Represents a number (integer or double)
 */
abstract class Num extends Value {
  def getDoubleVal: BigDecimal
  /**
   * Overloading of operator to get a double value
   */
  def +(that: Num): DoubleNum = DoubleNum(this.getDoubleVal + that.getDoubleVal)
  /**
   * Overloading of operator to get a double value
   */
  def -(that: Num): DoubleNum = DoubleNum(this.getDoubleVal - that.getDoubleVal)
  /**
   * Overloading of operator to get a double value
   */
  def *(that: Num): DoubleNum = DoubleNum(this.getDoubleVal * that.getDoubleVal)
  /**
   * Overloading of operator to get a double value
   */
  def /(that: Num): DoubleNum = DoubleNum(this.getDoubleVal / that.getDoubleVal)
  /**
   * Overloading of operator to get a double value
   */
  def %(that: Num): DoubleNum = DoubleNum(this.getDoubleVal % that.getDoubleVal)
  /**
   * Overloading of operator to get a double value
   */
  def **(that: Num): DoubleNum = DoubleNum(doublePow(this.getDoubleVal, that.getDoubleVal))
}

/**
 * Represents an integer
 * @param n the integer value
 */
case class IntNum(n: BigInt) extends Num {
  override def toString = n toString
  /**
   * Overloading of operator to get an integer
   */
  def +(that: IntNum): IntNum = IntNum(this.n + that.n)
  /**
   * Overloading of operator to get an integer
   */
  def -(that: IntNum): IntNum = IntNum(this.n - that.n)
  /**
   * Overloading of operator to get an integer
   */
  def *(that: IntNum): IntNum = IntNum(this.n * that.n)
  /**
   * Overloading of operator to get an integer
   */
  def /(that: IntNum): IntNum = IntNum(this.n / that.n)
  /**
   * Overloading of operator to get an integer
   */
  def %(that: IntNum): IntNum = IntNum(this.n % that.n)
  /**
   * Overloading of operator to get an integer
   */
  def **(that: IntNum): IntNum = IntNum(intPow(this.n, that.n) toBigInt)
  def getVal: BigInt = n
  def getDoubleVal = BigDecimal(n)
}

/**
 * Represents a double
 * @param n the double value
 */
case class DoubleNum(n: BigDecimal) extends Num {
  override def toString = n toString
  def getVal: BigDecimal = n
  def getDoubleVal = n
}

/**
 * Represents a function
 * @param name the function name
 * @param paramNumber the number of arguments of the function
 */
abstract class Fun(name: String, paramNumber: Int) extends Object {
  /**
   * Executes the function
   * @param params the parameters to give to the function
   * @return Object the result of the function computation
   */
  def execute(params: List[Exp]): Object

  /**
   * Checks the given number of arguments when calling the function
   */ 
  def checkParamsNum(params: List[Exp]) = {
    if(params.length != paramNumber) {
      throw new EvalException("function " + name + " expects " + paramNumber + " parameters, " + params.length + " given") 
    }
  }
}

/**
 * Represents a built-in function
 * @param fun the function to execute
 */
case class BuiltInFun(name: String, paramNumber: Int, fun: (List[Object] => Num)) extends Fun(name, paramNumber) {
  def execute(params: List[Exp]) = {
    checkParamsNum(params)
    fun(params map { exp => exp eval })
  }
}

/**
 * Represents a function declared in the program
 * @param stmts the statements to execute
 */
case class DeclaredFunction(name: String, varList: List[Var], stmts: StmtList) extends Fun(name, varList length) {
  override def toString = {
    "function: " + name + "(" + 
      (varList map (_ toString) reduceRight(_ + ", " + _)) +  ")"
  }
  //val stackFrame = StackFrame(StackFrame.getCurrentFrame)
  def execute(params: List[Exp]) = {
    checkParamsNum(params)
    val stack = new StackFrame
    varList zip(params) foreach {case (v, exp) => stack.addVal(v, exp eval)}
    StackFrame.startNewFrame(stack)    
    val ret = stmts.execute
    StackFrame.stopFrame
    ret    
  }
}

case class Str(s: String) extends Value {
  override def toString = s
}

abstract class Bool extends Value {
  def ||(that: Bool) = Or.fun(this, that)
  def &&(that: Bool) = And.fun(this, that)
  def ! = Not.fun(this)
}

case object True extends Bool
case object False extends Bool

/**
 * Represents a null value
 */
case object Null extends Object {
  override def eval = Null
}
