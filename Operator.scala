/**
 * Represents an operator
 */
abstract class Operator

/**
 * Represents the assignment operator =
 */
case object DefVar extends Operator {
  /**
   * Assigns the expression e to the variable v in the current stack frame
   * @param v the variable to be assigned
   * @param e the expression to assign
   * @return returns the expression to assign
   */
  def assign(v: Var, e: Exp) = {
    val exprVal = e.eval
    StackFrame.addValue(v, exprVal)
    exprVal
  }
}

/**
 * Represents a binary operator
 */
abstract class Binary extends Operator {
  /**
   * Overloaded method f to apply the operator on the given operands
   * @param a the left expression
   * @param b the right expression
   * @return Exp returns the result of the operation
   */
  def f(a: Num, b: Num): Num
  def f(a: Object, b: Object): Object = (a,b) match {
    case (i: Num, j: Num) => this.f(i, j)
    case _ => throw new TypeException(a, Some(b))
  }
  def f(exp1: Exp, exp2: Exp): Exp
}

/**
 * Represents a unary operator
 */
abstract class Unary extends Operator {
  /**
   * Overloaded method f to apply the operator on the given operands
   * @param a the expression
   * @return Exp returns the result of the operation
   */
  def f(a: Num): Num
  def f(a: Object): Object = a match {
    case (i: Num) => this.f(i)
    case _ => throw new TypeException(a)
  }
  def f(exp: Exp): Exp
}

/**
 * Represents an addition
 */
case object Add extends Binary {
  def f(a: Num, b: Num) = (a, b) match {
    case (i: IntNum, j: IntNum) => i + j
    case _ => a + b
  }
  def f(exp1: Exp, exp2: Exp) = exp1 + exp2
}

/**
 * Represents a substraction
 */
case object Sub extends Binary {
  def f(a: Num, b: Num) = (a, b) match {
    case (i: IntNum, j: IntNum) => i - j
    case _ => a - b
  }
  def f(exp1: Exp, exp2: Exp) = exp1 - exp2
}

/**
 * Represents a multiplication
 */
case object Mul extends Binary {
  def f(a: Num, b: Num) = (a, b) match {
    case (i: IntNum, j: IntNum) => i * j
    case _ => a * b
  }
  def f(exp1: Exp, exp2: Exp) = exp1 * exp2
}

/**
 * Represents a division
 */
case object Div extends Binary {
  def f(a: Num, b: Num) = (a, b) match {
    case (i: IntNum, j: IntNum) => i / j
    case _ => a / b
  }
  def f(exp1: Exp, exp2: Exp) = exp1 / exp2
}

/**
 * Represents an exponentiation
 */
case object Pow extends Binary {
  def f(a: Num, b: Num) = (a, b) match {
    case (i: IntNum, j: IntNum) => i ** j
    case _ => a ** b
  }
  def f(exp1: Exp, exp2: Exp) = exp1 ** exp2
}

/**
 * Represents a unary plus (eg: +8)
 */
case object UnaryAdd extends Unary {
  def f(a: Num) = a match {
    case i:IntNum => IntNum(i getVal)
    case _ => DoubleNum(a getDoubleVal)
  }
  def f(exp: Exp) = exp
}

/**
 * Represents a unary minus (eg: -8)
 */
case object UnarySub extends Unary {
  def f(a: Num) = a match {
    case i:IntNum => IntNum(-i.getVal)
    case _ => DoubleNum(-a.getDoubleVal)
  }
  def f(exp: Exp) = -exp 
}

