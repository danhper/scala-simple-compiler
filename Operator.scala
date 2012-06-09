object Operator {
  import Expression._
  import BuiltinFunctions._

  class TypeException extends Exception

  abstract class Op

  case object DefVar extends Op {
    def assign(v: Var, e: Exp) = {
      val exprVal = e.eval
      StackFrame.addValue(v, exprVal)
      exprVal
    }
  }

  abstract class Binary extends Op {
    def f(a: Num, b: Num): Num
    def f(a: Object, b: Object): Object = (a,b) match {
      case (i: Num, j: Num) => this.f(i, j)
      case _ => throw new TypeException
    }
    def f(exp1: Exp, exp2: Exp): Exp
  }

  abstract class Unary extends Op {
    def f(a: Num): Num
    def f(a: Object): Object = a match {
      case (i: Num) => this.f(i)
      case _ => throw new TypeException
    }
    def f(exp: Exp): Exp
  }

  case object Add extends Binary {
    def f(a: Num, b: Num) = (a, b) match {
      case (i: IntNum, j: IntNum) => i + j
      case _ => a + b
    }
    def f(exp1: Exp, exp2: Exp) = exp1 + exp2
  }
  case object Sub extends Binary {
    def f(a: Num, b: Num) = (a, b) match {
      case (i: IntNum, j: IntNum) => i - j
      case _ => a - b
    }
    def f(exp1: Exp, exp2: Exp) = exp1 - exp2
  }
  case object Mul extends Binary {
    def f(a: Num, b: Num) = (a, b) match {
      case (i: IntNum, j: IntNum) => i * j
      case _ => a * b
    }
    def f(exp1: Exp, exp2: Exp) = exp1 * exp2
  }
  case object Div extends Binary {
    def f(a: Num, b: Num) = (a, b) match {
      case (i: IntNum, j: IntNum) => i / j
      case _ => a / b
    }
    def f(exp1: Exp, exp2: Exp) = exp1 / exp2
  }

  case object Pow extends Binary {
    def f(a: Num, b: Num) = (a, b) match {
      case (i: IntNum, j: IntNum) => i ** j
      case _ => a ** b
    }
    def f(exp1: Exp, exp2: Exp) = exp1 ** exp2
  }

  case object UnaryAdd extends Unary {
    def f(a: Num) = a match {
      case i:IntNum => IntNum(i getVal)
      case _ => DoubleNum(a getDoubleVal)
    }
    def f(exp: Exp) = exp
  }
  
  case object UnarySub extends Unary {
    def f(a: Num) = a match {
      case i:IntNum => IntNum(-i.getVal)
      case _ => DoubleNum(-a.getDoubleVal)
    }
    def f(exp: Exp) = -exp 
  }
}
