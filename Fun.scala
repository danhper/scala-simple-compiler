object Function {
  import Expression._

  abstract class Fun

  def makeNum(n : Int) = IntNum(n) 
  def makeNum(n : Double) = DoubleNum(n)
  def makeCalc(a : Num, b : Num, f1 : (Int, Int) => Int, f2 : (Double, Double) => Double) = (a, b) match {
    case (i: IntNum, j: IntNum) => makeNum(f1(i.getVal, j.getVal))
    case _ => makeNum(f2(a.getDoubleVal, b.getDoubleVal))
  }

  case object DefVar extends Fun {
    def assign(v: Var, e: Exp) = {
      val exprVal = e.eval
      T.addVar(v, exprVal)
      exprVal
    }
  }

  abstract class Binary extends Fun {
    def f(a: Num, b: Num): Num
    def f(exp1: Exp, exp2: Exp): Exp
  }
  abstract class Unary extends Fun {
    def f(a: Num): Num
    def f(exp: Exp): Exp
  }

  case object Add extends Binary {
    def f(a: Num, b: Num) = makeCalc(a,b, _+_, _+_)
    def f(exp1: Exp, exp2: Exp) = exp1 + exp2
  }
  case object Sub extends Binary {
    def f(a: Num, b: Num) = makeCalc(a,b, _-_, _-_)
    def f(exp1: Exp, exp2: Exp) = exp1 - exp2
  }
  case object Mul extends Binary {
    def f(a: Num, b: Num) = makeCalc(a,b, _*_, _*_)
    def f(exp1: Exp, exp2: Exp) = exp1 * exp2
  }
  case object Div extends Binary {
    def f(a: Num, b: Num) = makeCalc(a,b, _/_, _/_)
    def f(exp1: Exp, exp2: Exp) = exp1 / exp2
  }

  case object Pow extends Binary {
    private def intPow(x: Int, n: Int): Int = n match {
      case n if n <= 0 => 1
      case n if n % 2 == 0 => val tmp = intPow(x, n / 2); tmp * tmp
      case n if n % 2 == 1 => x * intPow(x, n - 1)
    }
    def f(a: Num, b: Num) = makeCalc(a,b, intPow, math.pow)
    def f(exp1: Exp, exp2: Exp) = exp1 ** exp2
  }

  case object UnaryAdd extends Unary {
    def f(a: Num) = a match {
      case i:IntNum => makeNum(i.getVal)
      case _ => makeNum(a.getDoubleVal)
    }
    def f(exp: Exp) = exp
  }
  
  case object UnarySub extends Unary {
    def f(a: Num) = a match {
      case i:IntNum => makeNum(-i.getVal)
      case _ => makeNum(-a.getDoubleVal)
    }
    def f(exp: Exp) = -exp 
  }
}
