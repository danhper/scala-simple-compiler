import java.math.{MathContext=>jMC} 
import java.math.{RoundingMode=>jRM} 

/**
 * Object containing program builtins functions
 */
object BuiltinFunctions {
  /**
   * Converts a list of object to a number
   * to use sin, cos, tan with the parameters (List[Object])
   * @param li the list to convert
   * @return Num the converted number
   */
  implicit def objList2Num(li: List[Object]) = li.head match {
    case n: Num => n
    case n => throw new TypeException(n)
  }

  /**
   * Computes the factorial of a number
   * @param List[Object] list of object converted to num
   * @return IntNum
   */
  def fact(li: List[Object]): Num = li.head match {
    case i: IntNum => IntNum(fact(i getVal))
    case n => throw new TypeException(n)
  }
  def fact(n: BigInt): BigInt = {
    def fact_(n: BigInt, acc: BigInt): BigInt = n match {
      case n if n == 0 => acc
      case _ => fact_(n - 1, acc * n)
    }
    if(n >= 0) fact_(n, 1) else throw new CalcException("factorial not defined for negative numbers")
  }
  
  def cos(n: List[Object]): Num = DoubleNum(math.cos(n.getDoubleVal doubleValue))
  def sin(n: List[Object]): Num = DoubleNum(math.sin(n.getDoubleVal doubleValue))
  def tan(n: List[Object]): Num = DoubleNum(math.tan(n.getDoubleVal doubleValue))
  
  def sqrt(n: List[Object]): Num = DoubleNum(doublePow(n.getDoubleVal, 0.5))

  implicit def bigIntToBigDec(n: BigInt) = BigDecimal(n)

  def abs(li: List[Object]) = li.head match {
    case IntNum(b) => IntNum(b abs)
    case DoubleNum(b) => DoubleNum(b abs)
    case n => throw new TypeException(n)
  }
 
  def ceil(li: List[Object]) = li.head match {
    case v: IntNum => v
    case DoubleNum(n) => IntNum(n round(new jMC(1, jRM.CEILING)) toBigInt)
    case n => throw new TypeException(n)
  }

  def round(li: List[Object]) = li.head match {
    case v: IntNum => v
    case DoubleNum(n) => IntNum(n round(new jMC(1)) toBigInt)
    case n => throw new TypeException(n)
  }

  def floor(li: List[Object]) = li.head match {
    case v: IntNum => v
    case DoubleNum(n) => IntNum(n round(new jMC(1, jRM.FLOOR)) toBigInt)
    case n => throw new TypeException(n)
  }

  def intPow(x: BigDecimal, n: BigInt): BigDecimal = n match {
      case n if n <= 0 => 1
      case n if n % 2 == 0 => val tmp = intPow(x, n / 2); tmp * tmp
      case n if n % 2 == 1 => x * intPow(x, n - 1)
    }

  def log(li: List[Object]): Num = li match {
    case (v: Num)::(b: Num)::Nil => DoubleNum(ln(v getDoubleVal) / ln(b getDoubleVal))
    case _ => throw new CalcException("wrong arguments for function log")
  }

  def exp(li: List[Object]): Num = DoubleNum(exp(li getDoubleVal))

  def exp(x: BigDecimal) = {
    var n: BigDecimal = BigDecimal(0)
    for(k <- 0 to 100) {
      n += intPow(x, k) / fact(k)
    }
    n
  }

  def ln(li: List[Object]): Num = DoubleNum(ln(li getDoubleVal))

  def ln(x: BigDecimal) = {
    var n: BigDecimal = BigDecimal(0)
    for(k <- 0 to 200) {
      val p = intPow((x - 1) / (x + 1), 2 * k + 1)
      val q = 1.0 / (2 * k + 1)
      n +=  p * q
    }
    n * 2
  }

  def doublePow(x: BigDecimal, n: BigDecimal) = exp(n * ln(x))

  val cosFun = BuiltInFun("cos", 1, cos)
  val sinFun = BuiltInFun("sin", 1, sin)
  val tanFun = BuiltInFun("tan", 1, tan)
  val logFun = BuiltInFun("log", 2, log)
  val sqrtFun = BuiltInFun("sqrt", 1, sqrt)
  val factFun = BuiltInFun("fact", 1, fact)
  val lnFun = BuiltInFun("ln", 1, ln)
  val expFun = BuiltInFun("exp", 1, exp)
  val absFun = BuiltInFun("abs", 1, abs)
  val floorFun = BuiltInFun("floor", 1, floor)
  val ceilFun = BuiltInFun("ceil", 1, ceil)
  val roundFun = BuiltInFun("round", 1, round)

  /*val builtInFuncList = List(cosFun, sinFun, tanFun, expFun,
                             logFun, sqrtFun, factFun, lnFun,
                             absFun, floorFun, ceilFun, roundFun)*/
  val builtInFuncList = List(ceilFun, roundFun, floorFun)
}
