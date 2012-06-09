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

  def intPow(x: BigDecimal, n: BigInt): BigDecimal = n match {
      case n if n <= 0 => 1
      case n if n % 2 == 0 => val tmp = intPow(x, n / 2); tmp * tmp
      case n if n % 2 == 1 => x * intPow(x, n - 1)
    }

  def log(li: List[Object]): Num = li match {
    case (v: Num)::(b: Num)::Nil => DoubleNum(ln(v getDoubleVal) / ln(b getDoubleVal))
    case _ => throw new CalcException("wrong arguments for function log")
  }

  def ln(li: List[Object]): Num = li.head match {
    case i: Num => DoubleNum(ln(i getDoubleVal))
    case n => throw new TypeException(n)
  }

  def exp(li: List[Object]): Num = li.head match {
    case i: Num => DoubleNum(exp(i getDoubleVal))
    case n => throw new TypeException(n)
  }

  def exp(x: BigDecimal) = {
    var n: BigDecimal = BigDecimal(0)
    for(k <- 0 to 500) {
      n += intPow(x, k) / fact(k)
    }
    n
  }

  def ln(x: BigDecimal) = {
    var n: BigDecimal = BigDecimal(0)
    for(k <- 0 to 500) {
      val p = intPow((x - 1) / (x + 1), 2 * k + 1)
      val q = 1.0 / (2 * k + 1)
      n +=  p * q
    }
    2 * n
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

  val builtInFuncList = List(cosFun, sinFun, tanFun, expFun,
                             logFun, sqrtFun, factFun, lnFun)

}
