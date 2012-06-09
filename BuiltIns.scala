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
  def fact(n: List[Object]): Num = n match {
    case i: IntNum => IntNum(fact(i getVal))
    case _ => throw new TypeException(n)
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
  def log(n: List[Object]): Num = DoubleNum(math.log(n.getDoubleVal doubleValue))
  def sqrt(n: List[Object]): Num = DoubleNum(math.sqrt(n.getDoubleVal doubleValue))

  def intPow(x: BigInt, n: BigInt): BigInt = n match {
      case n if n <= 0 => 1
      case n if n % 2 == 0 => val tmp = intPow(x, n / 2); tmp * tmp
      case n if n % 2 == 1 => x * intPow(x, n - 1)
    }

  val cosFun = BuiltInFun("cos", 1, cos)
  val sinFun = BuiltInFun("sin", 1, sin)
  val tanFun = BuiltInFun("tan", 1, tan)
  val logFun = BuiltInFun("log", 1, log)
  val sqrtFun = BuiltInFun("sqrt", 1, sqrt)
  val factFun = BuiltInFun("fact", 1, fact)

  val builtInFuncList = List(cosFun, sinFun, tanFun,
                             logFun, sqrtFun, factFun)
}
