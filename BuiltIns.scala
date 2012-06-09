object BuiltinFunctions {
  import Expression._
  import Operator._

  class CalcException(err: String) extends Exception 

  implicit def objList2Num(li: List[Object]) = li.head match {
    case n: Num => n
    case _ => throw new TypeException
  }

  def fact(n: List[Object]): Num = n match {
    case i: IntNum => IntNum(fact(i getVal))
    case _ => throw new CalcException("Double factorial")
  }
  def fact(n: Int): Int = {
    def fact_(n: Int, acc: Int): Int = n match {
      case 0 => acc
      case _ => fact_(n - 1, acc * n)
    }
    if(n >= 0) fact_(n, 1) else throw new CalcException("Negative fact")
  }
  
  def cos(n: List[Object]): Num = DoubleNum(math.cos(n.getDoubleVal))
  def sin(n: List[Object]): Num = DoubleNum(math.sin(n.getDoubleVal))
  def tan(n: List[Object]): Num = DoubleNum(math.tan(n.getDoubleVal))
  def log(n: List[Object]): Num = DoubleNum(math.log(n.getDoubleVal))
  def sqrt(n: List[Object]): Num = DoubleNum(math.sqrt(n.getDoubleVal))

  def intPow(x: Int, n: Int): Int = n match {
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
