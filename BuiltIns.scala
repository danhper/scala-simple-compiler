object BuiltinFunctions {
  import Expression._
  import Function._

  class CalcException(err: String) extends Exception {
  }

  def fact(n: List[Num]): Num = n match {
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

  implicit def numList2Num(li: List[Num]) = li.head
  
  def cos(n: List[Num]): Num = DoubleNum(math.cos(n.getDoubleVal))
  def sin(n: List[Num]): Num = DoubleNum(math.sin(n.getDoubleVal))
  def tan(n: List[Num]): Num = DoubleNum(math.tan(n.getDoubleVal))
  def log(n: List[Num]): Num = DoubleNum(math.log(n.getDoubleVal))
  def sqrt(n: List[Num]): Num = DoubleNum(math.sqrt(n.getDoubleVal))

  val cosFun = BuiltInFun("cos", 1, cos)
  val sinFun = BuiltInFun("sin", 1, sin)
  val tanFun = BuiltInFun("tan", 1, tan)
  val logFun = BuiltInFun("log", 1, log)
  val sqrtFun = BuiltInFun("sqrt", 1, sqrt)
  val factFun = BuiltInFun("fact", 1, fact)

  val builtInFuncList = List(cosFun, sinFun, tanFun,
                             logFun, sqrtFun, factFun)
}
