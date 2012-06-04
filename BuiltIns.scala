
object BuiltinFunctions {
  import Expression._

  class CalcException(err: String) extends Exception {
  }
  
  def fact(n: Num): Num = n match {
    case i: IntNum => IntNum(fact(i.getVal))
    case _ => throw new CalcException("Double factorial")
  }
  def fact(n: Int): Int = {
    def fact_(n: Int, acc: Int): Int = n match {
      case 0 => acc
      case _ => fact_(n - 1, acc * n)
    }
    if(n >= 0) fact_(n, 1) else throw new CalcException("Negative fact")
  }

    def cos(n: Num): Num = DoubleNum(math.cos(n.getDoubleVal))
    def sin(n: Num): Num = DoubleNum(math.sin(n.getDoubleVal))
    def tan(n: Num): Num = DoubleNum(math.tan(n.getDoubleVal))
    def log(n: Num): Num = DoubleNum(math.log(n.getDoubleVal))
    def sqrt(n: Num): Num = DoubleNum(math.sqrt(n.getDoubleVal))
}
