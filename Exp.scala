object Expression {
  import Function._

  class EvalException(s: String) extends Exception {
    def printError = println(s)
  }

  def makeUnaryApp(f: Fun, left: Exp): Exp = App(f, left, None)
  def makeBinaryApp(f: Fun, left: Exp, right: Exp): Exp = App(f, left, Some(right))

  object T {
    var table = new collection.mutable.HashMap[Var, Num]
    def addVar(key: Var, v: Num) = (table += ((key, v)))
    def addVar(key: String, v: Num) = (table += ((Var(key), v)))
    var lastInput: Exp = IntNum(0)
  }

  object BuiltIns {
    var table = new collection.mutable.HashMap[String, (Num) => Num]
    def addBuiltin(key: String, f: (Num) => Num) = (table += ((key, f)))
  }

  abstract class Exp {
    def eval: Num
    def +(other: Exp): Exp = makeBinaryApp(Add, this, other)
    def -(other: Exp): Exp = makeBinaryApp(Sub, this, other)
    def *(other: Exp): Exp = makeBinaryApp(Mul, this, other)
    def /(other: Exp): Exp = makeBinaryApp(Div, this, other)
    def **(other: Exp): Exp = makeBinaryApp(Pow, this, other)
    def unary_- = makeUnaryApp(UnarySub, this)
  }

  abstract class Num extends Exp {
    def eval = this
    def getDoubleVal: Double
  }

  case class IntNum(n: Int) extends Num {
    override def toString = n.toString
    def getVal: Int = n
    def getDoubleVal = n
  }

  case class DoubleNum(n: Double) extends Num {
    override def toString = n.toString
    def getVal: Double = n
    def getDoubleVal = n
  }

  case class Var(s: String) extends Exp {
    def eval = T.table.get(this) match {
      case None => throw new EvalException("Variable " + this.s + " is missing.")
      case Some(n) => n
    }
  }

  case class App(fun: Fun, left: Exp, right: Option[Exp]) extends Exp {
    def eval = fun match {
      case un: Unary => un.f(left.eval)
      case bin: Binary => bin.f(left.eval, right.get.eval)
    }
  }
}
