object Expression {
  import Operator._
  import Function._

  class EvalException(s: String) extends Exception {
    def printError = println(s)
  }

  def makeUnaryApp(f: Op, left: Exp): Exp = App(f, left, None)
  def makeBinaryApp(f: Op, left: Exp, right: Exp): Exp = App(f, left, Some(right))

  object T {
    var table = new collection.mutable.HashMap[Var, Num]
    def addVar(key: Var, v: Num) = (table += ((key, v)))
      def addVar(key: String, v: Num) = (table += ((Var(key), v)))
        var lastInput: Exp = IntNum(0)
  }

  object BuiltIns {
    var table = new collection.mutable.HashMap[String, Fun]
    def addBuiltin(key: String, f: Fun) = (table += ((key, f)))
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

  case class FunCall(funcName: String, params: List[Exp]) extends Exp {
    def eval = {
      val value = BuiltIns.table get(funcName) match {
        case Some(f) => f.execute(params)
        case _ => throw new EvalException("function not found")
      }
      value
    }
  }

  case class IntNum(n: Int) extends Num {
    override def toString = n toString
    def getVal: Int = n
    def getDoubleVal = n
  }

  case class DoubleNum(n: Double) extends Num {
    override def toString = n toString
    def getVal: Double = n
    def getDoubleVal = n
  }

  case class Var(s: String) extends Exp {
    def :=(other: Exp): Exp = makeBinaryApp(DefVar, this, other)
    def eval = T.table get(this) match {
      case None => throw new EvalException("Variable " + this.s + " is missing.")
      case Some(n) => n
    }
  }

  case class App(fun: Op, left: Exp, right: Option[Exp]) extends Exp {
    def eval = fun match {
      case un: Unary => un f(left eval)
      case bin: Binary => bin f(left eval, right.get eval)
      case d @ DefVar => left match {
        case v: Var => d.assign(v, right get)
        case _ => throw new EvalException("Wrong assignement")
      }
    }
  }
}
