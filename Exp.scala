object Expression {
  import Operator._
  import BuiltinFunctions._

  class EvalException(s: String) extends Exception {
    def printError = println(s)
  }

  def makeUnaryApp(f: Op, left: Exp): Exp = App(f, left, None)
  def makeBinaryApp(f: Op, left: Exp, right: Exp): Exp = App(f, left, Some(right))

  object T {
    var table = new collection.mutable.HashMap[Var, Object]
    def addVar(key: Var, v: Object) = (table += ((key, v)))
    def addVar(key: String, v: Object) = (table += ((Var(key), v)))
    var lastInput: Exp = IntNum(0)
  }

  object BuiltIns {
    var table = new collection.mutable.HashMap[String, Fun]
    def addBuiltin(key: String, f: Fun) = (table += ((key, f)))
  }

  abstract class Exp {
    def eval: Object
    def +(other: Exp): Exp = makeBinaryApp(Add, this, other)
    def -(other: Exp): Exp = makeBinaryApp(Sub, this, other)
    def *(other: Exp): Exp = makeBinaryApp(Mul, this, other)
    def /(other: Exp): Exp = makeBinaryApp(Div, this, other)
    def **(other: Exp): Exp = makeBinaryApp(Pow, this, other)
    def unary_- = makeUnaryApp(UnarySub, this)
  }

  case class FunCall(funcName: Var, params: List[Exp]) extends Exp {
    def eval = {
      val value = StackFrame.getValue(funcName) match {
        case f: Fun => f.execute(params)
        case _ => throw new EvalException(funcName + " is not callable")
      }
      value
    }
  }

  case class Var(s: String) extends Exp {
    override def toString = s
    def :=(other: Exp): Exp = makeBinaryApp(DefVar, this, other)
    def eval = StackFrame.getValue(this)
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
