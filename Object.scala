import Expression._
import Statement._
import BuiltinFunctions._

abstract class Object extends Exp

abstract class Num extends Object {
  def eval = this
  def getDoubleVal: Double
  def +(that: Num): DoubleNum = DoubleNum(this.getDoubleVal + that.getDoubleVal)
  def -(that: Num): DoubleNum = DoubleNum(this.getDoubleVal - that.getDoubleVal)
  def *(that: Num): DoubleNum = DoubleNum(this.getDoubleVal * that.getDoubleVal)
  def /(that: Num): DoubleNum = DoubleNum(this.getDoubleVal / that.getDoubleVal)
  def **(that: Num): DoubleNum = DoubleNum(math.pow(this.getDoubleVal, that.getDoubleVal))
}

case class IntNum(n: Int) extends Num {
  override def toString = n toString
  def +(that: IntNum): IntNum = IntNum(this.n + that.n)
  def -(that: IntNum): IntNum = IntNum(this.n - that.n)
  def *(that: IntNum): IntNum = IntNum(this.n * that.n)
  def /(that: IntNum): IntNum = IntNum(this.n / that.n)
  def **(that: IntNum): IntNum = IntNum(intPow(this.n, that.n))
  def getVal: Int = n
  def getDoubleVal = n
}

case class DoubleNum(n: Double) extends Num {
  override def toString = n toString
  def getVal: Double = n
  def getDoubleVal = n
}


abstract class Fun(name: String, paramNumber: Int) extends Object {
  def eval = this
  def execute(params: List[Exp]): Object

  def checkParamsNum(params: List[Exp]) = {
    if(params.length != paramNumber) {
      throw new EvalException("function " + name + " expects " + paramNumber + " parameters, " + params.length + " given") 
    }
  }
}

case class BuiltInFun(name: String, paramNumber: Int, fun: (List[Object] => Num)) extends Fun(name, paramNumber) {
  def execute(params: List[Exp]) = {
    checkParamsNum(params)
    fun(params map { exp => exp eval })
  }
}

case class DeclaredFunction(name: String, varList: List[Var], stmts: StmtList) extends Fun(name, varList length) {
  def execute(params: List[Exp]) = {
    checkParamsNum(params)
    StackFrame.startNewFrame
    varList zip(params) foreach {case (v, exp) => StackFrame.addValue(v, exp eval)}
    val ret = stmts.execute
    StackFrame.stopFrame
    ret    
  }
}

case object Null extends Object {
  def eval = Null
}
