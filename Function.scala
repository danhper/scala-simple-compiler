object Function {
  import Expression._
  import Statement._

  abstract class Fun(name: String, paramNumber: Int) {
    def execute(params: List[Exp]): Num

    def checkParamsNum(params: List[Exp]) = {
      if(params.length != paramNumber) {
        throw new EvalException("function " + name + " expects " + paramNumber + " parameters, " + params.length + " given") 
      }
    }
  }

  case class BuiltInFun(name: String, paramNumber: Int, fun: (List[Num] => Num)) extends Fun(name, paramNumber) {
    def execute(params: List[Exp]) = {
      checkParamsNum(params)
      fun(params map { exp => exp eval })
    }
  }

  case class DeclaredFunction(name: String, paramNumber: Int, stmts: StmtList) extends Fun(name, paramNumber) {
    def execute(params: List[Exp]) = {
      checkParamsNum(params)
      IntNum(0)
    }
  }
}
