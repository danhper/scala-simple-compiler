object Function {
  import Expression._
  abstract class Fun {
    def execute(params: List[Exp]): Num
  }

  case class BuiltInFun(name: String, paramNumber: Int, fun: (List[Num] => Num)) extends Fun {
    def execute(params: List[Exp]) = {
      if(params.length != paramNumber) {
        throw new EvalException("function " + name + " expects " + paramNumber + " parameters, " + params.length + " given")
      } else {
        fun(params map { exp => exp eval })
      }
    }
  }

  
}
