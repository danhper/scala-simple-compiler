object Statement {
  import Expression._
  
  abstract class Stmt {
    def execute: Object
  }

  case class FuncDef(name: String, vars: List[Var], stmts: StmtList) extends Stmt {
    def execute = {
      val func = new DeclaredFunction(name, vars, stmts)
      StackFrame.addValue(name, func)
      Null
    }
  }

  case class ExprStmt(exp: Exp) extends Stmt {
    def execute = exp eval
  }

  case class StmtList(stmtList: List[Stmt]) extends Stmt {
    def execute = {
      def exec(stmtList: List[Stmt]): Object = stmtList match {
        case Nil => Null
        case head::Nil => head execute
        case head::tail => head execute; exec(tail)
      }
      exec(stmtList)
    }
  }

  case object EmptyStmt extends Stmt {
    def execute = Null
  }
}
