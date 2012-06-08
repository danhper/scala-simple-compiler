object Statement {
  import Expression._
  
  abstract class Stmt {
    def execute: Unit
  }

  case class FuncDef(name: String, vars: List[Var], stmts: StmtList) extends Stmt {
    def execute = ()
  }

  case class ExprStmt(exp: Exp) extends Stmt {
    def execute = println(exp.eval)
  }

  case class StmtList(stmtList: List[Stmt]) extends Stmt {
    def execute = stmtList foreach (stmt => stmt.execute)
  }

  case object EmptyStmt extends Stmt {
    def execute = ()
  }
}
