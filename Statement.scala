object Statement {
  import Expression._
  
  abstract class Stmt

  case class FuncDef(name: String, vars: List[Var], stmts: StmtList) extends Stmt

  case class ExprStmt(exp: Exp) extends Stmt

  case class StmtList(stmtList: List[Stmt]) extends Stmt
}
