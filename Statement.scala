object Statement {
  import Expression._
  
  abstract class Stmt

  case class FuncDef(name: String, vars: List[Var], statements: List[Stmt]) extends Stmt

  case class ExprStmt(exp: Exp) extends Stmt
}
