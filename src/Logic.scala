object CompOperator {
  implicit def toBoolean(b: Bool): Boolean = b match {
    case True => true
    case False => false
  }

  implicit def toBool(b: Boolean): Bool = b match {
    case true => True
    case false => False
  }

  implicit def objectToBool(o: Object): Bool = o match {
    case n: IntNum => n.getVal != 0
    case n: DoubleNum => n.getVal != 0
    case Null => False
    case b: Bool => b
    case f: Fun => throw new TypeException(f)
    case _ => throw new EvalException("Unknown type " + o)
  }
}

import CompOperator._

abstract class LogicOperator {
  def fun(a: Bool, b: Option[Bool]): Bool
}

case object Or extends LogicOperator {
  def fun(a: Bool, b: Option[Bool]): Bool = fun(a, b get)
  def fun(a: Bool, b: Bool): Bool = (a, b) match {
    case (False, False) => False
    case (_, _) => True
  }
}

case object And extends LogicOperator {
  def fun(a: Bool, b: Option[Bool]): Bool = fun(a, b get)
  def fun(a: Bool, b: Bool): Bool = (a, b) match {
    case (True, True) => True
    case (_, _) => False
  }
}

case object Not extends LogicOperator {
  def fun(a: Bool, b: Option[Bool]): Bool = fun(a)
  def fun(a: Bool): Bool = a match {
    case True => False
    case False => True
  }
}


abstract class CompOperator {
  def fun(a: Object, b: Object): Bool
}

case object Eq extends CompOperator {
  def fun(a: Object, b: Object) = (a, b) match {
    case (IntNum(x), IntNum(y)) => x == y
    case (DoubleNum(x), DoubleNum(y)) => x == y
    case (BuiltInFun(s, n, _), BuiltInFun(s2, n2, _)) => s == s2 && n == n2
    case (DeclaredFunction(s, v, e), DeclaredFunction(s2, v2, e2)) => s == s2 && v == v2 && e == e2
    case (i: Bool, j: Bool) => i == j
    case (Null, Null) => True
    case (_, _) => False
  }
}

case object Neq extends CompOperator {
  def fun(a: Object, b: Object) = Not.fun(Eq.fun(a, b))
}

case object Lt extends CompOperator {
  def fun(a: Object, b: Object) = (a, b) match {
    case (IntNum(x), IntNum(y)) => x < y
    case (x: Num, y: Num) => x.getDoubleVal < y.getDoubleVal
    case (x, y) => throw new TypeException(x, Some(y))
  }
}

case object Lte extends CompOperator { 
  def fun(a: Object, b: Object): Bool = Or.fun(Eq.fun(a, b), Lt.fun(a, b))
}

case object Gt extends CompOperator {
  def fun(a: Object, b: Object): Bool = Not.fun(Lte.fun(a, b))
}

case object Gte extends CompOperator {
  def fun(a: Object, b: Object): Bool = Not.fun(Lt.fun(a, b))
}
