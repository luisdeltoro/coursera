package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues(s => Signal(eval(s(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalWithCyclicCheck(expr: Expr, references: Map[String, Signal[Expr]], vars: Set[String]): Double = {
      expr match {
        case Literal(num) => num
        case Ref(variable) if (references.keys exists (_ == variable)) && !(vars contains variable) => evalWithCyclicCheck(references(variable)(), references, vars + variable)
        case Plus(a, b) => evalWithCyclicCheck(a, references, vars) + evalWithCyclicCheck(b, references, vars)
        case Minus(a, b) => evalWithCyclicCheck(a, references, vars) - evalWithCyclicCheck(b, references, vars)
        case Times(a, b) => evalWithCyclicCheck(a, references, vars) * evalWithCyclicCheck(b, references, vars)
        case Divide(a, b) => evalWithCyclicCheck(a, references, vars) / evalWithCyclicCheck(b, references, vars)
        case _ => Double.NaN
      }
    }
    evalWithCyclicCheck(expr, references, Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
