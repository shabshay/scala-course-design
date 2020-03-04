package calculator

import scala.collection.mutable

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    val mapExp: mutable.Set[(String, Signal[Double])] = scala.collection.mutable.Set[(String, Signal[Double])]()

    for ((expName, expSignal) <- namedExpressions) {
      val expVal: Signal[Double] = Var(eval(expSignal(), namedExpressions))
      val expKeyVal: (String, Signal[Double]) = (expName, expVal)

      mapExp += expKeyVal
    }

    mapExp.toMap
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    val currentRefs: mutable.Set[String] = scala.collection.mutable.Set[String]()

    def evalAcc(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
      expr match {
        case e: Literal => e.v

        case e: Plus =>
          evalAcc(e.a, references) + evalAcc(e.b, references)

        case e: Minus =>
          evalAcc(e.a, references) - evalAcc(e.b, references)

        case e: Times =>
          evalAcc(e.a, references) * evalAcc(e.b, references)

        case e: Divide =>
          evalAcc(e.a, references) / evalAcc(e.b, references)

        case e: Ref =>
          if (currentRefs.contains(e.name)) {
            Double.NaN
          }
          else {
            val refExp = getReferenceExpr(e.name, references)
            currentRefs += e.name
            val res: Double = evalAcc(refExp, references)
            currentRefs -= e.name
            res
          }
      }
    }

    evalAcc(expr, references)
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
