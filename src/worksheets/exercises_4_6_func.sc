object Exercises {

  sealed trait Expression {
    def eval: Calculation = this match {
      case Addition(left, right) => binaryEval(left, right)((l, r) => Success(l + r))
      case Subtraction(left, right) => binaryEval(left, right)((l, r) => Success(l - r))
      case Division(left, right) => binaryEval(left, right)((l, r) => if (r == 0) Failure("Division by zero") else Success(l / r))
      case SquareRoot(value) => value.eval match {
        case Failure(reason) => Failure(reason)
        case Success(result) => if (result >= 0) Success(Math.sqrt(result)) else Failure("Square root of negative number")
      }
      case Number(value) => Success(value)
    }

    private def binaryEval(left: Expression, right: Expression)(f: (Double, Double) => Calculation): Calculation = left.eval match {
      case Failure(reason) => Failure(reason)
      case Success(leftResult) => right.eval match {
        case Failure(reason) => Failure(reason)
        case Success(rightResult) => f(leftResult, rightResult)
      }
    }
  }

  final case class Addition(left: Expression, right: Expression) extends Expression

  final case class Subtraction(left: Expression, right: Expression) extends Expression

  final case class Division(left: Expression, right: Expression) extends Expression

  final case class SquareRoot(value: Expression) extends Expression

  final case class Number(value: Double) extends Expression

  sealed trait Calculation

  final case class Success(result: Double) extends Calculation

  final case class Failure(reason: String) extends Calculation

  assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure("Square root of negative number"))
  assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
  assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))

}