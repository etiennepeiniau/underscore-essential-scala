
object Exercises {

  sealed trait Sum[+A, +B] {
    def fold[C](error: A => C)(success: B => C): C =
      this match {
        case Failure(a) => error(a)
        case Success(b) => success(b)
      }

    def map[C](f: B => C): Sum[A, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => Success(f(b))
      }

    def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => f(b)
      }
  }

  final case class Failure[A, B](value: A) extends Sum[A, Nothing]

  final case class Success[A, B](value: B) extends Sum[Nothing, B]

  // Abstract syntax tree

  sealed trait Expression {
    def eval: Sum[String, Double]
  }

  final case class Addition(left: Expression, right: Expression) extends Expression {
    def eval: Sum[String, Double] = left.eval.flatMap(x => right.eval.map(y => x + y))
  }

  final case class Subtraction(left: Expression, right: Expression) extends Expression {
    def eval: Sum[String, Double] = left.eval.flatMap(x => right.eval.map(y => x - y))
  }

  final case class Division(left: Expression, right: Expression) extends Expression {
    def eval: Sum[String, Double] = right.eval.flatMap(x => if (x == 0) Failure("Division by zero") else left.eval.map(y => y / x))
  }

  final case class SquareRoot(value: Expression) extends Expression {
    def eval: Sum[String, Double] = value.eval.flatMap(x => if (x < 0) Failure("Square root of negative number") else Success(Math.sqrt(x)))
  }

  final case class Number(value: Double) extends Expression {
    def eval: Sum[String, Double] = Success(value)
  }

  assert(Addition(Number(1), Number(2)).eval == Success(3))
  assert(SquareRoot(Number(-1)).eval == Failure("Square root of negative number"))
  assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
  assert(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval == Success(2.0))

}