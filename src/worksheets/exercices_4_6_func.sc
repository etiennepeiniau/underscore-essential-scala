object Excercice {

  sealed trait IntList {
    def length: Int = this match {
      case End => 0
      case Pair(_, tail) => 1 + tail.length
    }

    def product: Int = this match {
      case End => 1
      case Pair(head, tail) => head * tail.product
    }

    def double: IntList = this match {
      case End => End
      case Pair(head, tail) => Pair(head * 2, tail.double)
    }
  }

  case object End extends IntList

  final case class Pair(head: Int, tail: IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, End)))

  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)

  assert(example.product == 6)
  assert(example.tail.product == 6)
  assert(End.product == 1)

  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  assert(example.tail.double == Pair(4, Pair(6, End)))
  assert(End.double == End)

  sealed trait Tree {

    def sum: Int

    def double: Tree
  }

  object TreeOps {
    def sum(tree: Tree): Int = tree match {
      case Leaf(value) => value
      case Node(left, right) => left.sum + right.sum
    }

    def double(tree: Tree): Tree = tree match {
      case Leaf(value) => Leaf(value * 2)
      case Node(left, right) => Node(left.double, right.double)
    }
  }

  final case class Leaf(value: Int) extends Tree {
    override def sum: Int = value

    override def double: Tree = Leaf(value * 2)
  }

  final case class Node(left: Tree, right: Tree) extends Tree {
    override def sum: Int = left.sum + right.sum

    override def double: Tree = Node(left.double, right.double)
  }

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