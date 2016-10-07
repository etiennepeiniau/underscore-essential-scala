import scala.annotation.tailrec

object Exercice {

  sealed trait LinkedList[A] {
    @tailrec
    final def length(total: Int = 0): Int = this match {
      case Pair(head, tail) => tail.length(1 + total)
      case End() => total
    }

    @tailrec
    final def contains(value: A, result: Boolean = false): Boolean = this match {
      case Pair(head, tail) => tail.contains(value, result || head == value)
      case End() => result
    }

    def apply(index: Int): Result[A] = this match {
      case Pair(head, tail) => if (index == 0) Success(head) else tail(index - 1)
      case End() => Failure("Index out of bounds")
    }
  }

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  final case class End[A]() extends LinkedList[A]

  sealed trait Result[A]

  case class Success[A](result: A) extends Result[A]

  case class Failure[A](reason: String) extends Result[A]

  val example = Pair(1, Pair(2, Pair(3, End())))
  assert(example.length() == 3)
  assert(example.tail.length() == 2)
  assert(End().length() == 0)

  assert(example.contains(3))
  assert(!example.contains(4))
  assert(!End().contains(0))

  assert(example(0) == Success(1))
  assert(example(1) == Success(2))
  assert(example(2) == Success(3))
  assert(example(3) == Failure("Index out of bounds"))

}