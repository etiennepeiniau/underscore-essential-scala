object Exercises {

  sealed trait List[A] {

    def fold[B](end: B)(f: (A, B) => B): B = this match {
      case End() => end
      case Cons(hd, tl) => f(hd, tl.fold(end)(f))
    }

    def length: Int =
      fold(0) { (_, tl) => 1 + tl }

    def contains(value: A): Boolean =
      fold(false) { (hd, tl) => hd == value || tl }

    def apply(index: Int): Result[A] =
    // TODO
    //fold[Result[A]](Failure("Index out of bounds"), (hd, tl) => if (index == 0) Success(hd) else tl)
      this match {
        case Cons(head, tail) => if (index == 0) Success(head) else tail(index - 1)
        case End() => Failure("Index out of bounds")
      }

  }

  final case class End[A]() extends List[A]

  final case class Cons[A](head: A, tail: List[A]) extends List[A]

  sealed trait Result[A]

  case class Success[A](result: A) extends Result[A]

  case class Failure[A](reason: String) extends Result[A]

  val example = Cons(1, Cons(2, Cons(3, End())))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)

  assert(example.contains(3))
  assert(!example.contains(4))
  assert(!End().contains(0))

  assert(example(0) == Success(1))
  assert(example(1) == Success(2))
  assert(example(2) == Success(3))
  assert(example(3) == Failure("Index out of bounds"))

}