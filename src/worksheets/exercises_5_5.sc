object Exercises {

  sealed trait LinkedList[A] {
    def map[B](fn: A => B): LinkedList[B] =
      this match {
        case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
        case End() => End[B]()
      }
  }

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  final case class End[A]() extends LinkedList[A]

  val list: LinkedList[Int] = Pair(1, Pair(2, Pair(3, End())))
  list.map(_ * 2)
  list.map(_ / 3)
  list.map(_ + 1)

  sealed trait Maybe[A] {
    def map[B](fn: A => B): Maybe[B] = this match {
      case Full(v) => Full(fn(v))
      case Empty() => Empty[B]()
    }

    def mapByFlat[B](fn: A => B): Maybe[B] =
      flatMap(v => Full(fn(v)))


    def flatMap[B](fn: A => Maybe[B]): Maybe[B] =
      this match {
        case Full(v) => fn(v)
        case Empty() => Empty[B]()
      }
  }

  final case class Full[A](value: A) extends Maybe[A]

  final case class Empty[A]() extends Maybe[A]

  val list2 = List(1, 2, 3)
  list2.flatMap(x => List(x, -x))

  val list3 = List(Full(3), Full(2), Full(1))
  list3.map(maybe => maybe flatMap[Int] { x => if (x % 2 == 0) Full(x) else Empty() })

  sealed trait Sum[A, B] {
    def fold[C](error: A => C, success: B => C): C =
      this match {
        case Failure(a) => error(a)
        case Success(b) => success(b)
      }

    def map[C](f: B => C): Sum[A, C] =
      this match {
        case Success(a) => Success(f(a))
        case Failure(b) => Failure(b)
      }

    def flatMap[C](f: B => Sum[A, C]): Sum[A, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => f(b)
      }
  }

  final case class Failure[A, B](value: A) extends Sum[A, B]

  final case class Success[A, B](value: B) extends Sum[A, B]


}