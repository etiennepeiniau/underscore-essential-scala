object Exercises {

  final case class Pair[A, B](one: A, two: B)

  val pair = Pair("hi", 2)
  pair.one
  pair.two

  sealed trait Sum[A, B] {
    def fold[C](left: A => C)(right: B => C): C = this match {
      case Left(value) => left(value)
      case Right(value) => right(value)
    }
  }

  final case class Left[A, B](value: A) extends Sum[A, B]

  final case class Right[A, B](value: B) extends Sum[A, B]

  Left[Int, String](1).value
  Right[Int, String]("foo").value

  val sum: Sum[Int, String] = Right("foo")
  sum match {
    case Left(x) => x.toString
    case Right(x) => x
  }

  sealed trait Maybe[A] {
    def fold[B](end: B)(f: A => B): B = this match {
      case Empty() => end
      case Full(value) => f(value)
    }
  }

  final case class Empty[A]() extends Maybe[A]

  final case class Full[A](value: A) extends Maybe[A]

  val empty: Maybe[Int] = Empty[Int]
  val full: Maybe[Int] = Full(1)
}