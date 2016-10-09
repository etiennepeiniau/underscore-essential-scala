object Exercice {

  sealed trait IntList {

    def fold[A](end: A, f: (Int, A) => A): A = this match {
      case End => end
      case Pair(hd, tl) => f(hd, tl.fold(end, f))
    }

    def length: Int = fold[Int](0, (_, tl) => 1 + tl)

    def sum: Int = fold[Int](0, (hd, tl) => hd + tl)

    def product: Int = fold[Int](0, (hd, tl) => hd * tl)

    def double: IntList = fold[IntList](End, (hd, tl) => Pair(hd * 2, tl.double))

  }

  case object End extends IntList

  final case class Pair(head: Int, tail: IntList) extends IntList

}