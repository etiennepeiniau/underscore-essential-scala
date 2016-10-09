
object Exercice {

  sealed trait Tree[A] {

    def fold[B](f: A => B)(g: (B, B) => B): B = this match {
      case Leaf(value) => f(value)
      case Node(left, right) => g(left.fold(f)(g), right.fold(f)(g))
    }

    override def toString: String =
      fold { (value) => value.toString } { (s1, s2) => s1 + " " + s2 }
  }

  final case class Leaf[A](value: A) extends Tree[A]

  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  val tree: Tree[String] =
    Node(Node(Leaf("To"), Leaf("iterate")),
      Node(Node(Leaf("is"), Leaf("human,")),
        Node(Leaf("to"), Node(Leaf("recurse"),
          Node(Leaf("is"), Leaf("divine"))))))

}