object Exercices {

  object IntImplicits {

    implicit class IntOps(val i: Int) {

      def yeah(): Unit = times((i) => println("Oh yeah!"))

      def times(f: Int => Unit): Unit = if (i > 0) (1 to i).foreach(f)
    }

  }

  import IntImplicits._

  2.yeah
  3.yeah
  (-1).yeah

  3.times(i => println(s"Look - it's the number $i!"))

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }

  object Equal {
    def apply[A](implicit instance: Equal[A]) = instance

    implicit class ToEqual[A](a: A) {
      def ===(b: A): Boolean = Equal[A].equal(a, b)
    }
  }

  import Equal._

  implicit val caseInsensitiveEquals =  new Equal[String] {
    override def equal(v1: String, v2: String): Boolean = v1.toLowerCase == v2.toLowerCase
  }

  "abcd" === "ABCD"

}