object Exercises {

  case class Person(name: String, email: String)

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }

  object Equal {

    def apply[A](implicit equal: Equal[A]) = equal

    implicit object EmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email
    }

    implicit object NameEmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email && v1.name == v2.name
    }

  }

  object Eq {
    def apply[A](v1: A, v2: A)(implicit equal: Equal[A]): Boolean = {
      equal.equal(v1, v2)
    }
  }

  object EqTests {
    def byNameAndEmail = {
      import Equal.NameEmailEqual
      Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
    }

    def byEmail = {
      import Equal.EmailEqual
      Eq(Person("Noel", "noel@example.com"), Person("Dave", "noel@example.com"))
    }
  }

  object EqualTests {
    def byNameAndEmail = {
      import Equal.NameEmailEqual
      Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
    }

    def byEmail = {
      import Equal.EmailEqual
      Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
    }
  }

}