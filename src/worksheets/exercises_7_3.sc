object Exercises {

  trait Equal[A] {
    def equal(x: A, y: A): Boolean
  }

  case class Person(name: String, email: String)

  object EmailEqual extends Equal[Person] {
    def equal(v1: Person, v2: Person): Boolean =
      v1.email == v2.email
  }

  object NameEmailEqual extends Equal[Person] {
    def equal(v1: Person, v2: Person): Boolean =
      v1.email == v2.email && v1.name == v2.name
  }

}